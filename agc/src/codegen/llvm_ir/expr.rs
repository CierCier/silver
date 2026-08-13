use rustc_hash::FxHashMap as HashMap;

use inkwell::AddressSpace;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use inkwell::module::Linkage;
use inkwell::targets::TargetData;
use inkwell::types::StringRadix;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue,
};

use crate::codegen::SilverGenerator;
use crate::codegen::llvm_ir::LlvmIrGenerator;
use crate::codegen::llvm_ir::VarInfo;
use crate::codegen::{CodegenError, CodegenResult};
use crate::lexer::Span;
use crate::parser::ast;
use crate::symbol_table::{CompilerPhase, SymbolKind};

impl<'ctx> LlvmIrGenerator<'ctx> {
    pub(crate) fn create_entry_alloca(
        &self,
        function: FunctionValue<'ctx>,
        name: &str,
        ty: BasicTypeEnum<'ctx>,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let builder = self.context.create_builder();
        let entry = function
            .get_first_basic_block()
            .ok_or_else(|| CodegenError::new("function has no entry block"))?;
        if let Some(first_instr) = entry.get_first_instruction() {
            builder.position_before(&first_instr);
        } else {
            builder.position_at_end(entry);
        }
        builder
            .build_alloca(ty, name)
            .map_err(|e| CodegenError::new(format!("failed to allocate local `{name}`: {e}")))
    }

    pub(crate) fn intern_string_literal(
        &mut self,
        value: &str,
    ) -> CodegenResult<PointerValue<'ctx>> {
        Ok(self.intern_const_string_global(value))
    }

    pub(crate) fn const_cast_value_to_basic_type(
        &self,
        value: BasicValueEnum<'ctx>,
        target: BasicTypeEnum<'ctx>,
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        if value.get_type() == target {
            return Ok(value);
        }

        match (value, target) {
            (BasicValueEnum::IntValue(int_val), BasicTypeEnum::IntType(int_ty)) => Ok(int_ty
                .const_int(
                    int_val
                        .get_sign_extended_constant()
                        .or_else(|| int_val.get_zero_extended_constant().map(|v| v as i64))
                        .ok_or_else(|| {
                            CodegenError::with_span(
                                "expected integer constant in global initializer",
                                *span,
                            )
                        })? as u64,
                    true,
                )
                .as_basic_value_enum()),
            (BasicValueEnum::IntValue(int_val), BasicTypeEnum::FloatType(float_ty)) => Ok(float_ty
                .const_float(
                    int_val
                        .get_sign_extended_constant()
                        .or_else(|| int_val.get_zero_extended_constant().map(|v| v as i64))
                        .ok_or_else(|| {
                            CodegenError::with_span(
                                "expected integer constant in global initializer",
                                *span,
                            )
                        })? as f64,
                )
                .as_basic_value_enum()),
            (BasicValueEnum::FloatValue(float_val), BasicTypeEnum::IntType(int_ty)) => Ok(int_ty
                .const_int(
                    float_val
                        .get_constant()
                        .map(|(value, _)| value as i64)
                        .ok_or_else(|| {
                            CodegenError::with_span(
                                "expected float constant in global initializer",
                                *span,
                            )
                        })? as u64,
                    true,
                )
                .as_basic_value_enum()),
            (BasicValueEnum::IntValue(int_val), BasicTypeEnum::PointerType(ptr_ty)) => {
                // Integer-to-pointer constant cast, valid in global initializers
                // (e.g. a MMIO/video buffer base like `(volatile u8*)0xB8000`).
                let raw: u64 = int_val
                    .get_sign_extended_constant()
                    .or_else(|| int_val.get_zero_extended_constant().map(|v| v as i64))
                    .ok_or_else(|| {
                        CodegenError::with_span(
                            "expected integer constant in global initializer",
                            *span,
                        )
                    })? as u64;
                if raw == 0 {
                    Ok(ptr_ty.const_null().as_basic_value_enum())
                } else {
                    Ok(int_val.const_to_pointer(ptr_ty).as_basic_value_enum())
                }
            }
            (BasicValueEnum::FloatValue(float_val), BasicTypeEnum::FloatType(float_ty)) => {
                Ok(float_ty
                    .const_float(
                        float_val
                            .get_constant()
                            .map(|(value, _)| value)
                            .ok_or_else(|| {
                                CodegenError::with_span(
                                    "expected float constant in global initializer",
                                    *span,
                                )
                            })?,
                    )
                    .as_basic_value_enum())
            }
            (source, _) => Err(CodegenError::with_span(
                format!(
                    "unsupported constant cast from `{}` to `{}`",
                    source.get_type().print_to_string(),
                    target.print_to_string()
                ),
                *span,
            )),
        }
    }

    /// Total element count of a (possibly nested) fixed-size array type.
    fn array_total_len(ty: &ast::Type) -> i64 {
        match ty.kind.as_ref() {
            ast::TypeKind::Array(arr) => arr.size * Self::array_total_len(&arr.element_type),
            _ => 1,
        }
    }

    /// The innermost (scalar) element type of a possibly nested array type.
    fn innermost_element_type(ty: &ast::Type) -> &ast::Type {
        match ty.kind.as_ref() {
            ast::TypeKind::Array(arr) => Self::innermost_element_type(&arr.element_type),
            _ => ty,
        }
    }

    /// Flat index vector for position `k` in a row-major shape `[s0, s1, ..]`
    /// (C layout: `grid[2][3]`, flat 5 → indices [1, 2]).
    fn flat_index_vector(k: usize, shape: &[i64]) -> Vec<u64> {
        let mut idx = vec![0u64; shape.len()];
        let mut rem = k as u64;
        for d in (0..shape.len()).rev() {
            let s = shape[d] as u64;
            idx[d] = rem % s;
            rem /= s;
        }
        idx
    }

    /// Dimension sizes of a (possibly nested) array type, outermost first.
    fn array_shape(ty: &ast::Type) -> Vec<i64> {
        match ty.kind.as_ref() {
            ast::TypeKind::Array(arr) => {
                let mut shape = vec![arr.size];
                shape.extend(Self::array_shape(&arr.element_type));
                shape
            }
            _ => Vec::new(),
        }
    }

    /// Build a constant nested-array value from a flat iterator of element
    /// constants (C-style `{1,2,3,4,5,6}` for `[2][3]`), outermost-first.
    fn build_const_array_flat(
        &mut self,
        ty: &ast::Type,
        values: &mut impl Iterator<Item = BasicValueEnum<'ctx>>,
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match ty.kind.as_ref() {
            ast::TypeKind::Array(arr) => {
                let elem_llvm_ty = self.lower_basic_type(&arr.element_type)?;
                let array_llvm_ty = elem_llvm_ty.array_type(arr.size as u32);
                let mut elems = Vec::with_capacity(arr.size as usize);
                for _ in 0..arr.size {
                    elems.push(self.build_const_array_flat(&arr.element_type, values, span)?);
                }
                Ok(unsafe {
                    inkwell::values::ArrayValue::new_const_array(&array_llvm_ty, &elems)
                        .as_basic_value_enum()
                })
            }
            _ => values.next().ok_or_else(|| {
                CodegenError::with_span("array initializer has too few elements", *span)
            }),
        }
    }

    pub(crate) fn emit_const_initializer_value(
        &mut self,
        items: &[ast::InitializerItem],
        target_type: &ast::Type,
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match target_type.kind.as_ref() {
            ast::TypeKind::Named(named) => {
                let struct_name = Self::named_type_key(named);
                let struct_ty = self.ensure_named_struct_type(named)?;
                let declared_fields =
                    self.struct_fields
                        .get(&struct_name)
                        .cloned()
                        .ok_or_else(|| {
                            CodegenError::with_span(
                                format!("missing field metadata for struct `{struct_name}`"),
                                *span,
                            )
                        })?;
                let named_mode = items
                    .iter()
                    .any(|item| matches!(item, ast::InitializerItem::Field { .. }));
                let mut values = Vec::with_capacity(declared_fields.len());

                if named_mode {
                    let mut by_name: HashMap<String, &ast::Expression> = HashMap::default();
                    for item in items {
                        match item {
                            ast::InitializerItem::Field { name, value } => {
                                if by_name.insert(name.name.clone(), value).is_some() {
                                    return Err(CodegenError::with_span(
                                        format!("duplicate field `{}` in initializer", name.name),
                                        name.span,
                                    ));
                                }
                            }
                            _ => {
                                return Err(CodegenError::with_span(
                                    "cannot mix positional items with named struct initializer",
                                    *span,
                                ));
                            }
                        }
                    }

                    for (field_name, field_ty) in declared_fields {
                        let Some(value_expr) = by_name.get(&field_name) else {
                            return Err(CodegenError::with_span(
                                format!("missing field `{field_name}` in initializer"),
                                *span,
                            ));
                        };
                        values.push(self.emit_const_value_for_type(value_expr, &field_ty)?);
                    }
                } else {
                    if items.len() != declared_fields.len() {
                        return Err(CodegenError::with_span(
                            "positional struct initializer field count mismatch",
                            *span,
                        ));
                    }
                    for (item, (_, field_ty)) in items.iter().zip(declared_fields.iter()) {
                        let ast::InitializerItem::Positional(expr) = item else {
                            return Err(CodegenError::with_span(
                                "struct positional initializer only supports positional items",
                                *span,
                            ));
                        };
                        values.push(self.emit_const_value_for_type(expr, field_ty)?);
                    }
                }

                Ok(struct_ty.const_named_struct(&values).as_basic_value_enum())
            }
            ast::TypeKind::Array(array) => {
                let element_llvm_ty = self.lower_basic_type(&array.element_type)?;
                let array_llvm_ty = element_llvm_ty.array_type(array.size as u32);
                let n = array.size as usize;
                let positional = items
                    .iter()
                    .all(|item| matches!(item, ast::InitializerItem::Positional(_)));
                if positional {
                    // C-style flat initializer for multi-dimensional arrays:
                    // `i32 table[2][3] = {1,2,3,4,5,6}` lays values out across
                    // the nested shape in row-major order (partial lists are
                    // zero-padded). Nested `{{1,2},{3,4}}` lists are handled by
                    // the per-element recursion below.
                    let full_array_ty = || ast::Type {
                        kind: Box::new(ast::TypeKind::Array(array.clone())),
                        span: *span,
                    };
                    let flat_total = Self::array_total_len(&full_array_ty());
                    if matches!(array.element_type.kind.as_ref(), ast::TypeKind::Array(_))
                        && items.len() as i64 != n as i64
                        && (items.len() as i64) <= flat_total
                    {
                        let base_elem = Self::innermost_element_type(&full_array_ty()).clone();
                        let mut values = Vec::with_capacity(flat_total as usize);
                        for item in items {
                            let ast::InitializerItem::Positional(expr) = item else {
                                unreachable!("checked all-positional above")
                            };
                            values.push(self.emit_const_value_for_type(expr, &base_elem)?);
                        }
                        let base_llvm_ty = self.lower_basic_type(&base_elem)?;
                        while (values.len() as i64) < flat_total {
                            values.push(base_llvm_ty.const_zero().as_basic_value_enum());
                        }
                        return self.build_const_array_flat(
                            &full_array_ty(),
                            &mut values.into_iter(),
                            span,
                        );
                    }
                    if items.len() != n {
                        return Err(CodegenError::with_span(
                            format!(
                                "array initializer has {} elements but array size is {}",
                                items.len(),
                                array.size
                            ),
                            *span,
                        ));
                    }
                    let mut values = Vec::with_capacity(n);
                    for item in items {
                        let ast::InitializerItem::Positional(expr) = item else {
                            unreachable!("checked all-positional above")
                        };
                        values.push(self.emit_const_value_for_type(expr, &array.element_type)?);
                    }
                    // `const_array` requires ArrayValue elements; emit_const_value_for_type
                    // returns BasicValueEnum, so build the constant directly
                    // (safe: all elements were typed against element_llvm_ty).
                    Ok(unsafe {
                        inkwell::values::ArrayValue::new_const_array(&array_llvm_ty, &values)
                            .as_basic_value_enum()
                    })
                } else {
                    // Sparse indexed initializer: zero-fill, then overwrite
                    // literal-indexed positions (indices must be constants).
                    let mut values = vec![element_llvm_ty.const_zero(); n];
                    for item in items {
                        let ast::InitializerItem::Index { index, value } = item else {
                            return Err(CodegenError::with_span(
                                "cannot mix positional items with indexed array initializer",
                                *span,
                            ));
                        };
                        let ast::ExpressionKind::Literal(ast::Literal::Integer(idx)) =
                            index.kind.as_ref()
                        else {
                            return Err(CodegenError::with_span(
                                "array initializer index must be a constant integer",
                                index.span,
                            ));
                        };
                        let idx = *idx;
                        if idx < 0 || idx as usize >= n {
                            return Err(CodegenError::with_span(
                                format!(
                                    "array initializer index {idx} out of bounds for size {}",
                                    array.size
                                ),
                                index.span,
                            ));
                        }
                        values[idx as usize] =
                            self.emit_const_value_for_type(value, &array.element_type)?;
                    }
                    // `const_array` requires ArrayValue elements; emit_const_value_for_type
                    // returns BasicValueEnum, so build the constant directly
                    // (safe: all elements were typed against element_llvm_ty).
                    Ok(unsafe {
                        inkwell::values::ArrayValue::new_const_array(&array_llvm_ty, &values)
                            .as_basic_value_enum()
                    })
                }
            }
            ast::TypeKind::Tuple(types) => {
                if items.len() != types.len() {
                    return Err(CodegenError::with_span(
                        "tuple initializer arity mismatch",
                        *span,
                    ));
                }
                let mut values = Vec::with_capacity(types.len());
                for (item, ty) in items.iter().zip(types.iter()) {
                    let ast::InitializerItem::Positional(expr) = item else {
                        return Err(CodegenError::with_span(
                            "tuple initializer only supports positional items",
                            *span,
                        ));
                    };
                    values.push(self.emit_const_value_for_type(expr, ty)?);
                }
                Ok(self
                    .context
                    .const_struct(&values, false)
                    .as_basic_value_enum())
            }
            _ => Err(CodegenError::with_span(
                "initializer is not supported for this global type",
                target_type.span,
            )),
        }
    }

    pub(crate) fn emit_const_value_for_type(
        &mut self,
        expr: &ast::Expression,
        target_type: &ast::Type,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let target = self.lower_basic_type(target_type)?;
        let value = match expr.kind.as_ref() {
            ast::ExpressionKind::Literal(ast::Literal::Integer(value)) => {
                // Emit at the target width; const_int_from_string preserves the
                // full i128 value (const_int truncates through u64).
                match target {
                    BasicTypeEnum::IntType(int_ty) => int_ty
                        .const_int_from_string(&value.to_string(), StringRadix::Decimal)
                        .unwrap_or_else(|| int_ty.const_zero())
                        .as_basic_value_enum(),
                    _ => self
                        .context
                        .i64_type()
                        .const_int(*value as u64, true)
                        .as_basic_value_enum(),
                }
            }
            ast::ExpressionKind::Literal(ast::Literal::Float(value)) => self
                .context
                .f64_type()
                .const_float(*value)
                .as_basic_value_enum(),
            ast::ExpressionKind::Literal(ast::Literal::Bool(value)) => self
                .context
                .bool_type()
                .const_int(u64::from(*value), false)
                .as_basic_value_enum(),
            ast::ExpressionKind::Literal(ast::Literal::Char(value)) => self
                .context
                .i32_type()
                .const_int((*value) as u64, false)
                .as_basic_value_enum(),
            ast::ExpressionKind::Literal(ast::Literal::String(value)) => {
                self.intern_const_string_global(value).as_basic_value_enum()
            }
            ast::ExpressionKind::Unary { operator, operand } => {
                let inner = self.emit_const_value_for_type(operand, target_type)?;
                match (operator, inner) {
                    (ast::UnaryOperator::Plus, value) => value,
                    (ast::UnaryOperator::Minus, BasicValueEnum::IntValue(int_value)) => {
                        int_value.const_neg().as_basic_value_enum()
                    }
                    (ast::UnaryOperator::Minus, BasicValueEnum::FloatValue(float_value)) => self
                        .context
                        .f64_type()
                        .const_float(
                            -float_value
                                .get_constant()
                                .map(|(value, _)| value)
                                .ok_or_else(|| {
                                    CodegenError::with_span(
                                        "expected float constant in global initializer",
                                        expr.span,
                                    )
                                })?,
                        )
                        .as_basic_value_enum(),
                    (ast::UnaryOperator::Not, BasicValueEnum::IntValue(int_value))
                    | (ast::UnaryOperator::BitwiseNot, BasicValueEnum::IntValue(int_value)) => {
                        int_value.const_not().as_basic_value_enum()
                    }
                    _ => {
                        return Err(CodegenError::with_span(
                            "unsupported constant unary operator in global initializer",
                            expr.span,
                        ));
                    }
                }
            }
            ast::ExpressionKind::Cast {
                expression,
                target_type: cast_target,
            } => {
                let inner = self.emit_const_value_for_type(expression, cast_target)?;
                let cast_target = self.lower_basic_type(cast_target)?;
                self.const_cast_value_to_basic_type(inner, cast_target, &expr.span)?
            }
            ast::ExpressionKind::Initializer { items } => {
                return self.emit_const_initializer_value(items, target_type, &expr.span);
            }
            _ => {
                return Err(CodegenError::with_span(
                    "global initializer must be a compile-time constant expression",
                    expr.span,
                ));
            }
        };

        self.const_cast_value_to_basic_type(value, target, &expr.span)
    }

    pub(crate) fn generate_global_variable_item(
        &mut self,
        item: &ast::GlobalVariableItem,
        visibility: &ast::Visibility,
    ) -> CodegenResult<()> {
        let llvm_ty = self.lower_basic_type(&item.var_type)?;
        let global = self
            .module
            .get_global(&item.name.name)
            .unwrap_or_else(|| self.module.add_global(llvm_ty, None, &item.name.name));
        global.set_linkage(if Self::is_private(visibility) {
            Linkage::Internal
        } else {
            Linkage::External
        });
        if item.is_static {
            // `static` on a global is the C-style spelling of internal linkage
            // (the same linkage `private` already provides).
            global.set_linkage(Linkage::Internal);
        }
        global.set_constant(!item.is_mutable);
        let initializer = if let Some(init) = &item.initializer {
            self.emit_const_value_for_type(init, &item.var_type)?
        } else {
            llvm_ty.const_zero()
        };
        global.set_initializer(&initializer);
        if item.is_volatile {
            self.volatile_globals.insert(item.name.name.clone());
        }
        self.global_variables
            .insert(item.name.name.clone(), item.var_type.clone());
        if !item.is_mutable && !item.is_volatile {
            // Record an immutable constant that holds a single integer literal
            // so `__atomic_*` ops can name a constant (e.g. `seq_cst`) and have
            // it folded to a compile-time value, since LLVM atomic instructions
            // require a compile-time ordering.
            if let Some(ast::ExpressionKind::Literal(ast::Literal::Integer(value))) =
                item.initializer.as_ref().map(|init| init.kind.as_ref())
            {
                self.global_const_values
                    .insert(item.name.name.clone(), *value);
            }
        }
        self.symbol_table.intern_symbol(
            format!("codegen::global::{}", item.name.name),
            SymbolKind::GlobalVariable,
            Some(item.name.span),
            CompilerPhase::Codegen,
        );
        Ok(())
    }

    /// True if the lvalue's root identifier names a volatile variable or global.
    /// `FieldAccess` chains (volatile struct variables, e.g. MMIO register
    /// blocks) and `Index` expressions (volatile arrays) are walked to the
    /// root, so element reads/writes/compound-ops/incdec of a volatile array
    /// are all volatile. Accessing through a `volatile T*` pointee (via
    /// `p[i]`, `p.field`, or `*p`) is also volatile, which is how volatile
    /// buffers reached by pointer — function parameters, struct fields, or
    /// globals holding a framebuffer address — get volatile semantics.
    /// Accesses that codegen through method calls (__index_get/__index_set,
    /// value receivers, for-in caches) are NOT volatile; only loads/stores of
    /// the variable's own storage or through a volatile pointee are guaranteed
    /// volatile.
    pub(crate) fn lvalue_is_volatile(&mut self, expr: &ast::Expression) -> bool {
        // Volatile pointee: any step of the lvalue chain that dereferences or
        // indexes a `volatile T*` makes the whole access volatile.
        let mut root = expr;
        loop {
            match root.kind.as_ref() {
                ast::ExpressionKind::FieldAccess { object, .. }
                | ast::ExpressionKind::Index { object, .. } => {
                    if self.pointee_is_volatile(object) {
                        return true;
                    }
                    root = object;
                }
                ast::ExpressionKind::Unary {
                    operator: ast::UnaryOperator::Dereference,
                    operand,
                } => {
                    if self.pointee_is_volatile(operand) {
                        return true;
                    }
                    root = operand;
                }
                _ => break,
            }
        }
        // Declaration-level volatility of the root variable/global.
        let ast::ExpressionKind::Identifier(ident) = root.kind.as_ref() else {
            return false;
        };
        if let Some(info) = self.lookup_variable(&ident.name) {
            return info.is_volatile;
        }
        self.volatile_globals.contains(&ident.name)
    }

    /// True if `expr`'s type is a `volatile T*` (pointee volatile).
    fn pointee_is_volatile(&mut self, expr: &ast::Expression) -> bool {
        self.resolve_receiver_type(expr).is_some_and(
            |ty| matches!(ty.kind.as_ref(), ast::TypeKind::Pointer(p) if p.is_volatile),
        )
    }

    /// build_load + as_instruction_value() + set_volatile(true).
    pub(crate) fn emit_volatile_load(
        &self,
        llvm_ty: BasicTypeEnum<'ctx>,
        ptr: PointerValue<'ctx>,
        name: &str,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let value = self
            .builder
            .build_load(llvm_ty, ptr, name)
            .map_err(|e| CodegenError::new(format!("volatile load failed: {e}")))?;
        if let Some(instr) = value.as_instruction_value() {
            instr
                .set_volatile(true)
                .map_err(|e| CodegenError::new(format!("volatile load failed: {e}")))?;
        }
        Ok(value)
    }

    /// build_store + set_volatile(true).
    pub(crate) fn emit_volatile_store(
        &self,
        ptr: PointerValue<'ctx>,
        value: BasicValueEnum<'ctx>,
    ) -> CodegenResult<()> {
        self.builder
            .build_store(ptr, value)
            .map_err(|e| CodegenError::new(format!("volatile store failed: {e}")))?
            .set_volatile(true)
            .map_err(|e| CodegenError::new(format!("volatile store failed: {e}")))?;
        Ok(())
    }

    pub(crate) fn emit_expression_value(
        &mut self,
        expr: &ast::Expression,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        self.set_debug_location(&expr.span);
        match expr.kind.as_ref() {
            ast::ExpressionKind::Literal(ast::Literal::Integer(value)) => {
                if *value >= i64::MIN as i128 && *value <= i64::MAX as i128 {
                    Ok(self
                        .context
                        .i64_type()
                        .const_int(*value as u64, true)
                        .as_basic_value_enum())
                } else {
                    // Values beyond i64 need their full width preserved:
                    // const_int truncates through u64. Emit as i128 and let
                    // the enclosing cast/let narrow to the expected type.
                    self.context
                        .i128_type()
                        .const_int_from_string(&value.to_string(), StringRadix::Decimal)
                        .map(|v| v.as_basic_value_enum())
                        .ok_or_else(|| {
                            CodegenError::with_span("invalid integer literal", expr.span)
                        })
                }
            }
            ast::ExpressionKind::Literal(ast::Literal::Float(value)) => Ok(self
                .context
                .f64_type()
                .const_float(*value)
                .as_basic_value_enum()),
            ast::ExpressionKind::Literal(ast::Literal::Bool(value)) => Ok(self
                .context
                .bool_type()
                .const_int(u64::from(*value), false)
                .as_basic_value_enum()),
            ast::ExpressionKind::Literal(ast::Literal::Char(value)) => Ok(self
                .context
                .i32_type()
                .const_int((*value) as u64, false)
                .as_basic_value_enum()),
            ast::ExpressionKind::Literal(ast::Literal::String(value)) => self
                .intern_string_literal(value)
                .map(|ptr| ptr.as_basic_value_enum()),
            ast::ExpressionKind::Identifier(identifier) => {
                if let Some((ptr, ty)) = self.lookup_storage(&identifier.name) {
                    // Array-to-pointer decay: emit pointer to first element,
                    // not a load of the whole array value.
                    if matches!(ty.kind.as_ref(), ast::TypeKind::Array(_)) {
                        let array_llvm_ty = self.lower_basic_type(&ty)?;
                        let zero = self.context.i32_type().const_zero();
                        let element_ptr = unsafe {
                            self.builder.build_in_bounds_gep(
                                array_llvm_ty,
                                ptr,
                                &[zero, zero],
                                format!("{}.ptr", identifier.name).as_str(),
                            )
                        }
                        .map_err(|e| {
                            CodegenError::with_span(
                                format!("failed to get array pointer `{}`: {e}", identifier.name),
                                identifier.span,
                            )
                        })?;
                        return Ok(element_ptr.as_basic_value_enum());
                    }
                    let llvm_ty = self.lower_basic_type(&ty)?;
                    if self.lvalue_is_volatile(expr) {
                        return self.emit_volatile_load(llvm_ty, ptr, &identifier.name);
                    }
                    return self
                        .builder
                        .build_load(llvm_ty, ptr, &identifier.name)
                        .map_err(|e| {
                            CodegenError::with_span(
                                format!("failed to load variable `{}`: {e}", identifier.name),
                                identifier.span,
                            )
                        });
                }
                let name = &identifier.name;
                let mut func = self.module.get_function(name);
                if func.is_none()
                    && let Some(mangled) = self.imported_function_links.get(name)
                {
                    func = self.module.get_function(mangled);
                }
                if let Some(f) = func {
                    return Ok(f.as_global_value().as_pointer_value().as_basic_value_enum());
                }
                Err(CodegenError::with_span(
                    format!("unknown variable `{}`", identifier.name),
                    identifier.span,
                ))
            }
            ast::ExpressionKind::FieldAccess { object, field } => {
                // Unit-variant construction without parens: `OptInt.None` or
                // `Optional.None` (the latter has a TypeName object because
                // `Optional` lexes as a dedicated type token).
                let enum_name = match object.kind.as_ref() {
                    ast::ExpressionKind::Identifier(owner)
                        if self
                            .enum_member_constant(&owner.name, &field.name)
                            .is_some() =>
                    {
                        Some(owner.name.clone())
                    }
                    ast::ExpressionKind::TypeName(ty) => {
                        if let ast::TypeKind::Named(named) = ty.kind.as_ref()
                            && named.path.len() == 1
                            && self
                                .enum_member_constant(&named.path[0].name, &field.name)
                                .is_some()
                        {
                            // Prefer the monomorphized concrete instantiation
                            // (e.g. `Optional__i32` for `Optional<i32>`) so the
                            // payload layout matches the concrete type.
                            let monomorph = Self::monomorph_owner_name_from_named(named);
                            if self.enum_payload_layouts.contains_key(&monomorph)
                                || self.enum_variants.contains_key(&monomorph)
                            {
                                Some(monomorph)
                            } else {
                                Some(named.path[0].name.clone())
                            }
                        } else {
                            None
                        }
                    }
                    _ => None,
                };
                if let Some(owner_name) = enum_name {
                    let owner = ast::Identifier {
                        name: owner_name,
                        span: object.span,
                    };
                    if let Some(_struct_ty) = self.enum_payload_layouts.get(&owner.name) {
                        return self.emit_enum_construction_impl(&owner, field, &[], &expr.span);
                    }
                    if let Some(value) = self.enum_member_constant(&owner.name, &field.name) {
                        return Ok(value);
                    }
                }
                let (ptr, ty) = self.resolve_lvalue_ptr(expr)?;
                let llvm_ty = self.lower_basic_type(&ty)?;
                if self.lvalue_is_volatile(expr) {
                    return self.emit_volatile_load(llvm_ty, ptr, "lvalue.load");
                }
                self.builder
                    .build_load(llvm_ty, ptr, "lvalue.load")
                    .map_err(|e| {
                        CodegenError::with_span(format!("failed to load lvalue: {e}"), expr.span)
                    })
            }
            ast::ExpressionKind::Binary {
                left,
                operator,
                right,
            } => self.emit_binary_expression(left, operator, right, expr),
            ast::ExpressionKind::Unary { operator, operand } => {
                self.emit_unary_expression(operator, operand, expr)
            }
            ast::ExpressionKind::Postfix { operator, operand } => {
                self.emit_postfix_expression(operator, operand, expr)
            }
            ast::ExpressionKind::Call {
                function,
                arguments,
            } => {
                let value = self.emit_call_expression(function, arguments, false, &expr.span)?;
                value.ok_or_else(|| {
                    CodegenError::with_span(
                        "void function call cannot be used as a value",
                        expr.span,
                    )
                })
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                method,
                arguments,
            } => {
                // Check if this is an enum variant construction: either a bare
                // type-name receiver (`Optional.Some(...)`, `Box2<i32>.Full(...)`)
                // or an identifier naming an enum (`OptInt.Some(42)`).
                let enum_receiver = match receiver.kind.as_ref() {
                    ast::ExpressionKind::Identifier(receiver_ident) => {
                        if self.enum_variants.contains_key(&receiver_ident.name)
                            && self
                                .enum_variants
                                .get(&receiver_ident.name)
                                .is_some_and(|variants| variants.contains_key(&method.name))
                        {
                            Some(receiver_ident.name.clone())
                        } else {
                            None
                        }
                    }
                    ast::ExpressionKind::TypeName(ty) => {
                        if let ast::TypeKind::Named(named) = ty.kind.as_ref() {
                            let base = Self::named_type_name(named);
                            // For a generic enum the monomorphized concrete
                            // instantiation (e.g. `Box2__i32`) is the registered
                            // layout; fall back to the bare name for non-generic
                            // enums (`Optional`, `OptInt`). Only route to
                            // construction when the method names a real variant.
                            let monomorph = Self::monomorph_owner_name_from_named(named);
                            let is_variant = |candidate: &str| {
                                self.enum_variants
                                    .get(candidate)
                                    .is_some_and(|variants| variants.contains_key(&method.name))
                            };
                            if is_variant(&monomorph) {
                                Some(monomorph)
                            } else if is_variant(&base) {
                                Some(base)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    }
                    _ => None,
                };
                if let Some(enum_name) = enum_receiver {
                    let receiver_ident = ast::Identifier {
                        name: enum_name,
                        span: receiver.span,
                    };
                    return self.emit_enum_construction_impl(
                        &receiver_ident,
                        method,
                        arguments,
                        &expr.span,
                    );
                }
                let value = self
                    .emit_method_call_expression(receiver, method, arguments, false, &expr.span)?;
                value.ok_or_else(|| {
                    CodegenError::with_span("void method call cannot be used as a value", expr.span)
                })
            }
            ast::ExpressionKind::Cast {
                expression,
                target_type,
            } => {
                let source = self.emit_expression_value(expression)?;

                let cast_method_name = Self::cast_method_name(target_type);
                let owners = self.receiver_owner_candidates(expression);
                let found = owners.iter().find_map(|owner| {
                    let name = Self::mangle_method_name(owner, &cast_method_name);
                    self.module.get_function(&name)
                });

                if let Some(cast_fn) = found {
                    let args = vec![BasicMetadataValueEnum::from(source)];
                    let call = self
                        .builder
                        .build_call(cast_fn, &args, "cast.result")
                        .map_err(|e| {
                            CodegenError::with_span(
                                format!("failed to call user-defined cast: {e}"),
                                expr.span,
                            )
                        })?;
                    call.try_as_basic_value().basic().ok_or_else(|| {
                        CodegenError::with_span(
                            "user-defined cast returned void".to_string(),
                            expr.span,
                        )
                    })
                } else {
                    if self.expression_is_unsigned(expression) {
                        self.cast_unsigned_value_to_ast_type(source, target_type, &expr.span)
                    } else {
                        self.cast_value_to_ast_type(source, target_type, &expr.span)
                    }
                }
            }
            ast::ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let Some(else_branch) = else_branch.as_ref() else {
                    return Err(CodegenError::with_span(
                        "if expression requires an else branch",
                        expr.span,
                    ));
                };
                self.emit_if_expression_value(condition, then_branch, else_branch, &expr.span)
            }
            ast::ExpressionKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                // Reuse the value-if machinery: wrap each branch in a
                // single-expression block, then phi-merge the results.
                let then_block = ast::Block {
                    statements: vec![ast::Statement {
                        kind: ast::StatementKind::Expression((**then_expr).clone()),
                        span: then_expr.span,
                    }],
                    span: then_expr.span,
                };
                let else_block = ast::Block {
                    statements: vec![ast::Statement {
                        kind: ast::StatementKind::Expression((**else_expr).clone()),
                        span: else_expr.span,
                    }],
                    span: else_expr.span,
                };
                self.emit_if_expression_value(condition, &then_block, &else_block, &expr.span)
            }
            ast::ExpressionKind::Match { expression, arms } => {
                self.emit_match_expression_value(expression, arms, &expr.span)
            }
            ast::ExpressionKind::StructLiteral { path, fields } => {
                self.emit_struct_literal_value(path, fields, &expr.span)
            }
            ast::ExpressionKind::Array(items) => self.emit_array_literal_value(items, &expr.span),
            ast::ExpressionKind::Tuple(items) => self.emit_tuple_literal_value(items, &expr.span),
            ast::ExpressionKind::Initializer { .. } => Err(CodegenError::with_span(
                "initializer expression requires a target type context",
                expr.span,
            )),
            ast::ExpressionKind::Index { object, index } => {
                // For pointer types, use resolve_lvalue_ptr (load the pointer, GEP)
                let object_ty = self.resolve_receiver_type(object);
                if let Some(ty) = &object_ty
                    && matches!(
                        ty.kind.as_ref(),
                        ast::TypeKind::Pointer(_) | ast::TypeKind::Array(_)
                    )
                {
                    let (ptr, ty) = self.resolve_lvalue_ptr(expr)?;
                    let llvm_ty = self.lower_basic_type(&ty)?;
                    if self.lvalue_is_volatile(expr) {
                        return self.emit_volatile_load(llvm_ty, ptr, "lvalue.load");
                    }
                    return self
                        .builder
                        .build_load(llvm_ty, ptr, "lvalue.load")
                        .map_err(|e| {
                            CodegenError::with_span(
                                format!("failed to load lvalue: {e}"),
                                expr.span,
                            )
                        });
                }
                // For non-pointer types (struct, slice), use the trait path (__index_get)
                let method_ident = ast::Identifier {
                    name: "__index_get".to_string(),
                    span: expr.span,
                };
                let result = self.emit_method_call_expression(
                    object,
                    &method_ident,
                    &[*index.clone()],
                    false,
                    &expr.span,
                )?;
                result
                    .ok_or_else(|| CodegenError::with_span("__index_get returned void", expr.span))
            }
            ast::ExpressionKind::Reference { expression, .. } => {
                let (ptr, _) = self.resolve_lvalue_ptr(expression)?;
                Ok(ptr.as_basic_value_enum())
            }
            ast::ExpressionKind::MacroCall { name, args } => {
                if let Some(result) =
                    crate::builtin_macros::handle_codegen(&name.name, self, expr, args)
                {
                    return result;
                }
                Err(CodegenError::with_span(
                    format!("unknown builtin macro '@{}'", name.name),
                    expr.span,
                ))
            }
            ast::ExpressionKind::ForIn { .. } => Err(CodegenError::with_span(
                "for-in loop cannot be used as a value expression",
                expr.span,
            )),
            ast::ExpressionKind::Move(inner) => {
                let value = self.emit_expression_value(inner)?;
                if let ast::ExpressionKind::Identifier(ident) = inner.kind.as_ref()
                    && let Some(flag_ptr) = self.drop_flags.get(&ident.name).copied()
                {
                    self.builder
                        .build_store(flag_ptr, self.context.bool_type().const_int(0, false))
                        .map_err(|e| {
                            CodegenError::new(format!("failed to clear drop flag: {e}"))
                        })?;
                }
                Ok(value)
            }
            ast::ExpressionKind::Comptime(inner) => self.emit_expression_value(inner),
            ast::ExpressionKind::Launch(_) => self.emit_launch_expression(expr),
            ast::ExpressionKind::Wait(_) => self.emit_wait_expression(expr),
            ast::ExpressionKind::Asm {
                code,
                inputs,
                clobbers,
            } => self.emit_asm_expression(code, inputs, clobbers, &expr.span),
            ast::ExpressionKind::EnumVariant {
                path,
                variant,
                fields,
            } => {
                let enum_name = if path.len() == 1 {
                    &path[0].name
                } else {
                    return Err(CodegenError::with_span(
                        "enum type path must be a single name".to_string(),
                        expr.span,
                    ));
                };
                // Try payload enum layout first
                if let Some(struct_ty) = self.enum_payload_layouts.get(enum_name).cloned() {
                    self.current_fn.as_ref().ok_or_else(|| {
                        CodegenError::new("no active function for enum variant construction")
                    })?;
                    let ptr = self
                        .builder
                        .build_alloca(struct_ty, enum_name)
                        .map_err(|e| CodegenError::new(format!("alloca enum: {e}")))?;
                    let zero_struct = struct_ty.const_zero();
                    self.builder
                        .build_store(ptr, zero_struct)
                        .map_err(|e| CodegenError::new(format!("zero init enum: {e}")))?;
                    // Store tag (i16)
                    if let Some(tag_value) = self
                        .enum_variants
                        .get(enum_name)
                        .and_then(|variants| variants.get(&variant.name))
                    {
                        let tag = self.context.i16_type().const_int(*tag_value as u64, false);
                        let tag_ptr = self
                            .builder
                            .build_struct_gep(struct_ty, ptr, 0, "enum.tag")
                            .map_err(|e| CodegenError::new(format!("GEP enum tag: {e}")))?;
                        self.builder
                            .build_store(tag_ptr, tag)
                            .map_err(|e| CodegenError::new(format!("store enum tag: {e}")))?;
                    }
                    // Store payload values into data field
                    if !fields.is_empty() {
                        let data_ptr = self
                            .builder
                            .build_struct_gep(struct_ty, ptr, 1, "enum.data")
                            .map_err(|e| CodegenError::new(format!("GEP enum data: {e}")))?;
                        let payload_types_opt = self
                            .enum_variant_payload_types
                            .get(enum_name)
                            .and_then(|m| m.get(&variant.name))
                            .cloned();
                        if let Some(payload_types) = payload_types_opt {
                            let target_data = TargetData::create(
                                self.module.get_data_layout().as_str().to_str().unwrap(),
                            );
                            let mut offset: u32 = 0;
                            for (i, field) in fields.iter().enumerate() {
                                let payload_type = match payload_types.get(i) {
                                    Some(pt) => pt,
                                    None => break,
                                };
                                let target_llvm_ty = self.lower_basic_type(payload_type)?;
                                let field_size = target_data.get_abi_size(&target_llvm_ty) as u32;
                                let field_ptr = if offset == 0 {
                                    data_ptr
                                } else {
                                    unsafe {
                                        self.builder.build_gep(
                                            self.context.i8_type(),
                                            data_ptr,
                                            &[self
                                                .context
                                                .i32_type()
                                                .const_int(offset as u64, false)],
                                            "enum.field.gep",
                                        )
                                    }
                                    .map_err(|e| {
                                        CodegenError::new(format!("GEP enum field: {e}"))
                                    })?
                                };
                                let mut val = self.emit_expression_value(field)?;
                                val = self.cast_value_to_basic_type(
                                    val,
                                    target_llvm_ty,
                                    &field.span,
                                )?;
                                let val_ptr = self
                                    .builder
                                    .build_pointer_cast(
                                        field_ptr,
                                        self.context.ptr_type(AddressSpace::default()),
                                        "enum.val.cast",
                                    )
                                    .map_err(|e| {
                                        CodegenError::new(format!("pointer cast enum: {e}"))
                                    })?;
                                self.builder.build_store(val_ptr, val).map_err(|e| {
                                    CodegenError::new(format!("store enum payload: {e}"))
                                })?;
                                offset += field_size;
                            }
                            let val = self.emit_expression_value(&fields[0])?;
                            let val_ptr = self
                                .builder
                                .build_pointer_cast(
                                    data_ptr,
                                    self.context.ptr_type(AddressSpace::default()),
                                    "enum.val.cast",
                                )
                                .map_err(|e| {
                                    CodegenError::new(format!("pointer cast enum: {e}"))
                                })?;
                            self.builder.build_store(val_ptr, val).map_err(|e| {
                                CodegenError::new(format!("store enum payload: {e}"))
                            })?;
                        }
                    }
                    Ok(self
                        .builder
                        .build_load(struct_ty.as_basic_type_enum(), ptr, enum_name)
                        .map_err(|e| CodegenError::new(format!("failed to load enum: {e}")))?)
                } else {
                    // Unit variant — fall back to integer constant
                    if let Some(val) = self.enum_member_constant(enum_name, &variant.name) {
                        Ok(val)
                    } else {
                        Err(CodegenError::with_span(
                            format!("unknown enum variant '{}::{}'", enum_name, variant.name),
                            expr.span,
                        ))
                    }
                }
            }
            _ => Err(CodegenError::with_span(
                format!(
                    "expression kind is not supported in LLVM IR codegen yet: {:?}",
                    expr.kind
                ),
                expr.span,
            )),
        }
    }

    pub(crate) fn emit_enum_construction_impl(
        &mut self,
        enum_ident: &ast::Identifier,
        variant: &ast::Identifier,
        arguments: &[ast::Expression],
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let enum_name = &enum_ident.name;
        if let Some(struct_ty) = self.enum_payload_layouts.get(enum_name).copied() {
            let ptr = self
                .builder
                .build_alloca(struct_ty, enum_name)
                .map_err(|e| CodegenError::new(format!("alloca enum: {e}")))?;
            let zero_struct = struct_ty.const_zero();
            self.builder
                .build_store(ptr, zero_struct)
                .map_err(|e| CodegenError::new(format!("zero init enum: {e}")))?;
            if let Some(tag_value) = self
                .enum_variants
                .get(enum_name)
                .and_then(|variants| variants.get(&variant.name))
            {
                let tag = self.context.i16_type().const_int(*tag_value as u64, false);
                let tag_ptr = self
                    .builder
                    .build_struct_gep(struct_ty, ptr, 0, "enum.tag")
                    .map_err(|e| CodegenError::new(format!("GEP enum tag: {e}")))?;
                self.builder
                    .build_store(tag_ptr, tag)
                    .map_err(|e| CodegenError::new(format!("store enum tag: {e}")))?;
            }
            if !arguments.is_empty() {
                let data_ptr = self
                    .builder
                    .build_struct_gep(struct_ty, ptr, 1, "enum.data")
                    .map_err(|e| CodegenError::new(format!("GEP enum data: {e}")))?;
                let payload_types_opt = self
                    .enum_variant_payload_types
                    .get(enum_name)
                    .and_then(|m| m.get(&variant.name))
                    .cloned();
                if let Some(payload_types) = payload_types_opt {
                    let target_data = TargetData::create(
                        self.module.get_data_layout().as_str().to_str().unwrap(),
                    );
                    let mut offset: u32 = 0;
                    for (i, arg) in arguments.iter().enumerate() {
                        let payload_type = match payload_types.get(i) {
                            Some(pt) => pt,
                            None => break,
                        };
                        let target_llvm_ty = self.lower_basic_type(payload_type)?;
                        let field_size = target_data.get_abi_size(&target_llvm_ty) as u32;
                        let field_ptr = if offset == 0 {
                            data_ptr
                        } else {
                            unsafe {
                                self.builder.build_gep(
                                    self.context.i8_type(),
                                    data_ptr,
                                    &[self.context.i32_type().const_int(offset as u64, false)],
                                    "enum.field.gep",
                                )
                            }
                            .map_err(|e| CodegenError::new(format!("GEP enum field: {e}")))?
                        };
                        let mut val = self.emit_expression_value(arg)?;
                        val = self.cast_value_to_basic_type(val, target_llvm_ty, &arg.span)?;
                        let val_ptr = self
                            .builder
                            .build_pointer_cast(
                                field_ptr,
                                self.context.ptr_type(AddressSpace::default()),
                                "enum.val.cast",
                            )
                            .map_err(|e| CodegenError::new(format!("pointer cast enum: {e}")))?;
                        self.builder
                            .build_store(val_ptr, val)
                            .map_err(|e| CodegenError::new(format!("store enum payload: {e}")))?;
                        offset += field_size;
                    }
                } else if arguments.len() == 1 {
                    let val = self.emit_expression_value(&arguments[0])?;
                    let val_ptr = self
                        .builder
                        .build_pointer_cast(
                            data_ptr,
                            self.context.ptr_type(AddressSpace::default()),
                            "enum.val.cast",
                        )
                        .map_err(|e| CodegenError::new(format!("pointer cast enum: {e}")))?;
                    self.builder
                        .build_store(val_ptr, val)
                        .map_err(|e| CodegenError::new(format!("store enum payload: {e}")))?;
                }
            }
            Ok(self
                .builder
                .build_load(struct_ty.as_basic_type_enum(), ptr, enum_name)
                .map_err(|e| CodegenError::new(format!("failed to load enum: {e}")))?)
        } else {
            if let Some(val) = self.enum_member_constant(enum_name, &variant.name) {
                Ok(val)
            } else {
                Err(CodegenError::with_span(
                    format!("unknown enum variant '{}::{}'", enum_name, variant.name),
                    *span,
                ))
            }
        }
    }

    pub(crate) fn emit_asm_expression(
        &mut self,
        code: &str,
        inputs: &[ast::Expression],
        clobbers: &[String],
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        // Validate: x86_64 syscall has 1 syscall number (rax) + 6 arg registers
        if inputs.len() > 7 {
            return Err(CodegenError::with_span(
                format!(
                    "inline asm with {} input(s) unsupported: x86_64 syscall ABI has 1 syscall number register (rax) and 6 argument registers at most",
                    inputs.len()
                ),
                *span,
            ));
        }

        let i64_ty = self.context.i64_type();

        // 1. Evaluate and cast all input expressions to i64
        let mut args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::with_capacity(inputs.len());
        for input in inputs {
            let value = self.emit_expression_value(input)?;
            let i64_ty_ast = ast::Type {
                kind: Box::new(ast::TypeKind::Primitive(ast::PrimitiveType::I64)),
                span: *span,
            };
            let i64_val = self.cast_value_to_ast_type(value, &i64_ty_ast, span)?;
            args.push(BasicMetadataValueEnum::from(i64_val));
        }

        // 2. Build constraint string
        // Output: {rax} — syscall return value is always in %rax
        // Inputs (in syscall order): {rax}, {rdi}, {rsi}, {rdx}, {r10}, {r8}, {r9}
        // Clobbers: ~{rcx}, ~{r11} — always destroyed by Linux syscall, plus
        // any caller-supplied clobbers (e.g. "rbx", "rdx" for `cpuid`).
        let regs = ["{rax}", "{rdi}", "{rsi}", "{rdx}", "{r10}", "{r8}", "{r9}"];
        let mut constraints = String::from("={rax}");
        for reg in regs.iter().take(inputs.len()) {
            constraints.push(',');
            constraints.push_str(reg);
        }
        constraints.push_str(",~{rcx},~{r11}");
        for clobber in clobbers {
            let name = clobber.trim();
            if name.is_empty() {
                continue;
            }
            let name = name.strip_prefix('%').unwrap_or(name);
            let named = format!("~{{{name}}}");
            if !constraints.contains(&named) {
                constraints.push(',');
                constraints.push_str(&named);
            }
        }

        // 3. Build LLVM function type: i64(i64, i64, ...)
        let param_types: Vec<BasicMetadataTypeEnum<'ctx>> = (0..inputs.len())
            .map(|_| BasicMetadataTypeEnum::from(i64_ty.as_basic_type_enum()))
            .collect();
        let fn_type = i64_ty.fn_type(&param_types, false);

        // 4. Create inline asm pointer
        let asm_fn = self.context.create_inline_asm(
            fn_type,
            code.to_string(),
            constraints,
            true,  // sideeffects: syscall has side effects
            false, // alignstack: false (syscall ABI doesn't require aligned stack)
            None,  // dialect: ATT (default)
            false, // can_throw: false (syscalls don't throw C++ exceptions)
        );

        // 5. Call the inline asm via indirect call
        let call = self
            .builder
            .build_indirect_call(fn_type, asm_fn, &args, "asm_result")
            .map_err(|e| CodegenError::with_span(format!("inline asm call failed: {e}"), *span))?;

        // 6. Extract the i64 return value
        call.try_as_basic_value().basic().ok_or_else(|| {
            CodegenError::with_span("inline asm returned void, expected i64".to_string(), *span)
        })
    }

    pub(crate) fn emit_struct_literal_value(
        &mut self,
        path: &[ast::Identifier],
        fields: &[ast::FieldInit],
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let struct_name = Self::path_name(path);
        let struct_ty = *self.struct_types.get(&struct_name).ok_or_else(|| {
            CodegenError::with_span(format!("unknown struct type `{struct_name}`"), *span)
        })?;

        if struct_ty.is_opaque() {
            return Err(CodegenError::with_span(
                format!("struct `{struct_name}` is not fully defined yet"),
                *span,
            ));
        }

        let declared_fields = self
            .struct_fields
            .get(&struct_name)
            .cloned()
            .ok_or_else(|| {
                CodegenError::with_span(
                    format!("missing field metadata for struct `{struct_name}`"),
                    *span,
                )
            })?;

        let mut provided_by_name: HashMap<String, &ast::FieldInit> = HashMap::default();
        for field in fields {
            if provided_by_name
                .insert(field.name.name.clone(), field)
                .is_some()
            {
                return Err(CodegenError::with_span(
                    format!("duplicate field `{}` in struct literal", field.name.name),
                    field.name.span,
                ));
            }
        }

        for field in fields {
            if !declared_fields
                .iter()
                .any(|(name, _)| name == &field.name.name)
            {
                return Err(CodegenError::with_span(
                    format!("unknown field `{}` on `{struct_name}`", field.name.name),
                    field.name.span,
                ));
            }
        }

        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for struct literal"))?;
        let temp =
            self.create_entry_alloca(function, "struct.lit.tmp", struct_ty.as_basic_type_enum())?;
        for (index, (field_name, _field_ty)) in declared_fields.iter().enumerate() {
            let Some(field_init) = provided_by_name.get(field_name) else {
                return Err(CodegenError::with_span(
                    format!("missing field `{field_name}` in struct literal"),
                    *span,
                ));
            };
            let field_value = self.emit_expression_value(&field_init.value)?;
            let llvm_field_ty =
                struct_ty
                    .get_field_type_at_index(index as u32)
                    .ok_or_else(|| {
                        CodegenError::with_span(
                            format!("missing LLVM field type for `{field_name}`"),
                            field_init.name.span,
                        )
                    })?;
            let field_value =
                self.cast_value_to_basic_type(field_value, llvm_field_ty, &field_init.value.span)?;
            let field_ptr = self
                .builder
                .build_struct_gep(struct_ty, temp, index as u32, "struct.lit.ptr")
                .map_err(|e| {
                    CodegenError::with_span(
                        format!("failed to access struct literal field `{field_name}`: {e}"),
                        field_init.name.span,
                    )
                })?;
            self.builder
                .build_store(field_ptr, field_value)
                .map_err(|e| {
                    CodegenError::with_span(
                        format!("failed to store struct literal field `{field_name}`: {e}"),
                        field_init.name.span,
                    )
                })?;
        }
        self.builder
            .build_load(struct_ty.as_basic_type_enum(), temp, "struct.lit.value")
            .map_err(|e| {
                CodegenError::with_span(format!("failed to load struct literal: {e}"), *span)
            })
    }

    pub(crate) fn emit_array_literal_value(
        &mut self,
        items: &[ast::Expression],
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        if items.is_empty() {
            return Err(CodegenError::with_span(
                "array literal cannot be empty without type context",
                *span,
            ));
        }

        let first = self.emit_expression_value(&items[0])?;
        let element_ty = first.get_type();
        let mut values = vec![first];
        for item in &items[1..] {
            let value = self.emit_expression_value(item)?;
            if value.get_type() != element_ty {
                return Err(CodegenError::with_span(
                    "array literal elements must have the same type",
                    item.span,
                ));
            }
            values.push(value);
        }

        // Emit as a Slice<T> struct: { T* data; i64 len; }
        let n = values.len() as u32;

        // Allocate a stack buffer for the elements
        let array_llvm_ty = element_ty.array_type(n);
        let alloca = self
            .builder
            .build_alloca(array_llvm_ty, "arr.lit.buf")
            .map_err(|e| {
                CodegenError::with_span(format!("failed to allocate array buffer: {e}"), *span)
            })?;

        // Store each element
        for (index, value) in values.iter().enumerate() {
            let indices = [
                self.context.i32_type().const_zero(),
                self.context.i32_type().const_int(index as u64, false),
            ];
            let ptr = unsafe {
                self.builder
                    .build_in_bounds_gep(array_llvm_ty, alloca, &indices, "arr.lit.ptr")
            }
            .map_err(|e| {
                CodegenError::with_span(
                    format!("failed to build array literal element pointer {index}: {e}"),
                    *span,
                )
            })?;
            self.builder.build_store(ptr, *value).map_err(|e| {
                CodegenError::with_span(
                    format!("failed to store array literal element {index}: {e}"),
                    *span,
                )
            })?;
        }

        // Build the Slice struct value
        let ptr_ty = self.context.ptr_type(AddressSpace::default());
        let i64_ty = self.context.i64_type();
        let slice_ty = self.context.struct_type(
            &[ptr_ty.as_basic_type_enum(), i64_ty.as_basic_type_enum()],
            false,
        );
        let mut slice = slice_ty.get_undef();

        // Field 0: data pointer
        let data_ptr = alloca.as_basic_value_enum();
        slice = self
            .builder
            .build_insert_value(slice, data_ptr, 0, "slice.data")
            .map_err(|e| {
                CodegenError::with_span(format!("failed to build slice data field: {e}"), *span)
            })?
            .into_struct_value();

        // Field 1: length
        let len = i64_ty.const_int(values.len() as u64, false);
        slice = self
            .builder
            .build_insert_value(slice, len.as_basic_value_enum(), 1, "slice.len")
            .map_err(|e| {
                CodegenError::with_span(format!("failed to build slice len field: {e}"), *span)
            })?
            .into_struct_value();

        Ok(slice.as_basic_value_enum())
    }

    pub(crate) fn emit_tuple_literal_value(
        &mut self,
        items: &[ast::Expression],
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let mut values = Vec::with_capacity(items.len());
        let mut field_types = Vec::with_capacity(items.len());
        for item in items {
            let value = self.emit_expression_value(item)?;
            field_types.push(value.get_type());
            values.push(value);
        }

        let tuple_ty = self.context.struct_type(&field_types, false);
        let mut aggregate = tuple_ty.get_undef();
        for (index, value) in values.iter().enumerate() {
            aggregate = self
                .builder
                .build_insert_value(aggregate, *value, index as u32, "tuple.lit.ins")
                .map_err(|e| {
                    CodegenError::with_span(
                        format!("failed to build tuple literal element {index}: {e}"),
                        *span,
                    )
                })?
                .into_struct_value();
        }

        Ok(aggregate.as_basic_value_enum())
    }

    pub(crate) fn emit_typed_initializer_value(
        &mut self,
        items: &[ast::InitializerItem],
        target_type: &ast::Type,
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match target_type.kind.as_ref() {
            ast::TypeKind::Named(named) => {
                let struct_name = Self::named_type_key(named);
                let struct_ty = self.ensure_named_struct_type(named)?;
                let declared_fields =
                    self.struct_fields
                        .get(&struct_name)
                        .cloned()
                        .ok_or_else(|| {
                            CodegenError::with_span(
                                format!("missing field metadata for struct `{struct_name}`"),
                                *span,
                            )
                        })?;

                let function = self.current_fn.ok_or_else(|| {
                    CodegenError::new("no active function for struct initializer")
                })?;
                let temp = self.create_entry_alloca(
                    function,
                    "init.struct.tmp",
                    struct_ty.as_basic_type_enum(),
                )?;
                let named_mode = items
                    .iter()
                    .any(|item| matches!(item, ast::InitializerItem::Field { .. }));

                if named_mode {
                    let mut by_name: HashMap<String, &ast::Expression> = HashMap::default();
                    for item in items {
                        match item {
                            ast::InitializerItem::Field { name, value } => {
                                if by_name.insert(name.name.clone(), value).is_some() {
                                    return Err(CodegenError::with_span(
                                        format!("duplicate field `{}` in initializer", name.name),
                                        name.span,
                                    ));
                                }
                            }
                            _ => {
                                return Err(CodegenError::with_span(
                                    "cannot mix positional/indexed items with named struct initializer",
                                    *span,
                                ));
                            }
                        }
                    }

                    for (index, (field_name, field_ty)) in declared_fields.iter().enumerate() {
                        let Some(value_expr) = by_name.get(field_name) else {
                            return Err(CodegenError::with_span(
                                format!("missing field `{field_name}` in initializer"),
                                *span,
                            ));
                        };
                        let value =
                            self.emit_expression_value_for_expected(value_expr, field_ty)?;
                        let llvm_field_ty = struct_ty
                            .get_field_type_at_index(index as u32)
                            .ok_or_else(|| {
                                CodegenError::with_span(
                                    format!("missing LLVM field type for `{field_name}`"),
                                    value_expr.span,
                                )
                            })?;
                        let value =
                            self.cast_value_to_basic_type(value, llvm_field_ty, &value_expr.span)?;
                        let field_ptr = self
                            .builder
                            .build_struct_gep(struct_ty, temp, index as u32, "init.struct.ptr")
                            .map_err(|e| {
                                CodegenError::with_span(
                                    format!(
                                        "failed to access struct initializer field `{field_name}`: {e}"
                                    ),
                                    value_expr.span,
                                )
                            })?;
                        self.builder.build_store(field_ptr, value).map_err(|e| {
                            CodegenError::with_span(
                                format!(
                                    "failed to store struct initializer field `{field_name}`: {e}"
                                ),
                                value_expr.span,
                            )
                        })?;
                    }
                } else {
                    if items.len() != declared_fields.len() {
                        return Err(CodegenError::with_span(
                            "positional struct initializer field count mismatch",
                            *span,
                        ));
                    }
                    for (index, item) in items.iter().enumerate() {
                        let ast::InitializerItem::Positional(expr) = item else {
                            return Err(CodegenError::with_span(
                                "struct positional initializer only supports positional items",
                                *span,
                            ));
                        };
                        let (_, field_ty) = declared_fields.get(index).ok_or_else(|| {
                            CodegenError::with_span(
                                format!("missing field metadata at index {index}"),
                                expr.span,
                            )
                        })?;
                        let value = self.emit_expression_value_for_expected(expr, field_ty)?;
                        let llvm_field_ty = struct_ty
                            .get_field_type_at_index(index as u32)
                            .ok_or_else(|| {
                                CodegenError::with_span(
                                    format!("missing LLVM field type at index {index}"),
                                    expr.span,
                                )
                            })?;
                        let value =
                            self.cast_value_to_basic_type(value, llvm_field_ty, &expr.span)?;
                        let field_ptr = self
                            .builder
                            .build_struct_gep(struct_ty, temp, index as u32, "init.struct.ptr")
                            .map_err(|e| {
                                CodegenError::with_span(
                                    format!("failed to access struct initializer field: {e}"),
                                    expr.span,
                                )
                            })?;
                        self.builder.build_store(field_ptr, value).map_err(|e| {
                            CodegenError::with_span(
                                format!("failed to store struct initializer field: {e}"),
                                expr.span,
                            )
                        })?;
                    }
                }
                self.builder
                    .build_load(struct_ty.as_basic_type_enum(), temp, "init.struct.value")
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to load struct initializer: {e}"),
                            *span,
                        )
                    })
            }
            ast::TypeKind::Tuple(types) => {
                if items.len() != types.len() {
                    return Err(CodegenError::with_span(
                        "tuple initializer arity mismatch",
                        *span,
                    ));
                }
                let mut field_types = Vec::with_capacity(types.len());
                for ty in types {
                    field_types.push(self.lower_basic_type(ty)?);
                }
                let tuple_ty = self.context.struct_type(&field_types, false);
                let mut aggregate = tuple_ty.get_undef();
                for (index, item) in items.iter().enumerate() {
                    let ast::InitializerItem::Positional(expr) = item else {
                        return Err(CodegenError::with_span(
                            "tuple initializer only supports positional items",
                            *span,
                        ));
                    };
                    let value = self.emit_expression_value_for_expected(expr, &types[index])?;
                    let value =
                        self.cast_value_to_basic_type(value, field_types[index], &expr.span)?;
                    aggregate = self
                        .builder
                        .build_insert_value(aggregate, value, index as u32, "init.tuple.ins")
                        .map_err(|e| {
                            CodegenError::with_span(
                                format!("failed to build tuple initializer element {index}: {e}"),
                                expr.span,
                            )
                        })?
                        .into_struct_value();
                }
                Ok(aggregate.as_basic_value_enum())
            }
            ast::TypeKind::Array(array) => {
                let element_llvm_ty = self.lower_basic_type(&array.element_type)?;
                let array_llvm_ty = element_llvm_ty.array_type(array.size as u32);
                let function = self
                    .current_fn
                    .ok_or_else(|| CodegenError::new("no active function for array initializer"))?;
                let temp = self.create_entry_alloca(
                    function,
                    "init.arr.tmp",
                    array_llvm_ty.as_basic_type_enum(),
                )?;

                // Zero-initialize the temp, then overwrite specified positions
                self.builder
                    .build_store(temp, array_llvm_ty.const_zero())
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to zero-init array initializer: {e}"),
                            *span,
                        )
                    })?;

                let positional = items
                    .iter()
                    .all(|item| matches!(item, ast::InitializerItem::Positional(_)));

                if positional {
                    // C-style flat initializer for multi-dimensional arrays:
                    // `{1,2,3,4,5,6}` for `[2][3]` lays values out across the
                    // nested shape in row-major order; the temp is zero-filled
                    // first, so partial lists zero-pad automatically.
                    let full_array_ty = || ast::Type {
                        kind: Box::new(ast::TypeKind::Array(array.clone())),
                        span: *span,
                    };
                    let flat_total = Self::array_total_len(&full_array_ty());
                    let shape = Self::array_shape(&full_array_ty());
                    if matches!(array.element_type.kind.as_ref(), ast::TypeKind::Array(_))
                        && items.len() as i64 != array.size
                        && (items.len() as i64) <= flat_total
                    {
                        let base_elem = Self::innermost_element_type(&full_array_ty()).clone();
                        let base_llvm_ty = self.lower_basic_type(&base_elem)?;
                        for (i, item) in items.iter().enumerate() {
                            let ast::InitializerItem::Positional(expr) = item else {
                                unreachable!()
                            };
                            let value =
                                self.emit_expression_value_for_expected(expr, &base_elem)?;
                            let value =
                                self.cast_value_to_basic_type(value, base_llvm_ty, &expr.span)?;
                            let mut indices = vec![self.context.i32_type().const_zero()];
                            indices.extend(
                                Self::flat_index_vector(i, &shape)
                                    .into_iter()
                                    .map(|idx| self.context.i32_type().const_int(idx, false)),
                            );
                            let ptr = unsafe {
                                self.builder.build_in_bounds_gep(
                                    array_llvm_ty,
                                    temp,
                                    &indices,
                                    "arr.init.ptr",
                                )
                            }
                            .map_err(|e| {
                                CodegenError::with_span(
                                    format!("GEP array initializer element {i}: {e}"),
                                    expr.span,
                                )
                            })?;
                            self.builder.build_store(ptr, value).map_err(|e| {
                                CodegenError::with_span(
                                    format!("store array initializer element {i}: {e}"),
                                    expr.span,
                                )
                            })?;
                        }
                    } else if (items.len() as i64) > array.size {
                        return Err(CodegenError::with_span(
                            format!(
                                "array initializer has {} elements but array size is {}",
                                items.len(),
                                array.size
                            ),
                            *span,
                        ));
                    } else {
                        for (i, item) in items.iter().enumerate() {
                            let ast::InitializerItem::Positional(expr) = item else {
                                unreachable!()
                            };
                            let value =
                                self.emit_expression_value_for_expected(expr, &array.element_type)?;
                            let value =
                                self.cast_value_to_basic_type(value, element_llvm_ty, &expr.span)?;
                            let indices = [
                                self.context.i32_type().const_zero(),
                                self.context.i32_type().const_int(i as u64, false),
                            ];
                            let ptr = unsafe {
                                self.builder.build_in_bounds_gep(
                                    array_llvm_ty,
                                    temp,
                                    &indices,
                                    "arr.init.ptr",
                                )
                            }
                            .map_err(|e| {
                                CodegenError::with_span(
                                    format!("GEP array initializer element {i}: {e}"),
                                    expr.span,
                                )
                            })?;
                            self.builder.build_store(ptr, value).map_err(|e| {
                                CodegenError::with_span(
                                    format!("store array initializer element {i}: {e}"),
                                    expr.span,
                                )
                            })?;
                        }
                    }
                } else {
                    for item in items {
                        match item {
                            ast::InitializerItem::Index { index, value } => {
                                let idx_val = self.emit_expression_value(index)?;
                                let BasicValueEnum::IntValue(idx_int) = idx_val else {
                                    return Err(CodegenError::with_span(
                                        "array index must be an integer",
                                        index.span,
                                    ));
                                };
                                let elem_val = self.emit_expression_value_for_expected(
                                    value,
                                    &array.element_type,
                                )?;
                                let elem_val = self.cast_value_to_basic_type(
                                    elem_val,
                                    element_llvm_ty,
                                    &value.span,
                                )?;

                                let i64_ty = self.context.i64_type();
                                let idx_i64 = if idx_int.get_type().get_bit_width() == 64 {
                                    idx_int
                                } else {
                                    self.builder
                                        .build_int_cast(idx_int, i64_ty, "idx.cast")
                                        .map_err(|e| {
                                            CodegenError::with_span(
                                                format!("failed to cast array index: {e}"),
                                                index.span,
                                            )
                                        })?
                                };
                                let ptr = unsafe {
                                    self.builder.build_in_bounds_gep(
                                        array_llvm_ty,
                                        temp,
                                        &[i64_ty.const_zero(), idx_i64],
                                        "arr.init.ptr",
                                    )
                                }
                                .map_err(|e| {
                                    CodegenError::with_span(
                                        format!("GEP array initializer: {e}"),
                                        value.span,
                                    )
                                })?;
                                self.builder.build_store(ptr, elem_val).map_err(|e| {
                                    CodegenError::with_span(
                                        format!("store array initializer: {e}"),
                                        value.span,
                                    )
                                })?;
                            }
                            _ => {
                                return Err(CodegenError::with_span(
                                    "array initializer only supports positional and indexed items",
                                    *span,
                                ));
                            }
                        }
                    }
                }

                self.builder
                    .build_load(array_llvm_ty.as_basic_type_enum(), temp, "init.arr.val")
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to load array initializer: {e}"),
                            *span,
                        )
                    })
            }
            _ => Err(CodegenError::with_span(
                "initializer is not supported for this target type",
                target_type.span,
            )),
        }
    }

    pub(crate) fn emit_expression_value_for_expected(
        &mut self,
        expr: &ast::Expression,
        expected_type: &ast::Type,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        if let ast::ExpressionKind::Initializer { items } = expr.kind.as_ref() {
            self.emit_typed_initializer_value(items, expected_type, &expr.span)
        } else {
            self.emit_expression_value(expr)
        }
    }

    /// Lowers a block in value position.
    ///
    /// All statements except the last are emitted for side effects. The final
    /// statement must be an expression and becomes the block's result value.
    pub(crate) fn emit_block_value(
        &mut self,
        block: &ast::Block,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        if block.statements.is_empty() {
            return Err(CodegenError::with_span(
                "value-producing block cannot be empty",
                block.span,
            ));
        }

        let has_debug_scope = if let Some(debug) = &mut self.debug {
            let (line, col, _, _) = debug.source_map.span_to_line_col(&block.span);
            debug.push_lexical_block(line, col);
            true
        } else {
            false
        };

        self.push_scope();
        for statement in &block.statements[..block.statements.len() - 1] {
            self.generate_statement(statement)?;
            let terminated = self
                .builder
                .get_insert_block()
                .and_then(|bb| bb.get_terminator())
                .is_some();
            if terminated {
                self.pop_scope();
                if has_debug_scope && let Some(debug) = &mut self.debug {
                    debug.pop_lexical_block();
                }
                return Err(CodegenError::with_span(
                    "value-producing block terminated before final expression",
                    statement.span,
                ));
            }
        }

        let last = block.statements.last().expect("non-empty checked above");
        let value = match &last.kind {
            ast::StatementKind::Expression(expr) => self.emit_expression_value(expr)?,
            _ => {
                self.pop_scope();
                if has_debug_scope && let Some(debug) = &mut self.debug {
                    debug.pop_lexical_block();
                }
                return Err(CodegenError::with_span(
                    "value-producing block must end with an expression",
                    last.span,
                ));
            }
        };
        self.pop_scope();

        if has_debug_scope && let Some(debug) = &mut self.debug {
            debug.pop_lexical_block();
        }

        Ok(value)
    }

    /// Lowers `if` as an expression using a PHI merge.
    ///
    /// CFG shape:
    /// - `if.expr.then`
    /// - `if.expr.else`
    /// - `if.expr.cont` (contains the PHI)
    pub(crate) fn emit_if_expression_value(
        &mut self,
        condition: &ast::Expression,
        then_branch: &ast::Block,
        else_branch: &ast::Block,
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for if expression"))?;

        let then_bb = self.context.append_basic_block(function, "if.expr.then");
        let else_bb = self.context.append_basic_block(function, "if.expr.else");
        let cont_bb = self.context.append_basic_block(function, "if.expr.cont");

        let cond_value = self.emit_expression_value(condition)?;
        let cond_bool = self.emit_as_bool(&cond_value, &condition.span)?;
        self.builder
            .build_conditional_branch(cond_bool, then_bb, else_bb)
            .map_err(|e| CodegenError::new(format!("failed to branch for if expression: {e}")))?;

        let mut incoming: Vec<(BasicValueEnum<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)> =
            Vec::new();

        self.builder.position_at_end(then_bb);
        let then_value = self.emit_block_value(then_branch)?;
        let then_end = self
            .builder
            .get_insert_block()
            .ok_or_else(|| CodegenError::new("missing then insertion block"))?;
        let then_terminated = then_end.get_terminator().is_some();
        if !then_terminated {
            self.builder
                .build_unconditional_branch(cont_bb)
                .map_err(|e| CodegenError::new(format!("failed then->cont branch: {e}")))?;
            incoming.push((then_value, then_end));
        }

        self.builder.position_at_end(else_bb);
        let else_value = self.emit_block_value(else_branch)?;
        let else_end = self
            .builder
            .get_insert_block()
            .ok_or_else(|| CodegenError::new("missing else insertion block"))?;
        let else_terminated = else_end.get_terminator().is_some();
        if !else_terminated {
            self.builder
                .build_unconditional_branch(cont_bb)
                .map_err(|e| CodegenError::new(format!("failed else->cont branch: {e}")))?;
            incoming.push((else_value, else_end));
        }

        if incoming.is_empty() {
            return Err(CodegenError::with_span(
                "if expression has no value-producing path",
                *span,
            ));
        }

        let phi_ty = incoming[0].0.get_type();
        for (value, _) in incoming.iter().skip(1) {
            if value.get_type() != phi_ty {
                return Err(CodegenError::with_span(
                    "if expression branches produce different types",
                    *span,
                ));
            }
        }

        self.builder.position_at_end(cont_bb);
        let phi = self
            .builder
            .build_phi(phi_ty, "if.expr")
            .map_err(|e| CodegenError::new(format!("failed to build if phi: {e}")))?;
        let refs: Vec<(
            &dyn BasicValue<'ctx>,
            inkwell::basic_block::BasicBlock<'ctx>,
        )> = incoming
            .iter()
            .map(|(value, bb)| (value as &dyn BasicValue<'ctx>, *bb))
            .collect();
        phi.add_incoming(&refs);
        Ok(phi.as_basic_value())
    }

    /// Lowers `match` as an expression using chained condition blocks and PHI merge.
    ///
    /// Each arm gets an `arm` and `next` block. Value-producing paths branch to
    /// `match.expr.end`, where a PHI joins all incoming arm values.
    pub(crate) fn emit_match_expression_value(
        &mut self,
        expression: &ast::Expression,
        arms: &[ast::MatchArm],
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for match expression"))?;
        let scrutinee = self.emit_expression_value(expression)?;
        let end_bb = self.context.append_basic_block(function, "match.expr.end");
        let mut cond_bb = self
            .builder
            .get_insert_block()
            .ok_or_else(|| CodegenError::new("builder is not positioned in a basic block"))?;
        let mut incoming: Vec<(BasicValueEnum<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)> =
            Vec::new();
        let mut catch_all = false;

        for (arm_index, arm) in arms.iter().enumerate() {
            let arm_bb = self
                .context
                .append_basic_block(function, &format!("match.expr.arm.{arm_index}"));
            let next_bb = self
                .context
                .append_basic_block(function, &format!("match.expr.next.{arm_index}"));

            self.builder.position_at_end(cond_bb);
            if arm.guard.is_some() {
                return Err(CodegenError::with_span(
                    "match guards are not supported in LLVM IR codegen yet",
                    arm.span,
                ));
            }

            match &arm.pattern.kind {
                ast::PatternKind::Wildcard | ast::PatternKind::Identifier(_) => {
                    self.builder
                        .build_unconditional_branch(arm_bb)
                        .map_err(|e| CodegenError::new(format!("failed match expr branch: {e}")))?;
                }
                ast::PatternKind::Literal(literal) => {
                    let cond = match (&scrutinee, literal) {
                        (BasicValueEnum::IntValue(lhs), ast::Literal::Integer(value)) => {
                            let rhs = lhs
                                .get_type()
                                .const_int_from_string(&value.to_string(), StringRadix::Decimal)
                                .unwrap_or_else(|| lhs.get_type().const_int(*value as u64, true));
                            self.builder
                                .build_int_compare(IntPredicate::EQ, *lhs, rhs, "match.expr.int")
                                .map_err(|e| {
                                    CodegenError::new(format!("failed match int compare: {e}"))
                                })?
                        }
                        (BasicValueEnum::IntValue(lhs), ast::Literal::Bool(value)) => {
                            let rhs = lhs.get_type().const_int(u64::from(*value), false);
                            self.builder
                                .build_int_compare(IntPredicate::EQ, *lhs, rhs, "match.expr.bool")
                                .map_err(|e| {
                                    CodegenError::new(format!("failed match bool compare: {e}"))
                                })?
                        }
                        (BasicValueEnum::IntValue(lhs), ast::Literal::Char(value)) => {
                            let rhs = lhs.get_type().const_int(*value as u64, false);
                            self.builder
                                .build_int_compare(IntPredicate::EQ, *lhs, rhs, "match.expr.char")
                                .map_err(|e| {
                                    CodegenError::new(format!("failed match char compare: {e}"))
                                })?
                        }
                        (BasicValueEnum::FloatValue(lhs), ast::Literal::Float(value)) => {
                            let rhs = lhs.get_type().const_float(*value);
                            self.builder
                                .build_float_compare(FloatPredicate::OEQ, *lhs, rhs, "match.expr.f")
                                .map_err(|e| {
                                    CodegenError::new(format!("failed match float compare: {e}"))
                                })?
                        }
                        (BasicValueEnum::PointerValue(_), ast::Literal::String(s)) => {
                            // String patterns compare via strcmp == 0.
                            let rhs = self.intern_string_literal(s)?.as_basic_value_enum();
                            let cmp = self.emit_strcmp_comparison(
                                scrutinee,
                                &ast::BinaryOperator::Equal,
                                rhs,
                                &arm.pattern.span,
                            )?;
                            cmp.into_int_value()
                        }
                        _ => {
                            return Err(CodegenError::with_span(
                                "unsupported match literal for scrutinee type",
                                arm.pattern.span,
                            ));
                        }
                    };

                    self.builder
                        .build_conditional_branch(cond, arm_bb, next_bb)
                        .map_err(|e| CodegenError::new(format!("failed match expr branch: {e}")))?;
                }
                ast::PatternKind::Enum {
                    path,
                    variant,
                    data,
                } => {
                    // Resolve the enum name: either the explicit `Enum.Variant`
                    // path, or (for bare `Variant` patterns) the scrutinee's
                    // enum type, which typeck already validated.
                    let enum_name = if path.len() == 1 {
                        path[0].name.clone()
                    } else if let Some(ty) = self.resolve_receiver_type(expression)
                        && let ast::TypeKind::Named(named) = ty.kind.as_ref()
                        && named.path.len() == 1
                    {
                        named.path[0].name.clone()
                    } else {
                        return Err(CodegenError::with_span(
                            "enum type path must be a single name in match".to_string(),
                            arm.pattern.span,
                        ));
                    };
                    // For a generic enum (`Box2.Full(...)` matching a
                    // `Box2<i32>` value), the payload layout and payload types
                    // are registered under the monomorphized name
                    // (`Box2__i32`); prefer it when present, falling back to
                    // the bare name for non-generic enums.
                    let concrete_name = if let Some(ty) = self.resolve_receiver_type(expression)
                        && let ast::TypeKind::Named(named) = ty.kind.as_ref()
                        && named.path.len() == 1
                    {
                        let monomorph = Self::monomorph_owner_name_from_named(named);
                        if self.enum_payload_layouts.contains_key(&monomorph)
                            || self.enum_variant_payload_types.contains_key(&monomorph)
                        {
                            monomorph
                        } else {
                            enum_name.clone()
                        }
                    } else {
                        enum_name.clone()
                    };
                    if let Some(struct_ty) = self.enum_payload_layouts.get(&concrete_name).cloned()
                    {
                        let function = self.current_fn.ok_or_else(|| {
                            CodegenError::new("no active function for match enum")
                        })?;
                        let scrut_ptr = self.create_entry_alloca(
                            function,
                            "match.scrut.ptr",
                            struct_ty.as_basic_type_enum(),
                        )?;
                        let zero_struct = struct_ty.const_zero();
                        self.builder
                            .build_store(scrut_ptr, zero_struct)
                            .map_err(|e| {
                                CodegenError::new(format!("zero init match scrutinee: {e}"))
                            })?;
                        self.builder
                            .build_store(scrut_ptr, scrutinee)
                            .map_err(|e| {
                                CodegenError::new(format!("store match scrutinee: {e}"))
                            })?;
                        let tag_ptr = self
                            .builder
                            .build_struct_gep(struct_ty, scrut_ptr, 0, "match.tag.ptr")
                            .map_err(|e| CodegenError::new(format!("match tag GEP: {e}")))?;
                        let tag_load = self
                            .builder
                            .build_load(self.context.i16_type(), tag_ptr, "match.tag")
                            .map_err(|e| CodegenError::new(format!("match tag load: {e}")))?;
                        let tag_val = match tag_load {
                            BasicValueEnum::IntValue(v) => v,
                            _ => {
                                return Err(CodegenError::new(
                                    "enum tag is not an i16 value".to_string(),
                                ));
                            }
                        };
                        let expected_tag =
                            if let Some(variants) = self.enum_variants.get(&enum_name) {
                                if let Some(val) = variants.get(&variant.name) {
                                    self.context.i16_type().const_int(*val as u64, false)
                                } else {
                                    return Err(CodegenError::with_span(
                                        format!(
                                            "unknown variant '{}' in enum '{}'",
                                            variant.name, enum_name
                                        ),
                                        variant.span,
                                    ));
                                }
                            } else {
                                return Err(CodegenError::with_span(
                                    format!("unknown enum '{}' in match", enum_name),
                                    arm.pattern.span,
                                ));
                            };
                        let cond = self
                            .builder
                            .build_int_compare(
                                IntPredicate::EQ,
                                tag_val,
                                expected_tag,
                                "match.enum.tag",
                            )
                            .map_err(|e| {
                                CodegenError::new(format!("match enum tag compare: {e}"))
                            })?;
                        self.builder
                            .build_conditional_branch(cond, arm_bb, next_bb)
                            .map_err(|e| CodegenError::new(format!("match enum branch: {e}")))?;
                        self.builder.position_at_end(arm_bb);
                        self.push_scope();
                        if let Some(data_pattern) = data {
                            let payload_types = self
                                .enum_variant_payload_types
                                .get(&concrete_name)
                                .and_then(|m| m.get(&variant.name))
                                .cloned()
                                .unwrap_or_default();
                            let bindings: Vec<Option<&ast::Identifier>> = match &data_pattern.kind {
                                ast::PatternKind::Identifier(binding) => vec![Some(binding)],
                                ast::PatternKind::Tuple(items) => items
                                    .iter()
                                    .map(|item| match &item.kind {
                                        ast::PatternKind::Identifier(binding) => Some(binding),
                                        _ => None,
                                    })
                                    .collect(),
                                _ => Vec::new(),
                            };
                            if !bindings.is_empty() {
                                let data_ptr = self
                                    .builder
                                    .build_struct_gep(struct_ty, scrut_ptr, 1, "match.data.ptr")
                                    .map_err(|e| {
                                        CodegenError::new(format!("match data GEP: {e}"))
                                    })?;
                                let target_data = TargetData::create(
                                    self.module.get_data_layout().as_str().to_str().unwrap(),
                                );
                                let mut byte_offset: u32 = 0;
                                for (i, pt) in payload_types.iter().enumerate() {
                                    let llvm_ty = self.lower_basic_type(pt)?;
                                    let binding = bindings.get(i).copied().flatten();
                                    if let Some(binding) = binding {
                                        let field_ptr =
                                            if byte_offset == 0 {
                                                data_ptr
                                            } else {
                                                unsafe {
                                                    self.builder.build_gep(
                                                        self.context.i8_type(),
                                                        data_ptr,
                                                        &[self
                                                            .context
                                                            .i32_type()
                                                            .const_int(byte_offset as u64, false)],
                                                        "match.field.gep",
                                                    )
                                                }
                                                .map_err(|e| {
                                                    CodegenError::new(format!(
                                                        "GEP match field: {e}"
                                                    ))
                                                })?
                                            };
                                        let cast_ptr = self
                                            .builder
                                            .build_pointer_cast(
                                                field_ptr,
                                                self.context.ptr_type(AddressSpace::default()),
                                                "data.cast",
                                            )
                                            .map_err(|e| {
                                                CodegenError::new(format!("pointer cast: {e}"))
                                            })?;
                                        let loaded = self
                                            .builder
                                            .build_load(llvm_ty, cast_ptr, &binding.name)
                                            .map_err(|e| {
                                                CodegenError::new(format!("load data payload: {e}"))
                                            })?;
                                        let alloca = self.create_entry_alloca(
                                            function,
                                            &binding.name,
                                            llvm_ty,
                                        )?;
                                        self.builder.build_store(alloca, loaded).map_err(|e| {
                                            CodegenError::new(format!("store data binding: {e}"))
                                        })?;
                                        if let Some(scope) = self.variables.last_mut() {
                                            scope.insert(
                                                binding.name.clone(),
                                                VarInfo {
                                                    ptr: alloca,
                                                    ty: pt.clone(),
                                                    is_mutable: false,
                                                    is_volatile: false,
                                                },
                                            );
                                        }
                                    }
                                    byte_offset += target_data.get_abi_size(&llvm_ty) as u32;
                                }
                            }
                        }
                    } else {
                        // Unit enum — compare integer discriminant
                        if let BasicValueEnum::IntValue(scrutinee_int) = scrutinee {
                            let expected =
                                if let Some(variants) = self.enum_variants.get(&enum_name) {
                                    if let Some(val) = variants.get(&variant.name) {
                                        scrutinee_int.get_type().const_int(*val as u64, true)
                                    } else {
                                        return Err(CodegenError::with_span(
                                            format!(
                                                "unknown variant '{}' in enum '{}'",
                                                variant.name, enum_name
                                            ),
                                            variant.span,
                                        ));
                                    }
                                } else {
                                    return Err(CodegenError::with_span(
                                        format!("unknown enum '{}' in match", enum_name),
                                        arm.pattern.span,
                                    ));
                                };
                            let cond = self
                                .builder
                                .build_int_compare(
                                    IntPredicate::EQ,
                                    scrutinee_int,
                                    expected,
                                    "match.enum.disc",
                                )
                                .map_err(|e| {
                                    CodegenError::new(format!("match enum disc compare: {e}"))
                                })?;
                            self.builder
                                .build_conditional_branch(cond, arm_bb, next_bb)
                                .map_err(|e| {
                                    CodegenError::new(format!("match enum branch: {e}"))
                                })?;
                            self.builder.position_at_end(arm_bb);
                            self.push_scope();
                        } else {
                            return Err(CodegenError::with_span(
                                "unit enum match requires integer scrutinee".to_string(),
                                arm.pattern.span,
                            ));
                        }
                    }
                }
                _ => {
                    return Err(CodegenError::with_span(
                        "match pattern kind is not supported in LLVM IR codegen yet",
                        arm.pattern.span,
                    ));
                }
            }
            if !matches!(arm.pattern.kind, ast::PatternKind::Enum { .. }) {
                self.builder.position_at_end(arm_bb);
                self.push_scope();
            }
            if let ast::PatternKind::Identifier(identifier) = &arm.pattern.kind {
                let function = self.current_fn.ok_or_else(|| {
                    CodegenError::new("no active function for match identifier binding")
                })?;
                let alloca =
                    self.create_entry_alloca(function, &identifier.name, scrutinee.get_type())?;
                self.builder.build_store(alloca, scrutinee).map_err(|e| {
                    CodegenError::with_span(
                        format!("failed to bind match identifier `{}`: {e}", identifier.name),
                        identifier.span,
                    )
                })?;

                let inferred = self.infer_ast_type_from_value(&scrutinee, &identifier.span);
                if let Some(scope) = self.variables.last_mut() {
                    scope.insert(
                        identifier.name.clone(),
                        VarInfo {
                            ptr: alloca,
                            ty: inferred,
                            is_mutable: false,
                            is_volatile: false,
                        },
                    );
                }
            }

            let mut arm_value = self.emit_expression_value(&arm.body)?;
            // Cast to first arm's type if needed (handles i32 vs i64 mismatch)
            if let Some((first_value, _)) = incoming.first() {
                let target_ty = first_value.get_type();
                if arm_value.get_type() != target_ty {
                    arm_value =
                        self.cast_value_to_basic_type(arm_value, target_ty, &arm.body.span)?;
                }
            }
            let arm_end = self
                .builder
                .get_insert_block()
                .ok_or_else(|| CodegenError::new("missing match arm insertion block"))?;
            let arm_terminated = arm_end.get_terminator().is_some();
            if !arm_terminated {
                self.builder
                    .build_unconditional_branch(end_bb)
                    .map_err(|e| CodegenError::new(format!("failed match expr end branch: {e}")))?;
                incoming.push((arm_value, arm_end));
            }
            self.pop_scope();

            match arm.pattern.kind {
                ast::PatternKind::Wildcard | ast::PatternKind::Identifier(_) => {
                    cond_bb = next_bb;
                    catch_all = true;
                    break;
                }
                _ => {
                    cond_bb = next_bb;
                }
            }
        }

        if !catch_all {
            self.builder.position_at_end(cond_bb);
            let cond_terminated = self
                .builder
                .get_insert_block()
                .and_then(|bb| bb.get_terminator())
                .is_some();
            if !cond_terminated {
                self.builder.build_unreachable().map_err(|e| {
                    CodegenError::new(format!("failed final match expr branch: {e}"))
                })?;
            }
        } else {
            // Terminate the dead next_bb created for the catch-all arm
            self.builder.position_at_end(cond_bb);
            self.builder
                .build_unreachable()
                .map_err(|e| CodegenError::new(format!("failed catch-all terminator: {e}")))?;
        }

        if incoming.is_empty() {
            return Err(CodegenError::with_span(
                "match expression has no value-producing path",
                *span,
            ));
        }

        let phi_ty = incoming[0].0.get_type();
        for (value, _) in incoming.iter().skip(1) {
            if value.get_type() != phi_ty {
                return Err(CodegenError::with_span(
                    "match expression arms produce different types",
                    *span,
                ));
            }
        }

        self.builder.position_at_end(end_bb);
        let phi = self
            .builder
            .build_phi(phi_ty, "match.expr")
            .map_err(|e| CodegenError::new(format!("failed to build match phi: {e}")))?;
        let refs: Vec<(
            &dyn BasicValue<'ctx>,
            inkwell::basic_block::BasicBlock<'ctx>,
        )> = incoming
            .iter()
            .map(|(value, bb)| (value as &dyn BasicValue<'ctx>, *bb))
            .collect();
        phi.add_incoming(&refs);
        Ok(phi.as_basic_value())
    }

    /// Resolves an assignable expression to a pointer + AST type pair.
    ///
    /// This is the shared lvalue path used by assignment, compound-assignment,
    /// and inc/dec so all of them support identifiers, struct fields, and
    /// array indexes consistently.
    pub(crate) fn resolve_lvalue_ptr(
        &mut self,
        expr: &ast::Expression,
    ) -> CodegenResult<(PointerValue<'ctx>, ast::Type)> {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Identifier(identifier) => {
                if let Some(storage) = self.lookup_storage(&identifier.name) {
                    return Ok(storage);
                }
                let name = &identifier.name;
                let mut func = self.module.get_function(name);
                let mut llvm_name = name.clone();
                if func.is_none()
                    && let Some(mangled) = self.imported_function_links.get(name)
                {
                    func = self.module.get_function(mangled);
                    if func.is_some() {
                        llvm_name = mangled.clone();
                    }
                }
                if let Some(f) = func {
                    let sig = self.signature_for_name(&llvm_name);
                    let return_type = sig
                        .as_ref()
                        .and_then(|s| s.return_type.clone())
                        .unwrap_or_else(|| ast::Type {
                            kind: Box::new(ast::TypeKind::Primitive(ast::PrimitiveType::Void)),
                            span: identifier.span,
                        });
                    let parameters = sig.as_ref().map(|s| s.params.clone()).unwrap_or_default();
                    let fn_ty = ast::Type {
                        kind: Box::new(ast::TypeKind::Function(ast::FunctionType {
                            parameters,
                            return_type: Box::new(return_type),
                        })),
                        span: identifier.span,
                    };
                    return Ok((f.as_global_value().as_pointer_value(), fn_ty));
                }
                Err(CodegenError::with_span(
                    format!("unknown variable `{}`", identifier.name),
                    identifier.span,
                ))
            }
            ast::ExpressionKind::FieldAccess { object, field } => {
                let (object_ptr, object_ty) = self.resolve_lvalue_ptr(object)?;
                let (struct_ptr, named) = match object_ty.kind.as_ref() {
                    ast::TypeKind::Named(named) => (object_ptr, named),
                    ast::TypeKind::Reference(reference) => {
                        let ast::TypeKind::Named(named) = reference.inner.kind.as_ref() else {
                            return Err(CodegenError::with_span(
                                "field access on reference requires a struct pointee",
                                object.span,
                            ));
                        };
                        let ref_llvm_ty = self.lower_basic_type(&object_ty)?;
                        let loaded = self
                            .builder
                            .build_load(ref_llvm_ty, object_ptr, "field.ref.load")
                            .map_err(|e| {
                                CodegenError::with_span(
                                    format!("failed to load reference receiver: {e}"),
                                    object.span,
                                )
                            })?;
                        let BasicValueEnum::PointerValue(struct_ptr) = loaded else {
                            return Err(CodegenError::with_span(
                                "reference receiver did not lower to a pointer",
                                object.span,
                            ));
                        };
                        (struct_ptr, named)
                    }
                    ast::TypeKind::Pointer(pointer) => {
                        let ast::TypeKind::Named(named) = pointer.inner.kind.as_ref() else {
                            return Err(CodegenError::with_span(
                                "field access on pointer requires a struct pointee",
                                object.span,
                            ));
                        };
                        let ptr_llvm_ty = self.lower_basic_type(&object_ty)?;
                        let loaded = self
                            .builder
                            .build_load(ptr_llvm_ty, object_ptr, "field.ptr.load")
                            .map_err(|e| {
                                CodegenError::with_span(
                                    format!("failed to load pointer receiver: {e}"),
                                    object.span,
                                )
                            })?;
                        let BasicValueEnum::PointerValue(struct_ptr) = loaded else {
                            return Err(CodegenError::with_span(
                                "pointer receiver did not lower to a pointer",
                                object.span,
                            ));
                        };
                        (struct_ptr, named)
                    }
                    _ => {
                        return Err(CodegenError::with_span(
                            "field access currently supports only struct values/references/pointers",
                            object.span,
                        ));
                    }
                };

                let _ = self.ensure_named_struct_type(named)?;
                let owner_name = Self::named_type_key(named);
                let fields = self.struct_fields.get(&owner_name).ok_or_else(|| {
                    CodegenError::with_span(
                        format!("unknown struct type `{owner_name}`"),
                        object.span,
                    )
                })?;
                let Some((field_index, (_, field_ty))) = fields
                    .iter()
                    .enumerate()
                    .find(|(_, (name, _))| name == &field.name)
                else {
                    return Err(CodegenError::with_span(
                        format!("unknown field `{}` on `{owner_name}`", field.name),
                        field.span,
                    ));
                };

                let struct_ty = *self.struct_types.get(&owner_name).ok_or_else(|| {
                    CodegenError::with_span(
                        format!("missing LLVM struct type for `{owner_name}`"),
                        object.span,
                    )
                })?;

                let field_ptr = self
                    .builder
                    .build_struct_gep(struct_ty, struct_ptr, field_index as u32, &field.name)
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed struct field access: {e}"),
                            field.span,
                        )
                    })?;

                Ok((field_ptr, field_ty.clone()))
            }
            ast::ExpressionKind::Unary {
                operator: ast::UnaryOperator::Dereference,
                operand,
            } => {
                let (operand_ptr, operand_ty) = self.resolve_lvalue_ptr(operand)?;
                let ptr_llvm_ty = self.lower_basic_type(&operand_ty)?;
                let loaded_ptr = self
                    .builder
                    .build_load(ptr_llvm_ty, operand_ptr, "deref.lvalue.ptr")
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to load dereference operand for lvalue: {e}"),
                            expr.span,
                        )
                    })?;
                let BasicValueEnum::PointerValue(ptr) = loaded_ptr else {
                    return Err(CodegenError::with_span(
                        "dereference operand must be a pointer value",
                        expr.span,
                    ));
                };
                let inner_ty = match operand_ty.kind.as_ref() {
                    ast::TypeKind::Pointer(pointer) => *pointer.inner.clone(),
                    ast::TypeKind::Reference(reference) => *reference.inner.clone(),
                    _ => {
                        return Err(CodegenError::with_span(
                            "dereference requires pointer or reference type",
                            expr.span,
                        ));
                    }
                };
                Ok((ptr, inner_ty))
            }
            ast::ExpressionKind::Index { object, index } => {
                let (object_ptr, object_ty) = self.resolve_lvalue_ptr(object)?;
                match object_ty.kind.as_ref() {
                    ast::TypeKind::Slice(_slice) => Err(CodegenError::with_span(
                        "indexing Slice through resolve_lvalue_ptr is not supported — use the trait path (__index_get)",
                        object.span,
                    )),
                    ast::TypeKind::Pointer(pointer) => {
                        let ptr_llvm_ty = self.lower_basic_type(&object_ty)?;
                        let loaded = self
                            .builder
                            .build_load(ptr_llvm_ty, object_ptr, "ptr.idx.load")
                            .map_err(|e| {
                                CodegenError::with_span(
                                    format!("failed to load pointer for indexing: {e}"),
                                    object.span,
                                )
                            })?;
                        let BasicValueEnum::PointerValue(base_ptr) = loaded else {
                            return Err(CodegenError::with_span(
                                "pointer indexing requires a pointer value",
                                object.span,
                            ));
                        };
                        let index_value = self.emit_expression_value(index)?;
                        let BasicValueEnum::IntValue(index_int) = index_value else {
                            return Err(CodegenError::with_span(
                                "pointer index must be an integer",
                                index.span,
                            ));
                        };
                        let i64_ty = self.context.i64_type();
                        let index_i64 = if index_int.get_type().get_bit_width() == 64 {
                            index_int
                        } else {
                            self.builder
                                .build_int_cast(index_int, i64_ty, "idx.cast")
                                .map_err(|e| {
                                    CodegenError::with_span(
                                        format!("failed to cast pointer index: {e}"),
                                        index.span,
                                    )
                                })?
                        };
                        let element_llvm_ty = self.lower_basic_type(&pointer.inner)?;
                        let element_ptr = unsafe {
                            self.builder.build_in_bounds_gep(
                                element_llvm_ty,
                                base_ptr,
                                &[index_i64],
                                "ptr.idx.ptr",
                            )
                        }
                        .map_err(|e| {
                            CodegenError::with_span(
                                format!("failed pointer indexing: {e}"),
                                index.span,
                            )
                        })?;
                        Ok((element_ptr, (*pointer.inner).clone()))
                    }
                    ast::TypeKind::Array(array) => {
                        let index_value = self.emit_expression_value(index)?;
                        let BasicValueEnum::IntValue(index_int) = index_value else {
                            return Err(CodegenError::with_span(
                                "array index must be an integer",
                                index.span,
                            ));
                        };
                        let i64_ty = self.context.i64_type();
                        let index_i64 = if index_int.get_type().get_bit_width() == 64 {
                            index_int
                        } else {
                            self.builder
                                .build_int_cast(index_int, i64_ty, "idx.cast")
                                .map_err(|e| {
                                    CodegenError::with_span(
                                        format!("failed to cast array index: {e}"),
                                        index.span,
                                    )
                                })?
                        };
                        let array_llvm_ty = self.lower_basic_type(&object_ty)?;
                        let zero = i64_ty.const_zero();
                        let element_ptr = unsafe {
                            self.builder.build_in_bounds_gep(
                                array_llvm_ty,
                                object_ptr,
                                &[zero, index_i64],
                                "arr.idx.ptr",
                            )
                        }
                        .map_err(|e| {
                            CodegenError::with_span(
                                format!("failed array index GEP: {e}"),
                                index.span,
                            )
                        })?;
                        Ok((element_ptr, (*array.element_type).clone()))
                    }
                    _ => Err(CodegenError::with_span(
                        "index access currently supports only array and pointer values",
                        object.span,
                    )),
                }
            }
            _ => Err(CodegenError::with_span(
                "expression is not assignable",
                expr.span,
            )),
        }
    }
}
