use inkwell::AddressSpace;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use inkwell::targets::TargetData;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue};

use crate::codegen::llvm_ir::LlvmIrGenerator;
use crate::codegen::{CodegenError, CodegenResult};
use crate::lexer::Span;
use crate::parser::ast;
use crate::semantic::typeck::{operator_method_name, unary_operator_method_name};

impl<'ctx> LlvmIrGenerator<'ctx> {
    pub(crate) fn emit_unary_expression(
        &mut self,
        operator: &ast::UnaryOperator,
        operand: &ast::Expression,
        whole_expr: &ast::Expression,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        // Value-using operators emit the operand exactly ONCE, then either
        // dispatch to an operator overload (struct types) or apply the built-in
        // operation to that same value. Re-emitting the operand inside each arm
        // would evaluate side-effecting operands (e.g. `a.compare_exchange(..)`)
        // twice, corrupting atomic and other stateful operations.
        match operator {
            ast::UnaryOperator::Plus
            | ast::UnaryOperator::Minus
            | ast::UnaryOperator::Not
            | ast::UnaryOperator::BitwiseNot => {
                let operand_val = self.emit_expression_value(operand)?;
                if operand_val.get_type().is_struct_type()
                    && let Some(method_name) = unary_operator_method_name(operator)
                {
                    let method_ident = ast::Identifier {
                        name: method_name.to_string(),
                        span: whole_expr.span,
                    };
                    let result = self.emit_method_call_expression(
                        operand,
                        &method_ident,
                        &[],
                        false,
                        &whole_expr.span,
                    )?;
                    if let Some(val) = result {
                        return Ok(val);
                    }
                }
                match operator {
                    ast::UnaryOperator::Plus => Ok(operand_val),
                    ast::UnaryOperator::Minus => match operand_val {
                        BasicValueEnum::IntValue(int_value) => self
                            .builder
                            .build_int_neg(int_value, "ineg")
                            .map(|v| v.as_basic_value_enum())
                            .map_err(|e| {
                                CodegenError::with_span(
                                    format!("failed integer negation: {e}"),
                                    whole_expr.span,
                                )
                            }),
                        BasicValueEnum::FloatValue(float_value) => self
                            .builder
                            .build_float_neg(float_value, "fneg")
                            .map(|v| v.as_basic_value_enum())
                            .map_err(|e| {
                                CodegenError::with_span(
                                    format!("failed float negation: {e}"),
                                    whole_expr.span,
                                )
                            }),
                        _ => Err(CodegenError::with_span(
                            "unary minus requires numeric operand",
                            whole_expr.span,
                        )),
                    },
                    ast::UnaryOperator::Not => {
                        let bool_value = self.emit_as_bool(&operand_val, &operand.span)?;
                        self.builder
                            .build_not(bool_value, "lnot")
                            .map(|v| v.as_basic_value_enum())
                            .map_err(|e| {
                                CodegenError::with_span(
                                    format!("failed logical not: {e}"),
                                    whole_expr.span,
                                )
                            })
                    }
                    ast::UnaryOperator::BitwiseNot => {
                        let BasicValueEnum::IntValue(int_value) = operand_val else {
                            return Err(CodegenError::with_span(
                                "bitwise not requires integer operand",
                                whole_expr.span,
                            ));
                        };
                        self.builder
                            .build_not(int_value, "bnot")
                            .map(|v| v.as_basic_value_enum())
                            .map_err(|e| {
                                CodegenError::with_span(
                                    format!("failed bitwise not: {e}"),
                                    whole_expr.span,
                                )
                            })
                    }
                    _ => unreachable!("value-using unary arms are exhaustive above"),
                }
            }
            ast::UnaryOperator::Dereference => {
                let (operand_ptr, operand_ty) = self.resolve_lvalue_ptr(operand)?;
                let ptr_llvm_ty = self.lower_basic_type(&operand_ty)?;
                let loaded_ptr = self
                    .builder
                    .build_load(ptr_llvm_ty, operand_ptr, "deref.ptr")
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to load dereference operand: {e}"),
                            whole_expr.span,
                        )
                    })?;
                let inner_ty = match operand_ty.kind.as_ref() {
                    ast::TypeKind::Pointer(pointer) => &pointer.inner,
                    ast::TypeKind::Reference(reference) => &reference.inner,
                    _ => {
                        return Err(CodegenError::with_span(
                            "dereference requires pointer or reference operand",
                            whole_expr.span,
                        ));
                    }
                };
                let BasicValueEnum::PointerValue(ptr_value) = loaded_ptr else {
                    return Err(CodegenError::with_span(
                        "dereference operand did not lower to a pointer",
                        whole_expr.span,
                    ));
                };
                let inner_llvm_ty = self.lower_basic_type(inner_ty)?;
                if self.lvalue_is_volatile(whole_expr) {
                    return self.emit_volatile_load(inner_llvm_ty, ptr_value, "deref.load");
                }
                self.builder
                    .build_load(inner_llvm_ty, ptr_value, "deref.load")
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to load dereference result: {e}"),
                            whole_expr.span,
                        )
                    })
            }
            ast::UnaryOperator::Increment => self.emit_inc_dec(operand, true, false, whole_expr),
            ast::UnaryOperator::Decrement => self.emit_inc_dec(operand, false, false, whole_expr),
        }
    }

    pub(crate) fn emit_postfix_expression(
        &mut self,
        operator: &ast::UnaryOperator,
        operand: &ast::Expression,
        whole_expr: &ast::Expression,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match operator {
            ast::UnaryOperator::Increment => self.emit_inc_dec(operand, true, true, whole_expr),
            ast::UnaryOperator::Decrement => self.emit_inc_dec(operand, false, true, whole_expr),
            _ => Err(CodegenError::with_span(
                "unsupported postfix operator in LLVM IR codegen",
                whole_expr.span,
            )),
        }
    }

    pub(crate) fn emit_inc_dec(
        &mut self,
        operand: &ast::Expression,
        increment: bool,
        return_old: bool,
        whole_expr: &ast::Expression,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        self.check_assignment_mutability(operand)?;
        let (target_ptr, target_ty) = self.resolve_lvalue_ptr(operand)?;
        let llvm_ty = self.lower_basic_type(&target_ty)?;
        let volatile = self.lvalue_is_volatile(operand);
        let current = if volatile {
            self.emit_volatile_load(llvm_ty, target_ptr, "incdec.load")?
        } else {
            self.builder
                .build_load(llvm_ty, target_ptr, "incdec.load")
                .map_err(|e| CodegenError::new(format!("load failed: {e}")))?
        };

        let updated = match current {
            BasicValueEnum::IntValue(value) => {
                let one = value.get_type().const_int(1, false);
                let next = if increment {
                    self.builder
                        .build_int_add(value, one, "inc")
                        .map_err(|e| CodegenError::new(format!("inc failed: {e}")))?
                } else {
                    self.builder
                        .build_int_sub(value, one, "dec")
                        .map_err(|e| CodegenError::new(format!("dec failed: {e}")))?
                };
                next.as_basic_value_enum()
            }
            BasicValueEnum::FloatValue(value) => {
                let one = value.get_type().const_float(1.0);
                let next = if increment {
                    self.builder
                        .build_float_add(value, one, "finc")
                        .map_err(|e| CodegenError::new(format!("inc failed: {e}")))?
                } else {
                    self.builder
                        .build_float_sub(value, one, "fdec")
                        .map_err(|e| CodegenError::new(format!("dec failed: {e}")))?
                };
                next.as_basic_value_enum()
            }
            _ => {
                return Err(CodegenError::with_span(
                    "increment/decrement requires numeric operand",
                    whole_expr.span,
                ));
            }
        };

        if volatile {
            self.emit_volatile_store(target_ptr, updated)?;
        } else {
            self.builder.build_store(target_ptr, updated).map_err(|e| {
                CodegenError::with_span(format!("failed to update value: {e}"), whole_expr.span)
            })?;
        }

        if return_old { Ok(current) } else { Ok(updated) }
    }

    /// Check that an assignment target is mutable. Returns Ok(()) or a CodegenError
    /// with a message like "cannot assign to const variable 'x'".
    pub(crate) fn check_assignment_mutability(
        &mut self,
        target: &ast::Expression,
    ) -> CodegenResult<()> {
        match target.kind.as_ref() {
            ast::ExpressionKind::Identifier(ident) => {
                if let Some(info) = self.lookup_variable(&ident.name) {
                    if !info.is_mutable {
                        return Err(CodegenError::with_span(
                            format!("cannot assign to const variable '{}'", ident.name),
                            ident.span,
                        ));
                    }
                    Ok(())
                } else if self.global_variables.contains_key(&ident.name)
                    || self.extern_globals.contains_key(&ident.name)
                    || self.lookup_module_global(&ident.name).is_some()
                {
                    Ok(()) // globals are always mutable
                } else {
                    Err(CodegenError::with_span(
                        format!("unknown variable `{}`", ident.name),
                        ident.span,
                    ))
                }
            }
            ast::ExpressionKind::FieldAccess { object, .. } => {
                if let Some(ty) = self.resolve_receiver_type(object) {
                    match ty.kind.as_ref() {
                        ast::TypeKind::Pointer(ptr) => {
                            if !ptr.is_mutable {
                                return Err(CodegenError::with_span(
                                    "cannot write through immutable pointer",
                                    object.span,
                                ));
                            }
                            return Ok(());
                        }
                        ast::TypeKind::Reference(r) => {
                            if !r.is_mutable {
                                return Err(CodegenError::with_span(
                                    "cannot write through immutable reference",
                                    object.span,
                                ));
                            }
                            return Ok(());
                        }
                        _ => {}
                    }
                }
                self.check_assignment_mutability(object)
            }
            ast::ExpressionKind::Unary {
                operator: ast::UnaryOperator::Dereference,
                operand,
            } => {
                // *ptr = val — check pointer/reference type mutability
                if let ast::ExpressionKind::Identifier(ident) = operand.kind.as_ref()
                    && let Some(ty) = self.lookup_value_type(&ident.name)
                {
                    match ty.kind.as_ref() {
                        ast::TypeKind::Pointer(ptr) if !ptr.is_mutable => {
                            return Err(CodegenError::with_span(
                                "cannot write through immutable pointer",
                                ident.span,
                            ));
                        }
                        ast::TypeKind::Reference(r) if !r.is_mutable => {
                            return Err(CodegenError::with_span(
                                "cannot write through immutable reference",
                                ident.span,
                            ));
                        }
                        _ => {}
                    }
                }
                Ok(())
            }
            ast::ExpressionKind::Index { object, .. } => self.check_assignment_mutability(object),
            _ => Ok(()),
        }
    }

    pub(crate) fn emit_binary_expression(
        &mut self,
        left: &ast::Expression,
        operator: &ast::BinaryOperator,
        right: &ast::Expression,
        whole_expr: &ast::Expression,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match operator {
            ast::BinaryOperator::LogicalAnd => {
                self.emit_short_circuit_logical(left, right, true, &whole_expr.span)
            }
            ast::BinaryOperator::LogicalOr => {
                self.emit_short_circuit_logical(left, right, false, &whole_expr.span)
            }
            ast::BinaryOperator::Assign => {
                // For Index on a non-pointer type, emit __index_set instead of direct store
                if let ast::ExpressionKind::Index { object, index } = left.kind.as_ref() {
                    let object_ty = self.resolve_receiver_type(object);
                    if !object_ty.is_some_and(|ty| {
                        matches!(
                            ty.kind.as_ref(),
                            ast::TypeKind::Pointer(_) | ast::TypeKind::Array(_)
                        )
                    }) {
                        self.check_assignment_mutability(left)?;
                        let value = self.emit_expression_value(right)?;
                        let method_ident = ast::Identifier {
                            name: "__index_set".to_string(),
                            span: left.span,
                        };
                        self.emit_method_call_expression(
                            object,
                            &method_ident,
                            &[(**index).clone(), right.clone()],
                            true,
                            &whole_expr.span,
                        )?;
                        return Ok(value);
                    }
                }
                self.check_assignment_mutability(left)?;
                let (target_ptr, target_ty) = self.resolve_lvalue_ptr(left)?;
                let value = if let ast::ExpressionKind::Initializer { items } = right.kind.as_ref()
                {
                    self.emit_typed_initializer_value(items, &target_ty, &right.span)?
                } else {
                    let rhs = self.emit_expression_value(right)?;
                    self.cast_expr_to_ast_type(rhs, Some(right), &target_ty, &right.span)?
                };
                // Release the value being overwritten so `x = y` on a
                // Drop-typed x does not leak the old resource (the scope-exit
                // drop then frees the new value). Skipped for literal
                // self-assignment (`x = x`) and unguarded lvalues.
                let is_self_assign = match (left.kind.as_ref(), right.kind.as_ref()) {
                    (ast::ExpressionKind::Identifier(l), ast::ExpressionKind::Identifier(r)) => {
                        l.name == r.name
                    }
                    _ => false,
                };
                if !is_self_assign {
                    self.emit_assignment_pre_drop(left, target_ptr, &target_ty)?;
                }
                if self.lvalue_is_volatile(left) {
                    self.emit_volatile_store(target_ptr, value)?;
                } else {
                    self.builder.build_store(target_ptr, value).map_err(|e| {
                        CodegenError::with_span(format!("failed assignment: {e}"), whole_expr.span)
                    })?;
                }
                // The incoming value now lives in the target: mark the
                // overwritten field(s) as live so the scope-exit cascade
                // drops them.
                match left.kind.as_ref() {
                    ast::ExpressionKind::FieldAccess { .. } => {
                        self.set_assigned_field_flags(left)?;
                    }
                    ast::ExpressionKind::Identifier(_) => {
                        self.set_all_field_flags(left)?;
                    }
                    _ => {}
                }
                Ok(value)
            }
            ast::BinaryOperator::AddAssign
            | ast::BinaryOperator::SubtractAssign
            | ast::BinaryOperator::MultiplyAssign
            | ast::BinaryOperator::DivideAssign
            | ast::BinaryOperator::ModuloAssign => {
                self.check_assignment_mutability(left)?;
                let (target_ptr, target_ty) = self.resolve_lvalue_ptr(left)?;
                let llvm_ty = self.lower_basic_type(&target_ty)?;
                let lhs = if self.lvalue_is_volatile(left) {
                    self.emit_volatile_load(llvm_ty, target_ptr, "assign.load")?
                } else {
                    self.builder
                        .build_load(llvm_ty, target_ptr, "assign.load")
                        .map_err(|e| CodegenError::new(format!("load failed: {e}")))?
                };
                let rhs = self.emit_expression_value(right)?;

                let updated = if lhs.get_type().is_struct_type() {
                    // Use trait method for struct types: a += b desugars to a = a + b
                    let bin_op = match operator {
                        ast::BinaryOperator::AddAssign => ast::BinaryOperator::Add,
                        ast::BinaryOperator::SubtractAssign => ast::BinaryOperator::Subtract,
                        ast::BinaryOperator::MultiplyAssign => ast::BinaryOperator::Multiply,
                        ast::BinaryOperator::DivideAssign => ast::BinaryOperator::Divide,
                        ast::BinaryOperator::ModuloAssign => ast::BinaryOperator::Modulo,
                        _ => unreachable!(),
                    };
                    if let Some(method_name) = operator_method_name(&bin_op) {
                        let method_ident = ast::Identifier {
                            name: method_name.to_string(),
                            span: whole_expr.span,
                        };
                        let result = self.emit_method_call_expression(
                            left,
                            &method_ident,
                            std::slice::from_ref(right),
                            false,
                            &whole_expr.span,
                        )?;
                        if let Some(val) = result {
                            val
                        } else {
                            return Err(CodegenError::with_span(
                                "operator returned void",
                                whole_expr.span,
                            ));
                        }
                    } else {
                        return Err(CodegenError::with_span(
                            "operator not found for struct type",
                            whole_expr.span,
                        ));
                    }
                } else {
                    let rhs = self.cast_value_to_basic_type(rhs, llvm_ty, &right.span)?;
                    let is_unsigned = type_is_unsigned(&target_ty);
                    self.emit_arith_values(&lhs, operator, &rhs, whole_expr, is_unsigned)?
                };
                if self.lvalue_is_volatile(left) {
                    self.emit_volatile_store(target_ptr, updated)?;
                } else {
                    self.builder.build_store(target_ptr, updated).map_err(|e| {
                        CodegenError::with_span(format!("failed assignment: {e}"), whole_expr.span)
                    })?;
                }
                Ok(updated)
            }
            _ => {
                // For struct types, use trait method call instead of inline
                // IR. The struct check must come from the TYPE, not a value:
                // evaluating the lhs just to inspect it (then again inside
                // the __add dispatch) ran its side effects twice and leaked
                // the first result (String.from_str(x) + y).
                let lhs_struct = self.resolve_receiver_type(left).is_some_and(|ty| {
                    // Pointers/references compare by address, not by __eq:
                    // resolve_receiver_type auto-derefs, so exclude them.
                    !matches!(
                        ty.kind.as_ref(),
                        ast::TypeKind::Pointer(_) | ast::TypeKind::Reference(_)
                    ) && (matches!(ty.kind.as_ref(), ast::TypeKind::Slice(_))
                        || Self::extract_named_type_owned(&ty)
                            .map(|named| {
                                let key = Self::named_type_key(&named);
                                self.struct_fields.contains_key(&key)
                                    || self.struct_fields.contains_key(&named.path[0].name)
                                    || self.enum_payload_layouts.contains_key(&key)
                            })
                            .unwrap_or(false))
                });
                if lhs_struct && let Some(method_name) = operator_method_name(operator) {
                    let method_ident = ast::Identifier {
                        name: method_name.to_string(),
                        span: whole_expr.span,
                    };
                    let result = self.emit_method_call_expression(
                        left,
                        &method_ident,
                        std::slice::from_ref(right),
                        false,
                        &whole_expr.span,
                    )?;
                    if let Some(val) = result {
                        return Ok(val);
                    }
                    return Err(CodegenError::with_span(
                        format!("operator '{method_name}' is not implemented for struct type"),
                        whole_expr.span,
                    ));
                }

                // For str equality (==, !=), use strcmp instead of pointer comparison
                if matches!(
                    operator,
                    ast::BinaryOperator::Equal | ast::BinaryOperator::NotEqual
                ) && (self.expression_is_str_type(left) || self.expression_is_str_type(right))
                {
                    let lhs = self.emit_expression_value(left)?;
                    let rhs = self.emit_expression_value(right)?;
                    return self.emit_strcmp_comparison(lhs, operator, rhs, &whole_expr.span);
                }
                let mut lhs = self.emit_expression_value(left)?;
                let mut rhs = self.emit_expression_value(right)?;
                // Pointer arithmetic (p + i, i + p, p - i) keeps the pointer
                // and integer operands at their own types; only numeric and
                // pointer/pointer pairs are normalized to a common type.
                let is_ptr_arith = matches!(
                    operator,
                    ast::BinaryOperator::Add | ast::BinaryOperator::Subtract
                ) && ((lhs.get_type().is_pointer_type()
                    && rhs.get_type().is_int_type())
                    || (lhs.get_type().is_int_type() && rhs.get_type().is_pointer_type()));
                if !is_ptr_arith && rhs.get_type() != lhs.get_type() {
                    if lhs.get_type().is_int_type() && rhs.get_type().is_int_type() {
                        // Mixed-width integer comparison (e.g. `buf[i] ==
                        // 'x'`: u8 vs char, or an i32 vs an i64 literal):
                        // widen the narrower operand to the wider type so
                        // values are preserved instead of truncated.
                        let lhs_w = lhs.get_type().into_int_type().get_bit_width();
                        let rhs_w = rhs.get_type().into_int_type().get_bit_width();
                        if lhs_w < rhs_w {
                            let left_unsigned = self.expression_is_unsigned(left);
                            lhs = self
                                .widen_int_value(
                                    lhs.into_int_value(),
                                    rhs.get_type().into_int_type(),
                                    left_unsigned,
                                    &left.span,
                                )?
                                .as_basic_value_enum();
                        } else if rhs_w < lhs_w {
                            let right_unsigned = self.expression_is_unsigned(right);
                            rhs = self
                                .widen_int_value(
                                    rhs.into_int_value(),
                                    lhs.get_type().into_int_type(),
                                    right_unsigned,
                                    &right.span,
                                )?
                                .as_basic_value_enum();
                        }
                    } else {
                        rhs = self.cast_value_to_basic_type(rhs, lhs.get_type(), &right.span)?;
                    }
                }
                let is_unsigned = self.expression_is_unsigned(left);
                self.emit_binary_values(&lhs, operator, &rhs, whole_expr, is_unsigned)
            }
        }
    }

    /// Check whether an expression evaluates to `str` type.
    /// Handles the most common expression forms; returns `false` for unhandled kinds
    /// (falling back to standard pointer comparison in those cases).
    pub(crate) fn expression_is_str_type(&mut self, expr: &ast::Expression) -> bool {
        let str_ty = ast::TypeKind::Primitive(ast::PrimitiveType::Str);
        if let ast::ExpressionKind::Literal(ast::Literal::String(_)) = expr.kind.as_ref() {
            return true;
        }
        self.resolve_receiver_type(expr)
            .is_some_and(|ty| *ty.kind == str_ty)
    }

    /// Determine whether an expression evaluates to an unsigned integer type.
    /// Used to pick unsigned LLVM compare predicates (ULT/UGT/ULE/UGE) and
    /// zero-extending casts.
    pub(crate) fn expression_is_unsigned(&mut self, expr: &ast::Expression) -> bool {
        self.resolve_receiver_type(expr)
            .is_some_and(|ty| type_is_unsigned(&ty))
    }

    /// Emit `strcmp(left, right)` and compare the result against zero.
    /// Used for `str == str` and `str != str` comparisons.
    ///
    /// If either operand is a null pointer, falls back to pointer equality
    /// (null check semantics like `other == (str)0`).
    pub(crate) fn emit_strcmp_comparison(
        &mut self,
        lhs: BasicValueEnum<'ctx>,
        operator: &ast::BinaryOperator,
        rhs: BasicValueEnum<'ctx>,
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        // Both values must be pointers (str is ptr_type)
        let lhs_ptr = match lhs {
            BasicValueEnum::PointerValue(p) => p,
            _ => {
                return Err(CodegenError::with_span(
                    "strcmp requires pointer operands",
                    *span,
                ));
            }
        };
        let rhs_ptr = match rhs {
            BasicValueEnum::PointerValue(p) => p,
            _ => {
                return Err(CodegenError::with_span(
                    "strcmp requires pointer operands",
                    *span,
                ));
            }
        };

        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for strcmp"))?;
        let i1_ty = self.context.bool_type();
        let i32_ty = self.context.i32_type();
        let i64_ty = self.context.i64_type();
        let ptr_ty = self.context.ptr_type(AddressSpace::default());

        // Declare strcmp if not already present: int strcmp(const char*, const char*)
        let strcmp_ty = i32_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false);
        let strcmp_fn = self
            .module
            .get_function("strcmp")
            .unwrap_or_else(|| self.module.add_function("strcmp", strcmp_ty, None));

        // Check: if lhs_ptr == null || rhs_ptr == null → use pointer equality
        let lhs_is_null = self
            .builder
            .build_is_null(lhs_ptr, "strcmp.lhs_null")
            .map_err(|e| CodegenError::new(format!("strcmp null check: {e}")))?;
        let rhs_is_null = self
            .builder
            .build_is_null(rhs_ptr, "strcmp.rhs_null")
            .map_err(|e| CodegenError::new(format!("strcmp null check: {e}")))?;
        let any_null = self
            .builder
            .build_or(lhs_is_null, rhs_is_null, "strcmp.any_null")
            .map_err(|e| CodegenError::new(format!("strcmp null or: {e}")))?;

        // Create blocks: then compare pointers, else call strcmp, merge
        let ptr_cmp_bb = self.context.append_basic_block(function, "strcmp.ptr");
        let strcmp_bb = self.context.append_basic_block(function, "strcmp.call");
        let merge_bb = self.context.append_basic_block(function, "strcmp.merge");

        self.builder
            .build_conditional_branch(any_null, ptr_cmp_bb, strcmp_bb)
            .map_err(|e| CodegenError::new(format!("strcmp branch: {e}")))?;

        // Block 1: pointer comparison
        self.builder.position_at_end(ptr_cmp_bb);
        let ptr_pred = match operator {
            ast::BinaryOperator::Equal => IntPredicate::EQ,
            ast::BinaryOperator::NotEqual => IntPredicate::NE,
            _ => unreachable!(),
        };
        let lhs_int = self
            .builder
            .build_ptr_to_int(lhs_ptr, i64_ty, "strcmp.ptr.lhs")
            .map_err(|e| CodegenError::new(format!("strcmp ptr-to-int: {e}")))?;
        let rhs_int = self
            .builder
            .build_ptr_to_int(rhs_ptr, i64_ty, "strcmp.ptr.rhs")
            .map_err(|e| CodegenError::new(format!("strcmp ptr-to-int: {e}")))?;
        let ptr_result = self
            .builder
            .build_int_compare(ptr_pred, lhs_int, rhs_int, "strcmp.ptr.cmp")
            .map_err(|e| CodegenError::new(format!("strcmp ptr compare: {e}")))?;
        let ptr_result_bb = self
            .builder
            .get_insert_block()
            .ok_or_else(|| CodegenError::new("missing ptr cmp block"))?;
        self.builder
            .build_unconditional_branch(merge_bb)
            .map_err(|e| CodegenError::new(format!("strcmp branch merge: {e}")))?;

        // Block 2: strcmp call
        self.builder.position_at_end(strcmp_bb);
        let call_result = self
            .builder
            .build_call(strcmp_fn, &[lhs_ptr.into(), rhs_ptr.into()], "strcmp")
            .map_err(|e| CodegenError::new(format!("strcmp call failed: {e}")))?;
        let result_val = call_result
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::new("strcmp returned void"))?;
        let result_int = result_val.into_int_value();

        // Compare strcmp result against 0
        let zero = i32_ty.const_zero();
        let cmp = self
            .builder
            .build_int_compare(ptr_pred, result_int, zero, "strcmp.cmp")
            .map_err(|e| CodegenError::new(format!("strcmp compare failed: {e}")))?;
        let strcmp_result_bb = self
            .builder
            .get_insert_block()
            .ok_or_else(|| CodegenError::new("missing strcmp block"))?;
        self.builder
            .build_unconditional_branch(merge_bb)
            .map_err(|e| CodegenError::new(format!("strcmp branch merge: {e}")))?;

        // Merge block: phi node for the result
        self.builder.position_at_end(merge_bb);
        let phi = self
            .builder
            .build_phi(i1_ty, "strcmp.result")
            .map_err(|e| CodegenError::new(format!("strcmp phi: {e}")))?;
        phi.add_incoming(&[(&ptr_result, ptr_result_bb), (&cmp, strcmp_result_bb)]);
        Ok(phi.as_basic_value().as_basic_value_enum())
    }

    /// Emits short-circuiting `&&` / `||` semantics with explicit control flow.
    ///
    /// - `&&`: false short-circuits directly to `logic.cont`
    /// - `||`: true short-circuits directly to `logic.cont`
    ///
    /// A PHI in `logic.cont` merges the short-circuit constant and RHS result.
    pub(crate) fn emit_short_circuit_logical(
        &mut self,
        left: &ast::Expression,
        right: &ast::Expression,
        is_and: bool,
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for logical expression"))?;

        let lhs_value = self.emit_expression_value(left)?;
        let lhs_bool = self.emit_as_bool(&lhs_value, &left.span)?;
        let lhs_block = self
            .builder
            .get_insert_block()
            .ok_or_else(|| CodegenError::new("builder is not positioned in a basic block"))?;

        let rhs_bb = self.context.append_basic_block(function, "logic.rhs");
        let cont_bb = self.context.append_basic_block(function, "logic.cont");

        if is_and {
            self.builder
                .build_conditional_branch(lhs_bool, rhs_bb, cont_bb)
                .map_err(|e| CodegenError::new(format!("failed logical-and branch: {e}")))?;
        } else {
            self.builder
                .build_conditional_branch(lhs_bool, cont_bb, rhs_bb)
                .map_err(|e| CodegenError::new(format!("failed logical-or branch: {e}")))?;
        }

        self.builder.position_at_end(rhs_bb);
        let rhs_value = self.emit_expression_value(right)?;
        let rhs_bool = self.emit_as_bool(&rhs_value, &right.span)?;
        let rhs_end = self
            .builder
            .get_insert_block()
            .ok_or_else(|| CodegenError::new("missing rhs insertion block"))?;
        let rhs_terminated = rhs_end.get_terminator().is_some();
        if !rhs_terminated {
            self.builder
                .build_unconditional_branch(cont_bb)
                .map_err(|e| CodegenError::new(format!("failed rhs->cont branch: {e}")))?;
        }

        self.builder.position_at_end(cont_bb);
        let phi = self
            .builder
            .build_phi(self.context.bool_type(), "logic.phi")
            .map_err(|e| CodegenError::new(format!("failed logical phi: {e}")))?;

        let short_const = self
            .context
            .bool_type()
            .const_int(u64::from(!is_and), false);
        let mut incoming: Vec<(
            &dyn BasicValue<'ctx>,
            inkwell::basic_block::BasicBlock<'ctx>,
        )> = vec![(&short_const, lhs_block)];

        if !rhs_terminated {
            incoming.push((&rhs_bool, rhs_end));
        }

        if incoming.len() < 2 {
            return Err(CodegenError::with_span(
                "logical expression has no value-producing rhs path",
                *span,
            ));
        }

        phi.add_incoming(&incoming);
        Ok(phi.as_basic_value())
    }

    pub(crate) fn emit_arith_values(
        &mut self,
        lhs: &BasicValueEnum<'ctx>,
        operator: &ast::BinaryOperator,
        rhs: &BasicValueEnum<'ctx>,
        whole_expr: &ast::Expression,
        is_unsigned: bool,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        // Pointer arithmetic: p + i, i + p, p - i. The offset is scaled by
        // the pointee size via GEP; str pointers step in bytes. Uses plain
        // (non-inbounds) GEP so p - n beyond the object stays defined.
        if matches!(
            operator,
            ast::BinaryOperator::Add | ast::BinaryOperator::Subtract
        ) {
            let (left_ast, right_ast) = match whole_expr.kind.as_ref() {
                ast::ExpressionKind::Binary { left, right, .. } => (left.as_ref(), right.as_ref()),
                _ => (whole_expr, whole_expr),
            };
            if let (BasicValueEnum::PointerValue(ptr), BasicValueEnum::IntValue(offset)) =
                (lhs, rhs)
            {
                return self.emit_pointer_arith(
                    *ptr,
                    *offset,
                    operator,
                    left_ast,
                    &whole_expr.span,
                );
            }
            if *operator == ast::BinaryOperator::Add
                && let (BasicValueEnum::IntValue(offset), BasicValueEnum::PointerValue(ptr)) =
                    (lhs, rhs)
            {
                return self.emit_pointer_arith(
                    *ptr,
                    *offset,
                    operator,
                    right_ast,
                    &whole_expr.span,
                );
            }
        }
        match (lhs, rhs) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let value = match operator {
                    ast::BinaryOperator::Add | ast::BinaryOperator::AddAssign => self
                        .builder
                        .build_int_add(*lhs, *rhs, "iadd")
                        .map_err(|e| CodegenError::new(format!("int add failed: {e}")))?,
                    ast::BinaryOperator::Subtract | ast::BinaryOperator::SubtractAssign => self
                        .builder
                        .build_int_sub(*lhs, *rhs, "isub")
                        .map_err(|e| CodegenError::new(format!("int sub failed: {e}")))?,
                    ast::BinaryOperator::Multiply | ast::BinaryOperator::MultiplyAssign => self
                        .builder
                        .build_int_mul(*lhs, *rhs, "imul")
                        .map_err(|e| CodegenError::new(format!("int mul failed: {e}")))?,
                    ast::BinaryOperator::Divide | ast::BinaryOperator::DivideAssign => {
                        // Signed vs unsigned division matters once the high bit
                        // is set (e.g. u64::MAX / 10 must not be -1 / 10).
                        if is_unsigned {
                            self.builder
                                .build_int_unsigned_div(*lhs, *rhs, "idiv")
                                .map_err(|e| CodegenError::new(format!("int div failed: {e}")))?
                        } else {
                            self.builder
                                .build_int_signed_div(*lhs, *rhs, "idiv")
                                .map_err(|e| CodegenError::new(format!("int div failed: {e}")))?
                        }
                    }
                    ast::BinaryOperator::Modulo | ast::BinaryOperator::ModuloAssign => {
                        if is_unsigned {
                            self.builder
                                .build_int_unsigned_rem(*lhs, *rhs, "irem")
                                .map_err(|e| CodegenError::new(format!("int rem failed: {e}")))?
                        } else {
                            self.builder
                                .build_int_signed_rem(*lhs, *rhs, "irem")
                                .map_err(|e| CodegenError::new(format!("int rem failed: {e}")))?
                        }
                    }
                    _ => {
                        return Err(CodegenError::with_span(
                            "unsupported arithmetic operation",
                            whole_expr.span,
                        ));
                    }
                };
                Ok(value.as_basic_value_enum())
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let value = match operator {
                    ast::BinaryOperator::Add | ast::BinaryOperator::AddAssign => self
                        .builder
                        .build_float_add(*lhs, *rhs, "fadd")
                        .map_err(|e| CodegenError::new(format!("float add failed: {e}")))?,
                    ast::BinaryOperator::Subtract | ast::BinaryOperator::SubtractAssign => self
                        .builder
                        .build_float_sub(*lhs, *rhs, "fsub")
                        .map_err(|e| CodegenError::new(format!("float sub failed: {e}")))?,
                    ast::BinaryOperator::Multiply | ast::BinaryOperator::MultiplyAssign => self
                        .builder
                        .build_float_mul(*lhs, *rhs, "fmul")
                        .map_err(|e| CodegenError::new(format!("float mul failed: {e}")))?,
                    ast::BinaryOperator::Divide | ast::BinaryOperator::DivideAssign => self
                        .builder
                        .build_float_div(*lhs, *rhs, "fdiv")
                        .map_err(|e| CodegenError::new(format!("float div failed: {e}")))?,
                    ast::BinaryOperator::Modulo | ast::BinaryOperator::ModuloAssign => self
                        .builder
                        .build_float_rem(*lhs, *rhs, "frem")
                        .map_err(|e| CodegenError::new(format!("float rem failed: {e}")))?,
                    _ => {
                        return Err(CodegenError::with_span(
                            "unsupported float arithmetic operation",
                            whole_expr.span,
                        ));
                    }
                };
                Ok(value.as_basic_value_enum())
            }
            _ => Err(CodegenError::with_span(
                "binary arithmetic requires matching numeric types",
                whole_expr.span,
            )),
        }
    }

    /// Lower pointer arithmetic `ptr +/- offset` (scaled by the pointee
    /// size). The pointer expression's type drives the element width; `str`
    /// steps in bytes.
    fn emit_pointer_arith(
        &mut self,
        ptr: inkwell::values::PointerValue<'ctx>,
        offset: inkwell::values::IntValue<'ctx>,
        operator: &ast::BinaryOperator,
        pointer_expr: &ast::Expression,
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let element_ty = self.pointer_arith_element_type(pointer_expr)?;
        let index = if *operator == ast::BinaryOperator::Subtract {
            offset.const_neg()
        } else {
            offset
        };
        let gep = unsafe {
            self.builder
                .build_gep(element_ty, ptr, &[index], "ptr.offset")
                .map_err(|e| {
                    CodegenError::with_span(format!("pointer arithmetic failed: {e}"), *span)
                })?
        };
        Ok(gep.as_basic_value_enum())
    }

    /// LLVM element type for pointer arithmetic on `expr` (the pointer side
    /// of p + i): the pointee type, or i8 for str.
    fn pointer_arith_element_type(
        &mut self,
        pointer_expr: &ast::Expression,
    ) -> CodegenResult<BasicTypeEnum<'ctx>> {
        if let Some(ty) = self.resolve_receiver_type(pointer_expr) {
            match ty.kind.as_ref() {
                ast::TypeKind::Pointer(pointer) => {
                    return self.lower_basic_type(&pointer.inner);
                }
                ast::TypeKind::Reference(reference) => {
                    return self.lower_basic_type(&reference.inner);
                }
                ast::TypeKind::Primitive(ast::PrimitiveType::Str) => {
                    return Ok(self.context.i8_type().into());
                }
                _ => {}
            }
        }
        // Fall back to byte steps when the type cannot be resolved.
        Ok(self.context.i8_type().into())
    }

    pub(crate) fn emit_binary_values(
        &mut self,
        lhs: &BasicValueEnum<'ctx>,
        operator: &ast::BinaryOperator,
        rhs: &BasicValueEnum<'ctx>,
        whole_expr: &ast::Expression,
        is_unsigned: bool,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match operator {
            ast::BinaryOperator::Add
            | ast::BinaryOperator::Subtract
            | ast::BinaryOperator::Multiply
            | ast::BinaryOperator::Divide
            | ast::BinaryOperator::Modulo => {
                self.emit_arith_values(lhs, operator, rhs, whole_expr, is_unsigned)
            }
            ast::BinaryOperator::Equal
            | ast::BinaryOperator::NotEqual
            | ast::BinaryOperator::Less
            | ast::BinaryOperator::Greater
            | ast::BinaryOperator::LessEqual
            | ast::BinaryOperator::GreaterEqual => {
                self.emit_compare_values(lhs, operator, rhs, whole_expr, is_unsigned)
            }
            ast::BinaryOperator::LogicalAnd => {
                let lhs_bool = self.emit_as_bool(lhs, &whole_expr.span)?;
                let rhs_bool = self.emit_as_bool(rhs, &whole_expr.span)?;
                let value = self
                    .builder
                    .build_and(lhs_bool, rhs_bool, "land")
                    .map_err(|e| CodegenError::new(format!("logical and failed: {e}")))?;
                Ok(value.as_basic_value_enum())
            }
            ast::BinaryOperator::LogicalOr => {
                let lhs_bool = self.emit_as_bool(lhs, &whole_expr.span)?;
                let rhs_bool = self.emit_as_bool(rhs, &whole_expr.span)?;
                let value = self
                    .builder
                    .build_or(lhs_bool, rhs_bool, "lor")
                    .map_err(|e| CodegenError::new(format!("logical or failed: {e}")))?;
                Ok(value.as_basic_value_enum())
            }
            ast::BinaryOperator::BitwiseAnd => {
                let (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) = (lhs, rhs)
                else {
                    return Err(CodegenError::with_span(
                        "bitwise and requires integer operands",
                        whole_expr.span,
                    ));
                };
                self.builder
                    .build_and(*lhs, *rhs, "band")
                    .map(|v| v.as_basic_value_enum())
                    .map_err(|e| CodegenError::new(format!("bitwise and failed: {e}")))
            }
            ast::BinaryOperator::BitwiseOr => {
                let (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) = (lhs, rhs)
                else {
                    return Err(CodegenError::with_span(
                        "bitwise or requires integer operands",
                        whole_expr.span,
                    ));
                };
                self.builder
                    .build_or(*lhs, *rhs, "bor")
                    .map(|v| v.as_basic_value_enum())
                    .map_err(|e| CodegenError::new(format!("bitwise or failed: {e}")))
            }
            ast::BinaryOperator::BitwiseXor => {
                let (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) = (lhs, rhs)
                else {
                    return Err(CodegenError::with_span(
                        "bitwise xor requires integer operands",
                        whole_expr.span,
                    ));
                };
                self.builder
                    .build_xor(*lhs, *rhs, "bxor")
                    .map(|v| v.as_basic_value_enum())
                    .map_err(|e| CodegenError::new(format!("bitwise xor failed: {e}")))
            }
            ast::BinaryOperator::LeftShift => {
                let (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) = (lhs, rhs)
                else {
                    return Err(CodegenError::with_span(
                        "left shift requires integer operands",
                        whole_expr.span,
                    ));
                };
                self.builder
                    .build_left_shift(*lhs, *rhs, "shl")
                    .map(|v| v.as_basic_value_enum())
                    .map_err(|e| CodegenError::new(format!("left shift failed: {e}")))
            }
            ast::BinaryOperator::RightShift => {
                let (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) = (lhs, rhs)
                else {
                    return Err(CodegenError::with_span(
                        "right shift requires integer operands",
                        whole_expr.span,
                    ));
                };
                self.builder
                    .build_right_shift(*lhs, *rhs, true, "shr")
                    .map(|v| v.as_basic_value_enum())
                    .map_err(|e| CodegenError::new(format!("right shift failed: {e}")))
            }
            _ => Err(CodegenError::with_span(
                format!(
                    "binary operator {:?} is not supported for these operand types",
                    operator
                ),
                whole_expr.span,
            )),
        }
    }

    pub(crate) fn emit_compare_values(
        &mut self,
        lhs: &BasicValueEnum<'ctx>,
        operator: &ast::BinaryOperator,
        rhs: &BasicValueEnum<'ctx>,
        whole_expr: &ast::Expression,
        is_unsigned: bool,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match (lhs, rhs) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let pred = match operator {
                    ast::BinaryOperator::Equal => IntPredicate::EQ,
                    ast::BinaryOperator::NotEqual => IntPredicate::NE,
                    ast::BinaryOperator::Less => {
                        if is_unsigned {
                            IntPredicate::ULT
                        } else {
                            IntPredicate::SLT
                        }
                    }
                    ast::BinaryOperator::Greater => {
                        if is_unsigned {
                            IntPredicate::UGT
                        } else {
                            IntPredicate::SGT
                        }
                    }
                    ast::BinaryOperator::LessEqual => {
                        if is_unsigned {
                            IntPredicate::ULE
                        } else {
                            IntPredicate::SLE
                        }
                    }
                    ast::BinaryOperator::GreaterEqual => {
                        if is_unsigned {
                            IntPredicate::UGE
                        } else {
                            IntPredicate::SGE
                        }
                    }
                    _ => {
                        return Err(CodegenError::with_span(
                            "unsupported integer comparison",
                            whole_expr.span,
                        ));
                    }
                };
                let value = self
                    .builder
                    .build_int_compare(pred, *lhs, *rhs, "icmp")
                    .map_err(|e| CodegenError::new(format!("int compare failed: {e}")))?;
                Ok(value.as_basic_value_enum())
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let pred = match operator {
                    ast::BinaryOperator::Equal => FloatPredicate::OEQ,
                    ast::BinaryOperator::NotEqual => FloatPredicate::ONE,
                    ast::BinaryOperator::Less => FloatPredicate::OLT,
                    ast::BinaryOperator::Greater => FloatPredicate::OGT,
                    ast::BinaryOperator::LessEqual => FloatPredicate::OLE,
                    ast::BinaryOperator::GreaterEqual => FloatPredicate::OGE,
                    _ => {
                        return Err(CodegenError::with_span(
                            "unsupported float comparison",
                            whole_expr.span,
                        ));
                    }
                };
                let value = self
                    .builder
                    .build_float_compare(pred, *lhs, *rhs, "fcmp")
                    .map_err(|e| CodegenError::new(format!("float compare failed: {e}")))?;
                Ok(value.as_basic_value_enum())
            }
            (BasicValueEnum::PointerValue(lhs), BasicValueEnum::PointerValue(rhs)) => {
                let pred = match operator {
                    ast::BinaryOperator::Equal => IntPredicate::EQ,
                    ast::BinaryOperator::NotEqual => IntPredicate::NE,
                    _ => {
                        return Err(CodegenError::with_span(
                            "pointer comparisons only support == and !=",
                            whole_expr.span,
                        ));
                    }
                };
                let intptr = self.context.i64_type();
                let lhs_int = self
                    .builder
                    .build_ptr_to_int(*lhs, intptr, "pcmp.lhs")
                    .map_err(|e| CodegenError::new(format!("ptr-to-int cast failed: {e}")))?;
                let rhs_int = self
                    .builder
                    .build_ptr_to_int(*rhs, intptr, "pcmp.rhs")
                    .map_err(|e| CodegenError::new(format!("ptr-to-int cast failed: {e}")))?;
                let value = self
                    .builder
                    .build_int_compare(pred, lhs_int, rhs_int, "pcmp")
                    .map_err(|e| CodegenError::new(format!("pointer compare failed: {e}")))?;
                Ok(value.as_basic_value_enum())
            }
            _ => Err(CodegenError::with_span(
                "comparison requires matching numeric types",
                whole_expr.span,
            )),
        }
    }

    pub(crate) fn emit_as_bool(
        &mut self,
        value: &BasicValueEnum<'ctx>,
        span: &Span,
    ) -> CodegenResult<inkwell::values::IntValue<'ctx>> {
        match value {
            BasicValueEnum::IntValue(v) => {
                if v.get_type().get_bit_width() == 1 {
                    Ok(*v)
                } else {
                    let zero = v.get_type().const_zero();
                    self.builder
                        .build_int_compare(IntPredicate::NE, *v, zero, "tobool")
                        .map_err(|e| CodegenError::new(format!("int-to-bool failed: {e}")))
                }
            }
            BasicValueEnum::FloatValue(v) => {
                let zero = v.get_type().const_zero();
                self.builder
                    .build_float_compare(FloatPredicate::ONE, *v, zero, "tobool")
                    .map_err(|e| CodegenError::new(format!("float-to-bool failed: {e}")))
            }
            _ => Err(CodegenError::with_span(
                "expression cannot be used as a boolean condition",
                *span,
            )),
        }
    }

    pub(crate) fn cast_value_to_basic_type(
        &mut self,
        value: BasicValueEnum<'ctx>,
        target: BasicTypeEnum<'ctx>,
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        if value.get_type() == target {
            return Ok(value);
        }

        match (value, target) {
            (BasicValueEnum::IntValue(int_val), BasicTypeEnum::IntType(int_ty)) => self
                .builder
                .build_int_cast(int_val, int_ty, "cast.i2i")
                .map(|v| v.as_basic_value_enum())
                .map_err(|e| CodegenError::with_span(format!("integer cast failed: {e}"), *span)),
            (BasicValueEnum::IntValue(int_val), BasicTypeEnum::FloatType(float_ty)) => self
                .builder
                .build_signed_int_to_float(int_val, float_ty, "cast.i2f")
                .map(|v| v.as_basic_value_enum())
                .map_err(|e| {
                    CodegenError::with_span(format!("int-to-float cast failed: {e}"), *span)
                }),
            (BasicValueEnum::FloatValue(float_val), BasicTypeEnum::IntType(int_ty)) => self
                .builder
                .build_float_to_signed_int(float_val, int_ty, "cast.f2i")
                .map(|v| v.as_basic_value_enum())
                .map_err(|e| {
                    CodegenError::with_span(format!("float-to-int cast failed: {e}"), *span)
                }),
            (BasicValueEnum::FloatValue(float_val), BasicTypeEnum::FloatType(float_ty)) => self
                .builder
                .build_float_cast(float_val, float_ty, "cast.f2f")
                .map(|v| v.as_basic_value_enum())
                .map_err(|e| CodegenError::with_span(format!("float cast failed: {e}"), *span)),
            (BasicValueEnum::PointerValue(ptr_val), BasicTypeEnum::PointerType(ptr_ty)) => self
                .builder
                .build_pointer_cast(ptr_val, ptr_ty, "cast.p2p")
                .map(|v| v.as_basic_value_enum())
                .map_err(|e| CodegenError::with_span(format!("pointer cast failed: {e}"), *span)),
            (BasicValueEnum::PointerValue(ptr_val), BasicTypeEnum::IntType(int_ty)) => self
                .builder
                .build_ptr_to_int(ptr_val, int_ty, "cast.p2i")
                .map(|v| v.as_basic_value_enum())
                .map_err(|e| {
                    CodegenError::with_span(format!("pointer-to-int cast failed: {e}"), *span)
                }),
            (BasicValueEnum::IntValue(int_val), BasicTypeEnum::PointerType(ptr_ty)) => self
                .builder
                .build_int_to_ptr(int_val, ptr_ty, "cast.i2p")
                .map(|v| v.as_basic_value_enum())
                .map_err(|e| {
                    CodegenError::with_span(format!("int-to-pointer cast failed: {e}"), *span)
                }),
            (BasicValueEnum::StructValue(struct_val), BasicTypeEnum::StructType(target_struct_ty)) => {
                if struct_val.get_type() == target_struct_ty {
                    Ok(struct_val.as_basic_value_enum())
                } else {
                    let src_fields = struct_val.get_type().get_field_types();
                    let dst_fields = target_struct_ty.get_field_types();
                    if src_fields.len() == dst_fields.len() {
                        let mut result = target_struct_ty.get_undef();
                        for (i, dst_ty) in dst_fields.iter().enumerate() {
                            let elem = self
                                .builder
                                .build_extract_value(struct_val, i as u32, &format!("struct.cast.ext.{i}"))
                                .map_err(|e| CodegenError::with_span(format!("struct cast extract failed: {e}"), *span))?;
                            let casted = self.cast_value_to_basic_type(elem, *dst_ty, span)?;
                            result = self
                                .builder
                                .build_insert_value(result, casted, i as u32, &format!("struct.cast.ins.{i}"))
                                .map_err(|e| CodegenError::with_span(format!("struct cast insert failed: {e}"), *span))?
                                .into_struct_value();
                        }
                        Ok(result.as_basic_value_enum())
                    } else if let Some(coerced) = self.try_coerce_to_slice(value, None, target_struct_ty, span)? {
                        Ok(coerced)
                    } else {
                        Err(CodegenError::with_span(
                            format!(
                                "unsupported cast from `{}` to `{}`",
                                struct_val.get_type().print_to_string(),
                                target.print_to_string()
                            ),
                            *span,
                        ))
                    }
                }
            }
            (BasicValueEnum::StructValue(struct_val), BasicTypeEnum::PointerType(ptr_ty)) => {
                let data_val = self
                    .builder
                    .build_extract_value(struct_val, 0, "slice.to_ptr")
                    .map_err(|e| CodegenError::with_span(format!("failed to extract slice data: {e}"), *span))?;
                if data_val.is_pointer_value() {
                    let ptr = data_val.into_pointer_value();
                    if ptr.get_type() == ptr_ty {
                        Ok(ptr.as_basic_value_enum())
                    } else {
                        self.builder
                            .build_pointer_cast(ptr, ptr_ty, "cast.slice2p")
                            .map(|v| v.as_basic_value_enum())
                            .map_err(|e| CodegenError::with_span(format!("pointer cast failed: {e}"), *span))
                    }
                } else {
                    Err(CodegenError::with_span(
                        format!(
                            "unsupported cast from `{}` to `{}`",
                            struct_val.get_type().print_to_string(),
                            target.print_to_string()
                        ),
                        *span,
                    ))
                }
            }
            (BasicValueEnum::PointerValue(ptr_val), BasicTypeEnum::StructType(struct_ty)) => {
                if let Some(coerced) = self.try_coerce_to_slice(value, None, struct_ty, span)? {
                    Ok(coerced)
                } else {
                    Err(CodegenError::with_span(
                        format!(
                            "unsupported cast from `{}` to `{}`",
                            ptr_val.get_type().print_to_string(),
                            target.print_to_string()
                        ),
                        *span,
                    ))
                }
            }
            (BasicValueEnum::ArrayValue(arr_val), BasicTypeEnum::ArrayType(target_arr_ty)) => {
                let function = self.current_fn.ok_or_else(|| {
                    CodegenError::new("no active function for array cast")
                })?;
                let target_alloca = self.create_entry_alloca(
                    function,
                    "cast.arr.dst",
                    target_arr_ty.as_basic_type_enum(),
                )?;
                self.builder
                    .build_store(target_alloca, target_arr_ty.const_zero())
                    .map_err(|e| CodegenError::with_span(e.to_string(), *span))?;
                let src_alloca = self.create_entry_alloca(
                    function,
                    "cast.arr.src",
                    arr_val.get_type().as_basic_type_enum(),
                )?;
                self.builder
                    .build_store(src_alloca, arr_val)
                    .map_err(|e| CodegenError::with_span(e.to_string(), *span))?;

                let target_data =
                    TargetData::create(self.module.get_data_layout().as_str().to_str().unwrap());
                let src_size = target_data.get_abi_size(&arr_val.get_type());
                let dst_size = target_data.get_abi_size(&target_arr_ty);
                let copy_size = src_size.min(dst_size);

                self.build_memcpy(target_alloca, 1, src_alloca, 1, copy_size)?;
                let loaded = self
                    .builder
                    .build_load(target_arr_ty, target_alloca, "cast.arr.loaded")
                    .map_err(|e| CodegenError::with_span(e.to_string(), *span))?;
                Ok(loaded)
            }
            (source, _) => Err(CodegenError::with_span(
                format!(
                    "unsupported cast from `{}` to `{}`",
                    source.get_type().print_to_string(),
                    target.print_to_string()
                ),
                *span,
            )),
        }
    }

    pub(crate) fn try_coerce_to_slice(
        &mut self,
        value: BasicValueEnum<'ctx>,
        source_expr: Option<&ast::Expression>,
        target_struct_ty: inkwell::types::StructType<'ctx>,
        span: &Span,
    ) -> CodegenResult<Option<BasicValueEnum<'ctx>>> {
        let dst_fields = target_struct_ty.get_field_types();
        if dst_fields.len() != 2
            || !dst_fields[0].is_pointer_type()
            || !dst_fields[1].is_int_type()
        {
            return Ok(None);
        }

        let ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
        let i64_ty = self.context.i64_type();

        // 1. Check if source_expr is a string literal -> compile-time constant length!
        if let Some(expr) = source_expr {
            if let ast::ExpressionKind::Literal(ast::Literal::String(s)) = expr.kind.as_ref() {
                if let BasicValueEnum::PointerValue(ptr_val) = value {
                    let const_len = i64_ty.const_int(s.len() as u64, false);
                    let mut result = target_struct_ty.get_undef();
                    result = self
                        .builder
                        .build_insert_value(result, ptr_val, 0, "lit2slice.ptr")
                        .map_err(|e| CodegenError::with_span(e.to_string(), *span))?
                        .into_struct_value();
                    result = self
                        .builder
                        .build_insert_value(result, const_len, 1, "lit2slice.len")
                        .map_err(|e| CodegenError::with_span(e.to_string(), *span))?
                        .into_struct_value();
                    return Ok(Some(result.as_basic_value_enum()));
                }
            }

            let src_ast_ty = self.resolve_argument_type(expr);
            if let Some(src_ty) = src_ast_ty {
                let mut is_ref = false;
                let mut inner_ty = &src_ty;
                if let ast::TypeKind::Reference(r) = src_ty.kind.as_ref() {
                    is_ref = true;
                    inner_ty = &r.inner;
                } else if let ast::TypeKind::Pointer(p) = src_ty.kind.as_ref() {
                    is_ref = true;
                    inner_ty = &p.inner;
                }

                // Check for String or Vec
                let is_string_or_vec = match inner_ty.kind.as_ref() {
                    ast::TypeKind::Named(named) => {
                        let name = named.path.last().map(|id| id.name.as_str());
                        name == Some("String") || name == Some("Vec")
                    }
                    _ => false,
                };

                if is_string_or_vec {
                    let (data_ptr, len_val) = if is_ref {
                        let ptr_val = value.into_pointer_value();
                        let three_field_ty = self.context.struct_type(
                            &[ptr_ty.into(), i64_ty.into(), i64_ty.into()],
                            false,
                        );
                        let data_gep = self
                            .builder
                            .build_struct_gep(three_field_ty, ptr_val, 0, "obj.ref.data")
                            .map_err(|e| CodegenError::with_span(e.to_string(), *span))?;
                        let data = self
                            .builder
                            .build_load(ptr_ty, data_gep, "obj.data")
                            .map_err(|e| CodegenError::with_span(e.to_string(), *span))?;
                        let len_gep = self
                            .builder
                            .build_struct_gep(three_field_ty, ptr_val, 1, "obj.ref.len")
                            .map_err(|e| CodegenError::with_span(e.to_string(), *span))?;
                        let len = self
                            .builder
                            .build_load(i64_ty, len_gep, "obj.len")
                            .map_err(|e| CodegenError::with_span(e.to_string(), *span))?;
                        (data, len)
                    } else if value.is_struct_value() {
                        let struct_val = value.into_struct_value();
                        let data = self
                            .builder
                            .build_extract_value(struct_val, 0, "obj.data")
                            .map_err(|e| CodegenError::with_span(e.to_string(), *span))?;
                        let len = self
                            .builder
                            .build_extract_value(struct_val, 1, "obj.len")
                            .map_err(|e| CodegenError::with_span(e.to_string(), *span))?;
                        (data, len)
                    } else {
                        return Ok(None);
                    };

                    let cast_ptr = self.cast_value_to_basic_type(data_ptr, dst_fields[0], span)?;
                    let cast_len = self.cast_value_to_basic_type(len_val, dst_fields[1], span)?;
                    let mut result = target_struct_ty.get_undef();
                    result = self
                        .builder
                        .build_insert_value(result, cast_ptr, 0, "slice.res.ptr")
                        .map_err(|e| CodegenError::with_span(e.to_string(), *span))?
                        .into_struct_value();
                    result = self
                        .builder
                        .build_insert_value(result, cast_len, 1, "slice.res.len")
                        .map_err(|e| CodegenError::with_span(e.to_string(), *span))?
                        .into_struct_value();
                    return Ok(Some(result.as_basic_value_enum()));
                }

                // Check for Array
                if let ast::TypeKind::Array(array) = inner_ty.kind.as_ref() {
                    let const_len = i64_ty.const_int(array.size as u64, false);
                    let elem_0_ptr = if is_ref {
                        let ptr_val = value.into_pointer_value();
                        let array_llvm_ty = self.lower_basic_type(inner_ty)?;
                        let zero = self.context.i32_type().const_zero();
                        unsafe {
                            self.builder
                                .build_in_bounds_gep(
                                    array_llvm_ty,
                                    ptr_val,
                                    &[zero, zero],
                                    "arr.slice.ptr",
                                )
                                .map_err(|e| CodegenError::with_span(e.to_string(), *span))?
                        }
                    } else if value.is_pointer_value() {
                        value.into_pointer_value()
                    } else {
                        return Ok(None);
                    };

                    let cast_ptr = self.cast_value_to_basic_type(
                        elem_0_ptr.as_basic_value_enum(),
                        dst_fields[0],
                        span,
                    )?;
                    let mut result = target_struct_ty.get_undef();
                    result = self
                        .builder
                        .build_insert_value(result, cast_ptr, 0, "slice.arr.ptr")
                        .map_err(|e| CodegenError::with_span(e.to_string(), *span))?
                        .into_struct_value();
                    result = self
                        .builder
                        .build_insert_value(result, const_len, 1, "slice.arr.len")
                        .map_err(|e| CodegenError::with_span(e.to_string(), *span))?
                        .into_struct_value();
                    return Ok(Some(result.as_basic_value_enum()));
                }
            }
        }

        // 2. StructValue with >= 2 fields (e.g. String or Vec passed by value without source_expr):
        if let BasicValueEnum::StructValue(struct_val) = value {
            let src_fields = struct_val.get_type().get_field_types();
            if src_fields.len() >= 2
                && src_fields[0].is_pointer_type()
                && src_fields[1].is_int_type()
            {
                let elem_ptr = self
                    .builder
                    .build_extract_value(struct_val, 0, "slice.cast.ptr")
                    .map_err(|e| CodegenError::with_span(e.to_string(), *span))?;
                let cast_ptr = self.cast_value_to_basic_type(elem_ptr, dst_fields[0], span)?;
                let len_val = self
                    .builder
                    .build_extract_value(struct_val, 1, "slice.cast.len")
                    .map_err(|e| CodegenError::with_span(e.to_string(), *span))?;
                let cast_len = self.cast_value_to_basic_type(len_val, dst_fields[1], span)?;
                let mut result = target_struct_ty.get_undef();
                result = self
                    .builder
                    .build_insert_value(result, cast_ptr, 0, "slice.res.ptr")
                    .map_err(|e| CodegenError::with_span(e.to_string(), *span))?
                    .into_struct_value();
                result = self
                    .builder
                    .build_insert_value(result, cast_len, 1, "slice.res.len")
                    .map_err(|e| CodegenError::with_span(e.to_string(), *span))?
                    .into_struct_value();
                return Ok(Some(result.as_basic_value_enum()));
            }
        }

        // 3. Fallback for PointerValue (C-string str -> Slice<u8> via strlen):
        if let BasicValueEnum::PointerValue(ptr_val) = value {
            let strlen_ty = i64_ty.fn_type(&[ptr_ty.into()], false);
            let strlen_fn = self
                .module
                .get_function("strlen")
                .unwrap_or_else(|| self.module.add_function("strlen", strlen_ty, None));
            let len_call = self
                .builder
                .build_call(strlen_fn, &[ptr_val.into()], "str.len")
                .map_err(|e| CodegenError::with_span(e.to_string(), *span))?;
            let len_val = len_call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::with_span("strlen returned void", *span))?
                .into_int_value();
            let undef = target_struct_ty.get_undef();
            let s1 = self
                .builder
                .build_insert_value(undef, ptr_val, 0, "str2slice.ptr")
                .map_err(|e| CodegenError::with_span(e.to_string(), *span))?;
            let s2 = self
                .builder
                .build_insert_value(s1, len_val, 1, "str2slice.len")
                .map_err(|e| CodegenError::with_span(e.to_string(), *span))?;
            return Ok(Some(s2.as_basic_value_enum()));
        }

        Ok(None)
    }

    pub(crate) fn cast_expr_to_ast_type(
        &mut self,
        value: BasicValueEnum<'ctx>,
        source_expr: Option<&ast::Expression>,
        target_type: &ast::Type,
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let target = self.lower_basic_type(target_type)?;
        if let BasicTypeEnum::StructType(target_struct_ty) = target {
            if let Some(coerced) =
                self.try_coerce_to_slice(value, source_expr, target_struct_ty, span)?
            {
                return Ok(coerced);
            }
        }
        self.cast_value_to_basic_type(value, target, span)
    }

    pub(crate) fn cast_value_to_ast_type(
        &mut self,
        value: BasicValueEnum<'ctx>,
        target_type: &ast::Type,
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        self.cast_expr_to_ast_type(value, None, target_type, span)
    }

    /// Apply a user-defined `cast Target(...)` impl method if one exists for
    /// the source expression's type; otherwise return `None` so the caller can
    /// fall back to builtin casts.
    pub(crate) fn try_apply_user_cast(
        &mut self,
        value: BasicValueEnum<'ctx>,
        source_expr: &ast::Expression,
        target_type: &ast::Type,
        span: &Span,
    ) -> CodegenResult<Option<BasicValueEnum<'ctx>>> {
        let cast_method_name = Self::cast_method_name(target_type);
        let owners = self.receiver_owner_candidates(source_expr);
        let Some(cast_fn) = owners.iter().find_map(|owner| {
            self.overloaded_method_candidates(owner, &cast_method_name)
                .iter()
                .find_map(|name| self.module.get_function(name))
        }) else {
            return Ok(None);
        };
        let args = vec![BasicMetadataValueEnum::from(value)];
        let call = self
            .builder
            .build_call(cast_fn, &args, "cast.arg")
            .map_err(|e| {
                CodegenError::with_span(format!("failed to call user-defined cast: {e}"), *span)
            })?;
        let result = call.try_as_basic_value().basic().ok_or_else(|| {
            CodegenError::with_span("user-defined cast returned void".to_string(), *span)
        })?;
        Ok(Some(result))
    }

    /// The root variable behind a possibly-nested lvalue (`a.b.c` -> `a`),
    /// so its drop flag can guard a pre-drop. Returns None for unguarded
    /// lvalues (index expressions, temporaries).
    fn assignment_guard_flag(&mut self, left: &ast::Expression) -> Option<PointerValue<'ctx>> {
        let mut expr = left;
        loop {
            match expr.kind.as_ref() {
                ast::ExpressionKind::Identifier(ident) => {
                    return self.lookup_variable(&ident.name).and_then(|v| v.drop_flag);
                }
                ast::ExpressionKind::FieldAccess { object, .. } => expr = object,
                _ => return None,
            }
        }
    }

    /// Emit a flag-guarded destructor call for the value about to be
    /// overwritten by an assignment: load the owner's drop flag and call
    /// `drop_fn(target_ptr)` only when it is still set (the value was not
    /// moved out).
    fn emit_guarded_drop(
        &mut self,
        flag_ptr: PointerValue<'ctx>,
        drop_fn: &str,
        target_ptr: PointerValue<'ctx>,
        span: &Span,
    ) -> CodegenResult<()> {
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for pre-drop"))?;
        let flag_val = self
            .builder
            .build_load(self.context.bool_type(), flag_ptr, "predrop.flag")
            .map_err(|e| CodegenError::new(format!("failed to load pre-drop flag: {e}")))?;
        let run_bb = self.context.append_basic_block(function, "predrop.run");
        let after_bb = self.context.append_basic_block(function, "predrop.after");
        self.builder
            .build_conditional_branch(flag_val.into_int_value(), run_bb, after_bb)
            .map_err(|e| CodegenError::new(format!("failed to branch pre-drop: {e}")))?;
        self.builder.position_at_end(run_bb);
        if let Some(func) = self.module.get_function(drop_fn) {
            let args = vec![BasicMetadataValueEnum::from(target_ptr)];
            self.builder
                .build_call(func, &args, "predrop")
                .map_err(|e| CodegenError::new(format!("failed to call pre-drop: {e}")))?;
        }
        self.builder
            .build_unconditional_branch(after_bb)
            .map_err(|e| CodegenError::new(format!("failed to join pre-drop: {e}")))?;
        self.builder.position_at_end(after_bb);
        let _ = span;
        Ok(())
    }

    /// Split an lvalue into its root variable name and the dotted field
    /// path from the root outward (`a.b.c` -> `("a", "b.c")`). Returns
    /// None for non-field/identifier lvalues.
    pub(crate) fn lvalue_root_and_path(&self, left: &ast::Expression) -> Option<(String, String)> {
        let mut path = Vec::new();
        let mut expr = left;
        loop {
            match expr.kind.as_ref() {
                ast::ExpressionKind::Identifier(ident) => {
                    path.reverse();
                    return Some((ident.name.clone(), path.join(".")));
                }
                ast::ExpressionKind::FieldAccess { object, field } => {
                    path.push(field.name.clone());
                    expr = object;
                }
                _ => return None,
            }
        }
    }

    /// GEP to a dotted field path (`f.g`) from a struct pointer, walking
    /// struct_fields for each segment.
    fn resolve_field_path_ptr(
        &mut self,
        base_ptr: PointerValue<'ctx>,
        base_ty: &ast::Type,
        path: &str,
        span: &Span,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let mut ptr = base_ptr;
        let mut ty = base_ty.clone();
        for segment in path.split('.') {
            let Some(named) = Self::extract_named_type(&ty).cloned() else {
                return Err(CodegenError::with_span(
                    format!("field path {path} crosses a non-struct type"),
                    *span,
                ));
            };
            let named_key = Self::named_type_key(&named);
            let fields = self
                .struct_fields
                .get(&named_key)
                .cloned()
                .unwrap_or_default();
            let Some(index) = fields.iter().position(|(name, _)| name == segment) else {
                return Err(CodegenError::with_span(
                    format!("unknown field '{segment}' in field path {path}"),
                    *span,
                ));
            };
            let struct_ty = match self.struct_types.get(&named_key) {
                Some(st) => *st,
                None => {
                    return Err(CodegenError::with_span(
                        format!("no layout for struct '{segment}' in field path"),
                        *span,
                    ));
                }
            };
            ptr = if self.union_types.contains(&named_key) {
                ptr
            } else {
                self.builder
                    .build_struct_gep(struct_ty, ptr, index as u32, segment)
                    .map_err(|e| CodegenError::with_span(format!("field path GEP: {e}"), *span))?
            };
            ty = fields[index].1.clone();
        }
        Ok(ptr)
    }

    /// Before overwriting a Drop-typed lvalue (`x = y` or `x.f = y`),
    /// release the value currently stored there so it is not leaked; the
    /// scope-exit drop then frees the incoming value. Field targets are
    /// guarded by their own per-field flag, which is only set once the
    /// field has been assigned — uninitialized fields are never dropped.
    fn emit_assignment_pre_drop(
        &mut self,
        left: &ast::Expression,
        target_ptr: PointerValue<'ctx>,
        target_ty: &ast::Type,
    ) -> CodegenResult<()> {
        match left.kind.as_ref() {
            ast::ExpressionKind::Identifier(_) => {
                // Wholesale variable overwrite: own drop plus the live
                // field cascade, in scope-exit order.
                let Some(drop_fn) = self.get_drop_function_name(target_ty)? else {
                    return Ok(());
                };
                let Some(flag_ptr) = self.assignment_guard_flag(left) else {
                    return Ok(());
                };
                self.emit_guarded_drop(flag_ptr, &drop_fn, target_ptr, &left.span)?;
                if let ast::ExpressionKind::Identifier(ident) = left.kind.as_ref()
                    && let Some(var) = self.lookup_variable(&ident.name)
                {
                    // field_flags are in registration order (fields last
                    // first); reversed = scope-exit order (own drop before
                    // each field's nested drops, fields in declaration
                    // order).
                    for (path, flag) in var.field_flags.iter().rev() {
                        let field_ptr =
                            self.resolve_field_path_ptr(target_ptr, target_ty, path, &left.span)?;
                        let Some(field_drop) = self
                            .get_drop_function_name(&self.field_type_at_path(target_ty, path)?)?
                        else {
                            continue;
                        };
                        self.emit_guarded_drop(*flag, &field_drop, field_ptr, &left.span)?;
                    }
                }
                Ok(())
            }
            ast::ExpressionKind::FieldAccess { .. } => {
                let Some((root_name, path)) = self.lvalue_root_and_path(left) else {
                    return Ok(());
                };
                let Some(var) = self.lookup_variable(&root_name) else {
                    return Ok(());
                };
                // Drop the target field's own value plus any nested fields
                // whose paths extend it, in scope-exit order (own first,
                // then nested). target_ptr already points at the target
                // field, so nested paths resolve relative to it.
                let prefix = format!("{path}.");
                let mut matching: Vec<&(String, PointerValue<'ctx>)> = var
                    .field_flags
                    .iter()
                    .filter(|(p, _)| *p == path || p.starts_with(&prefix))
                    .collect();
                if matching.is_empty() {
                    return Ok(());
                }
                // field_flags are in registration order; reversed matches
                // scope-exit order.
                matching.reverse();
                for (fp, flag) in matching {
                    let (rel, base_ty) = if *fp == path {
                        (String::new(), target_ty.clone())
                    } else {
                        (fp[prefix.len()..].to_string(), target_ty.clone())
                    };
                    let field_ptr = if rel.is_empty() {
                        target_ptr
                    } else {
                        self.resolve_field_path_ptr(target_ptr, &base_ty, &rel, &left.span)?
                    };
                    let field_ty = if rel.is_empty() {
                        base_ty
                    } else {
                        self.field_type_at_path(&base_ty, &rel)?
                    };
                    let Some(field_drop) = self.get_drop_function_name(&field_ty)? else {
                        continue;
                    };
                    self.emit_guarded_drop(*flag, &field_drop, field_ptr, &left.span)?;
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    /// Resolve the type of a dotted field path from a base type.
    fn field_type_at_path(&self, base_ty: &ast::Type, path: &str) -> CodegenResult<ast::Type> {
        let mut ty = base_ty.clone();
        for segment in path.split('.') {
            let Some(named) = Self::extract_named_type(&ty).cloned() else {
                return Err(CodegenError::new(format!(
                    "field path {path} crosses a non-struct type"
                )));
            };
            let named_key = Self::named_type_key(&named);
            let fields = self
                .struct_fields
                .get(&named_key)
                .cloned()
                .unwrap_or_default();
            let Some((_, field_ty)) = fields.iter().find(|(name, _)| name == segment) else {
                return Err(CodegenError::new(format!(
                    "unknown field '{segment}' in field path {path}"
                )));
            };
            ty = field_ty.clone();
        }
        Ok(ty)
    }

    /// Set a per-field drop flag (marks a field as holding a live value).
    fn set_field_flag(&mut self, flag: PointerValue<'ctx>) -> CodegenResult<()> {
        self.builder
            .build_store(flag, self.context.bool_type().const_int(1, false))
            .map_err(|e| CodegenError::new(format!("failed to set field flag: {e}")))?;
        Ok(())
    }

    /// After assigning a whole struct value to `left`, mark every registered
    /// field as live.
    fn set_all_field_flags(&mut self, left: &ast::Expression) -> CodegenResult<()> {
        if let ast::ExpressionKind::Identifier(ident) = left.kind.as_ref()
            && let Some(var) = self.lookup_variable(&ident.name)
        {
            if let Some(drop_flag) = var.drop_flag {
                self.set_field_flag(drop_flag)?;
            }
            for (_, flag) in &var.field_flags {
                self.set_field_flag(*flag)?;
            }
        }
        Ok(())
    }

    /// After assigning a field of `left`, mark that field (and any nested
    /// path extending it) as live, as well as the root container.
    fn set_assigned_field_flags(&mut self, left: &ast::Expression) -> CodegenResult<()> {
        let Some((root_name, path)) = self.lvalue_root_and_path(left) else {
            return Ok(());
        };
        let Some(var) = self.lookup_variable(&root_name) else {
            return Ok(());
        };
        if let Some(drop_flag) = var.drop_flag {
            self.set_field_flag(drop_flag)?;
        }
        let prefix = format!("{path}.");
        for (p, flag) in &var.field_flags {
            if *p == path || p.starts_with(&prefix) {
                self.set_field_flag(*flag)?;
            }
        }
        Ok(())
    }

    /// Like `cast_value_to_ast_type`, but widens integer values from an
    /// *unsigned* source with zero-extension. LLVM integer types are signless,
    /// and the generic int cast sign-extends, which corrupts e.g.
    /// `(i32)(u8)255`. Widen an integer value to a wider type, zero- or
    /// sign-extending according to the operand's unsignedness (u8/u16 and
    /// chars widen by zero; signed ints by sign — identical for values that
    /// fit).
    fn widen_int_value(
        &mut self,
        value: inkwell::values::IntValue<'ctx>,
        target: inkwell::types::IntType<'ctx>,
        is_unsigned: bool,
        span: &Span,
    ) -> CodegenResult<inkwell::values::IntValue<'ctx>> {
        if is_unsigned {
            self.builder
                .build_int_z_extend_or_bit_cast(value, target, "cmp.zext")
                .map_err(|e| CodegenError::with_span(format!("zero-extend failed: {e}"), *span))
        } else {
            self.builder
                .build_int_s_extend_or_bit_cast(value, target, "cmp.sext")
                .map_err(|e| CodegenError::with_span(format!("sign-extend failed: {e}"), *span))
        }
    }

    /// Zero-extend (or bit-cast) an integer value to a wider type; used for
    /// unsigned sources where the generic int cast would sign-extend.
    pub(crate) fn cast_unsigned_value_to_ast_type(
        &mut self,
        value: BasicValueEnum<'ctx>,
        target_type: &ast::Type,
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let target = self.lower_basic_type(target_type)?;
        match (value, target) {
            (BasicValueEnum::IntValue(int_val), BasicTypeEnum::IntType(int_ty)) => {
                let src_width = int_val.get_type().get_bit_width();
                let dst_width = int_ty.get_bit_width();
                if dst_width > src_width {
                    self.builder
                        .build_int_z_extend_or_bit_cast(int_val, int_ty, "cast.u2i")
                        .map(|v| v.as_basic_value_enum())
                        .map_err(|e| {
                            CodegenError::with_span(format!("unsigned int cast failed: {e}"), *span)
                        })
                } else {
                    self.cast_value_to_basic_type(value, target, span)
                }
            }
            _ => self.cast_value_to_basic_type(value, target, span),
        }
    }
}

/// True when the type is an unsigned integer primitive.
fn type_is_unsigned(ty: &ast::Type) -> bool {
    matches!(
        ty.kind.as_ref(),
        ast::TypeKind::Primitive(
            ast::PrimitiveType::U8
                | ast::PrimitiveType::U16
                | ast::PrimitiveType::U32
                | ast::PrimitiveType::U64
                | ast::PrimitiveType::U128
        )
    )
}
