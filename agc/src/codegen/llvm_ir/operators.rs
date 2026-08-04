use inkwell::AddressSpace;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum};

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
                    self.cast_value_to_ast_type(rhs, &target_ty, &right.span)?
                };
                if self.lvalue_is_volatile(left) {
                    self.emit_volatile_store(target_ptr, value)?;
                } else {
                    self.builder.build_store(target_ptr, value).map_err(|e| {
                        CodegenError::with_span(format!("failed assignment: {e}"), whole_expr.span)
                    })?;
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

                let lhs = self.emit_expression_value(left)?;
                // For struct types, use trait method call instead of inline IR
                if lhs.get_type().is_struct_type()
                    && let Some(method_name) = operator_method_name(operator)
                {
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
                }
                let mut rhs = self.emit_expression_value(right)?;
                if rhs.get_type() != lhs.get_type() {
                    rhs = self.cast_value_to_basic_type(rhs, lhs.get_type(), &right.span)?;
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
        match expr.kind.as_ref() {
            ast::ExpressionKind::Literal(ast::Literal::String(_)) => true,
            ast::ExpressionKind::Identifier(ident) => self
                .lookup_value_type(&ident.name)
                .is_some_and(|ty| *ty.kind == str_ty),
            ast::ExpressionKind::Cast { target_type, .. } => *target_type.kind == str_ty,
            _ => false,
        }
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

    pub(crate) fn cast_value_to_ast_type(
        &mut self,
        value: BasicValueEnum<'ctx>,
        target_type: &ast::Type,
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let target = self.lower_basic_type(target_type)?;
        self.cast_value_to_basic_type(value, target, span)
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
            let name = Self::mangle_method_name(owner, &cast_method_name);
            self.module.get_function(&name)
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

    /// Like `cast_value_to_ast_type`, but widens integer values from an
    /// *unsigned* source with zero-extension. LLVM integer types are signless,
    /// and the generic int cast sign-extends, which corrupts e.g. `(i32)(u8)255`.
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
