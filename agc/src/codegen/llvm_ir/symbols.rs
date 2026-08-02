use rustc_hash::FxHashMap as HashMap;

use inkwell::types::StructType;
use inkwell::values::{BasicValue, BasicValueEnum};

use crate::codegen::llvm_ir::FunctionSig;
use crate::codegen::llvm_ir::LlvmIrGenerator;
use crate::codegen::{CodegenError, CodegenResult};
use crate::parser::ast;
use crate::semantic::monomorph::mangle_name;
use crate::symbol_table::SymbolKind;
use crate::types::Type;

impl<'ctx> LlvmIrGenerator<'ctx> {
    pub(crate) fn substitute_generic_type(
        ty: &ast::Type,
        substitutions: &HashMap<String, ast::Type>,
    ) -> ast::Type {
        let kind = match ty.kind.as_ref() {
            ast::TypeKind::Generic(generic) if generic.args.is_empty() => substitutions
                .get(&generic.name.name)
                .map(|t| t.kind.clone())
                .unwrap_or_else(|| ty.kind.clone()),
            ast::TypeKind::Named(named) => {
                if named.path.len() == 1
                    && named.generics.is_none()
                    && let Some(mapped) = substitutions.get(&named.path[0].name)
                {
                    return ast::Type {
                        kind: mapped.kind.clone(),
                        span: ty.span,
                    };
                }
                let generics = named.generics.as_ref().map(|args| {
                    args.iter()
                        .map(|arg| Self::substitute_generic_type(arg, substitutions))
                        .collect::<Vec<_>>()
                });
                Box::new(ast::TypeKind::Named(ast::NamedType {
                    path: named.path.clone(),
                    generics,
                }))
            }
            ast::TypeKind::Reference(reference) => {
                Box::new(ast::TypeKind::Reference(ast::ReferenceType {
                    is_mutable: reference.is_mutable,
                    lifetime: reference.lifetime.clone(),
                    inner: Box::new(Self::substitute_generic_type(
                        &reference.inner,
                        substitutions,
                    )),
                }))
            }
            ast::TypeKind::Pointer(pointer) => Box::new(ast::TypeKind::Pointer(ast::PointerType {
                is_mutable: pointer.is_mutable,
                is_volatile: pointer.is_volatile,
                inner: Box::new(Self::substitute_generic_type(&pointer.inner, substitutions)),
            })),
            ast::TypeKind::Slice(slice) => {
                Box::new(ast::TypeKind::Slice(Box::new(ast::SliceType {
                    element_type: Box::new(Self::substitute_generic_type(
                        &slice.element_type,
                        substitutions,
                    )),
                })))
            }
            ast::TypeKind::Optional(inner) => Box::new(ast::TypeKind::Optional(Box::new(
                Self::substitute_generic_type(inner, substitutions),
            ))),
            ast::TypeKind::Function(function) => {
                Box::new(ast::TypeKind::Function(ast::FunctionType {
                    parameters: function
                        .parameters
                        .iter()
                        .map(|param| Self::substitute_generic_type(param, substitutions))
                        .collect(),
                    return_type: Box::new(Self::substitute_generic_type(
                        &function.return_type,
                        substitutions,
                    )),
                }))
            }
            ast::TypeKind::Tuple(items) => Box::new(ast::TypeKind::Tuple(
                items
                    .iter()
                    .map(|item| Self::substitute_generic_type(item, substitutions))
                    .collect(),
            )),
            _ => ty.kind.clone(),
        };

        ast::Type {
            kind,
            span: ty.span,
        }
    }

    pub(crate) fn ensure_named_struct_type(
        &mut self,
        named: &ast::NamedType,
    ) -> CodegenResult<StructType<'ctx>> {
        let base_name = Self::named_type_name(named);
        let key = Self::named_type_key(named);

        let struct_ty = *self
            .struct_types
            .entry(key.clone())
            .or_insert_with(|| self.context.opaque_struct_type(&key));

        if !struct_ty.is_opaque() {
            return Ok(struct_ty);
        }

        let template_fields = self.struct_fields.get(&base_name).cloned().ok_or_else(|| {
            CodegenError::new(format!("missing field metadata for struct `{base_name}`"))
        })?;

        let concrete_fields = if let Some(params) = self.struct_generics.get(&base_name) {
            let args = named.generics.as_ref().ok_or_else(|| {
                CodegenError::new(format!(
                    "generic struct `{base_name}` requires concrete type arguments"
                ))
            })?;
            if params.len() != args.len() {
                return Err(CodegenError::new(format!(
                    "generic struct `{base_name}` expected {} type arguments, got {}",
                    params.len(),
                    args.len()
                )));
            }
            let substitutions: HashMap<String, ast::Type> =
                params.iter().cloned().zip(args.iter().cloned()).collect();
            template_fields
                .iter()
                .map(|(name, ty)| {
                    (
                        name.clone(),
                        Self::substitute_generic_type(ty, &substitutions),
                    )
                })
                .collect::<Vec<_>>()
        } else {
            template_fields
        };

        self.struct_fields
            .insert(key.clone(), concrete_fields.clone());

        let mut lowered = Vec::with_capacity(concrete_fields.len());
        for (_, field_ty) in &concrete_fields {
            lowered.push(self.lower_basic_type(field_ty)?);
        }
        struct_ty.set_body(&lowered, false);
        Ok(struct_ty)
    }

    pub(crate) fn enum_backing_type_for_named(
        &self,
        named: &ast::NamedType,
    ) -> Option<ast::PrimitiveType> {
        if named.path.len() != 1 {
            return None;
        }
        self.enum_backing_types.get(&named.path[0].name).cloned()
    }

    pub(crate) fn enum_member_constant(
        &self,
        enum_name: &str,
        variant_name: &str,
    ) -> Option<BasicValueEnum<'ctx>> {
        let backing = self.enum_backing_types.get(enum_name)?;
        let value = self.enum_variants.get(enum_name)?.get(variant_name)?;
        let int_ty = match backing {
            ast::PrimitiveType::I8 | ast::PrimitiveType::U8 => self.context.i8_type(),
            ast::PrimitiveType::I16 | ast::PrimitiveType::U16 => self.context.i16_type(),
            ast::PrimitiveType::I32 | ast::PrimitiveType::U32 => self.context.i32_type(),
            ast::PrimitiveType::I64 | ast::PrimitiveType::U64 => self.context.i64_type(),
            ast::PrimitiveType::I128 | ast::PrimitiveType::U128 => self.context.i128_type(),
            _ => return None,
        };
        let width = match backing {
            ast::PrimitiveType::I8 | ast::PrimitiveType::U8 => 8,
            ast::PrimitiveType::I16 | ast::PrimitiveType::U16 => 16,
            ast::PrimitiveType::I32 | ast::PrimitiveType::U32 => 32,
            ast::PrimitiveType::I64 | ast::PrimitiveType::U64 => 64,
            ast::PrimitiveType::I128 | ast::PrimitiveType::U128 => 128,
            _ => return None,
        };
        let raw = *value as u128;
        let words = if width <= 64 {
            vec![raw as u64]
        } else {
            vec![raw as u64, (raw >> 64) as u64]
        };
        Some(
            int_ty
                .const_int_arbitrary_precision(&words)
                .as_basic_value_enum(),
        )
    }

    pub(crate) fn path_name(path: &[ast::Identifier]) -> String {
        path.iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join("::")
    }

    pub(crate) fn sanitize_monomorph(value: &str) -> String {
        let mut out = String::new();
        let mut last_underscore = false;
        for ch in value.chars() {
            if ch.is_ascii_alphanumeric() {
                out.push(ch);
                last_underscore = false;
            } else if !last_underscore {
                out.push('_');
                last_underscore = true;
            }
        }
        if out.is_empty() { "_".to_string() } else { out }
    }

    pub(crate) fn monomorph_owner_name_from_named(named: &ast::NamedType) -> String {
        let base = Self::named_type_name(named);
        if let Some(args) = &named.generics {
            let parts = args
                .iter()
                .map(|arg| Self::sanitize_monomorph(&Type::from_ast(arg).canonical_key()))
                .collect::<Vec<_>>();
            if parts.is_empty() {
                base
            } else {
                format!("{}__{}", base, parts.join("_"))
            }
        } else {
            base
        }
    }

    pub(crate) fn mangle_method_name(owner: &str, method: &str) -> String {
        format!("{owner}__{method}")
    }

    pub(crate) fn cast_method_name(target_type: &ast::Type) -> String {
        match target_type.kind.as_ref() {
            ast::TypeKind::Primitive(prim) => format!("cast_{:?}", prim).to_lowercase(),
            ast::TypeKind::Named(named) => {
                format!("cast_{}", Self::named_type_name(named))
            }
            ast::TypeKind::Pointer(ptr) => {
                format!("cast_ptr_{}", Self::cast_method_name(&ptr.inner))
            }
            _ => "cast_custom".to_string(),
        }
    }

    pub(crate) fn owner_name_from_type(ty: &ast::Type) -> Option<String> {
        match ty.kind.as_ref() {
            ast::TypeKind::Named(named) => Some(Self::monomorph_owner_name_from_named(named)),
            ast::TypeKind::Reference(reference) => Self::owner_name_from_type(&reference.inner),
            ast::TypeKind::Pointer(pointer) => Self::owner_name_from_type(&pointer.inner),
            ast::TypeKind::Primitive(p) => Some(format!("{:?}", p).to_lowercase()),
            _ => None,
        }
    }

    pub(crate) fn owner_name_candidates_from_type(ty: &ast::Type) -> Vec<String> {
        match ty.kind.as_ref() {
            ast::TypeKind::Named(named) => {
                let mut out = Vec::new();
                let monomorph = Self::monomorph_owner_name_from_named(named);
                out.push(monomorph.clone());
                let key = Self::named_type_key(named);
                if key != monomorph {
                    out.push(key);
                }
                let base = Self::named_type_name(named);
                if base != monomorph {
                    out.push(base);
                }
                out
            }
            ast::TypeKind::Reference(reference) => {
                Self::owner_name_candidates_from_type(&reference.inner)
            }
            ast::TypeKind::Pointer(pointer) => {
                Self::owner_name_candidates_from_type(&pointer.inner)
            }
            ast::TypeKind::Primitive(p) => vec![format!("{:?}", p).to_lowercase()],
            _ => Vec::new(),
        }
    }

    pub(crate) fn substitute_expression_types(
        expr: &mut ast::Expression,
        mapping: &HashMap<String, ast::Type>,
    ) {
        match expr.kind.as_mut() {
            ast::ExpressionKind::Cast {
                expression,
                target_type,
            } => {
                Self::substitute_expression_types(expression, mapping);
                **target_type = Self::substitute_generic_type(target_type, mapping);
            }
            ast::ExpressionKind::TypeName(ty) => {
                *ty = Self::substitute_generic_type(ty, mapping);
            }
            ast::ExpressionKind::Call {
                function,
                arguments,
            } => {
                Self::substitute_expression_types(function, mapping);
                for arg in arguments {
                    Self::substitute_expression_types(arg, mapping);
                }
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                arguments,
                ..
            } => {
                Self::substitute_expression_types(receiver, mapping);
                for arg in arguments {
                    Self::substitute_expression_types(arg, mapping);
                }
            }
            ast::ExpressionKind::Binary { left, right, .. } => {
                Self::substitute_expression_types(left, mapping);
                Self::substitute_expression_types(right, mapping);
            }
            ast::ExpressionKind::Unary { operand, .. }
            | ast::ExpressionKind::Postfix { operand, .. }
            | ast::ExpressionKind::Move(operand)
            | ast::ExpressionKind::Comptime(operand)
            | ast::ExpressionKind::Reference {
                expression: operand,
                ..
            } => Self::substitute_expression_types(operand, mapping),
            ast::ExpressionKind::FieldAccess { object, .. }
            | ast::ExpressionKind::Index { object, .. } => {
                Self::substitute_expression_types(object, mapping)
            }
            ast::ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                Self::substitute_expression_types(condition, mapping);
                Self::substitute_block_types(then_branch, mapping);
                if let Some(else_branch) = else_branch {
                    Self::substitute_block_types(else_branch, mapping);
                }
            }
            ast::ExpressionKind::While { condition, body } => {
                Self::substitute_expression_types(condition, mapping);
                Self::substitute_block_types(body, mapping);
            }
            ast::ExpressionKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                if let Some(annotation) = &mut init.type_annotation {
                    *annotation = Self::substitute_generic_type(annotation, mapping);
                }
                if let Some(init_expr) = &mut init.initializer {
                    Self::substitute_expression_types(init_expr, mapping);
                }
                Self::substitute_expression_types(condition, mapping);
                Self::substitute_expression_types(increment, mapping);
                Self::substitute_block_types(body, mapping);
            }
            ast::ExpressionKind::Match { expression, arms } => {
                Self::substitute_expression_types(expression, mapping);
                for arm in arms {
                    if let Some(guard) = &mut arm.guard {
                        Self::substitute_expression_types(guard, mapping);
                    }
                    Self::substitute_expression_types(&mut arm.body, mapping);
                }
            }
            ast::ExpressionKind::Block(block) => Self::substitute_block_types(block, mapping),
            ast::ExpressionKind::Tuple(items) | ast::ExpressionKind::Array(items) => {
                for item in items {
                    Self::substitute_expression_types(item, mapping);
                }
            }
            ast::ExpressionKind::StructLiteral { fields, .. } => {
                for field in fields {
                    Self::substitute_expression_types(&mut field.value, mapping);
                }
            }
            ast::ExpressionKind::Initializer { items } => {
                for item in items {
                    match item {
                        ast::InitializerItem::Positional(expr) => {
                            Self::substitute_expression_types(expr, mapping)
                        }
                        ast::InitializerItem::Index { index, value } => {
                            Self::substitute_expression_types(index, mapping);
                            Self::substitute_expression_types(value, mapping);
                        }
                        ast::InitializerItem::Field { value, .. } => {
                            Self::substitute_expression_types(value, mapping)
                        }
                    }
                }
            }
            ast::ExpressionKind::Asm { .. } => {}
            ast::ExpressionKind::MacroCall { args, .. } => {
                for arg in args {
                    if let ast::MacroArg::Expression(expr) = arg {
                        Self::substitute_expression_types(expr, mapping);
                    }
                }
            }
            ast::ExpressionKind::ForIn { iterable, body, .. } => {
                Self::substitute_expression_types(iterable, mapping);
                Self::substitute_block_types(body, mapping);
            }
            ast::ExpressionKind::Identifier(ident) => {
                let name = ident.name.clone();
                if let Some(concrete_ty) = mapping.get(&name) {
                    *expr.kind = ast::ExpressionKind::TypeName(concrete_ty.clone());
                }
            }
            ast::ExpressionKind::EnumVariant { fields, .. } => {
                for field in fields {
                    Self::substitute_expression_types(field, mapping);
                }
            }
            ast::ExpressionKind::Literal(_) => {}
        }
    }

    pub(crate) fn substitute_block_types(
        block: &mut ast::Block,
        mapping: &HashMap<String, ast::Type>,
    ) {
        for statement in &mut block.statements {
            match &mut statement.kind {
                ast::StatementKind::Block(block) => Self::substitute_block_types(block, mapping),
                ast::StatementKind::Let(let_stmt) => {
                    if let Some(annotation) = &mut let_stmt.type_annotation {
                        *annotation = Self::substitute_generic_type(annotation, mapping);
                    }
                    if let Some(init) = &mut let_stmt.initializer {
                        Self::substitute_expression_types(init, mapping);
                    }
                }
                ast::StatementKind::Expression(expr)
                | ast::StatementKind::Return(Some(expr))
                | ast::StatementKind::Break(Some(expr)) => {
                    Self::substitute_expression_types(expr, mapping)
                }
                ast::StatementKind::Return(None)
                | ast::StatementKind::Break(None)
                | ast::StatementKind::Continue => {}
                ast::StatementKind::Defer(inner) => {
                    match &mut inner.kind {
                        ast::StatementKind::Block(block) => {
                            Self::substitute_block_types(block, mapping)
                        }
                        ast::StatementKind::Let(let_stmt) => {
                            if let Some(annotation) = &mut let_stmt.type_annotation {
                                *annotation = Self::substitute_generic_type(annotation, mapping);
                            }
                            if let Some(init) = &mut let_stmt.initializer {
                                Self::substitute_expression_types(init, mapping);
                            }
                        }
                        ast::StatementKind::Expression(expr)
                        | ast::StatementKind::Return(Some(expr))
                        | ast::StatementKind::Break(Some(expr)) => {
                            Self::substitute_expression_types(expr, mapping)
                        }
                        ast::StatementKind::Return(None)
                        | ast::StatementKind::Break(None)
                        | ast::StatementKind::Continue => {}
                        ast::StatementKind::Defer(_) => {} // nested defer (no further nesting to chase)
                    }
                }
            }
        }
    }

    /// After substituting generic types in a cloned body, rewrite any remaining
    /// TypeName-style generic function calls (e.g. `alloc<i32>(4)`) to Identifier
    /// calls (e.g. `alloc__i32_i64(4)`). Uses the LLVM module to look up whether
    /// a monomorphized function with the expected mangled name was already declared.
    pub(crate) fn rewrite_call_sites_in_block(&self, block: &mut ast::Block) {
        for stmt in &mut block.statements {
            match &mut stmt.kind {
                ast::StatementKind::Block(inner) => {
                    self.rewrite_call_sites_in_block(inner);
                }
                ast::StatementKind::Let(let_stmt) => {
                    if let Some(init) = &mut let_stmt.initializer {
                        self.rewrite_call_sites_in_expression(init);
                    }
                }
                ast::StatementKind::Expression(expr)
                | ast::StatementKind::Return(Some(expr))
                | ast::StatementKind::Break(Some(expr)) => {
                    self.rewrite_call_sites_in_expression(expr);
                }
                ast::StatementKind::Return(None)
                | ast::StatementKind::Break(None)
                | ast::StatementKind::Continue => {}
                ast::StatementKind::Defer(inner) => match &mut inner.kind {
                    ast::StatementKind::Block(inner_block) => {
                        self.rewrite_call_sites_in_block(inner_block);
                    }
                    ast::StatementKind::Let(let_stmt) => {
                        if let Some(init) = &mut let_stmt.initializer {
                            self.rewrite_call_sites_in_expression(init);
                        }
                    }
                    ast::StatementKind::Expression(expr)
                    | ast::StatementKind::Return(Some(expr))
                    | ast::StatementKind::Break(Some(expr)) => {
                        self.rewrite_call_sites_in_expression(expr);
                    }
                    _ => {}
                },
            }
        }
    }

    pub(crate) fn rewrite_call_sites_in_expression(&self, expr: &mut ast::Expression) {
        match expr.kind.as_mut() {
            ast::ExpressionKind::Call {
                function,
                arguments,
            } => {
                // Check if this is a TypeName call that should be rewritten.
                if let ast::ExpressionKind::TypeName(ty) = function.kind.as_mut()
                    && let ast::TypeKind::Named(named) = ty.kind.as_mut()
                    && let Some(generics) = &named.generics
                    && !generics.is_empty()
                    && generics.iter().all(|g| !Self::type_has_type_param(g))
                {
                    // All concrete args -> this is a generic function call
                    let fn_name = named
                        .path
                        .iter()
                        .map(|id| id.name.as_str())
                        .collect::<Vec<_>>()
                        .join(".");
                    let rhs_args: Vec<Type> = generics.iter().map(Type::from_ast).collect();
                    let base_mangled = mangle_name(&fn_name, &rhs_args);
                    // Check LLVM module: monomorphized functions are declared in Pass 1a
                    if self.module.get_function(&base_mangled).is_some() {
                        **function = ast::Expression {
                            kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                                name: base_mangled,
                                span: function.span,
                            })),
                            span: function.span,
                        };
                    } else {
                        // Try with parameter suffix (e.g. alloc__i32 vs alloc__i32_i64)
                        for name in self.function_name_to_symbol.keys() {
                            if name.starts_with(&format!("{}_", base_mangled)) {
                                **function = ast::Expression {
                                    kind: Box::new(ast::ExpressionKind::Identifier(
                                        ast::Identifier {
                                            name: name.clone(),
                                            span: function.span,
                                        },
                                    )),
                                    span: function.span,
                                };
                                break;
                            }
                        }
                    }
                }
                for arg in arguments {
                    self.rewrite_call_sites_in_expression(arg);
                }
            }
            ast::ExpressionKind::Block(block) => {
                self.rewrite_call_sites_in_block(block);
            }
            ast::ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.rewrite_call_sites_in_expression(condition);
                self.rewrite_call_sites_in_block(then_branch);
                if let Some(branch) = else_branch {
                    self.rewrite_call_sites_in_block(branch);
                }
            }
            ast::ExpressionKind::While { condition, body } => {
                self.rewrite_call_sites_in_expression(condition);
                self.rewrite_call_sites_in_block(body);
            }
            ast::ExpressionKind::Binary { left, right, .. } => {
                self.rewrite_call_sites_in_expression(left);
                self.rewrite_call_sites_in_expression(right);
            }
            ast::ExpressionKind::Unary { operand, .. }
            | ast::ExpressionKind::Postfix { operand, .. } => {
                self.rewrite_call_sites_in_expression(operand);
            }
            ast::ExpressionKind::Cast { expression, .. } => {
                self.rewrite_call_sites_in_expression(expression);
            }
            ast::ExpressionKind::FieldAccess { object, .. }
            | ast::ExpressionKind::Index { object, .. } => {
                self.rewrite_call_sites_in_expression(object);
            }
            _ => {}
        }
    }

    /// Check if an AST type contains any unresolved type parameter (which would
    /// mean it's still a generic type, not a concrete instantiation).
    pub(crate) fn type_has_type_param(ty: &ast::Type) -> bool {
        match ty.kind.as_ref() {
            ast::TypeKind::Primitive(_) => false,
            ast::TypeKind::Named(named) => {
                // A singleton named type with no generics and uppercase first char
                // is likely a type parameter
                if named.path.len() == 1 && named.generics.is_none() {
                    let first = named.path[0].name.chars().next();
                    matches!(first, Some(c) if c.is_uppercase())
                } else if let Some(generics) = &named.generics {
                    generics.iter().any(Self::type_has_type_param)
                } else {
                    false
                }
            }
            ast::TypeKind::Pointer(ptr) => Self::type_has_type_param(&ptr.inner),
            ast::TypeKind::Reference(inner) => Self::type_has_type_param(&inner.inner),
            ast::TypeKind::Optional(inner) => Self::type_has_type_param(inner),
            ast::TypeKind::Slice(slice) => Self::type_has_type_param(&slice.element_type),
            ast::TypeKind::Array(arr) => Self::type_has_type_param(&arr.element_type),
            _ => false,
        }
    }

    pub(crate) fn try_instantiate_generic_impl_method_for_type(
        &mut self,
        receiver_type: &ast::Type,
        method_name: &str,
    ) -> CodegenResult<Option<String>> {
        self.try_instantiate_generic_impl_method_for_type_filtered(receiver_type, method_name, None)
    }

    pub(crate) fn try_instantiate_generic_impl_method_for_type_filtered(
        &mut self,
        receiver_type: &ast::Type,
        method_name: &str,
        required_trait: Option<&str>,
    ) -> CodegenResult<Option<String>> {
        let Some(receiver_named) = Self::extract_named_type(receiver_type) else {
            return Ok(None);
        };
        let Some(receiver_args) = &receiver_named.generics else {
            return Ok(None);
        };
        let base_name = Self::named_type_name(receiver_named);
        let owner = Self::monomorph_owner_name_from_named(receiver_named);

        let templates = self.generic_impl_templates.clone();
        for template in templates {
            // Trait filter: skip if required_trait is set and doesn't match
            if let Some(required_trait) = required_trait {
                let Some(trait_ref) = &template.trait_ref else {
                    continue;
                };
                let Some(trait_name) = trait_ref.path.last().map(|id| id.name.as_str()) else {
                    continue;
                };
                if trait_name != required_trait {
                    continue;
                }
            }

            let Some(template_named) = Self::extract_named_type(&template.self_type) else {
                continue;
            };
            if Self::named_type_name(template_named) != base_name {
                continue;
            }
            let Some(template_args) = &template_named.generics else {
                continue;
            };
            if template_args.len() != receiver_args.len() {
                continue;
            }

            let mut mapping: HashMap<String, ast::Type> = HashMap::default();
            let mut valid = true;
            for (template_arg, concrete_arg) in template_args.iter().zip(receiver_args.iter()) {
                let ast::TypeKind::Named(named) = template_arg.kind.as_ref() else {
                    valid = false;
                    break;
                };
                if named.path.len() != 1 || named.generics.is_some() {
                    valid = false;
                    break;
                }
                mapping.insert(named.path[0].name.clone(), concrete_arg.clone());
            }
            if !valid {
                continue;
            }

            let Some(template_func) = template.items.iter().find_map(|item| match item {
                ast::ImplItemKind::Function(func)
                    if func.name.name == method_name && func.generics.is_none() =>
                {
                    Some(func.clone())
                }
                _ => None,
            }) else {
                continue;
            };

            let mangled_name = Self::mangle_method_name(&owner, method_name);
            if self.module.get_function(&mangled_name).is_none() {
                let mut func = template_func;
                for param in &mut func.parameters {
                    param.param_type = Self::substitute_generic_type(&param.param_type, &mapping);
                }
                if let Some(return_ty) = &mut func.return_type {
                    *return_ty = Self::substitute_generic_type(return_ty, &mapping);
                }
                Self::substitute_block_types(&mut func.body, &mapping);
                self.rewrite_call_sites_in_block(&mut func.body);

                let fn_ty = self.lower_function_type(
                    &func
                        .parameters
                        .iter()
                        .map(|param| param.param_type.clone())
                        .collect::<Vec<_>>(),
                    func.return_type.as_ref(),
                    false,
                    None,
                )?;
                let function = self.module.add_function(&mangled_name, fn_ty, None);
                Self::apply_function_linkage(function, &func.visibility);

                let expects_ref = func
                    .parameters
                    .first()
                    .map(|param| {
                        matches!(param.param_type.kind.as_ref(), ast::TypeKind::Pointer(_))
                    })
                    .unwrap_or(false);
                self.method_receivers
                    .insert((owner.clone(), method_name.to_string()), expects_ref);

                let saved_fn = self.current_fn;
                let saved_block = self.builder.get_insert_block();
                let saved_defers = std::mem::take(&mut self.defers);
                let saved_drop_flags = std::mem::take(&mut self.drop_flags);
                let saved_variables = std::mem::take(&mut self.variables);

                self.defers.push(Vec::new());
                self.variables.push(HashMap::default());

                self.emit_function_body(
                    function,
                    &func.parameters,
                    func.return_type.as_ref(),
                    &func.body,
                    &mangled_name,
                    &func.name.span,
                    false,
                )?;

                self.defers = saved_defers;
                self.drop_flags = saved_drop_flags;
                self.variables = saved_variables;
                self.current_fn = saved_fn;
                if let Some(saved_block) = saved_block {
                    self.builder.position_at_end(saved_block);
                }
            }

            // When a Drop-trait instantiation succeeds, record the owner
            if required_trait == Some("Drop") {
                self.drop_trait_impl_owners.insert(owner.clone());
            }

            return Ok(Some(mangled_name));
        }

        Ok(None)
    }

    pub(crate) fn resolve_receiver_type(&mut self, expr: &ast::Expression) -> Option<ast::Type> {
        match expr.kind.as_ref() {
            ast::ExpressionKind::TypeName(ty) => Some(ty.clone()),
            ast::ExpressionKind::Identifier(identifier) => self.lookup_value_type(&identifier.name),
            ast::ExpressionKind::Cast { target_type, .. } => Some((**target_type).clone()),
            ast::ExpressionKind::FieldAccess { .. }
            | ast::ExpressionKind::Index { .. }
            | ast::ExpressionKind::Unary {
                operator: ast::UnaryOperator::Dereference,
                ..
            } => self.resolve_lvalue_ptr(expr).ok().map(|(_, ty)| ty),
            ast::ExpressionKind::Reference {
                is_mutable,
                expression,
            } => {
                let inner_ty = self.resolve_receiver_type(expression)?;
                Some(ast::Type {
                    kind: Box::new(ast::TypeKind::Pointer(ast::PointerType {
                        inner: Box::new(inner_ty),
                        is_mutable: *is_mutable,
                        is_volatile: false,
                    })),
                    span: expr.span,
                })
            }
            ast::ExpressionKind::Literal(lit) => {
                let prim = match lit {
                    ast::Literal::Integer(_) => ast::PrimitiveType::I32,
                    ast::Literal::Float(_) => ast::PrimitiveType::F64,
                    ast::Literal::Complex(_, _) => ast::PrimitiveType::C64,
                    ast::Literal::String(_) => ast::PrimitiveType::Str,
                    ast::Literal::Char(_) => ast::PrimitiveType::Char,
                    ast::Literal::Bool(_) => ast::PrimitiveType::Bool,
                };
                Some(ast::Type {
                    kind: Box::new(ast::TypeKind::Primitive(prim)),
                    span: expr.span,
                })
            }
            ast::ExpressionKind::Call { function, .. } => {
                if let ast::ExpressionKind::Identifier(ident) = function.kind.as_ref()
                    && let Some(sig) = self.signature_for_name(&ident.name)
                {
                    return sig.return_type.clone();
                }
                if let Some(func_ty) = self.resolve_receiver_type(function) {
                    match func_ty.kind.as_ref() {
                        ast::TypeKind::Function(func) => Some((*func.return_type).clone()),
                        ast::TypeKind::Pointer(ptr) => {
                            if let ast::TypeKind::Function(func) = ptr.inner.kind.as_ref() {
                                Some((*func.return_type).clone())
                            } else {
                                None
                            }
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            ast::ExpressionKind::MethodCall {
                receiver, method, ..
            } => {
                let owners = self.receiver_owner_candidates(receiver);
                let mut return_ty = None;
                for owner_name in &owners {
                    let mangled = Self::mangle_method_name(owner_name, &method.name);
                    if let Some(sig) = self.signature_for_name(&mangled) {
                        return_ty = sig.return_type.clone();
                        break;
                    }
                }
                if return_ty.is_none()
                    && let Some(sig) = self.signature_for_name(&method.name)
                {
                    return_ty = sig.return_type.clone();
                }
                return_ty
            }
            ast::ExpressionKind::Binary { left, .. } => self.resolve_receiver_type(left),
            ast::ExpressionKind::Unary { operand, operator } => match operator {
                ast::UnaryOperator::Not => Some(ast::Type {
                    kind: Box::new(ast::TypeKind::Primitive(ast::PrimitiveType::Bool)),
                    span: expr.span,
                }),
                _ => self.resolve_receiver_type(operand),
            },
            _ => None,
        }
    }

    pub(crate) fn receiver_owner_candidates(&mut self, expr: &ast::Expression) -> Vec<String> {
        match expr.kind.as_ref() {
            ast::ExpressionKind::TypeName(ty) => Self::owner_name_candidates_from_type(ty),
            ast::ExpressionKind::StructLiteral { path, .. } => vec![Self::path_name(path)],
            _ => {
                if let Some(ty) = self.resolve_receiver_type(expr) {
                    Self::owner_name_candidates_from_type(&ty)
                } else if let ast::ExpressionKind::Identifier(identifier) = expr.kind.as_ref() {
                    vec![Self::sanitize_monomorph(&identifier.name)]
                } else {
                    Vec::new()
                }
            }
        }
    }

    pub(crate) fn has_generic_placeholder_type(&self, ty: &ast::Type) -> bool {
        match ty.kind.as_ref() {
            ast::TypeKind::Generic(_) => true,
            ast::TypeKind::Named(named) => {
                if named.path.len() == 1 && named.generics.is_none() {
                    let candidate = &named.path[0].name;
                    if self.is_generic_placeholder_name(candidate) {
                        return true;
                    }
                }
                named
                    .generics
                    .as_ref()
                    .map(|args| {
                        args.iter()
                            .any(|arg| self.has_generic_placeholder_type(arg))
                    })
                    .unwrap_or(false)
            }
            ast::TypeKind::Reference(reference) => {
                self.has_generic_placeholder_type(&reference.inner)
            }
            ast::TypeKind::Pointer(pointer) => self.has_generic_placeholder_type(&pointer.inner),
            ast::TypeKind::Slice(slice) => self.has_generic_placeholder_type(&slice.element_type),
            ast::TypeKind::Optional(inner) => self.has_generic_placeholder_type(inner),
            ast::TypeKind::Function(function) => {
                self.has_generic_placeholder_type(&function.return_type)
                    || function
                        .parameters
                        .iter()
                        .any(|param| self.has_generic_placeholder_type(param))
            }
            ast::TypeKind::Tuple(items) => items
                .iter()
                .any(|item| self.has_generic_placeholder_type(item)),
            _ => false,
        }
    }

    pub(crate) fn is_generic_placeholder_name(&self, name: &str) -> bool {
        self.struct_generics
            .values()
            .any(|params| params.iter().any(|p| p == name))
    }

    pub(crate) fn has_generic_placeholder_signature(
        &self,
        params: &[ast::Parameter],
        return_type: Option<&ast::Type>,
    ) -> bool {
        params
            .iter()
            .any(|param| self.has_generic_placeholder_type(&param.param_type))
            || return_type
                .map(|ret| self.has_generic_placeholder_type(ret))
                .unwrap_or(false)
    }

    /// Pass-1 collection for impl methods.
    ///
    /// Registers receiver mode and declares mangled LLVM function signatures so
    pub(crate) fn is_drop_trait_impl(item: &ast::ImplItem) -> bool {
        item.trait_ref.as_ref().is_some_and(|trait_ref| {
            trait_ref.path.last().map(|id| id.name.as_str()) == Some("Drop")
        })
    }

    /// method calls can resolve before bodies are emitted.
    pub(crate) fn collect_impl_method_signatures(
        &mut self,
        item: &ast::ImplItem,
        impl_visibility: &ast::Visibility,
    ) -> CodegenResult<()> {
        if item.generics.is_some() {
            return Ok(());
        }
        if self.has_generic_placeholder_type(&item.self_type) {
            return Ok(());
        }

        let Some(owner) = Self::owner_name_from_type(&item.self_type) else {
            return Ok(());
        };

        for impl_item in &item.items {
            match impl_item {
                ast::ImplItemKind::Function(func) => {
                    if func.generics.is_some() {
                        continue;
                    }
                    if self.has_generic_placeholder_signature(
                        &func.parameters,
                        func.return_type.as_ref(),
                    ) {
                        continue;
                    }

                    let expects_ref = func
                        .parameters
                        .first()
                        .map(|param| {
                            matches!(param.param_type.kind.as_ref(), ast::TypeKind::Pointer(_))
                        })
                        .unwrap_or(false);
                    self.method_receivers
                        .entry((owner.clone(), func.name.name.clone()))
                        .or_insert(expects_ref);

                    let mangled_name = Self::mangle_method_name(&owner, &func.name.name);
                    let effective_visibility =
                        Self::method_effective_visibility(impl_visibility, &func.visibility);
                    self.register_function_signature(
                        &mangled_name,
                        FunctionSig {
                            params: func
                                .parameters
                                .iter()
                                .map(|param| param.param_type.clone())
                                .collect(),
                            return_type: func.return_type.clone(),
                            is_variadic: func.is_variadic,
                            linkage: None,
                        },
                        Some(func.name.span),
                        SymbolKind::ImplMethod,
                    );

                    if self.module.get_function(&mangled_name).is_none() {
                        let fn_ty = self.lower_function_type(
                            &func
                                .parameters
                                .iter()
                                .map(|param| param.param_type.clone())
                                .collect::<Vec<_>>(),
                            func.return_type.as_ref(),
                            func.is_variadic,
                            None,
                        )?;
                        let function = self.module.add_function(&mangled_name, fn_ty, None);
                        Self::apply_function_linkage(function, &effective_visibility);
                    } else if let Some(function) = self.module.get_function(&mangled_name) {
                        Self::apply_function_linkage(function, &effective_visibility);
                    }
                }
                ast::ImplItemKind::Cast(cast) => {
                    if self.has_generic_placeholder_signature(
                        &cast.parameters,
                        Some(&cast.target_type),
                    ) {
                        continue;
                    }
                    let cast_method_name = Self::cast_method_name(&cast.target_type);
                    let mangled_name = Self::mangle_method_name(&owner, &cast_method_name);
                    let effective_visibility = Self::method_effective_visibility(
                        impl_visibility,
                        &ast::Visibility::Private,
                    );
                    self.register_function_signature(
                        &mangled_name,
                        FunctionSig {
                            params: cast
                                .parameters
                                .iter()
                                .map(|param| param.param_type.clone())
                                .collect(),
                            return_type: Some(cast.target_type.clone()),
                            is_variadic: false,
                            linkage: None,
                        },
                        Some(cast.span),
                        SymbolKind::ImplMethod,
                    );
                    if self.module.get_function(&mangled_name).is_none() {
                        let fn_ty = self.lower_function_type(
                            &cast
                                .parameters
                                .iter()
                                .map(|param| param.param_type.clone())
                                .collect::<Vec<_>>(),
                            Some(&cast.target_type),
                            false,
                            None,
                        )?;
                        let function = self.module.add_function(&mangled_name, fn_ty, None);
                        Self::apply_function_linkage(function, &effective_visibility);
                    } else if let Some(function) = self.module.get_function(&mangled_name) {
                        Self::apply_function_linkage(function, &effective_visibility);
                    }
                }
                _ => {}
            }
        }

        Ok(())
    }
}
