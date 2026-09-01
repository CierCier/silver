use rustc_hash::FxHashMap as HashMap;
use rustc_hash::FxHashSet;

use inkwell::targets::TargetData;
use inkwell::types::StructType;
use inkwell::values::{BasicValue, BasicValueEnum};

use crate::codegen::llvm_ir::LlvmIrGenerator;
use crate::codegen::llvm_ir::{FreeFunctionSig, FunctionSig};
use crate::codegen::{CodegenError, CodegenResult};
use crate::parser::ast;
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

        if let Some(layout) = self
            .enum_payload_layouts
            .get(&key)
            .or_else(|| self.enum_payload_layouts.get(&base_name))
        {
            return Ok(*layout);
        }
        if self.enum_backing_types.contains_key(&base_name)
            || self.enum_variants.contains_key(&base_name)
        {
            let backing = self
                .enum_backing_types
                .get(&base_name)
                .cloned()
                .unwrap_or(ast::PrimitiveType::I32);
            let llvm_ty = self.lower_basic_type(&ast::Type {
                kind: Box::new(ast::TypeKind::Primitive(backing)),
                span: crate::lexer::Span::default(),
            })?;
            struct_ty.set_body(&[llvm_ty], false);
            return Ok(struct_ty);
        }

        if base_name == "Slice" && !self.struct_fields.contains_key("Slice") {
            let ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
            let i64_ty = self.context.i64_type();
            struct_ty.set_body(&[ptr_ty.into(), i64_ty.into()], false);
            return Ok(struct_ty);
        }

        let template_fields = match self.struct_fields.get(&base_name).cloned() {
            Some(fields) => fields,
            None if base_name.starts_with("__") => {
                struct_ty.set_body(&[], false);
                return Ok(struct_ty);
            }
            None => {
                return Err(CodegenError::new(format!(
                    "missing field metadata for struct `{base_name}`"
                )));
            }
        };

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
                .map(|arg| crate::mangling::sanitize_type_key(&Type::from_ast(arg).canonical_key()))
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

    /// Full canonical signature of a free function item (concrete types),
    /// used for overload detection and collision-safe symbol hashing.
    pub(crate) fn free_signature_from_ast(
        params: &[ast::Parameter],
        return_type: Option<&ast::Type>,
        is_variadic: bool,
    ) -> FreeFunctionSig {
        FreeFunctionSig {
            params: params
                .iter()
                .map(|param| Type::from_ast(&param.param_type).canonical_key())
                .collect(),
            return_type: return_type.map(|ret| Type::from_ast(ret).canonical_key()),
            is_variadic,
        }
    }

    /// LLVM symbol for a free function: the plain name when the name has a
    /// single signature, `{name}__{hash}` when overloaded (see
    /// crate::mangling for the collision guarantees).
    pub(crate) fn free_function_symbol_name(&self, name: &str, sig: &FreeFunctionSig) -> String {
        if self
            .free_function_sigs
            .get(name)
            .is_some_and(|sigs| sigs.len() > 1)
        {
            crate::mangling::overloaded_free_function_symbol(
                name,
                &sig.params,
                sig.return_type.as_deref(),
                sig.is_variadic,
            )
        } else {
            name.to_string()
        }
    }

    /// Record a source function name -> LLVM symbol so call sites can
    /// enumerate overload candidates. Only overloaded names need entries;
    /// single-signature and extern names resolve through the plain-name
    /// fallback.
    pub(crate) fn register_source_function_symbol(&mut self, source: &str, symbol: &str) {
        if source == symbol {
            return;
        }
        self.source_function_symbols
            .entry(source.to_string())
            .or_default()
            .push(symbol.to_string());
    }

    /// Codegen symbol for `(owner, method)` with the given full signature
    /// (params including the receiver, return, variadic). Every method
    /// symbol carries an FNV-1a-64 hash of its full signature, so distinct
    /// methods — even ones whose (owner, method) strings concatenate
    /// identically, e.g. `Foo.bar__baz` vs `Foo__bar.baz` — never collide.
    pub(crate) fn overloaded_method_symbol_name(
        &self,
        owner: &str,
        method: &str,
        sig: &FreeFunctionSig,
    ) -> String {
        crate::mangling::method_symbol(
            owner,
            method,
            &sig.params,
            sig.return_type.as_deref(),
            sig.is_variadic,
        )
    }

    /// Convenience: hashed method symbol directly from the AST method item.
    pub(crate) fn method_symbol_from_ast(
        &self,
        owner: &str,
        method: &str,
        params: &[ast::Parameter],
        return_type: Option<&ast::Type>,
        is_variadic: bool,
    ) -> String {
        let sig = Self::free_signature_from_ast(params, return_type, is_variadic);
        self.overloaded_method_symbol_name(owner, method, &sig)
    }

    /// Candidate symbols for a call to `(owner, method)` — one hashed symbol
    /// per recorded signature. Falls back to the classic `<Owner>__<method>`
    /// name when the signature table has no entry (e.g. methods materialized
    /// lazily before registration).
    pub(crate) fn overloaded_method_candidates(&self, owner: &str, method: &str) -> Vec<String> {
        let key = (owner.to_string(), method.to_string());
        let Some(signatures) = self.method_overload_signatures.get(&key) else {
            return vec![Self::mangle_method_name(owner, method)];
        };
        if signatures.is_empty() {
            return vec![Self::mangle_method_name(owner, method)];
        }
        signatures
            .iter()
            .map(|sig| {
                crate::mangling::method_symbol(
                    owner,
                    method,
                    &sig.params,
                    sig.return_type.as_deref(),
                    sig.is_variadic,
                )
            })
            .collect()
    }

    /// Pure (side-effect-free) type of an argument expression, used to match
    /// overload candidates. Covers identifiers, literals, casts, references,
    /// and call/method-call return types; avoids `resolve_lvalue_ptr`, which
    /// emits IR for field/index/deref addresses.
    pub(crate) fn resolve_argument_type(&mut self, expr: &ast::Expression) -> Option<ast::Type> {
        match expr.kind.as_ref() {
            ast::ExpressionKind::TypeName(ty) => Some(ty.clone()),
            ast::ExpressionKind::Identifier(identifier) => self.lookup_value_type(&identifier.name),
            ast::ExpressionKind::Cast { target_type, .. } => Some((**target_type).clone()),
            ast::ExpressionKind::Reference {
                is_mutable,
                expression,
            } => {
                let inner_ty = self.resolve_argument_type(expression)?;
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
                None
            }
            ast::ExpressionKind::MethodCall {
                receiver, method, ..
            } => {
                let owners = self.receiver_owner_candidates(receiver);
                for owner_name in &owners {
                    for mangled in self.overloaded_method_candidates(owner_name, &method.name) {
                        if let Some(sig) = self.signature_for_name(&mangled) {
                            return sig.return_type.clone();
                        }
                    }
                }
                self.signature_for_name(&method.name)
                    .and_then(|sig| sig.return_type)
            }
            ast::ExpressionKind::FieldAccess { object, field } => {
                let mut obj_ty = self.resolve_argument_type(object)?;
                loop {
                    let inner = match obj_ty.kind.as_ref() {
                        ast::TypeKind::Pointer(ptr) => Some((*ptr.inner).clone()),
                        ast::TypeKind::Reference(reference) => Some((*reference.inner).clone()),
                        _ => None,
                    };
                    match inner {
                        Some(next) => obj_ty = next,
                        None => break,
                    }
                }
                let owner = Self::owner_name_from_type(&obj_ty)?;
                self.struct_fields.get(&owner).and_then(|fields| {
                    fields
                        .iter()
                        .find(|(name, _)| *name == field.name)
                        .map(|(_, field_ty)| field_ty.clone())
                })
            }
            ast::ExpressionKind::Index { object, .. } => {
                let obj_ty = self.resolve_argument_type(object)?;
                match obj_ty.kind.as_ref() {
                    ast::TypeKind::Pointer(ptr) => Some((*ptr.inner).clone()),
                    ast::TypeKind::Reference(reference) => Some((*reference.inner).clone()),
                    ast::TypeKind::Array(array) => Some((*array.element_type).clone()),
                    ast::TypeKind::Slice(slice) => Some((*slice.element_type).clone()),
                    ast::TypeKind::Named(named) => {
                        if let Some(args) = &named.generics
                            && let Some(first) = args.first()
                        {
                            return Some(first.clone());
                        }
                        // IndexedAccess impl: the `__index_get` return type is
                        // the element type (e.g. String -> u8).
                        for owner in Self::owner_name_candidates_from_type(&obj_ty) {
                            for mangled in self.overloaded_method_candidates(&owner, "__index_get")
                            {
                                if let Some(sig) = self.signature_for_name(&mangled) {
                                    return sig.return_type;
                                }
                            }
                        }
                        None
                    }
                    _ => None,
                }
            }
            ast::ExpressionKind::Slice { object, .. } => {
                let obj_ty = self.resolve_argument_type(object)?;
                match obj_ty.kind.as_ref() {
                    ast::TypeKind::Array(array) => Some(ast::Type {
                        kind: Box::new(ast::TypeKind::Slice(Box::new(ast::SliceType {
                            element_type: array.element_type.clone(),
                        }))),
                        span: object.span,
                    }),
                    ast::TypeKind::Primitive(ast::PrimitiveType::Str) => Some(ast::Type {
                        kind: Box::new(ast::TypeKind::Slice(Box::new(ast::SliceType {
                            element_type: Box::new(ast::Type {
                                kind: Box::new(ast::TypeKind::Primitive(ast::PrimitiveType::U8)),
                                span: object.span,
                            }),
                        }))),
                        span: object.span,
                    }),
                    ast::TypeKind::Slice(_) => Some(obj_ty),
                    _ => None,
                }
            }
            ast::ExpressionKind::Binary { left, .. } => self.resolve_argument_type(left),
            ast::ExpressionKind::Unary { operand, operator } => match operator {
                ast::UnaryOperator::Not => Some(ast::Type {
                    kind: Box::new(ast::TypeKind::Primitive(ast::PrimitiveType::Bool)),
                    span: expr.span,
                }),
                ast::UnaryOperator::Dereference => {
                    // `*p` — the pointee type, not the pointer itself.
                    let pointee = self.resolve_argument_type(operand)?;
                    match pointee.kind.as_ref() {
                        ast::TypeKind::Pointer(ptr) => Some((*ptr.inner).clone()),
                        ast::TypeKind::Reference(reference) => Some((*reference.inner).clone()),
                        _ => None,
                    }
                }
                _ => self.resolve_argument_type(operand),
            },
            _ => None,
        }
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
        // Enum constructors are parsed as field access on a bare identifier
        // (`Result.Ok(value)`), not as a `TypeName`. Rewrite that identifier
        // when a generic impl maps its enum owner to a concrete monomorph.
        if let ast::ExpressionKind::Identifier(identifier) = expr.kind.as_ref()
            && let Some(concrete_ty) = mapping.get(&identifier.name)
            && matches!(concrete_ty.kind.as_ref(), ast::TypeKind::Named(_))
        {
            *expr.kind = ast::ExpressionKind::TypeName(concrete_ty.clone());
            return;
        }

        match expr.kind.as_mut() {
            ast::ExpressionKind::Cast {
                expression,
                target_type,
            } => {
                Self::substitute_expression_types(expression, mapping);
                **target_type = Self::substitute_generic_type(target_type, mapping);
            }
            ast::ExpressionKind::TypeName(ty) => {
                // A whole-enum rewrite (e.g. base `Optional` -> concrete
                // `Optional__i32` in a monomorphized impl body) replaces the
                // entire type; otherwise substitute generics normally.
                if let ast::TypeKind::Named(named) = ty.kind.as_ref()
                    && named.path.len() == 1
                    && named.generics.is_none()
                    && let Some(concrete_ty) = mapping.get(&named.path[0].name)
                    && let ast::TypeKind::Named(concrete_named) = concrete_ty.kind.as_ref()
                    && concrete_named.path.last().map(|id| id.name.as_str())
                        != Some(named.path[0].name.as_str())
                {
                    *ty = concrete_ty.clone();
                } else {
                    *ty = Self::substitute_generic_type(ty, mapping);
                }
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
            | ast::ExpressionKind::Launch(operand)
            | ast::ExpressionKind::Wait(operand)
            | ast::ExpressionKind::Reference {
                expression: operand,
                ..
            } => Self::substitute_expression_types(operand, mapping),
            ast::ExpressionKind::FieldAccess { object, .. }
            | ast::ExpressionKind::Index { object, .. }
            | ast::ExpressionKind::Slice { object, .. } => {
                Self::substitute_expression_types(object, mapping)
            }
            ast::ExpressionKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                Self::substitute_expression_types(condition, mapping);
                Self::substitute_expression_types(then_expr, mapping);
                Self::substitute_expression_types(else_expr, mapping);
            }
            ast::ExpressionKind::UnwrapOr { value, fallback } => {
                Self::substitute_expression_types(value, mapping);
                Self::substitute_expression_types(fallback, mapping);
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
            ast::ExpressionKind::EnumVariant { path, fields, .. } => {
                if path.len() == 1
                    && let Some(concrete_ty) = mapping.get(&path[0].name)
                    && let ast::TypeKind::Named(named) = concrete_ty.kind.as_ref()
                    && let Some(owner) = named.path.last()
                {
                    path[0].name = owner.name.clone();
                }
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
                    && generics.iter().all(|g| !self.type_has_type_param(g))
                {
                    // All concrete args -> this is a generic function call.
                    // Monomorphized instances are declared in Pass 1a under
                    // `{name}__{K}_{args}__{P}_{params}__{hash}`; match the
                    // registered instance with the same type-arg count and
                    // list and the same value arity (P = arguments.len()).
                    let fn_name = named
                        .path
                        .iter()
                        .map(|id| id.name.as_str())
                        .collect::<Vec<_>>()
                        .join(".");
                    let rhs_args: Vec<Type> = generics.iter().map(Type::from_ast).collect();
                    let arg_keys = rhs_args
                        .iter()
                        .map(|ty| crate::mangling::sanitize_type_key(&ty.canonical_key()))
                        .collect::<Vec<_>>();
                    let base_prefix =
                        format!("{fn_name}__{}_{}__", arg_keys.len(), arg_keys.join("_"));
                    // The params part is `{P}_{param1}_{param2}__{hash}`: after
                    // the value-arity count comes a single underscore, then the
                    // sanitized param types.
                    let param_prefix = format!("{base_prefix}{}_", arguments.len());
                    let found = self
                        .function_name_to_symbol
                        .keys()
                        .filter(|name| name.starts_with(&param_prefix))
                        .min_by_key(|name| name.len())
                        .cloned();
                    if let Some(mangled) = found {
                        **function = ast::Expression {
                            kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                                name: mangled,
                                span: function.span,
                            })),
                            span: function.span,
                        };
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
            | ast::ExpressionKind::Index { object, .. }
            | ast::ExpressionKind::Slice { object, .. } => {
                self.rewrite_call_sites_in_expression(object);
            }
            _ => {}
        }
    }

    /// Emit generic free-function instances referenced by `block` that the
    /// semantic monomorph pass never requested — nested calls inside
    /// lazily instantiated generic impl methods (e.g. `realloc<T>` inside
    /// `Vec<T>.push`). Two-phase: declare every reachable instance first
    /// (so call-site rewriting sees them), then emit the bodies.
    fn emit_missing_generic_free_instances(
        &mut self,
        outer_body: &mut ast::Block,
    ) -> CodegenResult<()> {
        // Worklist of (template name, concrete type args, value arity).
        let mut pending: Vec<(String, Vec<ast::Type>, usize)> = Vec::new();
        Self::collect_concrete_generic_calls(outer_body, &mut pending);
        let mut declared: FxHashSet<String> = FxHashSet::default();
        let mut bodies: Vec<(ast::FunctionItem, String)> = Vec::new();
        let mut i = 0;
        while i < pending.len() {
            let (fn_name, args, arg_count) = pending[i].clone();
            i += 1;
            let Some(templates) = self.generic_function_templates.get(&fn_name) else {
                continue;
            };
            let Some(template) = templates
                .iter()
                .find(|t| t.parameters.len() == arg_count)
                .cloned()
            else {
                continue;
            };
            let Some(generics) = &template.generics else {
                continue;
            };
            let mut mapping: HashMap<String, ast::Type> = HashMap::default();
            let mut type_params: Vec<String> = Vec::new();
            for param in &generics.params {
                if let ast::GenericParam::Type(tp) = param {
                    type_params.push(tp.name.name.clone());
                }
            }
            if type_params.len() != args.len() {
                continue;
            }
            for (p, a) in type_params.iter().zip(args.iter()) {
                mapping.insert(p.clone(), a.clone());
            }
            let sem_args: Vec<crate::types::Type> =
                args.iter().map(crate::types::Type::from_ast).collect();
            let sem_mapping: HashMap<String, crate::types::Type> = mapping
                .iter()
                .map(|(k, v)| (k.clone(), crate::types::Type::from_ast(v)))
                .collect();
            let instance = crate::semantic::monomorph::mangle_function_instance(
                &template,
                &sem_args,
                &sem_mapping,
            );
            if self.module.get_function(&instance).is_some() || !declared.insert(instance.clone()) {
                continue;
            }
            // Substitute the body so nested generic calls become concrete
            // and can be collected into the worklist.
            let mut func = template;
            for param in &mut func.parameters {
                param.param_type = Self::substitute_generic_type(&param.param_type, &mapping);
            }
            if let Some(ret) = &mut func.return_type {
                *ret = Self::substitute_generic_type(ret, &mapping);
            }
            Self::substitute_block_types(&mut func.body, &mapping);
            Self::collect_concrete_generic_calls(&func.body, &mut pending);
            // Declare (signature + symbol table) before any body emission.
            self.register_function_signature(
                &instance,
                FunctionSig {
                    params: func
                        .parameters
                        .iter()
                        .map(|p| p.param_type.clone())
                        .collect(),
                    return_type: func.return_type.clone(),
                    is_variadic: func.is_variadic,
                    linkage: None,
                },
                Some(func.name.span),
                SymbolKind::Function,
            );
            self.register_source_function_symbol(&fn_name, &instance);
            let fn_ty = self.lower_function_type(
                &func
                    .parameters
                    .iter()
                    .map(|p| p.param_type.clone())
                    .collect::<Vec<_>>(),
                func.return_type.as_ref(),
                func.is_variadic,
                None,
            )?;
            self.module.add_function(&instance, fn_ty, None);
            bodies.push((func, instance));
        }
        // Second phase: rewrite + emit every declared body (all instances
        // exist now, so call-site rewriting resolves them). Save/restore the
        // builder position around each nested emission: emit_function_body
        // leaves the builder in the emitted function, which would truncate
        // the caller's body.
        for (mut func, instance) in bodies {
            self.rewrite_call_sites_in_block(&mut func.body);
            let function = self
                .module
                .get_function(&instance)
                .ok_or_else(|| CodegenError::new(format!("missing instance {instance}")))?;
            let saved_block = self.builder.get_insert_block();
            let saved_debug = self.debug.as_mut().map(|d| {
                (
                    d.current_subprogram,
                    std::mem::take(&mut d.current_lexical_blocks),
                )
            });
            self.emit_function_body(
                function,
                &func.parameters,
                func.return_type.as_ref(),
                &func.body,
                &instance,
                &func.name.span,
                false,
            )?;
            if let Some((saved_subprogram, saved_blocks)) = saved_debug {
                let debug = self.debug.as_mut().expect("saved debug state");
                debug.current_subprogram = saved_subprogram;
                debug.current_lexical_blocks = saved_blocks;
            }
            if let Some(saved_block) = saved_block {
                self.builder.position_at_end(saved_block);
            }
        }
        // Re-rewrite the outer body now that its callees exist: the earlier
        // rewrite (before declaration) left concrete generic calls in
        // TypeName form, which codegen resolves through the legacy mangling.
        self.rewrite_call_sites_in_block(outer_body);
        Ok(())
    }

    /// Collect every free-function call with concrete generic type args
    /// reachable from `block` into `out` (name, args, value arity).
    fn collect_concrete_generic_calls(
        block: &ast::Block,
        out: &mut Vec<(String, Vec<ast::Type>, usize)>,
    ) {
        fn walk_expr(expr: &ast::Expression, out: &mut Vec<(String, Vec<ast::Type>, usize)>) {
            match expr.kind.as_ref() {
                ast::ExpressionKind::Call {
                    function,
                    arguments,
                } => {
                    if let ast::ExpressionKind::TypeName(ty) = function.kind.as_ref()
                        && let ast::TypeKind::Named(named) = ty.kind.as_ref()
                        && named.path.len() == 1
                        && let Some(generics) = &named.generics
                        && !generics.is_empty()
                    {
                        out.push((
                            named.path[0].name.clone(),
                            generics.to_vec(),
                            arguments.len(),
                        ));
                    }
                    for arg in arguments {
                        walk_expr(arg, out);
                    }
                }
                ast::ExpressionKind::Block(b) => walk_block(b, out),
                ast::ExpressionKind::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    walk_expr(condition, out);
                    walk_block(then_branch, out);
                    if let Some(b) = else_branch {
                        walk_block(b, out);
                    }
                }
                ast::ExpressionKind::While { condition, body } => {
                    walk_expr(condition, out);
                    walk_block(body, out);
                }
                ast::ExpressionKind::Binary { left, right, .. } => {
                    walk_expr(left, out);
                    walk_expr(right, out);
                }
                ast::ExpressionKind::Unary { operand, .. }
                | ast::ExpressionKind::Postfix { operand, .. }
                | ast::ExpressionKind::Cast {
                    expression: operand,
                    ..
                }
                | ast::ExpressionKind::Move(operand)
                | ast::ExpressionKind::Comptime(operand) => walk_expr(operand, out),
                ast::ExpressionKind::MethodCall {
                    receiver,
                    arguments,
                    ..
                } => {
                    walk_expr(receiver, out);
                    for arg in arguments {
                        walk_expr(arg, out);
                    }
                }
                ast::ExpressionKind::FieldAccess { object, .. }
                | ast::ExpressionKind::Index { object, .. }
                | ast::ExpressionKind::Slice { object, .. } => walk_expr(object, out),
                ast::ExpressionKind::ForIn { iterable, body, .. } => {
                    walk_expr(iterable, out);
                    walk_block(body, out);
                }
                ast::ExpressionKind::Array(items) | ast::ExpressionKind::Tuple(items) => {
                    for item in items {
                        walk_expr(item, out);
                    }
                }
                ast::ExpressionKind::Initializer { items } => {
                    for item in items {
                        match item {
                            ast::InitializerItem::Positional(expr) => walk_expr(expr, out),
                            ast::InitializerItem::Field { value, .. } => walk_expr(value, out),
                            ast::InitializerItem::Index { value, .. } => walk_expr(value, out),
                        }
                    }
                }
                ast::ExpressionKind::Ternary {
                    condition,
                    then_expr,
                    else_expr,
                } => {
                    walk_expr(condition, out);
                    walk_expr(then_expr, out);
                    walk_expr(else_expr, out);
                }
                ast::ExpressionKind::UnwrapOr { value, fallback } => {
                    walk_expr(value, out);
                    walk_expr(fallback, out);
                }
                _ => {}
            }
        }
        fn walk_statement(stmt: &ast::Statement, out: &mut Vec<(String, Vec<ast::Type>, usize)>) {
            match &stmt.kind {
                ast::StatementKind::Block(b) => walk_block(b, out),
                ast::StatementKind::Let(let_stmt) => {
                    if let Some(init) = &let_stmt.initializer {
                        walk_expr(init, out);
                    }
                }
                ast::StatementKind::Expression(expr)
                | ast::StatementKind::Return(Some(expr))
                | ast::StatementKind::Break(Some(expr)) => walk_expr(expr, out),
                ast::StatementKind::Defer(inner) => walk_statement(inner, out),
                _ => {}
            }
        }
        fn walk_block(block: &ast::Block, out: &mut Vec<(String, Vec<ast::Type>, usize)>) {
            for stmt in &block.statements {
                walk_statement(stmt, out);
            }
        }
        walk_block(block, out);
    }

    /// Check if an AST type contains any unresolved type parameter (which would
    /// mean it's still a generic type, not a concrete instantiation).
    pub(crate) fn type_has_type_param(&self, ty: &ast::Type) -> bool {
        match ty.kind.as_ref() {
            ast::TypeKind::Primitive(_) => false,
            ast::TypeKind::Named(named) => {
                // A singleton named type with no generics and uppercase first
                // char is likely a type parameter — UNLESS a concrete type
                // claims the name (a user struct may be named `Item` or
                // `Cookie`, which the uppercase heuristic would otherwise
                // misread as a parameter and silently skip instantiating).
                if named.path.len() == 1 && named.generics.is_none() {
                    let name = &named.path[0].name;
                    let first = name.chars().next();
                    if matches!(first, Some(c) if c.is_uppercase())
                        && !(self.struct_fields.contains_key(name)
                            || self.enum_backing_types.contains_key(name)
                            || self.type_aliases.contains(name))
                    {
                        return true;
                    }
                    false
                } else if let Some(generics) = &named.generics {
                    generics.iter().any(|g| self.type_has_type_param(g))
                } else {
                    false
                }
            }
            ast::TypeKind::Pointer(ptr) => self.type_has_type_param(&ptr.inner),
            ast::TypeKind::Reference(inner) => self.type_has_type_param(&inner.inner),
            ast::TypeKind::Optional(inner) => self.type_has_type_param(inner),
            ast::TypeKind::Slice(slice) => self.type_has_type_param(&slice.element_type),
            ast::TypeKind::Array(arr) => self.type_has_type_param(&arr.element_type),
            _ => false,
        }
    }

    /// Register a monomorphized generic enum (`Optional__i32`) from its base
    /// definition + concrete type-arg mapping: variant tag values, substituted
    /// payload types, and the `{i16, [N x i8]}` payload layout. Called when a
    /// generic enum-impl method is materialized on the fly and the concrete
    /// enum was not yet registered by monomorph.
    pub(crate) fn register_monomorphized_enum(
        &mut self,
        base_name: &str,
        mangled_name: &str,
        mapping: &HashMap<String, ast::Type>,
    ) -> CodegenResult<()> {
        let variants = self
            .enum_variants
            .get(base_name)
            .cloned()
            .unwrap_or_default();
        self.enum_variants
            .insert(mangled_name.to_string(), variants);

        if let Some(backing) = self.enum_backing_types.get(base_name) {
            self.enum_backing_types
                .insert(mangled_name.to_string(), backing.clone());
        }

        let base_payloads = self
            .enum_variant_payload_types
            .get(base_name)
            .cloned()
            .unwrap_or_default();
        let mut substituted: HashMap<String, Vec<ast::Type>> = HashMap::default();
        let mut max_payload_size: u64 = 0;
        for (variant, payload) in base_payloads {
            let concrete: Vec<ast::Type> = payload
                .iter()
                .map(|pt| Self::substitute_generic_type(pt, mapping))
                .collect();
            let mut variant_size: u64 = 0;
            for pt in &concrete {
                variant_size += self.ast_type_size(pt);
            }
            max_payload_size = max_payload_size.max(variant_size);
            substituted.insert(variant, concrete);
        }
        self.enum_variant_payload_types
            .insert(mangled_name.to_string(), substituted);

        if max_payload_size > 0 {
            let i16_ty = self.context.i16_type();
            let array_ty = self.context.i8_type().array_type(max_payload_size as u32);
            let struct_ty = self
                .context
                .struct_type(&[i16_ty.into(), array_ty.into()], false);
            struct_ty.set_body(&[i16_ty.into(), array_ty.into()], false);
            self.enum_payload_layouts
                .insert(mangled_name.to_string(), struct_ty);
            self.struct_types
                .insert(mangled_name.to_string(), struct_ty);
        }
        Ok(())
    }

    /// The base name of a type if it is a `Named` with generics, else the type's
    /// own name (single-segment Named) or empty.
    fn named_base_name(ty: &ast::Type) -> String {
        match ty.kind.as_ref() {
            ast::TypeKind::Named(named) if named.generics.is_some() => Self::named_type_name(named),
            ast::TypeKind::Named(named) if named.path.len() == 1 => named.path[0].name.clone(),
            _ => String::new(),
        }
    }

    /// The mangled owner name for a type (e.g. `Optional__i32` for
    /// `Optional<i32>`), or the bare name when not generic.
    fn mangled_name_for_type(ty: &ast::Type) -> String {
        match ty.kind.as_ref() {
            ast::TypeKind::Named(named) => Self::monomorph_owner_name_from_named(named),
            _ => Self::named_base_name(ty),
        }
    }

    /// Byte size of an AST type for payload-layout sizing.
    fn ast_type_size(&mut self, ty: &ast::Type) -> u64 {
        if let Ok(llvm_ty) = self.lower_basic_type(ty) {
            let target_data =
                TargetData::create(self.module.get_data_layout().as_str().to_str().unwrap());
            return target_data.get_abi_size(&llvm_ty);
        }

        match ty.kind.as_ref() {
            ast::TypeKind::Primitive(p) => match p {
                ast::PrimitiveType::I8 | ast::PrimitiveType::U8 | ast::PrimitiveType::Bool => 1,
                ast::PrimitiveType::I16 | ast::PrimitiveType::U16 => 2,
                ast::PrimitiveType::I32
                | ast::PrimitiveType::U32
                | ast::PrimitiveType::Char
                | ast::PrimitiveType::F32 => 4,
                ast::PrimitiveType::I64 | ast::PrimitiveType::U64 | ast::PrimitiveType::F64 => 8,
                ast::PrimitiveType::I128 | ast::PrimitiveType::U128 => 16,
                _ => 8,
            },
            ast::TypeKind::Pointer(_) | ast::TypeKind::Reference(_) => 8,
            _ => 8,
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
                    Some((**func).clone())
                }
                _ => None,
            }) else {
                continue;
            };

            let mut func = template_func;
            for param in &mut func.parameters {
                param.param_type = Self::substitute_generic_type(&param.param_type, &mapping);
            }
            if let Some(return_ty) = &mut func.return_type {
                *return_ty = Self::substitute_generic_type(return_ty, &mapping);
            }
            // The symbol carries a hash of the substituted signature; it must
            // agree with the declaration emitted from the monomorphized impl.
            let mangled_name = self.method_symbol_from_ast(
                &owner,
                method_name,
                &func.parameters,
                func.return_type.as_ref(),
                func.is_variadic,
            );
            if self.module.get_function(&mangled_name).is_none() {
                // Rewrite bare references to the enum's own base name inside
                // the method body (e.g. `Optional.Some(x)` in `impl Optional<T>`
                // instantiated for `Optional<i32>`) so variant construction
                // targets the concrete monomorphized enum.
                let mut body_mapping = mapping.clone();
                body_mapping.insert(
                    base_name.clone(),
                    ast::Type {
                        kind: Box::new(ast::TypeKind::Named(ast::NamedType {
                            path: vec![ast::Identifier {
                                name: owner.clone(),
                                span: func.name.span,
                            }],
                            generics: None,
                        })),
                        span: func.name.span,
                    },
                );
                Self::substitute_block_types(&mut func.body, &body_mapping);
                self.rewrite_call_sites_in_block(&mut func.body);
                // The semantic monomorph pass never saw this method body;
                // emit generic free-function instances it calls (e.g.
                // realloc<i64> in Vec<i64>.push) before emitting the body.
                let saved_nested = self.debug_nested;
                self.debug_nested = true;
                self.emit_missing_generic_free_instances(&mut func.body)?;
                self.debug_nested = saved_nested;

                // If the owner is a generic enum being instantiated on the fly
                // (method call reached before monomorph registered the concrete
                // enum), register the mangled enum's variants, payload types,
                // and layout so enum construction inside the method body can
                // resolve `Optional__i32.Some(...)`.
                if base_name != owner
                    && (self.enum_variants.contains_key(&base_name)
                        || self.enum_variant_payload_types.contains_key(&base_name))
                    && !self.enum_variants.contains_key(&owner)
                {
                    self.register_monomorphized_enum(&base_name, &owner, &mapping)?;
                }
                // Also register any generic enum referenced in the signature
                // (e.g. `next() -> Optional<T>` for a struct impl): the return
                // type's concrete enum (`Optional__i32`) must exist before the
                // signature is lowered so it resolves to the 4-byte layout.
                let mut signature_types: Vec<ast::Type> = func
                    .parameters
                    .iter()
                    .map(|p| p.param_type.clone())
                    .collect();
                if let Some(ret) = &func.return_type {
                    signature_types.push(ret.clone());
                }
                for sig_ty in signature_types {
                    let sig_base = Self::named_base_name(&sig_ty);
                    let sig_owner = Self::mangled_name_for_type(&sig_ty);
                    if sig_base != sig_owner
                        && (self.enum_variants.contains_key(&sig_base)
                            || self.enum_variant_payload_types.contains_key(&sig_base))
                        && !self.enum_variants.contains_key(&sig_owner)
                    {
                        self.register_monomorphized_enum(&sig_base, &sig_owner, &mapping)?;
                    }
                }

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
                        matches!(
                            param.param_type.kind.as_ref(),
                            ast::TypeKind::Pointer(_) | ast::TypeKind::Reference(_)
                        )
                    })
                    .unwrap_or(false);
                self.method_receivers
                    .insert((owner.clone(), method_name.to_string()), expects_ref);

                let saved_fn = self.current_fn;
                let saved_block = self.builder.get_insert_block();
                let saved_defers = std::mem::take(&mut self.defers);
                let saved_variables = std::mem::take(&mut self.variables);
                let saved_subprogram = self.debug.as_mut().and_then(|d| d.current_subprogram);

                self.defers.push(Vec::new());
                self.variables.push(HashMap::default());
                // Save/restore: nested emissions must not clobber an outer
                // lazy emission's suppression flag (their bodies interleave).
                let saved_nested = self.debug_nested;
                self.debug_nested = true;

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
                self.variables = saved_variables;
                self.current_fn = saved_fn;
                self.debug_nested = saved_nested;
                // The nested emission reset current_subprogram to None;
                // restore the enclosing function's so its remaining
                // variables keep correct scopes.
                if let Some(sub) = saved_subprogram
                    && let Some(debug) = self.debug.as_mut()
                {
                    debug.current_subprogram = Some(sub);
                }
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
            | ast::ExpressionKind::Slice { .. }
            | ast::ExpressionKind::Unary {
                operator: ast::UnaryOperator::Dereference,
                ..
            } => self
                .resolve_lvalue_ptr(expr)
                .ok()
                .map(|(_, ty)| ty)
                .or_else(|| self.resolve_argument_type(expr)),
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
                    for mangled in self.overloaded_method_candidates(owner_name, &method.name) {
                        if let Some(sig) = self.signature_for_name(&mangled) {
                            return_ty = sig.return_type.clone();
                            break;
                        }
                    }
                    if return_ty.is_some() {
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
            ast::ExpressionKind::UnwrapOr { value, fallback } => {
                if let Some(val_ty) = self.resolve_receiver_type(value) {
                    match val_ty.kind.as_ref() {
                        ast::TypeKind::Optional(inner) => Some((**inner).clone()),
                        ast::TypeKind::Named(named) => {
                            let name = named.path.last().map(|s| s.name.as_str());
                            if name == Some("Optional")
                                || name == Some("Result")
                                || name == Some("SysResult")
                            {
                                if let Some(gens) = &named.generics {
                                    gens.first().cloned()
                                } else {
                                    self.resolve_receiver_type(fallback)
                                }
                            } else {
                                self.resolve_receiver_type(fallback)
                            }
                        }
                        ast::TypeKind::Pointer(_) => Some(val_ty.clone()),
                        _ => self.resolve_receiver_type(fallback),
                    }
                } else {
                    self.resolve_receiver_type(fallback)
                }
            }
            ast::ExpressionKind::MacroCall { name, .. } => {
                if name.name == "json" {
                    Some(ast::Type {
                        kind: Box::new(ast::TypeKind::Primitive(ast::PrimitiveType::Str)),
                        span: expr.span,
                    })
                } else if name.name == "format" {
                    Some(ast::Type {
                        kind: Box::new(ast::TypeKind::Named(ast::NamedType {
                            path: vec![ast::Identifier {
                                name: "String".to_string(),
                                span: expr.span,
                            }],
                            generics: None,
                        })),
                        span: expr.span,
                    })
                } else if name.name == "hash" || name.name == "align" {
                    Some(ast::Type {
                        kind: Box::new(ast::TypeKind::Primitive(ast::PrimitiveType::I64)),
                        span: expr.span,
                    })
                } else {
                    None
                }
            }
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
        // A name is only a generic placeholder when no concrete type claims
        // it: std's Result<T, E> registers "T" and "E" as parameter names
        // program-wide, so a user struct (or alias) named `T` must not be
        // misread as a placeholder — doing so silently dropped every free
        // function mentioning it (never declared, "unknown function" at
        // call sites).
        if self.struct_fields.contains_key(name)
            || self.enum_backing_types.contains_key(name)
            || self.struct_types.contains_key(name)
            || self.type_aliases.contains(name)
        {
            return false;
        }
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
                            matches!(
                                param.param_type.kind.as_ref(),
                                ast::TypeKind::Pointer(_) | ast::TypeKind::Reference(_)
                            )
                        })
                        .unwrap_or(false);
                    self.method_receivers
                        .entry((owner.clone(), func.name.name.clone()))
                        .or_insert(expects_ref);

                    let mangled_name = self.method_symbol_from_ast(
                        &owner,
                        &func.name.name,
                        &func.parameters,
                        func.return_type.as_ref(),
                        func.is_variadic,
                    );
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
                    let mangled_name = self.method_symbol_from_ast(
                        &owner,
                        &cast_method_name,
                        &cast.parameters,
                        Some(&cast.target_type),
                        false,
                    );
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
