use std::collections::{HashMap, HashSet};

use crate::lexer::Span;
use crate::parser::ast;
use crate::types::Type;

#[derive(Debug, Clone)]
pub struct TraitError {
    pub message: String,
    pub span: Span,
}

#[derive(Debug, Default)]
pub struct TraitRegistry {
    traits: HashMap<String, TraitDef>,
    local_types: HashMap<String, Span>,
    trait_names: HashSet<String>,
}

#[derive(Debug, Clone)]
struct TraitDef {
    name: String,
    methods: HashMap<String, TraitMethod>,
    assoc_types: HashMap<String, TraitAssocType>,
    super_traits: Vec<String>,
    type_params: Vec<String>,
    type_param_bounds: HashMap<String, Vec<ast::TraitBound>>,
    span: Span,
}

#[derive(Debug, Clone)]
struct TraitMethod {
    name: String,
    params: Vec<Type>,
    return_type: Option<Type>,
    has_default: bool,
    span: Span,
}

#[derive(Debug, Clone)]
struct TraitAssocType {
    name: String,
    has_default: bool,
    span: Span,
}

impl TraitRegistry {
    pub fn collect(program: &ast::Program) -> (Self, Vec<TraitError>) {
        let mut registry = TraitRegistry::default();
        let mut errors = Vec::new();
        let mut trait_names = HashSet::new();

        for item in &program.items {
            match &item.kind {
                ast::ItemKind::Struct(struct_item) => {
                    registry
                        .local_types
                        .insert(struct_item.name.name.clone(), struct_item.name.span.clone());
                }
                ast::ItemKind::Enum(enum_item) => {
                    registry
                        .local_types
                        .insert(enum_item.name.name.clone(), enum_item.name.span.clone());
                }
                ast::ItemKind::Trait(trait_item) => {
                    trait_names.insert(trait_item.name.name.clone());
                }
                _ => {}
            }
        }

        registry.trait_names = trait_names;

        for item in &program.items {
            let ast::ItemKind::Trait(trait_item) = &item.kind else {
                continue;
            };

            if registry.traits.contains_key(&trait_item.name.name) {
                errors.push(TraitError {
                    message: format!("duplicate trait definition '{}'", trait_item.name.name),
                    span: trait_item.name.span.clone(),
                });
                continue;
            }

            let mut def = TraitDef {
                name: trait_item.name.name.clone(),
                methods: HashMap::new(),
                assoc_types: HashMap::new(),
                super_traits: Vec::new(),
                type_params: Vec::new(),
                type_param_bounds: HashMap::new(),
                span: trait_item.name.span.clone(),
            };

            if let Some(generics) = &trait_item.generics {
                for param in &generics.params {
                    if let ast::GenericParam::Type(type_param) = param {
                        def.type_params.push(type_param.name.name.clone());
                        if !type_param.bounds.is_empty() {
                            def.type_param_bounds
                                .insert(type_param.name.name.clone(), type_param.bounds.clone());
                        }
                    }
                }
            }

            for bound in &trait_item.super_traits {
                let super_name = trait_name_from_ref(&bound.trait_ref);
                if super_name.is_empty() {
                    errors.push(TraitError {
                        message: "invalid super trait reference".to_string(),
                        span: bound.trait_ref.span.clone(),
                    });
                    continue;
                }
                if bound.trait_ref.path.len() > 1 {
                    errors.push(TraitError {
                        message: format!(
                            "unknown super trait '{}'",
                            trait_path_key(&bound.trait_ref.path)
                        ),
                        span: bound.trait_ref.span.clone(),
                    });
                    continue;
                }
                if !registry.trait_names.contains(&super_name) && super_name != trait_item.name.name
                {
                    errors.push(TraitError {
                        message: format!("unknown super trait '{}'", super_name),
                        span: bound.trait_ref.span.clone(),
                    });
                }
                def.super_traits.push(super_name);
            }

            if let Some(generics) = &trait_item.generics {
                validate_generic_bounds(
                    &registry.trait_names,
                    &generics.params,
                    generics.where_clause.as_ref(),
                    &mut errors,
                );
            }

            for trait_member in &trait_item.items {
                match trait_member {
                    ast::TraitItemKind::Function(func) => {
                        let name = func.name.name.clone();
                        if def.methods.contains_key(&name) {
                            errors.push(TraitError {
                                message: format!(
                                    "duplicate trait method '{}' in trait '{}'",
                                    name, trait_item.name.name
                                ),
                                span: func.name.span.clone(),
                            });
                            continue;
                        }
                        let params = func
                            .parameters
                            .iter()
                            .map(|param| Type::from_ast(&param.param_type))
                            .collect();
                        let return_type = func.return_type.as_ref().map(Type::from_ast);
                        def.methods.insert(
                            name.clone(),
                            TraitMethod {
                                name,
                                params,
                                return_type,
                                has_default: func.default_body.is_some(),
                                span: func.span.clone(),
                            },
                        );
                    }
                    ast::TraitItemKind::AssociatedType(assoc) => {
                        let name = assoc.name.name.clone();
                        if def.assoc_types.contains_key(&name) {
                            errors.push(TraitError {
                                message: format!(
                                    "duplicate associated type '{}' in trait '{}'",
                                    name, trait_item.name.name
                                ),
                                span: assoc.name.span.clone(),
                            });
                            continue;
                        }
                        def.assoc_types.insert(
                            name.clone(),
                            TraitAssocType {
                                name,
                                has_default: assoc.default.is_some(),
                                span: assoc.span.clone(),
                            },
                        );

                        for bound in &assoc.bounds {
                            validate_bound_trait(&registry.trait_names, bound, &mut errors);
                        }
                    }
                }
            }

            registry.traits.insert(def.name.clone(), def);
        }

        (registry, errors)
    }

    pub fn validate_impls(&self, program: &ast::Program) -> Vec<TraitError> {
        let mut errors = Vec::new();
        let mut impl_keys = HashMap::new();

        for item in &program.items {
            let ast::ItemKind::Impl(impl_item) = &item.kind else {
                continue;
            };

            let Some(trait_ref) = &impl_item.trait_ref else {
                continue;
            };

            let trait_key = trait_path_key(&trait_ref.path);
            let trait_name = trait_name_from_ref(trait_ref);
            if trait_ref.path.len() > 1 {
                errors.push(TraitError {
                    message: format!("unknown trait '{}'", trait_key),
                    span: trait_ref.span.clone(),
                });
                continue;
            }
            let Some(trait_def) = self.traits.get(&trait_name) else {
                errors.push(TraitError {
                    message: format!("unknown trait '{}'", trait_key),
                    span: trait_ref.span.clone(),
                });
                continue;
            };

            let self_key = type_key(&Type::from_ast(&impl_item.self_type));
            let impl_key = format!("{} for {}", trait_key, self_key);
            if impl_keys.contains_key(&impl_key) {
                errors.push(TraitError {
                    message: format!("conflicting impl for '{}'", impl_key),
                    span: trait_ref.span.clone(),
                });
            } else {
                impl_keys.insert(impl_key.clone(), trait_ref.span.clone());
            }

            if is_foreign_trait(trait_ref) && !self.is_local_type(&impl_item.self_type) {
                errors.push(TraitError {
                    message: format!("orphan rule violation for '{}'", impl_key),
                    span: trait_ref.span.clone(),
                });
            }

            let impl_generics = impl_generic_names(&impl_item.generics);
            validate_impl_bounds(
                &self.trait_names,
                &impl_item.generics,
                &impl_generics,
                self,
                program,
                &mut errors,
            );

            let mut subst = HashMap::new();
            let expected_params = trait_def.type_params.len();
            let provided_params = trait_ref.generics.as_ref().map(|g| g.len()).unwrap_or(0);
            if expected_params != provided_params {
                errors.push(TraitError {
                    message: format!(
                        "trait '{}' expects {} type parameters, found {}",
                        trait_def.name, expected_params, provided_params
                    ),
                    span: trait_ref.span.clone(),
                });
            } else if let Some(generics) = &trait_ref.generics {
                for (name, ty) in trait_def.type_params.iter().zip(generics.iter()) {
                    subst.insert(name.clone(), Type::from_ast(ty));
                }
            }

            for (param_name, bounds) in &trait_def.type_param_bounds {
                let Some(arg_type) = subst.get(param_name) else {
                    continue;
                };
                let concrete = !contains_impl_generic(arg_type, &impl_generics);
                for bound in bounds {
                    if bound.is_optional {
                        continue;
                    }
                    validate_bound_trait(&self.trait_names, bound, &mut errors);
                    if !concrete {
                        continue;
                    }
                    let trait_name = trait_name_from_ref(&bound.trait_ref);
                    if trait_name.is_empty() || bound.trait_ref.path.len() > 1 {
                        continue;
                    }
                    if !self.has_impl(&trait_name, &type_key(arg_type), program) {
                        errors.push(TraitError {
                            message: format!(
                                "missing impl for bound '{}' on {}",
                                trait_name,
                                type_key(arg_type)
                            ),
                            span: bound.trait_ref.span.clone(),
                        });
                    }
                }
            }

            let mut subst = HashMap::new();
            let expected_params = trait_def.type_params.len();
            let provided_params = trait_ref.generics.as_ref().map(|g| g.len()).unwrap_or(0);
            if expected_params != provided_params {
                errors.push(TraitError {
                    message: format!(
                        "trait '{}' expects {} type parameters, found {}",
                        trait_def.name, expected_params, provided_params
                    ),
                    span: trait_ref.span.clone(),
                });
            } else if let Some(generics) = &trait_ref.generics {
                for (name, ty) in trait_def.type_params.iter().zip(generics.iter()) {
                    subst.insert(name.clone(), Type::from_ast(ty));
                }
            }

            let mut impl_methods: HashMap<String, ast::ImplFunction> = HashMap::new();
            let mut impl_assoc: HashMap<String, ast::ImplAssociatedType> = HashMap::new();

            for impl_item in &impl_item.items {
                match impl_item {
                    ast::ImplItemKind::Function(func) => {
                        impl_methods.insert(func.name.name.clone(), func.clone());
                    }
                    ast::ImplItemKind::AssociatedType(assoc) => {
                        impl_assoc.insert(assoc.name.name.clone(), assoc.clone());
                    }
                    ast::ImplItemKind::Cast(_) => {}
                }
            }

            for super_name in &trait_def.super_traits {
                if !self.has_impl(super_name, &self_key, program) {
                    errors.push(TraitError {
                        message: format!(
                            "missing super trait '{}' for trait '{}'",
                            super_name, trait_def.name
                        ),
                        span: trait_ref.span.clone(),
                    });
                }
            }

            let mut impl_subst = HashMap::new();

            for method in trait_def.methods.values() {
                if method.has_default {
                    continue;
                }
                let Some(impl_method) = impl_methods.get(&method.name) else {
                    errors.push(TraitError {
                        message: format!(
                            "missing method '{}' for trait '{}'",
                            method.name, trait_def.name
                        ),
                        span: trait_def.span.clone(),
                    });
                    continue;
                };

                let params = impl_method
                    .parameters
                    .iter()
                    .map(|param| Type::from_ast(&param.param_type))
                    .collect::<Vec<_>>();
                if params.len() != method.params.len() {
                    errors.push(TraitError {
                        message: format!("method '{}' parameter count mismatch", method.name),
                        span: impl_method.span.clone(),
                    });
                    continue;
                }
                for (idx, (expected, found)) in method.params.iter().zip(params.iter()).enumerate()
                {
                    if !unify_type(expected, found, &subst, &impl_generics, &mut impl_subst) {
                        errors.push(TraitError {
                            message: format!(
                                "method '{}' parameter {} type mismatch",
                                method.name, idx
                            ),
                            span: impl_method.span.clone(),
                        });
                        break;
                    }
                }

                let return_type = impl_method.return_type.as_ref().map(Type::from_ast);
                let matches = match (&method.return_type, &return_type) {
                    (Some(expected), Some(found)) => {
                        unify_type(expected, found, &subst, &impl_generics, &mut impl_subst)
                    }
                    (None, None) => true,
                    _ => false,
                };
                if !matches {
                    errors.push(TraitError {
                        message: format!("method '{}' return type mismatch", method.name),
                        span: impl_method.span.clone(),
                    });
                }
            }

            for assoc in trait_def.assoc_types.values() {
                if assoc.has_default {
                    continue;
                }
                if !impl_assoc.contains_key(&assoc.name) {
                    errors.push(TraitError {
                        message: format!(
                            "missing associated type '{}' for trait '{}'",
                            assoc.name, trait_def.name
                        ),
                        span: assoc.span.clone(),
                    });
                }
            }
        }

        errors
    }
}

fn type_key(ty: &Type) -> String {
    ty.canonical_key()
}

fn trait_name_from_ref(trait_ref: &ast::TraitRef) -> String {
    trait_ref
        .path
        .last()
        .map(|id| id.name.clone())
        .unwrap_or_default()
}

fn validate_bound_trait(
    trait_names: &HashSet<String>,
    bound: &ast::TraitBound,
    errors: &mut Vec<TraitError>,
) {
    let name = trait_name_from_ref(&bound.trait_ref);
    if name.is_empty() {
        errors.push(TraitError {
            message: "invalid trait bound".to_string(),
            span: bound.trait_ref.span.clone(),
        });
        return;
    }
    if bound.trait_ref.path.len() > 1 {
        errors.push(TraitError {
            message: format!("unknown trait '{}'", trait_path_key(&bound.trait_ref.path)),
            span: bound.trait_ref.span.clone(),
        });
        return;
    }
    if !trait_names.contains(&name) {
        errors.push(TraitError {
            message: format!("unknown trait '{}'", trait_path_key(&bound.trait_ref.path)),
            span: bound.trait_ref.span.clone(),
        });
    }
}

fn validate_generic_bounds(
    trait_names: &HashSet<String>,
    params: &[ast::GenericParam],
    where_clause: Option<&ast::WhereClause>,
    errors: &mut Vec<TraitError>,
) {
    for param in params {
        if let ast::GenericParam::Type(type_param) = param {
            for bound in &type_param.bounds {
                validate_bound_trait(trait_names, bound, errors);
            }
        }
    }

    if let Some(where_clause) = where_clause {
        for predicate in &where_clause.predicates {
            if let ast::WherePredicate::Type { bounds, .. } = predicate {
                for bound in bounds {
                    validate_bound_trait(trait_names, bound, errors);
                }
            }
        }
    }
}

fn validate_impl_bounds(
    trait_names: &HashSet<String>,
    generics: &Option<ast::Generics>,
    impl_generics: &HashSet<String>,
    registry: &TraitRegistry,
    program: &ast::Program,
    errors: &mut Vec<TraitError>,
) {
    let Some(generics) = generics else {
        return;
    };

    for param in &generics.params {
        if let ast::GenericParam::Type(type_param) = param {
            for bound in &type_param.bounds {
                validate_bound_trait(trait_names, bound, errors);
            }
        }
    }

    if let Some(where_clause) = &generics.where_clause {
        for predicate in &where_clause.predicates {
            let ast::WherePredicate::Type {
                bounded_type,
                bounds,
            } = predicate
            else {
                continue;
            };
            let bounded = Type::from_ast(bounded_type);
            let concrete = !contains_impl_generic(&bounded, impl_generics);
            for bound in bounds {
                validate_bound_trait(trait_names, bound, errors);
                if concrete {
                    let trait_name = trait_name_from_ref(&bound.trait_ref);
                    if !trait_name.is_empty()
                        && !registry.has_impl(&trait_name, &type_key(&bounded), program)
                    {
                        errors.push(TraitError {
                            message: format!(
                                "missing impl for bound '{}' on {}",
                                trait_name,
                                type_key(&bounded)
                            ),
                            span: bound.trait_ref.span.clone(),
                        });
                    }
                }
            }
        }
    }
}

fn impl_generic_names(generics: &Option<ast::Generics>) -> HashSet<String> {
    let mut names = HashSet::new();
    let Some(generics) = generics else {
        return names;
    };
    for param in &generics.params {
        if let ast::GenericParam::Type(type_param) = param {
            names.insert(type_param.name.name.clone());
        }
    }
    names
}

fn substitute_type(ty: &Type, subst: &HashMap<String, Type>) -> Type {
    ty.substitute(subst)
}

fn unify_type(
    expected: &Type,
    found: &Type,
    subst: &HashMap<String, Type>,
    impl_generics: &HashSet<String>,
    impl_subst: &mut HashMap<String, Type>,
) -> bool {
    let expected = substitute_type(expected, subst);

    if let Type::Named { path, generics } = found {
        if path.len() == 1 && impl_generics.contains(&path[0]) {
            if !generics.is_empty() {
                return false;
            }
            let name = path[0].clone();
            if let Some(existing) = impl_subst.get(&name) {
                return existing == &expected;
            }
            impl_subst.insert(name, expected.clone());
            return true;
        }
    }

    match (&expected, found) {
        (Type::Unknown, _) => true,
        (Type::Primitive(_), _) | (Type::Unit, _) => &expected == found,
        (
            Type::Named { path, generics },
            Type::Named {
                path: found_path,
                generics: found_generics,
            },
        ) => {
            if path != found_path || generics.len() != found_generics.len() {
                return false;
            }
            for (exp, got) in generics.iter().zip(found_generics.iter()) {
                if !unify_type(exp, got, subst, impl_generics, impl_subst) {
                    return false;
                }
            }
            true
        }
        (
            Type::Reference { is_mutable, inner },
            Type::Reference {
                is_mutable: found_mut,
                inner: found_inner,
            },
        ) => {
            is_mutable == found_mut
                && unify_type(inner, found_inner, subst, impl_generics, impl_subst)
        }
        (
            Type::Pointer { is_mutable, inner },
            Type::Pointer {
                is_mutable: found_mut,
                inner: found_inner,
            },
        ) => {
            is_mutable == found_mut
                && unify_type(inner, found_inner, subst, impl_generics, impl_subst)
        }
        (
            Type::Array { element, length },
            Type::Array {
                element: found_elem,
                length: found_len,
            },
        ) => {
            if length != found_len {
                return false;
            }
            unify_type(element, found_elem, subst, impl_generics, impl_subst)
        }
        (Type::Optional { inner }, Type::Optional { inner: found_inner }) => {
            unify_type(inner, found_inner, subst, impl_generics, impl_subst)
        }
        (Type::Tuple(items), Type::Tuple(found_items)) => {
            if items.len() != found_items.len() {
                return false;
            }
            for (exp, got) in items.iter().zip(found_items.iter()) {
                if !unify_type(exp, got, subst, impl_generics, impl_subst) {
                    return false;
                }
            }
            true
        }
        (
            Type::Function {
                params,
                return_type,
            },
            Type::Function {
                params: found_params,
                return_type: found_return,
            },
        ) => {
            if params.len() != found_params.len() {
                return false;
            }
            for (exp, got) in params.iter().zip(found_params.iter()) {
                if !unify_type(exp, got, subst, impl_generics, impl_subst) {
                    return false;
                }
            }
            unify_type(return_type, found_return, subst, impl_generics, impl_subst)
        }
        _ => false,
    }
}

fn contains_impl_generic(ty: &Type, impl_generics: &HashSet<String>) -> bool {
    match ty {
        Type::Named { path, generics } => {
            if path.len() == 1 && impl_generics.contains(&path[0]) {
                return true;
            }
            generics
                .iter()
                .any(|inner| contains_impl_generic(inner, impl_generics))
        }
        Type::Reference { inner, .. }
        | Type::Pointer { inner, .. }
        | Type::Array { element: inner, .. }
        | Type::Optional { inner } => contains_impl_generic(inner, impl_generics),
        Type::Tuple(items) => items
            .iter()
            .any(|inner| contains_impl_generic(inner, impl_generics)),
        Type::Function {
            params,
            return_type,
        } => {
            params
                .iter()
                .any(|inner| contains_impl_generic(inner, impl_generics))
                || contains_impl_generic(return_type, impl_generics)
        }
        Type::Primitive(_) | Type::Unit | Type::Unknown => false,
    }
}

impl TraitRegistry {
    fn has_impl(&self, trait_name: &str, self_key: &str, program: &ast::Program) -> bool {
        for item in &program.items {
            let ast::ItemKind::Impl(impl_item) = &item.kind else {
                continue;
            };
            let Some(trait_ref) = &impl_item.trait_ref else {
                continue;
            };
            let name = trait_name_from_ref(trait_ref);
            if name != trait_name {
                continue;
            }
            let impl_key = type_key(&Type::from_ast(&impl_item.self_type));
            if impl_key == self_key {
                return true;
            }
        }
        false
    }

    fn is_local_type(&self, ty: &ast::Type) -> bool {
        match ty.kind.as_ref() {
            ast::TypeKind::Named(named) => {
                if let Some(first) = named.path.first() {
                    self.local_types.contains_key(&first.name)
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

fn is_foreign_trait(trait_ref: &ast::TraitRef) -> bool {
    trait_ref.path.len() > 1
}

fn trait_path_key(path: &[ast::Identifier]) -> String {
    let mut key = String::new();
    for (idx, part) in path.iter().enumerate() {
        if idx > 0 {
            key.push_str("::");
        }
        key.push_str(&part.name);
    }
    key
}

pub fn validate_traits(program: &ast::Program) -> Vec<TraitError> {
    let (registry, mut errors) = TraitRegistry::collect(program);
    errors.extend(registry.validate_impls(program));
    errors
}

pub fn validate_traits_with_imports(
    program: &ast::Program,
    imported_traits: &HashSet<String>,
) -> Vec<TraitError> {
    let (mut registry, mut errors) = TraitRegistry::collect(program);
    registry.trait_names.extend(imported_traits.iter().cloned());
    errors.extend(registry.validate_impls(program));
    errors
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;
    use crate::parser::Parser;

    fn parse(source: &str) -> ast::Program {
        let tokens = lex(source).expect("lex failed");
        let mut parser = Parser::new(tokens);
        let (program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {errors:?}");
        program
    }

    #[test]
    fn trait_impl_missing_method_is_error() {
        let program =
            parse("trait Add { fn add(i32 self, i32 other) -> i32; } impl Add for i32 { }");
        let errors = validate_traits(&program);
        assert!(!errors.is_empty(), "expected trait errors");
    }

    #[test]
    fn trait_impl_missing_assoc_type_is_error() {
        let program = parse("trait Iter { type Item; } impl Iter for i32 { }");
        let errors = validate_traits(&program);
        assert!(!errors.is_empty(), "expected trait errors");
    }

    #[test]
    fn trait_impl_with_defaults_is_ok() {
        let program = parse(
            "trait Foo { type Item = i32; fn bar(i32 self) -> i32 { return self; } } impl Foo for i32 { }",
        );
        let errors = validate_traits(&program);
        assert!(errors.is_empty(), "unexpected trait errors: {errors:?}");
    }

    #[test]
    fn trait_super_requires_impl() {
        let program = parse(
            "trait Base { fn base(i32 self) -> i32; } trait Derived: Base { fn derived(i32 self) -> i32; } impl Derived for i32 { i32 derived(i32 self) { return self; } }",
        );
        let errors = validate_traits(&program);
        assert!(!errors.is_empty(), "expected trait errors");
    }

    #[test]
    fn trait_super_impl_ok() {
        let program = parse(
            "trait Base { fn base(i32 self) -> i32; } trait Derived: Base { fn derived(i32 self) -> i32; } impl Base for i32 { i32 base(i32 self) { return self; } } impl Derived for i32 { i32 derived(i32 self) { return self; } }",
        );
        let errors = validate_traits(&program);
        assert!(errors.is_empty(), "unexpected trait errors: {errors:?}");
    }

    #[test]
    fn unknown_super_trait_is_error() {
        let program = parse("trait Derived: Missing { fn derived(i32 self) -> i32; }");
        let errors = validate_traits(&program);
        assert!(!errors.is_empty(), "expected trait errors");
    }

    #[test]
    fn conflicting_impl_is_error() {
        let program = parse(
            "trait Foo { fn foo(i32 self) -> i32; } impl Foo for i32 { i32 foo(i32 self) { return self; } } impl Foo for i32 { i32 foo(i32 self) { return self; } }",
        );
        let errors = validate_traits(&program);
        assert!(!errors.is_empty(), "expected trait errors");
    }

    #[test]
    fn orphan_rule_rejects_foreign_trait_for_foreign_type() {
        let program = parse(
            "trait Local { fn foo(i32 self) -> i32; } impl std::fmt::Display for i32 { i32 foo(i32 self) { return self; } }",
        );
        let errors = validate_traits(&program);
        assert!(!errors.is_empty(), "expected trait errors");
    }

    #[test]
    fn trait_generic_count_mismatch_is_error() {
        let program = parse(
            "trait Wrap<T, U> { fn wrap(T self, U other) -> T; } impl Wrap<i32> for i32 { i32 wrap(i32 self, i32 other) { return self; } }",
        );
        let errors = validate_traits(&program);
        assert!(!errors.is_empty(), "expected trait errors");
    }

    #[test]
    fn trait_generic_matching_substitutes_types() {
        let program = parse(
            "trait Wrap<T> { fn wrap(T self) -> T; } impl Wrap<i32> for i32 { i32 wrap(i32 self) { return self; } }",
        );
        let errors = validate_traits(&program);
        assert!(errors.is_empty(), "unexpected trait errors: {errors:?}");
    }

    #[test]
    fn trait_generic_mismatch_is_error() {
        let program = parse(
            "trait Wrap<T> { fn wrap(T self) -> T; } impl Wrap<i32> for i32 { i64 wrap(i64 self) { return self; } }",
        );
        let errors = validate_traits(&program);
        assert!(!errors.is_empty(), "expected trait errors");
    }

    #[test]
    fn trait_generic_impl_with_type_param_is_ok() {
        let program = parse(
            "trait Wrap<T> { fn wrap(T self) -> T; } impl<T> Wrap<T> for T { T wrap(T self) { return self; } }",
        );
        let errors = validate_traits(&program);
        assert!(errors.is_empty(), "unexpected trait errors: {errors:?}");
    }

    #[test]
    fn where_bound_requires_impl_for_concrete_type() {
        let program = parse(
            "trait Bar { fn bar(i32 self) -> i32; } trait Foo { fn foo(i32 self) -> i32; } impl<T> where i32: Bar Foo for i32 { i32 foo(i32 self) { return self; } }",
        );
        let errors = validate_traits(&program);
        assert!(!errors.is_empty(), "expected trait errors");
    }

    #[test]
    fn where_bound_satisfied_for_concrete_type() {
        let program = parse(
            "trait Bar { fn bar(i32 self) -> i32; } trait Foo { fn foo(i32 self) -> i32; } impl Bar for i32 { i32 bar(i32 self) { return self; } } impl<T> where i32: Bar Foo for i32 { i32 foo(i32 self) { return self; } }",
        );
        let errors = validate_traits(&program);
        assert!(errors.is_empty(), "unexpected trait errors: {errors:?}");
    }

    #[test]
    fn trait_param_bound_requires_impl_for_concrete_arg() {
        let program = parse(
            "trait Bar { fn bar(i32 self) -> i32; } trait Foo<T: Bar> { fn foo(T self) -> T; } impl Foo<i32> for i32 { i32 foo(i32 self) { return self; } }",
        );
        let errors = validate_traits(&program);
        assert!(!errors.is_empty(), "expected trait errors");
    }

    #[test]
    fn trait_param_bound_satisfied_for_concrete_arg() {
        let program = parse(
            "trait Bar { fn bar(i32 self) -> i32; } trait Foo<T: Bar> { fn foo(T self) -> T; } impl Bar for i32 { i32 bar(i32 self) { return self; } } impl Foo<i32> for i32 { i32 foo(i32 self) { return self; } }",
        );
        let errors = validate_traits(&program);
        assert!(errors.is_empty(), "unexpected trait errors: {errors:?}");
    }

    #[test]
    fn trait_param_bound_skips_generic_arg() {
        let program = parse(
            "trait Bar { fn bar(i32 self) -> i32; } trait Foo<T: Bar> { fn foo(T self) -> T; } impl<T> Foo<T> for T { T foo(T self) { return self; } }",
        );
        let errors = validate_traits(&program);
        assert!(errors.is_empty(), "unexpected trait errors: {errors:?}");
    }
}
