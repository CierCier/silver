use super::{TypeChecker, TypeError};
use crate::diagnostics::messages as msg;
use crate::lexer::Span;
use crate::module_artifact::{ExportKind, ModuleAbi};
use crate::parser::ast;
use crate::types::Type;
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Default)]
pub(super) struct CallbackAbiState {
    unsafe_values: FxHashSet<String>,
    aliases: FxHashMap<String, ast::Type>,
}

// Narrow integers need target-specific C extension attributes that function
// pointers do not retain. Aggregate and enum callbacks need ABI-aware types.
fn scalar_is_supported(ty: &Type) -> bool {
    match ty {
        Type::Unit => true,
        Type::Primitive(primitive) => matches!(
            primitive,
            ast::PrimitiveType::Void
                | ast::PrimitiveType::I32
                | ast::PrimitiveType::I64
                | ast::PrimitiveType::U32
                | ast::PrimitiveType::U64
                | ast::PrimitiveType::F32
                | ast::PrimitiveType::F64
                | ast::PrimitiveType::Char
                | ast::PrimitiveType::Str
        ),
        Type::Pointer { .. } | Type::Reference { .. } => true,
        Type::Function {
            params,
            return_type,
        } => scalar_is_supported(return_type) && params.iter().all(parameter_is_supported),
        _ => false,
    }
}

fn parameter_is_supported(ty: &Type) -> bool {
    !matches!(ty, Type::Unit | Type::Primitive(ast::PrimitiveType::Void)) && scalar_is_supported(ty)
}

impl TypeChecker {
    fn c_boundary_has_unsupported_callback(&self, ty: &Type) -> bool {
        self.callback_in_aggregate(ty, &mut FxHashSet::default())
    }

    fn callback_in_aggregate(&self, ty: &Type, visiting: &mut FxHashSet<String>) -> bool {
        let ty = Self::expand_imported_type_aliases(ty.clone(), &self.callback_abi.aliases);
        let key = ty.canonical_key();
        if visiting.contains(&key) {
            return false;
        }
        if visiting.len() >= 64 {
            return true;
        }
        visiting.insert(key.clone());
        let unsupported = match &ty {
            Type::Function {
                params,
                return_type,
            } => {
                !scalar_is_supported(&ty)
                    || self.callback_in_aggregate(return_type, visiting)
                    || params
                        .iter()
                        .any(|ty| self.callback_in_aggregate(ty, visiting))
            }
            Type::Pointer { inner, .. }
            | Type::Reference { inner, .. }
            | Type::Optional { inner }
            | Type::Array { element: inner, .. }
            | Type::Slice { element: inner } => self.callback_in_aggregate(inner, visiting),
            Type::Tuple(items) => items
                .iter()
                .any(|ty| self.callback_in_aggregate(ty, visiting)),
            Type::Named { path, generics } => {
                let name = path.last();
                if let Some(definition) = name.and_then(|name| self.struct_defs.get(name)) {
                    let mapping = definition
                        .type_params
                        .iter()
                        .cloned()
                        .zip(generics.iter().cloned())
                        .collect();
                    definition.fields.values().any(|field| {
                        self.callback_in_aggregate(&self.substitute_type(field, &mapping), visiting)
                    })
                } else if let Some(definition) = name.and_then(|name| self.enum_defs.get(name)) {
                    let mapping = definition
                        .type_params
                        .iter()
                        .cloned()
                        .zip(generics.iter().cloned())
                        .collect();
                    definition.variants.values().any(|variant| {
                        variant.payload.iter().any(|field| {
                            self.callback_in_aggregate(
                                &self.substitute_type(&Type::from_ast(field), &mapping),
                                visiting,
                            )
                        })
                    })
                } else {
                    false
                }
            }
            _ => false,
        };
        visiting.remove(&key);
        unsupported
    }

    pub(super) fn check_c_callback_contracts(&mut self, program: &ast::Program) {
        self.callback_abi.unsafe_values.clear();
        self.callback_abi.aliases = self.type_aliases.clone();
        self.callback_abi
            .aliases
            .extend(Self::local_type_aliases(program));
        for item in &program.items {
            match &item.kind {
                ast::ItemKind::ExternFunction(function) => {
                    self.check_c_callback_declaration(function)
                }
                ast::ItemKind::ExternVariable(variable) => self.check_c_callback_variable(variable),
                ast::ItemKind::ExternBlock(block) => {
                    for function in &block.functions {
                        self.check_c_callback_declaration(function);
                    }
                    for variable in &block.variables {
                        self.check_c_callback_variable(variable);
                    }
                }
                _ => {}
            }
        }
        for module in &self.imported_modules {
            for export in &module.exports {
                if export.abi != Some(ModuleAbi::C) {
                    continue;
                }
                if export.kind == ExportKind::Global {
                    if let Ok(ty) = Type::from_canonical_key(&export.signature) {
                        let ty = Self::expand_imported_type_aliases(ty, &self.callback_abi.aliases);
                        if self.c_boundary_has_unsupported_callback(&ty) {
                            self.errors.push(TypeError {
                                message: msg::c_callback_unsupported().to_string(),
                                span: Span::default(),
                            });
                        }
                    }
                    continue;
                }
                if export.kind != ExportKind::Function {
                    continue;
                }
                let Ok((params, result)) =
                    crate::types::parse_canonical_function_signature(&export.signature)
                else {
                    continue;
                };
                let params = params
                    .into_iter()
                    .map(|ty| Self::expand_imported_type_aliases(ty, &self.callback_abi.aliases))
                    .collect::<Vec<_>>();
                let result = Self::expand_imported_type_aliases(result, &self.callback_abi.aliases);
                if export.is_variadic
                    || !scalar_is_supported(&result)
                    || !params.iter().all(parameter_is_supported)
                {
                    self.callback_abi.unsafe_values.insert(export.name.clone());
                }
                if self.c_boundary_has_unsupported_callback(&result)
                    || params
                        .iter()
                        .any(|ty| self.c_boundary_has_unsupported_callback(ty))
                {
                    self.errors.push(TypeError {
                        message: msg::c_callback_unsupported().to_string(),
                        span: Span::default(),
                    });
                }
            }
        }
    }

    fn check_c_callback_declaration(&mut self, function: &ast::ExternFunctionItem) {
        if function.linkage != ast::ExternLinkage::C {
            return;
        }
        let params = function
            .signature
            .parameters
            .iter()
            .map(|parameter| {
                let ty = self.type_from_ast(&parameter.param_type);
                Self::expand_imported_type_aliases(ty, &self.callback_abi.aliases)
            })
            .collect::<Vec<_>>();
        let result = function
            .signature
            .return_type
            .as_ref()
            .map(|ty| {
                let ty = self.type_from_ast(ty);
                Self::expand_imported_type_aliases(ty, &self.callback_abi.aliases)
            })
            .unwrap_or(Type::Unit);
        if function.signature.is_variadic
            || !scalar_is_supported(&result)
            || !params.iter().all(parameter_is_supported)
        {
            self.callback_abi
                .unsafe_values
                .insert(function.name.name.clone());
        }
        if self.c_boundary_has_unsupported_callback(&result)
            || params
                .iter()
                .any(|ty| self.c_boundary_has_unsupported_callback(ty))
        {
            self.error(msg::c_callback_unsupported(), function.name.span);
        }
    }

    fn check_c_callback_variable(&mut self, variable: &ast::ExternVariableItem) {
        if variable.linkage != ast::ExternLinkage::C {
            return;
        }
        let ty = self.type_from_ast(&variable.var_type);
        let ty = Self::expand_imported_type_aliases(ty, &self.callback_abi.aliases);
        if self.c_boundary_has_unsupported_callback(&ty) {
            self.error(msg::c_callback_unsupported(), variable.name.span);
        }
    }

    pub(super) fn check_c_function_value(&mut self, name: &str, span: Span) {
        if self.callback_abi.unsafe_values.contains(name) {
            self.error(msg::c_function_value_unsupported(), span);
        }
    }
}
