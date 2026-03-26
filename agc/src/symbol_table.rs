use std::collections::{HashMap, HashSet};

use crate::lexer::Span;
use crate::parser::ast;

pub type SymbolId = u64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolKind {
    Function,
    GlobalVariable,
    ExternFunction,
    ExternVariable,
    Struct,
    Enum,
    Trait,
    Impl,
    TraitMethod,
    ImplMethod,
    Field,
    Parameter,
    Import,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompilerPhase {
    Parse,
    SemanticAnalyze,
    TypeCheck,
    Monomorphize,
    Codegen,
}

#[derive(Debug, Clone)]
pub struct SymbolEntry {
    pub kinds: HashSet<SymbolKind>,
    pub first_span: Option<Span>,
    pub touched_in: HashSet<CompilerPhase>,
    pub touches: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarRecord {
    pub name: String,
    pub mutated: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
struct VarInfo {
    span: Span,
    mutated: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindVarResult {
    Bound,
    DuplicateInScope,
    MissingScope,
}

#[derive(Debug, Default, Clone)]
pub struct CompilerSymbolTable {
    entries: HashMap<String, SymbolEntry>,
    key_to_id: HashMap<String, SymbolId>,
    id_to_key: HashMap<SymbolId, String>,
    next_symbol_id: SymbolId,
    events: Vec<String>,
    var_scopes: Vec<HashMap<String, VarInfo>>,
    var_records: Vec<VarRecord>,
}

impl CompilerSymbolTable {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn touch_phase(&mut self, phase: CompilerPhase, message: impl Into<String>) {
        self.events.push(format!("{phase:?}: {}", message.into()));
    }

    pub fn upsert(
        &mut self,
        key: impl Into<String>,
        kind: SymbolKind,
        span: Option<Span>,
        phase: CompilerPhase,
    ) {
        let key = key.into();
        let entry = self
            .entries
            .entry(key.clone())
            .or_insert_with(|| SymbolEntry {
                kinds: HashSet::new(),
                first_span: span.clone(),
                touched_in: HashSet::new(),
                touches: 0,
            });

        if entry.first_span.is_none() {
            entry.first_span = span;
        }
        entry.kinds.insert(kind);
        entry.touched_in.insert(phase);
        entry.touches += 1;

        self.key_to_id.entry(key.clone()).or_insert_with(|| {
            let id = self.next_symbol_id;
            self.next_symbol_id += 1;
            self.id_to_key.insert(id, key);
            id
        });
    }

    pub fn intern_symbol(
        &mut self,
        key: impl Into<String>,
        kind: SymbolKind,
        span: Option<Span>,
        phase: CompilerPhase,
    ) -> SymbolId {
        let key = key.into();
        self.upsert(key.clone(), kind, span, phase);
        self.key_to_id[&key]
    }

    pub fn symbol_id(&self, key: &str) -> Option<SymbolId> {
        self.key_to_id.get(key).copied()
    }

    pub fn symbol_key(&self, id: SymbolId) -> Option<&str> {
        self.id_to_key.get(&id).map(String::as_str)
    }

    pub fn record_program_symbols(&mut self, program: &ast::Program, phase: CompilerPhase) {
        for item in &program.items {
            self.record_item_symbols(item, phase);
        }
    }

    pub fn summary_line(&self) -> String {
        format!(
            "symbols={} events={}",
            self.entries.len(),
            self.events.len()
        )
    }

    pub fn absorb_from(&mut self, other: &CompilerSymbolTable) {
        for (key, incoming) in &other.entries {
            let entry = self
                .entries
                .entry(key.clone())
                .or_insert_with(|| SymbolEntry {
                    kinds: HashSet::new(),
                    first_span: incoming.first_span.clone(),
                    touched_in: HashSet::new(),
                    touches: 0,
                });
            entry.kinds.extend(incoming.kinds.iter().copied());
            if entry.first_span.is_none() {
                entry.first_span = incoming.first_span.clone();
            }
            entry.touched_in.extend(incoming.touched_in.iter().copied());
            entry.touches += incoming.touches;
        }
        self.events.extend(other.events.iter().cloned());
        self.var_records.extend(other.var_records.iter().cloned());

        for (key, id) in &other.key_to_id {
            let _ = id;
            self.key_to_id.entry(key.clone()).or_insert_with(|| {
                let fresh = self.next_symbol_id;
                self.next_symbol_id += 1;
                self.id_to_key.insert(fresh, key.clone());
                fresh
            });
        }
    }

    pub fn push_var_scope(&mut self) {
        self.var_scopes.push(HashMap::new());
    }

    pub fn pop_var_scope(&mut self) {
        if let Some(scope) = self.var_scopes.pop() {
            for (name, info) in scope {
                self.var_records.push(VarRecord {
                    name,
                    mutated: info.mutated,
                    span: info.span,
                });
            }
        }
    }

    pub fn bind_var(&mut self, name: &str, span: Span) -> BindVarResult {
        let Some(scope) = self.var_scopes.last_mut() else {
            return BindVarResult::MissingScope;
        };
        if scope.contains_key(name) {
            return BindVarResult::DuplicateInScope;
        }
        scope.insert(
            name.to_string(),
            VarInfo {
                span,
                mutated: false,
            },
        );
        BindVarResult::Bound
    }

    pub fn resolve_var(&self, name: &str) -> bool {
        self.var_scopes
            .iter()
            .rev()
            .any(|scope| scope.contains_key(name))
    }

    pub fn mark_var_mutated(&mut self, name: &str) -> bool {
        for scope in self.var_scopes.iter_mut().rev() {
            if let Some(info) = scope.get_mut(name) {
                info.mutated = true;
                return true;
            }
        }
        false
    }

    pub fn var_records(&self) -> &[VarRecord] {
        &self.var_records
    }

    fn record_item_symbols(&mut self, item: &ast::Item, phase: CompilerPhase) {
        match &item.kind {
            ast::ItemKind::Function(func) => {
                self.upsert(
                    &func.name.name,
                    SymbolKind::Function,
                    Some(func.name.span.clone()),
                    phase,
                );
                for param in &func.parameters {
                    self.upsert(
                        format!("{}::{}", func.name.name, param.name.name),
                        SymbolKind::Parameter,
                        Some(param.span.clone()),
                        phase,
                    );
                }
            }
            ast::ItemKind::GlobalVariable(var) => {
                self.upsert(
                    &var.name.name,
                    SymbolKind::GlobalVariable,
                    Some(var.name.span.clone()),
                    phase,
                );
            }
            ast::ItemKind::Struct(struct_item) => {
                self.upsert(
                    &struct_item.name.name,
                    SymbolKind::Struct,
                    Some(struct_item.name.span.clone()),
                    phase,
                );
                for field in &struct_item.fields {
                    self.upsert(
                        format!("{}::{}", struct_item.name.name, field.name.name),
                        SymbolKind::Field,
                        Some(field.span.clone()),
                        phase,
                    );
                }
            }
            ast::ItemKind::Enum(enum_item) => {
                self.upsert(
                    &enum_item.name.name,
                    SymbolKind::Enum,
                    Some(enum_item.name.span.clone()),
                    phase,
                );
            }
            ast::ItemKind::Trait(trait_item) => {
                self.upsert(
                    &trait_item.name.name,
                    SymbolKind::Trait,
                    Some(trait_item.name.span.clone()),
                    phase,
                );
                for member in &trait_item.items {
                    if let ast::TraitItemKind::Function(func) = member {
                        self.upsert(
                            format!("{}::{}", trait_item.name.name, func.name.name),
                            SymbolKind::TraitMethod,
                            Some(func.name.span.clone()),
                            phase,
                        );
                    }
                }
            }
            ast::ItemKind::Impl(impl_item) => {
                let owner = impl_item
                    .self_type
                    .kind
                    .as_ref()
                    .named_type_name()
                    .unwrap_or_else(|| "<anon>".to_string());
                self.upsert(
                    format!("impl:{owner}"),
                    SymbolKind::Impl,
                    Some(item.span.clone()),
                    phase,
                );
                for member in &impl_item.items {
                    if let ast::ImplItemKind::Function(func) = member {
                        self.upsert(
                            format!("{owner}::{}", func.name.name),
                            SymbolKind::ImplMethod,
                            Some(func.name.span.clone()),
                            phase,
                        );
                    }
                }
            }
            ast::ItemKind::Import(import_item) => {
                let path = import_item
                    .path
                    .iter()
                    .map(|segment| segment.name.as_str())
                    .collect::<Vec<_>>()
                    .join("::");
                self.upsert(path, SymbolKind::Import, Some(item.span.clone()), phase);
            }
            ast::ItemKind::ExternFunction(func) => {
                self.upsert(
                    &func.name.name,
                    SymbolKind::ExternFunction,
                    Some(func.name.span.clone()),
                    phase,
                );
            }
            ast::ItemKind::ExternVariable(var) => {
                self.upsert(
                    &var.name.name,
                    SymbolKind::ExternVariable,
                    Some(var.name.span.clone()),
                    phase,
                );
            }
            ast::ItemKind::ExternBlock(block) => {
                for func in &block.functions {
                    self.upsert(
                        &func.name.name,
                        SymbolKind::ExternFunction,
                        Some(func.name.span.clone()),
                        phase,
                    );
                }
                for var in &block.variables {
                    self.upsert(
                        &var.name.name,
                        SymbolKind::ExternVariable,
                        Some(var.name.span.clone()),
                        phase,
                    );
                }
            }
        }
    }
}

trait TypeKindExt {
    fn named_type_name(&self) -> Option<String>;
}

impl TypeKindExt for ast::TypeKind {
    fn named_type_name(&self) -> Option<String> {
        if let ast::TypeKind::Named(named) = self {
            return named.path.last().map(|ident| ident.name.clone());
        }
        None
    }
}
