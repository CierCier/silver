use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::lexer;
use crate::module_artifact::ModuleArtifact;
use crate::module_loader::{
    filter_exports, import_path_to_string, validate_import_conflicts, ModuleLoader,
    ResolvedSourceImportKind,
};
use crate::parser::ast;
use crate::parser::Parser;

#[derive(Debug, Default)]
pub struct ImportLoweringResult {
    pub module_artifacts: Vec<ModuleArtifact>,
    pub module_dependencies: Vec<String>,
}

pub struct FileImportResolverHook<'a> {
    loader: &'a ModuleLoader,
    seen_modules: HashSet<String>,
    seen_files: HashSet<PathBuf>,
    module_imports: Vec<(String, ModuleArtifact)>,
}

#[derive(Debug, Default)]
struct ImportAliasPlan {
    direct: HashMap<String, String>,
    namespaces: HashMap<String, HashSet<String>>,
}

impl ImportAliasPlan {
    fn add_direct(&mut self, alias: &ast::Identifier, target: &str) -> Result<(), String> {
        match self.direct.get(&alias.name) {
            Some(existing) if existing != target => Err(format!(
                "import alias `{}` is ambiguous between `{}` and `{}`",
                alias.name, existing, target
            )),
            Some(_) => Ok(()),
            None => {
                self.direct.insert(alias.name.clone(), target.to_string());
                Ok(())
            }
        }
    }

    fn add_namespace(
        &mut self,
        alias: &ast::Identifier,
        names: impl IntoIterator<Item = String>,
    ) -> Result<(), String> {
        let entry = self.namespaces.entry(alias.name.clone()).or_default();
        for name in names {
            entry.insert(name);
        }
        if self.direct.contains_key(&alias.name) {
            return Err(format!(
                "import alias `{}` is used both as a symbol and namespace",
                alias.name
            ));
        }
        Ok(())
    }
}

impl<'a> FileImportResolverHook<'a> {
    pub fn new(loader: &'a ModuleLoader) -> Self {
        Self {
            loader,
            seen_modules: HashSet::new(),
            seen_files: HashSet::new(),
            module_imports: Vec::new(),
        }
    }

    pub fn lower_program_imports(
        mut self,
        program: &mut ast::Program,
        base_dir: Option<&Path>,
        root_source: Option<&Path>,
    ) -> Result<ImportLoweringResult, String> {
        if let Some(root) = root_source {
            let _ = self.mark_file_seen(root);
        }

        self.lower_program_recursive(program, base_dir)?;
        validate_import_conflicts(
            self.module_imports
                .iter()
                .map(|(module_path, module)| (module_path.as_str(), module)),
        )?;

        Ok(ImportLoweringResult {
            module_dependencies: self
                .module_imports
                .iter()
                .map(|(module_path, _)| module_path.clone())
                .collect(),
            module_artifacts: self.module_imports.into_iter().map(|(_, m)| m).collect(),
        })
    }

    fn lower_program_recursive(
        &mut self,
        program: &mut ast::Program,
        base_dir: Option<&Path>,
    ) -> Result<(), String> {
        let mut lowered_items = Vec::new();
        let mut lowered_program_attributes = std::mem::take(&mut program.attributes);
        let mut alias_plan = ImportAliasPlan::default();

        for item in std::mem::take(&mut program.items) {
            let ast::Item {
                kind,
                span,
                visibility,
                attributes,
            } = item;

            let ast::ItemKind::Import(import_item) = kind else {
                lowered_items.push(ast::Item {
                    kind,
                    span,
                    visibility,
                    attributes,
                });
                continue;
            };

            let module_path = import_path_to_string(&import_item.path);
            if !self.seen_modules.insert(module_path.clone()) {
                continue;
            }

            let resolved = self
                .loader
                .find_source_import(&import_item.path, base_dir)
                .ok_or_else(|| format!("import `{module_path}` could not be resolved"))?;

            match resolved.kind {
                ResolvedSourceImportKind::File => {
                    if !self.mark_file_seen(&resolved.source_path) {
                        continue;
                    }
                    let mut imported_program = parse_program_from_file(&resolved.source_path)?;
                    self.lower_program_recursive(
                        &mut imported_program,
                        resolved.source_path.parent(),
                    )?;
                    let imported_names = public_importable_item_names(&imported_program);
                    record_import_aliases(&mut alias_plan, &import_item, &imported_names)?;
                    let imported_program = filter_program_items(imported_program, &import_item)?;
                    lowered_program_attributes.extend(imported_program.attributes);
                    lowered_items.extend(imported_program.items);
                }
                ResolvedSourceImportKind::Module => {
                    let mut artifact = ModuleArtifact::from_path(&resolved.source_path)?;
                    let filtered_exports = filter_exports(&artifact, &import_item);
                    if let Some(items) = &import_item.items {
                        for item in items {
                            if !filtered_exports
                                .iter()
                                .any(|export| export.name == item.name.name)
                            {
                                return Err(format!(
                                    "import `{}` does not export `{}`",
                                    resolved.module_path, item.name.name
                                ));
                            }
                        }
                    }
                    let exported_names = filtered_exports
                        .iter()
                        .map(|export| export.name.clone())
                        .collect::<Vec<_>>();
                    record_import_aliases(&mut alias_plan, &import_item, &exported_names)?;
                    artifact.exports = filtered_exports;
                    self.module_imports
                        .push((resolved.module_path.clone(), artifact));
                }
            }
        }

        rewrite_import_aliases_in_items(&mut lowered_items, &alias_plan);
        program.items = lowered_items;
        program.attributes = lowered_program_attributes;
        Ok(())
    }

    fn mark_file_seen(&mut self, path: &Path) -> bool {
        let stable = std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf());
        self.seen_files.insert(stable)
    }
}

fn parse_program_from_file(path: &Path) -> Result<ast::Program, String> {
    let src = std::fs::read_to_string(path)
        .map_err(|e| format!("failed to read {}: {e}", path.display()))?;
    let tokens =
        lexer::lex(&src).map_err(|e| format!("lexer errors in {}: {e:?}", path.display()))?;
    let mut parser = Parser::new_with_source(tokens, path.display().to_string());
    let (program, errors) = parser.parse_program();
    if errors.is_empty() {
        return Ok(program);
    }
    Err(format!(
        "parser errors in {}: {:?}",
        path.display(),
        errors[0].format_with_help()
    ))
}

fn public_importable_item_names(program: &ast::Program) -> Vec<String> {
    program
        .items
        .iter()
        .filter(|item| !matches!(item.visibility, ast::Visibility::Private))
        .filter_map(item_name)
        .collect()
}

fn item_name(item: &ast::Item) -> Option<String> {
    match &item.kind {
        ast::ItemKind::Function(func) => Some(func.name.name.clone()),
        ast::ItemKind::ExternFunction(func) => Some(func.name.name.clone()),
        ast::ItemKind::Struct(item) => Some(item.name.name.clone()),
        ast::ItemKind::Enum(item) => Some(item.name.name.clone()),
        ast::ItemKind::Trait(item) => Some(item.name.name.clone()),
        ast::ItemKind::GlobalVariable(item) => Some(item.name.name.clone()),
        _ => None,
    }
}

fn filter_program_items(
    mut program: ast::Program,
    import_item: &ast::ImportItem,
) -> Result<ast::Program, String> {
    let Some(items) = &import_item.items else {
        program
            .items
            .retain(|item| !matches!(item.visibility, ast::Visibility::Private));
        return Ok(program);
    };

    let selected = items
        .iter()
        .map(|item| item.name.name.clone())
        .collect::<HashSet<_>>();
    for requested in &selected {
        if !program
            .items
            .iter()
            .any(|item| item_name(item).as_deref() == Some(requested))
        {
            return Err(format!(
                "import `{}` does not export `{requested}`",
                import_path_to_string(&import_item.path)
            ));
        }
    }

    program.items.retain(|item| {
        !matches!(item.visibility, ast::Visibility::Private)
            && item_name(item)
                .map(|name| selected.contains(&name))
                .unwrap_or(false)
    });
    Ok(program)
}

fn record_import_aliases(
    plan: &mut ImportAliasPlan,
    import_item: &ast::ImportItem,
    imported_names: &[String],
) -> Result<(), String> {
    if let Some(alias) = &import_item.alias {
        plan.add_namespace(alias, imported_names.iter().cloned())?;
    }
    if let Some(items) = &import_item.items {
        for item in items {
            if let Some(alias) = &item.alias {
                plan.add_direct(alias, &item.name.name)?;
            }
        }
    }
    Ok(())
}

fn rewrite_import_aliases_in_items(items: &mut [ast::Item], plan: &ImportAliasPlan) {
    let mut rewriter = ImportAliasRewriter::new(plan);
    for item in items {
        rewriter.rewrite_item(item);
    }
}

struct ImportAliasRewriter<'a> {
    plan: &'a ImportAliasPlan,
    value_scopes: Vec<HashSet<String>>,
}

impl<'a> ImportAliasRewriter<'a> {
    fn new(plan: &'a ImportAliasPlan) -> Self {
        Self {
            plan,
            value_scopes: vec![HashSet::new()],
        }
    }

    fn rewrite_item(&mut self, item: &mut ast::Item) {
        match &mut item.kind {
            ast::ItemKind::Function(func) => self.rewrite_function(func),
            ast::ItemKind::ExternFunction(func) => {
                self.rewrite_function_signature(&mut func.signature)
            }
            ast::ItemKind::Struct(item) => {
                if let Some(generics) = &mut item.generics {
                    self.rewrite_generics(generics);
                }
                for field in &mut item.fields {
                    self.rewrite_type(&mut field.field_type);
                }
            }
            ast::ItemKind::Enum(item) => {
                if let Some(generics) = &mut item.generics {
                    self.rewrite_generics(generics);
                }
                for variant in &mut item.variants {
                    match &mut variant.data {
                        ast::EnumVariantData::Tuple(types) => {
                            for ty in types {
                                self.rewrite_type(ty);
                            }
                        }
                        ast::EnumVariantData::Struct(fields) => {
                            for field in fields {
                                self.rewrite_type(&mut field.field_type);
                            }
                        }
                        ast::EnumVariantData::Unit => {}
                    }
                }
            }
            ast::ItemKind::Trait(item) => {
                if let Some(generics) = &mut item.generics {
                    self.rewrite_generics(generics);
                }
                for trait_item in &mut item.items {
                    match trait_item {
                        ast::TraitItemKind::Function(func) => {
                            self.rewrite_function_signature_like(
                                &mut func.parameters,
                                &mut func.return_type,
                            );
                        }
                        ast::TraitItemKind::AssociatedType(_) => {}
                    }
                }
            }
            ast::ItemKind::Impl(item) => {
                self.rewrite_type(&mut item.self_type);
            }
            ast::ItemKind::GlobalVariable(item) => {
                self.rewrite_type(&mut item.var_type);
                if let Some(initializer) = &mut item.initializer {
                    self.rewrite_expr(initializer);
                }
            }
            _ => {}
        }
    }

    fn rewrite_function(&mut self, func: &mut ast::FunctionItem) {
        self.push_scope();
        if let Some(generics) = &mut func.generics {
            self.rewrite_generics(generics);
        }
        for param in &mut func.parameters {
            self.rewrite_type(&mut param.param_type);
            self.bind_value(&param.name.name);
        }
        if let Some(return_type) = &mut func.return_type {
            self.rewrite_type(return_type);
        }
        self.rewrite_block(&mut func.body);
        self.pop_scope();
    }

    fn rewrite_function_signature(&mut self, sig: &mut ast::FunctionSignature) {
        self.rewrite_function_signature_like(&mut sig.parameters, &mut sig.return_type);
    }

    fn rewrite_function_signature_like(
        &mut self,
        params: &mut [ast::Parameter],
        return_type: &mut Option<ast::Type>,
    ) {
        self.push_scope();
        for param in params {
            self.rewrite_type(&mut param.param_type);
            self.bind_value(&param.name.name);
        }
        if let Some(return_type) = return_type {
            self.rewrite_type(return_type);
        }
        self.pop_scope();
    }

    fn rewrite_generics(&mut self, generics: &mut ast::Generics) {
        for clause in &mut generics.where_clause {
            for predicate in &mut clause.predicates {
                match predicate {
                    ast::WherePredicate::Type {
                        bounded_type,
                        bounds,
                    } => {
                        self.rewrite_type(bounded_type);
                        for bound in bounds {
                            if let Some(args) = &mut bound.trait_ref.generics {
                                for arg in args {
                                    self.rewrite_type(arg);
                                }
                            }
                        }
                    }
                    ast::WherePredicate::Lifetime { .. } => {}
                }
            }
        }
    }

    fn rewrite_block(&mut self, block: &mut ast::Block) {
        self.push_scope();
        for statement in &mut block.statements {
            self.rewrite_statement(statement);
        }
        self.pop_scope();
    }

    fn rewrite_statement(&mut self, statement: &mut ast::Statement) {
        match &mut statement.kind {
            ast::StatementKind::Let(item) => {
                if let Some(type_annotation) = &mut item.type_annotation {
                    self.rewrite_type(type_annotation);
                }
                if let Some(init) = &mut item.initializer {
                    self.rewrite_expr(init);
                }
                self.bind_pattern(&item.pattern);
            }
            ast::StatementKind::Block(block) => self.rewrite_block(block),
            ast::StatementKind::Expression(expr)
            | ast::StatementKind::Return(Some(expr))
            | ast::StatementKind::Break(Some(expr)) => self.rewrite_expr(expr),
            ast::StatementKind::Return(None)
            | ast::StatementKind::Break(None)
            | ast::StatementKind::Continue => {}
        }
    }

    fn rewrite_expr(&mut self, expr: &mut ast::Expression) {
        match expr.kind.as_mut() {
            ast::ExpressionKind::Identifier(ident) => {
                if !self.is_value_bound(&ident.name) {
                    if let Some(target) = self.plan.direct.get(&ident.name) {
                        ident.name = target.clone();
                    }
                }
            }
            ast::ExpressionKind::TypeName(ty) => self.rewrite_type(ty),
            ast::ExpressionKind::FieldAccess { object, field } => {
                self.rewrite_expr(object);
                if let ast::ExpressionKind::Identifier(owner) = object.kind.as_ref() {
                    if !self.is_value_bound(&owner.name) {
                        if let Some(target) = self.plan.direct.get(&owner.name) {
                            owner_to_expr(object, target, owner.span.clone());
                        } else if self
                            .plan
                            .namespaces
                            .get(&owner.name)
                            .is_some_and(|items| items.contains(&field.name))
                        {
                            expr.kind = Box::new(ast::ExpressionKind::Identifier(field.clone()));
                        }
                    }
                }
            }
            ast::ExpressionKind::Call {
                function,
                arguments,
            } => {
                self.rewrite_expr(function);
                for arg in arguments {
                    self.rewrite_expr(arg);
                }
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                method,
                arguments,
            } => {
                self.rewrite_expr(receiver);
                for arg in arguments.iter_mut() {
                    self.rewrite_expr(arg);
                }
                if let ast::ExpressionKind::Identifier(owner) = receiver.kind.as_ref() {
                    if !self.is_value_bound(&owner.name)
                        && self
                            .plan
                            .namespaces
                            .get(&owner.name)
                            .is_some_and(|items| items.contains(&method.name))
                    {
                        expr.kind = Box::new(ast::ExpressionKind::Call {
                            function: Box::new(ast::Expression {
                                kind: Box::new(ast::ExpressionKind::Identifier(method.clone())),
                                span: method.span.clone(),
                            }),
                            arguments: arguments.clone(),
                        });
                    }
                }
            }
            ast::ExpressionKind::Binary { left, right, .. } => {
                self.rewrite_expr(left);
                self.rewrite_expr(right);
            }
            ast::ExpressionKind::Unary { operand, .. }
            | ast::ExpressionKind::Postfix { operand, .. }
            | ast::ExpressionKind::Move(operand)
            | ast::ExpressionKind::Comptime(operand) => self.rewrite_expr(operand),
            ast::ExpressionKind::Reference { expression, .. } => self.rewrite_expr(expression),
            ast::ExpressionKind::Index { object, index } => {
                self.rewrite_expr(object);
                self.rewrite_expr(index);
            }
            ast::ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.rewrite_expr(condition);
                self.rewrite_block(then_branch);
                if let Some(block) = else_branch {
                    self.rewrite_block(block);
                }
            }
            ast::ExpressionKind::While { condition, body } => {
                self.rewrite_expr(condition);
                self.rewrite_block(body);
            }
            ast::ExpressionKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                self.push_scope();
                if let Some(type_annotation) = &mut init.type_annotation {
                    self.rewrite_type(type_annotation);
                }
                if let Some(init_expr) = &mut init.initializer {
                    self.rewrite_expr(init_expr);
                }
                self.bind_pattern(&init.pattern);
                self.rewrite_expr(condition);
                self.rewrite_expr(increment);
                self.rewrite_block(body);
                self.pop_scope();
            }
            ast::ExpressionKind::Match { expression, arms } => {
                self.rewrite_expr(expression);
                for arm in arms {
                    self.push_scope();
                    self.bind_pattern(&arm.pattern);
                    if let Some(guard) = &mut arm.guard {
                        self.rewrite_expr(guard);
                    }
                    self.rewrite_expr(&mut arm.body);
                    self.pop_scope();
                }
            }
            ast::ExpressionKind::Block(block) => self.rewrite_block(block),
            ast::ExpressionKind::Initializer { items } => {
                for item in items {
                    match item {
                        ast::InitializerItem::Positional(expr)
                        | ast::InitializerItem::Field { value: expr, .. } => {
                            self.rewrite_expr(expr)
                        }
                        ast::InitializerItem::Index { index, value } => {
                            self.rewrite_expr(index);
                            self.rewrite_expr(value);
                        }
                    }
                }
            }
            ast::ExpressionKind::Array(items) | ast::ExpressionKind::Tuple(items) => {
                for item in items {
                    self.rewrite_expr(item);
                }
            }
            ast::ExpressionKind::StructLiteral { path, fields } => {
                self.rewrite_path(path);
                for field in fields {
                    self.rewrite_expr(&mut field.value);
                }
            }
            ast::ExpressionKind::Cast {
                expression,
                target_type,
            } => {
                self.rewrite_expr(expression);
                self.rewrite_type(target_type);
            }
            ast::ExpressionKind::Literal(_)
            | ast::ExpressionKind::Asm(_)
            | ast::ExpressionKind::MacroCall { .. } => {}
        }
    }

    fn rewrite_type(&mut self, ty: &mut ast::Type) {
        match ty.kind.as_mut() {
            ast::TypeKind::Named(named) => {
                self.rewrite_path(&mut named.path);
                if let Some(generics) = &mut named.generics {
                    for arg in generics {
                        self.rewrite_type(arg);
                    }
                }
            }
            ast::TypeKind::Reference(reference) => self.rewrite_type(&mut reference.inner),
            ast::TypeKind::Pointer(pointer) => self.rewrite_type(&mut pointer.inner),
            ast::TypeKind::Array(array) => self.rewrite_type(&mut array.element_type),
            ast::TypeKind::Optional(inner) => self.rewrite_type(inner),
            ast::TypeKind::Function(function) => {
                for param in &mut function.parameters {
                    self.rewrite_type(param);
                }
                self.rewrite_type(&mut function.return_type);
            }
            ast::TypeKind::Tuple(items) => {
                for item in items {
                    self.rewrite_type(item);
                }
            }
            ast::TypeKind::Primitive(_) | ast::TypeKind::Generic(_) => {}
        }
    }

    fn rewrite_path(&mut self, path: &mut Vec<ast::Identifier>) {
        if path.is_empty() {
            return;
        }
        if path.len() == 1 {
            if let Some(target) = self.plan.direct.get(&path[0].name) {
                path[0].name = target.clone();
            }
            return;
        }
        let first = path[0].name.clone();
        let second = path[1].name.clone();
        if self
            .plan
            .namespaces
            .get(&first)
            .is_some_and(|items| items.contains(&second))
        {
            path.remove(0);
        }
    }

    fn push_scope(&mut self) {
        self.value_scopes.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        self.value_scopes.pop();
    }

    fn bind_value(&mut self, name: &str) {
        if let Some(scope) = self.value_scopes.last_mut() {
            scope.insert(name.to_string());
        }
    }

    fn bind_pattern(&mut self, pattern: &ast::Pattern) {
        match &pattern.kind {
            ast::PatternKind::Identifier(ident) => self.bind_value(&ident.name),
            ast::PatternKind::Tuple(items) => {
                for item in items {
                    self.bind_pattern(item);
                }
            }
            ast::PatternKind::Struct { fields, .. } => {
                for field in fields {
                    if let Some(pattern) = &field.pattern {
                        self.bind_pattern(pattern);
                    }
                }
            }
            ast::PatternKind::Enum { data, .. } => {
                if let Some(data) = data {
                    self.bind_pattern(data);
                }
            }
            ast::PatternKind::Literal(_) | ast::PatternKind::Wildcard => {}
        }
    }

    fn is_value_bound(&self, name: &str) -> bool {
        self.value_scopes
            .iter()
            .rev()
            .any(|scope| scope.contains(name))
    }
}

fn owner_to_expr(expr: &mut ast::Expression, name: &str, span: lexer::Span) {
    expr.kind = Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
        name: name.to_string(),
        span,
    }));
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Span;
    use crate::module_artifact::{
        ExportKind, ModuleAbi, ModuleArtifact, ModuleCodeArtifacts, ModuleExport,
    };
    use std::time::{SystemTime, UNIX_EPOCH};

    fn ident(name: &str) -> ast::Identifier {
        ast::Identifier {
            name: name.to_string(),
            span: Span { start: 0, end: 0 },
        }
    }

    fn import_program(path: &[&str]) -> ast::Program {
        ast::Program {
            attributes: Vec::new(),
            items: vec![ast::Item {
                kind: ast::ItemKind::Import(ast::ImportItem {
                    path: path.iter().map(|segment| ident(segment)).collect(),
                    alias: None,
                    items: None,
                }),
                span: Span { start: 0, end: 0 },
                visibility: ast::Visibility::Private,
                attributes: Vec::new(),
            }],
            span: Span { start: 0, end: 0 },
        }
    }

    fn unique_temp_dir(label: &str) -> PathBuf {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!("agc-import-hook-{label}-{nonce}"))
    }

    #[test]
    fn lowers_file_imports_into_program_items() {
        let root = unique_temp_dir("file");
        std::fs::create_dir_all(root.join("std")).unwrap();
        std::fs::write(
            root.join("std").join("io.ag"),
            "i32 imported() { return 1; }",
        )
        .unwrap();

        let mut program = import_program(&["std", "io"]);
        let mut loader = ModuleLoader::new();
        loader.add_search_dir(&root);
        let hook = FileImportResolverHook::new(&loader);
        let result = hook
            .lower_program_imports(&mut program, None, None)
            .expect("import lowering should succeed");

        assert!(result.module_artifacts.is_empty());
        assert!(program
            .items
            .iter()
            .all(|item| !matches!(item.kind, ast::ItemKind::Import(_))));
        assert!(program
            .items
            .iter()
            .any(|item| matches!(item.kind, ast::ItemKind::Function(_))));

        let _ = std::fs::remove_dir_all(root);
    }

    #[test]
    fn preserves_program_attributes_from_imported_files() {
        let root = unique_temp_dir("file-attrs");
        std::fs::create_dir_all(root.join("std")).unwrap();
        std::fs::write(
            root.join("std").join("io.ag"),
            "#[link(m)] i32 imported() { return 1; }",
        )
        .unwrap();

        let mut program = import_program(&["std", "io"]);
        let mut loader = ModuleLoader::new();
        loader.add_search_dir(&root);
        let hook = FileImportResolverHook::new(&loader);
        hook.lower_program_imports(&mut program, None, None)
            .expect("import lowering should succeed");

        assert_eq!(program.attributes.len(), 1);
        assert_eq!(program.attributes[0].name.name, "link");

        let _ = std::fs::remove_dir_all(root);
    }

    #[test]
    fn lowers_selected_source_imports_only() {
        let root = unique_temp_dir("file-selective");
        std::fs::create_dir_all(root.join("std")).unwrap();
        std::fs::write(
            root.join("std").join("io.ag"),
            "i32 print() { return 1; } i32 hidden() { return 2; }",
        )
        .unwrap();
        let src = "import std.io { print }; i32 main() { return print(); }";
        let mut program = parse_program_from_source(src, &root.join("main.ag"));

        let mut loader = ModuleLoader::new();
        loader.add_search_dir(&root);
        let hook = FileImportResolverHook::new(&loader);
        hook.lower_program_imports(&mut program, None, None)
            .expect("import lowering should succeed");

        assert!(program
            .items
            .iter()
            .any(|item| item_name(item).as_deref() == Some("print")));
        assert!(!program
            .items
            .iter()
            .any(|item| item_name(item).as_deref() == Some("hidden")));

        let _ = std::fs::remove_dir_all(root);
    }

    #[test]
    fn rewrites_selected_import_aliases_in_source_program() {
        let root = unique_temp_dir("file-alias");
        std::fs::create_dir_all(root.join("std")).unwrap();
        std::fs::write(root.join("std").join("io.ag"), "i32 print() { return 1; }").unwrap();
        let src = "import std.io { print as p }; i32 main() { return p(); }";
        let mut program = parse_program_from_source(src, &root.join("main.ag"));

        let mut loader = ModuleLoader::new();
        loader.add_search_dir(&root);
        let hook = FileImportResolverHook::new(&loader);
        hook.lower_program_imports(&mut program, None, None)
            .expect("import lowering should succeed");

        let ast::ItemKind::Function(main_fn) = &program.items[1].kind else {
            panic!("expected main function");
        };
        let ast::StatementKind::Return(Some(expr)) = &main_fn.body.statements[0].kind else {
            panic!("expected return statement");
        };
        let ast::ExpressionKind::Call { function, .. } = expr.kind.as_ref() else {
            panic!("expected call expression");
        };
        let ast::ExpressionKind::Identifier(ident) = function.kind.as_ref() else {
            panic!("expected identifier callee");
        };
        assert_eq!(ident.name, "print");

        let _ = std::fs::remove_dir_all(root);
    }

    #[test]
    fn rewrites_module_namespace_aliases_in_program() {
        let root = unique_temp_dir("module-alias");
        std::fs::create_dir_all(root.join("std")).unwrap();

        let artifact = ModuleArtifact {
            module_name: "io".to_string(),
            module_path: "std.io".to_string(),
            source_path: "std/io.ag".to_string(),
            source_hash_fnv1a64: 0,
            compiler_version: "test".to_string(),
            target_triple: "unknown".to_string(),
            code_artifacts: ModuleCodeArtifacts {
                has_static_library: true,
                has_shared_library: false,
            },
            module_deps: Vec::new(),
            exports: vec![ModuleExport {
                kind: ExportKind::Function,
                name: "print".to_string(),
                signature: "fn()->i32".to_string(),
                link_name: Some("print".to_string()),
                abi: Some(ModuleAbi::Silver),
                is_variadic: false,
                type_key: None,
                fields: Vec::new(),
                layout: None,
                enum_backing_type: None,
                enum_variants: Vec::new(),
                trait_items: Vec::new(),
            }],
            native_libs: vec![],
            artifact_path: None,
        };
        std::fs::write(
            root.join("std").join("io.agm"),
            artifact
                .to_bytes()
                .expect("artifact encoding should succeed"),
        )
        .unwrap();
        let src = "import std.io as io; i32 main() { return io.print(); }";
        let mut program = parse_program_from_source(src, &root.join("main.ag"));

        let mut loader = ModuleLoader::new();
        loader.add_search_dir(&root);
        let hook = FileImportResolverHook::new(&loader);
        let result = hook
            .lower_program_imports(&mut program, None, None)
            .expect("import lowering should succeed");

        assert_eq!(result.module_artifacts.len(), 1);
        let ast::ItemKind::Function(main_fn) = &program.items[0].kind else {
            panic!("expected main function");
        };
        let ast::StatementKind::Return(Some(expr)) = &main_fn.body.statements[0].kind else {
            panic!("expected return statement");
        };
        let ast::ExpressionKind::Call { function, .. } = expr.kind.as_ref() else {
            panic!("expected call expression");
        };
        let ast::ExpressionKind::Identifier(ident) = function.kind.as_ref() else {
            panic!("expected identifier callee");
        };
        assert_eq!(ident.name, "print");

        let _ = std::fs::remove_dir_all(root);
    }

    #[test]
    fn collects_module_artifacts_for_agm_imports() {
        let root = unique_temp_dir("module");
        std::fs::create_dir_all(root.join("std")).unwrap();

        let artifact = ModuleArtifact {
            module_name: "io".to_string(),
            module_path: "std.io".to_string(),
            source_path: "std/io.ag".to_string(),
            source_hash_fnv1a64: 0,
            compiler_version: "test".to_string(),
            target_triple: "unknown".to_string(),
            code_artifacts: ModuleCodeArtifacts {
                has_static_library: true,
                has_shared_library: false,
            },
            module_deps: Vec::new(),
            exports: vec![ModuleExport {
                kind: ExportKind::Function,
                name: "print".to_string(),
                signature: "fn(str)->unit".to_string(),
                link_name: Some("print".to_string()),
                abi: Some(ModuleAbi::Silver),
                is_variadic: false,
                type_key: None,
                fields: Vec::new(),
                layout: None,
                enum_backing_type: None,
                enum_variants: Vec::new(),
                trait_items: Vec::new(),
            }],
            native_libs: vec!["c".to_string()],
            artifact_path: None,
        };
        std::fs::write(
            root.join("std").join("io.agm"),
            artifact
                .to_bytes()
                .expect("artifact encoding should succeed"),
        )
        .unwrap();

        let mut program = import_program(&["std", "io"]);
        let mut loader = ModuleLoader::new();
        loader.add_search_dir(&root);
        let hook = FileImportResolverHook::new(&loader);
        let result = hook
            .lower_program_imports(&mut program, None, None)
            .expect("import lowering should succeed");

        assert_eq!(result.module_artifacts.len(), 1);
        assert_eq!(result.module_artifacts[0].module_name, "io");
        assert!(program.items.is_empty());

        let _ = std::fs::remove_dir_all(root);
    }

    fn parse_program_from_source(src: &str, path: &Path) -> ast::Program {
        let tokens = crate::lexer::lex(src).expect("lex failed");
        let mut parser = Parser::new_with_source(tokens, path.display().to_string());
        let (program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {errors:?}");
        program
    }
}
