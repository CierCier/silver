use crate::lexer::Span;
use crate::parser::ast;

pub mod llvm_ir;

pub type CodegenResult<T> = Result<T, CodegenError>;

#[derive(Debug, Clone)]
pub struct CodegenError {
    pub message: String,
    pub span: Option<Span>,
}

impl CodegenError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            span: None,
        }
    }

    pub fn with_span(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span: Some(span),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct TextEmitter {
    output: String,
    indent: usize,
}

impl TextEmitter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_line(&mut self, line: &str) {
        if !line.is_empty() {
            let pad = " ".repeat(self.indent * 4);
            self.output.push_str(&pad);
        }
        self.output.push_str(line);
        self.output.push('\n');
    }

    pub fn push(&mut self, text: &str) {
        self.output.push_str(text);
    }

    pub fn indent(&mut self) {
        self.indent += 1;
    }

    pub fn dedent(&mut self) {
        if self.indent > 0 {
            self.indent -= 1;
        }
    }

    pub fn finish(self) -> String {
        self.output
    }
}

pub trait SilverGenerator {
    fn generate_program(&mut self, program: &ast::Program) -> CodegenResult<()>;
    fn generate_item(&mut self, item: &ast::Item) -> CodegenResult<()>;
    fn generate_function_item(
        &mut self,
        func: &ast::FunctionItem,
        visibility: &ast::Visibility,
        attributes: &[ast::Attribute],
    ) -> CodegenResult<()>;
    fn generate_struct_item(
        &mut self,
        item: &ast::StructItem,
        visibility: &ast::Visibility,
        attributes: &[ast::Attribute],
    ) -> CodegenResult<()>;
    fn generate_enum_item(
        &mut self,
        item: &ast::EnumItem,
        visibility: &ast::Visibility,
        attributes: &[ast::Attribute],
    ) -> CodegenResult<()>;
    fn generate_impl_item(
        &mut self,
        item: &ast::ImplItem,
        visibility: &ast::Visibility,
        attributes: &[ast::Attribute],
    ) -> CodegenResult<()>;
    fn generate_trait_item(
        &mut self,
        item: &ast::TraitItem,
        visibility: &ast::Visibility,
        attributes: &[ast::Attribute],
    ) -> CodegenResult<()>;
    fn generate_import_item(
        &mut self,
        item: &ast::ImportItem,
        visibility: &ast::Visibility,
        attributes: &[ast::Attribute],
    ) -> CodegenResult<()>;
    fn generate_extern_function_item(
        &mut self,
        item: &ast::ExternFunctionItem,
        visibility: &ast::Visibility,
        attributes: &[ast::Attribute],
    ) -> CodegenResult<()>;
    fn generate_extern_block_item(
        &mut self,
        item: &ast::ExternBlockItem,
        visibility: &ast::Visibility,
        attributes: &[ast::Attribute],
    ) -> CodegenResult<()>;
    fn generate_block(&mut self, block: &ast::Block) -> CodegenResult<()>;
    fn generate_statement(&mut self, statement: &ast::Statement) -> CodegenResult<()>;
    fn generate_expression(&mut self, expr: &ast::Expression) -> CodegenResult<()>;
    fn generate_pattern(&mut self, pattern: &ast::Pattern) -> CodegenResult<()>;
    fn generate_type(&mut self, ty: &ast::Type) -> CodegenResult<()>;
    fn generate_initializer_item(&mut self, item: &ast::InitializerItem) -> CodegenResult<()>;
    fn generate_macro_arg(&mut self, arg: &ast::MacroArg) -> CodegenResult<()>;
    fn generate_literal(&mut self, literal: &ast::Literal) -> CodegenResult<()>;
    fn finish(self) -> String;
}
