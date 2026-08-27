use inkwell::values::{BasicValueEnum, PointerValue};

use crate::codegen::llvm_ir::{LlvmIrGenerator, VarInfo};
use crate::codegen::{CodegenError, CodegenResult};
use crate::parser::ast;

impl<'ctx> LlvmIrGenerator<'ctx> {
    pub(crate) fn json_codegen(
        &mut self,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let value = match args.first() {
            Some(ast::MacroArg::Expression(value)) => value,
            _ => {
                return Err(CodegenError::with_span(
                    "@json requires a value expression".to_string(),
                    expr.span,
                ));
            }
        };

        let (writer, _) = self.json_begin_writer(&expr.span)?;
        if let Some(ast::MacroArg::Expression(prefix)) = args.get(1) {
            let method = Self::json_identifier("write_raw", expr.span);
            self.emit_method_call_expression(
                &writer,
                &method,
                std::slice::from_ref(prefix),
                true,
                &expr.span,
            )?;
        }

        let method = Self::json_identifier("to_json", expr.span);
        let writer_ref = Self::json_reference(&writer);
        self.emit_method_call_expression(
            value,
            &method,
            std::slice::from_ref(&writer_ref),
            true,
            &expr.span,
        )?;
        self.json_finish_writer(&writer, &expr.span)
    }

    pub(crate) fn json_from_codegen(
        &mut self,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let target = match args.first() {
            Some(ast::MacroArg::Expression(target)) => target,
            _ => {
                return Err(CodegenError::with_span(
                    "@from_json requires a target type".to_string(),
                    expr.span,
                ));
            }
        };
        let input = match args.get(1) {
            Some(ast::MacroArg::Expression(input)) => input,
            _ => {
                return Err(CodegenError::with_span(
                    "@from_json requires an input expression".to_string(),
                    expr.span,
                ));
            }
        };

        let (reader, _) = self.json_begin_reader(input, &expr.span)?;
        let method = Self::json_identifier("from_json", expr.span);
        let reader_ref = Self::json_reference(&reader);
        self.emit_method_call_expression(
            target,
            &method,
            std::slice::from_ref(&reader_ref),
            false,
            &expr.span,
        )?
        .ok_or_else(|| {
            CodegenError::with_span(
                "FromJson::from_json returned no value".to_string(),
                expr.span,
            )
        })
    }

    fn json_identifier(name: &str, span: crate::lexer::Span) -> ast::Identifier {
        ast::Identifier {
            name: name.to_string(),
            span,
        }
    }

    fn json_type(name: &str, span: crate::lexer::Span) -> ast::Type {
        ast::Type {
            kind: Box::new(ast::TypeKind::Named(ast::NamedType {
                path: vec![Self::json_identifier(name, span)],
                generics: None,
            })),
            span,
        }
    }

    fn json_type_receiver(name: &str, span: crate::lexer::Span) -> ast::Expression {
        ast::Expression {
            kind: Box::new(ast::ExpressionKind::TypeName(Self::json_type(name, span))),
            span,
        }
    }

    fn json_begin_writer(
        &mut self,
        span: &crate::lexer::Span,
    ) -> CodegenResult<(ast::Expression, PointerValue<'ctx>)> {
        let receiver = Self::json_type_receiver("JsonWriter", *span);
        let method = Self::json_identifier("new", *span);
        let value = self
            .emit_method_call_expression(&receiver, &method, &[], false, span)?
            .ok_or_else(|| CodegenError::with_span("JsonWriter.new returned no value", *span))?;
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::with_span("@json requires an active function", *span))?;
        let ptr = self.create_entry_alloca(function, "json.writer", value.get_type())?;
        self.builder
            .build_store(ptr, value)
            .map_err(|e| CodegenError::with_span(format!("JsonWriter store failed: {e}"), *span))?;
        self.insert_json_variable("__json_writer", ptr, "JsonWriter", *span);
        Ok((
            Self::json_identifier_expression("__json_writer", *span),
            ptr,
        ))
    }

    fn json_begin_reader(
        &mut self,
        input: &ast::Expression,
        span: &crate::lexer::Span,
    ) -> CodegenResult<(ast::Expression, PointerValue<'ctx>)> {
        let receiver = Self::json_type_receiver("JsonReader", *span);
        let method = Self::json_identifier("new", *span);
        let value = self
            .emit_method_call_expression(
                &receiver,
                &method,
                std::slice::from_ref(input),
                false,
                span,
            )?
            .ok_or_else(|| CodegenError::with_span("JsonReader.new returned no value", *span))?;
        let function = self.current_fn.ok_or_else(|| {
            CodegenError::with_span("@from_json requires an active function", *span)
        })?;
        let ptr = self.create_entry_alloca(function, "json.reader", value.get_type())?;
        self.builder
            .build_store(ptr, value)
            .map_err(|e| CodegenError::with_span(format!("JsonReader store failed: {e}"), *span))?;
        self.insert_json_variable("__json_reader", ptr, "JsonReader", *span);
        Ok((
            Self::json_identifier_expression("__json_reader", *span),
            ptr,
        ))
    }

    fn json_finish_writer(
        &mut self,
        writer: &ast::Expression,
        span: &crate::lexer::Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let method = Self::json_identifier("finish", *span);
        self.emit_method_call_expression(writer, &method, &[], false, span)?
            .ok_or_else(|| CodegenError::with_span("JsonWriter.finish returned no value", *span))
    }

    fn insert_json_variable(
        &mut self,
        name: &str,
        ptr: PointerValue<'ctx>,
        type_name: &str,
        span: crate::lexer::Span,
    ) {
        if let Some(scope) = self.variables.last_mut() {
            scope.insert(
                name.to_string(),
                VarInfo {
                    ptr,
                    ty: Self::json_type(type_name, span),
                    is_mutable: true,
                    is_volatile: false,
                    drop_flag: None,
                    field_flags: Vec::new(),
                },
            );
        }
    }
    fn json_reference(expression: &ast::Expression) -> ast::Expression {
        ast::Expression {
            kind: Box::new(ast::ExpressionKind::Reference {
                expression: Box::new(expression.clone()),
                is_mutable: true,
            }),
            span: expression.span,
        }
    }

    fn json_identifier_expression(name: &str, span: crate::lexer::Span) -> ast::Expression {
        ast::Expression {
            kind: Box::new(ast::ExpressionKind::Identifier(Self::json_identifier(
                name, span,
            ))),
            span,
        }
    }
}
