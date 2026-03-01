use std::collections::HashMap;

use crate::lexer::Span;
use crate::parser::ast;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Primitive(ast::PrimitiveType),
    Named {
        path: Vec<String>,
        generics: Vec<Type>,
    },
    Reference {
        is_mutable: bool,
        inner: Box<Type>,
    },
    Pointer {
        is_mutable: bool,
        inner: Box<Type>,
    },
    Array {
        element: Box<Type>,
        length: Option<usize>,
    },
    Optional {
        inner: Box<Type>,
    },
    Tuple(Vec<Type>),
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeLayout {
    pub size: Option<usize>,
    pub align: Option<usize>,
}

impl TypeLayout {
    pub fn known(size: usize, align: usize) -> Self {
        Self {
            size: Some(size),
            align: Some(align),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeContext {
    pointer_size: usize,
    pointer_align: usize,
    named_layouts: HashMap<String, TypeLayout>,
}

#[derive(Debug, Clone, Default)]
pub struct StructAttributes {
    pub packed: bool,
    pub repr_c: bool,
    pub align: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructAttrError {
    pub message: String,
    pub span: Span,
}

impl Default for TypeContext {
    fn default() -> Self {
        Self::new(8)
    }
}

impl TypeContext {
    pub fn new(pointer_size: usize) -> Self {
        Self {
            pointer_size,
            pointer_align: pointer_size,
            named_layouts: HashMap::new(),
        }
    }

    pub fn register_named(&mut self, path: &[String], layout: TypeLayout) {
        self.named_layouts.insert(path.join("::"), layout);
    }

    pub fn layout_of(&self, ty: &Type) -> TypeLayout {
        match ty {
            Type::Unit => TypeLayout::known(0, 1),
            Type::Primitive(p) => primitive_layout(p, self.pointer_size),
            Type::Named { path, .. } => self
                .named_layouts
                .get(&path.join("::"))
                .copied()
                .unwrap_or(TypeLayout {
                    size: None,
                    align: None,
                }),
            Type::Reference { .. } | Type::Pointer { .. } | Type::Function { .. } => {
                TypeLayout::known(self.pointer_size, self.pointer_align)
            }
            Type::Optional { .. } => TypeLayout::known(self.pointer_size, self.pointer_align),
            Type::Array { element, length } => {
                let elem_layout = self.layout_of(element);
                let size = match (elem_layout.size, length) {
                    (Some(elem_size), Some(len)) => elem_size.checked_mul(*len),
                    _ => None,
                };
                TypeLayout {
                    size,
                    align: elem_layout.align,
                }
            }
            Type::Tuple(items) => tuple_layout(self, items),
            Type::Unknown => TypeLayout {
                size: None,
                align: None,
            },
        }
    }
}

pub fn parse_struct_attributes(
    attributes: &[ast::Attribute],
) -> Result<StructAttributes, StructAttrError> {
    let mut out = StructAttributes {
        packed: false,
        repr_c: false,
        align: None,
    };

    for attr in attributes {
        match attr.name.name.as_str() {
            "packed" => {
                if !attr.args.is_empty() {
                    return Err(StructAttrError {
                        message: "packed takes no arguments".to_string(),
                        span: attr.span.clone(),
                    });
                }
                out.packed = true;
            }
            "repr" => {
                if attr.args.len() != 1 {
                    return Err(StructAttrError {
                        message: "repr expects one argument".to_string(),
                        span: attr.span.clone(),
                    });
                }
                match &attr.args[0] {
                    ast::AttributeArg::Identifier(ident) if ident.name == "C" => {
                        out.repr_c = true;
                    }
                    _ => {
                        return Err(StructAttrError {
                            message: "repr only supports C".to_string(),
                            span: attr.span.clone(),
                        });
                    }
                }
            }
            "align" => {
                if attr.args.len() != 1 {
                    return Err(StructAttrError {
                        message: "align expects one integer argument".to_string(),
                        span: attr.span.clone(),
                    });
                }
                let ast::AttributeArg::Literal(ast::Literal::Integer(value)) = &attr.args[0] else {
                    return Err(StructAttrError {
                        message: "align expects an integer literal".to_string(),
                        span: attr.span.clone(),
                    });
                };
                if *value <= 0 {
                    return Err(StructAttrError {
                        message: "align must be greater than zero".to_string(),
                        span: attr.span.clone(),
                    });
                }
                let align = usize::try_from(*value).map_err(|_| StructAttrError {
                    message: "align value is too large".to_string(),
                    span: attr.span.clone(),
                })?;
                if !align.is_power_of_two() {
                    return Err(StructAttrError {
                        message: "align must be a power of two".to_string(),
                        span: attr.span.clone(),
                    });
                }
                out.align = Some(align);
            }
            _ => {
                return Err(StructAttrError {
                    message: format!("unknown struct attribute '{}'", attr.name.name),
                    span: attr.span.clone(),
                });
            }
        }
    }

    if out.packed && out.align.is_some() {
        return Err(StructAttrError {
            message: "packed and align cannot be combined".to_string(),
            span: attributes
                .iter()
                .find(|attr| attr.name.name == "packed")
                .map(|attr| attr.span.clone())
                .unwrap_or_else(|| Span { start: 0, end: 0 }),
        });
    }

    Ok(out)
}

pub fn struct_layout(ctx: &TypeContext, fields: &[Type], attrs: &StructAttributes) -> TypeLayout {
    if attrs.packed {
        let mut total = 0usize;
        for field in fields {
            let layout = ctx.layout_of(field);
            let Some(size) = layout.size else {
                return TypeLayout {
                    size: None,
                    align: Some(1),
                };
            };
            total = total.saturating_add(size);
        }
        return TypeLayout::known(total, 1);
    }

    let mut offset = 0usize;
    let mut max_align = 1usize;
    for field in fields {
        let layout = ctx.layout_of(field);
        let Some(align) = layout.align else {
            return TypeLayout {
                size: None,
                align: None,
            };
        };
        let Some(size) = layout.size else {
            return TypeLayout {
                size: None,
                align: Some(align),
            };
        };
        max_align = max_align.max(align);
        offset = align_to(offset, align);
        offset = offset.saturating_add(size);
    }

    let overall_align = attrs.align.unwrap_or(max_align).max(max_align);
    let total = align_to(offset, overall_align);
    TypeLayout::known(total, overall_align)
}

impl Type {
    pub fn from_ast(ty: &ast::Type) -> Self {
        match ty.kind.as_ref() {
            ast::TypeKind::Primitive(p) => Type::Primitive(p.clone()),
            ast::TypeKind::Named(named) => Type::Named {
                path: named.path.iter().map(|id| id.name.clone()).collect(),
                generics: named
                    .generics
                    .as_ref()
                    .map(|gs| gs.iter().map(Type::from_ast).collect())
                    .unwrap_or_default(),
            },
            ast::TypeKind::Generic(generic) => Type::Named {
                path: vec![generic.name.name.clone()],
                generics: generic.args.iter().map(Type::from_ast).collect(),
            },
            ast::TypeKind::Reference(reference) => Type::Reference {
                is_mutable: reference.is_mutable,
                inner: Box::new(Type::from_ast(&reference.inner)),
            },
            ast::TypeKind::Pointer(pointer) => Type::Pointer {
                is_mutable: pointer.is_mutable,
                inner: Box::new(Type::from_ast(&pointer.inner)),
            },
            ast::TypeKind::Array(array) => Type::Array {
                element: Box::new(Type::from_ast(&array.element_type)),
                length: array.size.as_ref().and_then(|expr| literal_usize(expr)),
            },
            ast::TypeKind::Optional(inner) => Type::Optional {
                inner: Box::new(Type::from_ast(inner)),
            },
            ast::TypeKind::Function(func) => Type::Function {
                params: func.parameters.iter().map(Type::from_ast).collect(),
                return_type: Box::new(Type::from_ast(&func.return_type)),
            },
            ast::TypeKind::Tuple(items) => Type::Tuple(items.iter().map(Type::from_ast).collect()),
        }
    }

    pub fn from_canonical_key(text: &str) -> Result<Type, String> {
        let mut parser = TypeParser::new(text);
        let ty = parser.parse_type()?;
        parser.skip_ws();
        if !parser.is_eof() {
            return Err(format!("unexpected trailing input in type: '{}'", text));
        }
        Ok(ty)
    }

    pub fn substitute(&self, mapping: &HashMap<String, Type>) -> Type {
        match self {
            Type::Named { path, generics } => {
                if path.len() == 1 {
                    if let Some(mapped) = mapping.get(&path[0]) {
                        return mapped.clone();
                    }
                }
                Type::Named {
                    path: path.clone(),
                    generics: generics
                        .iter()
                        .map(|inner| inner.substitute(mapping))
                        .collect(),
                }
            }
            Type::Reference { is_mutable, inner } => Type::Reference {
                is_mutable: *is_mutable,
                inner: Box::new(inner.substitute(mapping)),
            },
            Type::Pointer { is_mutable, inner } => Type::Pointer {
                is_mutable: *is_mutable,
                inner: Box::new(inner.substitute(mapping)),
            },
            Type::Array { element, length } => Type::Array {
                element: Box::new(element.substitute(mapping)),
                length: *length,
            },
            Type::Optional { inner } => Type::Optional {
                inner: Box::new(inner.substitute(mapping)),
            },
            Type::Tuple(items) => Type::Tuple(
                items
                    .iter()
                    .map(|inner| inner.substitute(mapping))
                    .collect(),
            ),
            Type::Function {
                params,
                return_type,
            } => Type::Function {
                params: params
                    .iter()
                    .map(|inner| inner.substitute(mapping))
                    .collect(),
                return_type: Box::new(return_type.substitute(mapping)),
            },
            Type::Primitive(_) | Type::Unit | Type::Unknown => self.clone(),
        }
    }

    pub fn strip_refs(&self) -> &Type {
        match self {
            Type::Reference { inner, .. } | Type::Pointer { inner, .. } => inner.strip_refs(),
            _ => self,
        }
    }

    pub fn canonical_key(&self) -> String {
        let ty = self.strip_refs();
        match ty {
            Type::Unit => "unit".to_string(),
            Type::Primitive(p) => format!("{:?}", p).to_lowercase(),
            Type::Named { path, generics } => {
                if generics.is_empty() {
                    path.join("::")
                } else {
                    let args = generics
                        .iter()
                        .map(|inner| inner.canonical_key())
                        .collect::<Vec<_>>()
                        .join(",");
                    format!("{}<{}>", path.join("::"), args)
                }
            }
            Type::Array { element, length } => match length {
                Some(len) => format!("[{};{}]", element.canonical_key(), len),
                None => format!("[{}]", element.canonical_key()),
            },
            Type::Optional { inner } => format!("optional<{}>", inner.canonical_key()),
            Type::Tuple(items) => {
                let args = items
                    .iter()
                    .map(|inner| inner.canonical_key())
                    .collect::<Vec<_>>()
                    .join(",");
                format!("({})", args)
            }
            Type::Function {
                params,
                return_type,
            } => {
                let args = params
                    .iter()
                    .map(|inner| inner.canonical_key())
                    .collect::<Vec<_>>()
                    .join(",");
                format!("fn({})->{}", args, return_type.canonical_key())
            }
            Type::Reference { .. } | Type::Pointer { .. } => ty.canonical_key(),
            Type::Unknown => "unknown".to_string(),
        }
    }

    pub fn normalized_eq(&self, other: &Type) -> bool {
        self.canonical_key() == other.canonical_key()
    }
}

pub fn parse_canonical_function_signature(text: &str) -> Result<(Vec<Type>, Type), String> {
    let mut parser = TypeParser::new(text);
    let ty = parser.parse_type()?;
    parser.skip_ws();
    if !parser.is_eof() {
        return Err(format!(
            "unexpected trailing input in signature: '{}'",
            text
        ));
    }
    match ty {
        Type::Function {
            params,
            return_type,
        } => Ok((params, *return_type)),
        _ => Err(format!("expected function signature, found '{text}'")),
    }
}

struct TypeParser<'a> {
    text: &'a str,
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> TypeParser<'a> {
    fn new(text: &'a str) -> Self {
        Self {
            text,
            bytes: text.as_bytes(),
            pos: 0,
        }
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.bytes.len()
    }

    fn peek(&self) -> Option<u8> {
        self.bytes.get(self.pos).copied()
    }

    fn skip_ws(&mut self) {
        while matches!(self.peek(), Some(b' ' | b'\t' | b'\n' | b'\r')) {
            self.pos += 1;
        }
    }

    fn consume_byte(&mut self, byte: u8) -> bool {
        if self.peek() == Some(byte) {
            self.pos += 1;
            return true;
        }
        false
    }

    fn consume_str(&mut self, text: &str) -> bool {
        let bytes = text.as_bytes();
        if self.pos + bytes.len() > self.bytes.len() {
            return false;
        }
        if &self.bytes[self.pos..self.pos + bytes.len()] == bytes {
            self.pos += bytes.len();
            return true;
        }
        false
    }

    fn expect_byte(&mut self, byte: u8) -> Result<(), String> {
        if self.consume_byte(byte) {
            Ok(())
        } else {
            Err(format!(
                "expected '{}' in type '{}'",
                byte as char, self.text
            ))
        }
    }

    fn expect_str(&mut self, text: &str) -> Result<(), String> {
        if self.consume_str(text) {
            Ok(())
        } else {
            Err(format!("expected '{}' in type '{}'", text, self.text))
        }
    }

    fn parse_ident(&mut self) -> Result<String, String> {
        self.skip_ws();
        let start = self.pos;
        let Some(first) = self.peek() else {
            return Err(format!("expected identifier in type '{0}'", self.text));
        };
        if !(first.is_ascii_alphabetic() || first == b'_') {
            return Err(format!("invalid identifier start in type '{0}'", self.text));
        }
        self.pos += 1;
        while let Some(ch) = self.peek() {
            if ch.is_ascii_alphanumeric() || ch == b'_' {
                self.pos += 1;
            } else {
                break;
            }
        }
        Ok(self.text[start..self.pos].to_string())
    }

    fn parse_usize(&mut self) -> Result<usize, String> {
        self.skip_ws();
        let start = self.pos;
        while matches!(self.peek(), Some(b'0'..=b'9')) {
            self.pos += 1;
        }
        if start == self.pos {
            return Err(format!("expected integer in type '{0}'", self.text));
        }
        self.text[start..self.pos]
            .parse::<usize>()
            .map_err(|_| format!("invalid integer in type '{0}'", self.text))
    }

    fn parse_type_list(&mut self, terminator: u8) -> Result<Vec<Type>, String> {
        let mut items = Vec::new();
        self.skip_ws();
        if self.peek() == Some(terminator) {
            self.pos += 1;
            return Ok(items);
        }
        loop {
            let ty = self.parse_type()?;
            items.push(ty);
            self.skip_ws();
            match self.peek() {
                Some(b',') => {
                    self.pos += 1;
                    continue;
                }
                Some(c) if c == terminator => {
                    self.pos += 1;
                    break;
                }
                _ => {
                    return Err(format!(
                        "expected ',' or '{}' in type '{}'",
                        terminator as char, self.text
                    ));
                }
            }
        }
        Ok(items)
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        self.skip_ws();
        if self.consume_str("unit") {
            return Ok(Type::Unit);
        }
        if self.consume_str("unknown") {
            return Ok(Type::Unknown);
        }
        if self.consume_str("optional<") {
            let inner = self.parse_type()?;
            self.expect_byte(b'>')?;
            return Ok(Type::Optional {
                inner: Box::new(inner),
            });
        }
        if self.consume_str("fn(") {
            let params = self.parse_type_list(b')')?;
            self.expect_str("->")?;
            let return_type = self.parse_type()?;
            return Ok(Type::Function {
                params,
                return_type: Box::new(return_type),
            });
        }
        if self.consume_byte(b'[') {
            let element = self.parse_type()?;
            self.skip_ws();
            let length = if self.consume_byte(b';') {
                let len = self.parse_usize()?;
                self.skip_ws();
                self.expect_byte(b']')?;
                Some(len)
            } else {
                self.expect_byte(b']')?;
                None
            };
            return Ok(Type::Array {
                element: Box::new(element),
                length,
            });
        }
        if self.consume_byte(b'(') {
            let items = self.parse_type_list(b')')?;
            return Ok(Type::Tuple(items));
        }

        let mut path = Vec::new();
        path.push(self.parse_ident()?);
        while self.consume_str("::") {
            path.push(self.parse_ident()?);
        }

        let mut generics = Vec::new();
        if self.consume_byte(b'<') {
            generics = self.parse_type_list(b'>')?;
        }

        if generics.is_empty() && path.len() == 1 {
            if let Some(primitive) = parse_primitive_name(&path[0]) {
                return Ok(Type::Primitive(primitive));
            }
        }

        Ok(Type::Named { path, generics })
    }
}

fn parse_primitive_name(name: &str) -> Option<ast::PrimitiveType> {
    match name {
        "i8" => Some(ast::PrimitiveType::I8),
        "i16" => Some(ast::PrimitiveType::I16),
        "i32" => Some(ast::PrimitiveType::I32),
        "i64" => Some(ast::PrimitiveType::I64),
        "i128" => Some(ast::PrimitiveType::I128),
        "u8" => Some(ast::PrimitiveType::U8),
        "u16" => Some(ast::PrimitiveType::U16),
        "u32" => Some(ast::PrimitiveType::U32),
        "u64" => Some(ast::PrimitiveType::U64),
        "u128" => Some(ast::PrimitiveType::U128),
        "f32" => Some(ast::PrimitiveType::F32),
        "f64" => Some(ast::PrimitiveType::F64),
        "f80" => Some(ast::PrimitiveType::F80),
        "c32" => Some(ast::PrimitiveType::C32),
        "c64" => Some(ast::PrimitiveType::C64),
        "c80" => Some(ast::PrimitiveType::C80),
        "bool" => Some(ast::PrimitiveType::Bool),
        "str" => Some(ast::PrimitiveType::Str),
        "char" => Some(ast::PrimitiveType::Char),
        _ => None,
    }
}

pub fn is_numeric(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Primitive(
            ast::PrimitiveType::I8
                | ast::PrimitiveType::I16
                | ast::PrimitiveType::I32
                | ast::PrimitiveType::I64
                | ast::PrimitiveType::I128
                | ast::PrimitiveType::U8
                | ast::PrimitiveType::U16
                | ast::PrimitiveType::U32
                | ast::PrimitiveType::U64
                | ast::PrimitiveType::U128
                | ast::PrimitiveType::F32
                | ast::PrimitiveType::F64
                | ast::PrimitiveType::F80
                | ast::PrimitiveType::C32
                | ast::PrimitiveType::C64
                | ast::PrimitiveType::C80
        )
    )
}

pub fn is_integer(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Primitive(
            ast::PrimitiveType::I8
                | ast::PrimitiveType::I16
                | ast::PrimitiveType::I32
                | ast::PrimitiveType::I64
                | ast::PrimitiveType::I128
                | ast::PrimitiveType::U8
                | ast::PrimitiveType::U16
                | ast::PrimitiveType::U32
                | ast::PrimitiveType::U64
                | ast::PrimitiveType::U128
        )
    )
}

pub fn is_bool(ty: &Type) -> bool {
    matches!(ty, Type::Primitive(ast::PrimitiveType::Bool))
}

pub fn is_string(ty: &Type) -> bool {
    matches!(ty, Type::Primitive(ast::PrimitiveType::Str))
}

fn primitive_layout(primitive: &ast::PrimitiveType, pointer_size: usize) -> TypeLayout {
    match primitive {
        ast::PrimitiveType::I8 | ast::PrimitiveType::U8 => TypeLayout::known(1, 1),
        ast::PrimitiveType::I16 | ast::PrimitiveType::U16 => TypeLayout::known(2, 2),
        ast::PrimitiveType::I32 | ast::PrimitiveType::U32 => TypeLayout::known(4, 4),
        ast::PrimitiveType::I64 | ast::PrimitiveType::U64 => TypeLayout::known(8, 8),
        ast::PrimitiveType::I128 | ast::PrimitiveType::U128 => TypeLayout::known(16, 16),
        ast::PrimitiveType::F32 => TypeLayout::known(4, 4),
        ast::PrimitiveType::F64 => TypeLayout::known(8, 8),
        ast::PrimitiveType::F80 => TypeLayout::known(16, 16),
        ast::PrimitiveType::C32 => TypeLayout::known(8, 4),
        ast::PrimitiveType::C64 => TypeLayout::known(16, 8),
        ast::PrimitiveType::C80 => TypeLayout::known(32, 16),
        ast::PrimitiveType::Bool => TypeLayout::known(1, 1),
        ast::PrimitiveType::Char => TypeLayout::known(4, 4),
        ast::PrimitiveType::Str => TypeLayout::known(pointer_size, pointer_size),
    }
}

fn tuple_layout(ctx: &TypeContext, items: &[Type]) -> TypeLayout {
    let mut offset = 0usize;
    let mut max_align = 1usize;

    for item in items {
        let layout = ctx.layout_of(item);
        let Some(align) = layout.align else {
            return TypeLayout {
                size: None,
                align: None,
            };
        };
        let Some(size) = layout.size else {
            return TypeLayout {
                size: None,
                align: Some(align),
            };
        };
        max_align = max_align.max(align);
        offset = align_to(offset, align);
        offset = offset.saturating_add(size);
    }

    let total = align_to(offset, max_align);
    TypeLayout::known(total, max_align)
}

fn align_to(value: usize, align: usize) -> usize {
    if align == 0 {
        return value;
    }
    let rem = value % align;
    if rem == 0 {
        value
    } else {
        value + (align - rem)
    }
}

fn literal_usize(expr: &ast::Expression) -> Option<usize> {
    match expr.kind.as_ref() {
        ast::ExpressionKind::Literal(ast::Literal::Integer(value)) => usize::try_from(*value).ok(),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::{QuickCheck, TestResult};

    #[test]
    fn primitive_layouts_have_sizes() {
        let ctx = TypeContext::default();
        let ty = Type::Primitive(ast::PrimitiveType::I32);
        let layout = ctx.layout_of(&ty);
        assert_eq!(layout.size, Some(4));
        assert_eq!(layout.align, Some(4));
    }

    #[test]
    fn tuple_layout_aligns() {
        let ctx = TypeContext::default();
        let ty = Type::Tuple(vec![
            Type::Primitive(ast::PrimitiveType::I8),
            Type::Primitive(ast::PrimitiveType::I32),
        ]);
        let layout = ctx.layout_of(&ty);
        assert_eq!(layout.align, Some(4));
        assert_eq!(layout.size, Some(8));
    }

    #[test]
    fn struct_packed_layout_has_align_one() {
        let ctx = TypeContext::default();
        let fields = vec![
            Type::Primitive(ast::PrimitiveType::I8),
            Type::Primitive(ast::PrimitiveType::I32),
        ];
        let attrs = StructAttributes {
            packed: true,
            repr_c: true,
            align: None,
        };
        let layout = struct_layout(&ctx, &fields, &attrs);
        assert_eq!(layout.align, Some(1));
        assert_eq!(layout.size, Some(5));
    }

    #[test]
    fn struct_align_attribute_overrides_alignment() {
        let ctx = TypeContext::default();
        let fields = vec![
            Type::Primitive(ast::PrimitiveType::I32),
            Type::Primitive(ast::PrimitiveType::I32),
        ];
        let attrs = StructAttributes {
            packed: false,
            repr_c: true,
            align: Some(16),
        };
        let layout = struct_layout(&ctx, &fields, &attrs);
        assert_eq!(layout.align, Some(16));
        assert_eq!(layout.size, Some(16));
    }

    #[test]
    fn struct_default_layout_is_c_like() {
        let ctx = TypeContext::default();
        let fields = vec![
            Type::Primitive(ast::PrimitiveType::I8),
            Type::Primitive(ast::PrimitiveType::I32),
        ];
        let attrs = StructAttributes {
            packed: false,
            repr_c: false,
            align: None,
        };
        let layout = struct_layout(&ctx, &fields, &attrs);
        assert_eq!(layout.align, Some(4));
        assert_eq!(layout.size, Some(8));
    }

    #[test]
    fn array_layout_uses_length() {
        let ctx = TypeContext::default();
        let ty = Type::Array {
            element: Box::new(Type::Primitive(ast::PrimitiveType::I32)),
            length: Some(4),
        };
        let layout = ctx.layout_of(&ty);
        assert_eq!(layout.align, Some(4));
        assert_eq!(layout.size, Some(16));
    }

    #[test]
    fn prop_numeric_type_completeness() {
        fn property(primitive: u8) -> TestResult {
            let primitives = [
                ast::PrimitiveType::I8,
                ast::PrimitiveType::I16,
                ast::PrimitiveType::I32,
                ast::PrimitiveType::I64,
                ast::PrimitiveType::I128,
                ast::PrimitiveType::U8,
                ast::PrimitiveType::U16,
                ast::PrimitiveType::U32,
                ast::PrimitiveType::U64,
                ast::PrimitiveType::U128,
                ast::PrimitiveType::F32,
                ast::PrimitiveType::F64,
                ast::PrimitiveType::F80,
                ast::PrimitiveType::C32,
                ast::PrimitiveType::C64,
                ast::PrimitiveType::C80,
            ];
            let index = (primitive as usize) % primitives.len();
            let ty = Type::Primitive(primitives[index].clone());
            let ctx = TypeContext::default();
            let layout = ctx.layout_of(&ty);
            if !is_numeric(&ty) {
                return TestResult::failed();
            }
            if layout.size.is_none() || layout.align.is_none() {
                return TestResult::failed();
            }
            TestResult::passed()
        }

        QuickCheck::new()
            .tests(100)
            .quickcheck(property as fn(u8) -> TestResult);
    }
}
