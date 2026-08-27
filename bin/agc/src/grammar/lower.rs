//! Lowers an Elise `SourceGraph` into a strongly-typed Silver `ast::Program`.

use crate::lexer::Span;
use crate::parser::ast::*;
use elise_core::{NodeRef, SourceGraph};

use super::parser::NodeKind;

/// Lowers a `SourceGraph` produced by `parse_ag` into a Silver `ast::Program`.
pub fn lower_source_graph(graph: &SourceGraph, _file_id: usize) -> Program {
    let root = graph.root();
    let mut items = Vec::new();
    let comments = Vec::new();

    let root_span = Span::new(root.span().0, root.span().1);

    // Collect top-level items
    for child in root.children() {
        if let Some(kind) = NodeKind::from_u16(child.kind()) {
            match kind {
                NodeKind::Import => {
                    if let Some(item) = lower_import(&child) {
                        items.push(item);
                    }
                }
                NodeKind::Function => {
                    if let Some(item) = lower_function(&child) {
                        items.push(item);
                    }
                }
                NodeKind::Struct => {
                    if let Some(item) = lower_struct(&child) {
                        items.push(item);
                    }
                }
                NodeKind::Enum => {
                    if let Some(item) = lower_enum(&child) {
                        items.push(item);
                    }
                }
                NodeKind::Trait => {
                    if let Some(item) = lower_trait(&child) {
                        items.push(item);
                    }
                }
                NodeKind::Impl => {
                    if let Some(item) = lower_impl(&child) {
                        items.push(item);
                    }
                }
                NodeKind::GlobalVariable => {
                    if let Some(item) = lower_global_variable(&child) {
                        items.push(item);
                    }
                }
                NodeKind::TypeAlias => {
                    if let Some(item) = lower_type_alias(&child) {
                        items.push(item);
                    }
                }
                NodeKind::ExternDecl | NodeKind::ExternBlock => {
                    if let Some(item) = lower_extern(&child) {
                        items.push(item);
                    }
                }
                NodeKind::Macro => {
                    if let Some(item) = lower_macro(&child) {
                        items.push(item);
                    }
                }
                _ => {}
            }
        }
    }

    Program {
        attributes: Vec::new(),
        items,
        comments,
        span: root_span,
    }
}

fn lower_import(node: &NodeRef) -> Option<Item> {
    let text = node.text().trim();
    let span = Span::new(node.span().0, node.span().1);

    let import_str = text
        .trim_start_matches("pub")
        .trim_start_matches("import")
        .trim()
        .trim_end_matches(';')
        .trim();

    let mut path_idents = Vec::new();
    let mut selection = None;

    if let Some(brace_idx) = import_str.find('{') {
        let (prefix, suffix) = import_str.split_at(brace_idx);
        for seg in prefix.trim().split('.') {
            let seg = seg.trim();
            if !seg.is_empty() {
                path_idents.push(Identifier {
                    name: seg.to_string(),
                    span,
                });
            }
        }
        let list = suffix.trim_matches(|c| c == '{' || c == '}').trim();
        let mut names = Vec::new();
        for item in list.split(',') {
            let item = item.trim();
            if !item.is_empty() {
                let parts: Vec<&str> = item.split_whitespace().collect();
                if parts.len() == 3 && parts[1] == "as" {
                    names.push(ImportedName {
                        name: Identifier {
                            name: parts[0].to_string(),
                            span,
                        },
                        local_name: Identifier {
                            name: parts[2].to_string(),
                            span,
                        },
                    });
                } else {
                    names.push(ImportedName {
                        name: Identifier {
                            name: item.to_string(),
                            span,
                        },
                        local_name: Identifier {
                            name: item.to_string(),
                            span,
                        },
                    });
                }
            }
        }
        selection = Some(names);
    } else {
        for seg in import_str.split('.') {
            let seg = seg.trim();
            if !seg.is_empty() {
                path_idents.push(Identifier {
                    name: seg.to_string(),
                    span,
                });
            }
        }
    }

    let is_pub = text.starts_with("pub");

    Some(Item {
        kind: ItemKind::Import(ImportItem {
            path: path_idents,
            selection,
        }),
        span,
        visibility: if is_pub { Visibility::Public } else { Visibility::Private },
        attributes: Vec::new(),
    })
}

fn lower_function(node: &NodeRef) -> Option<Item> {
    let span = Span::new(node.span().0, node.span().1);
    let text = node.text();
    let is_pub = text.starts_with("pub");

    // Extract function name: find open paren '(' and get preceding identifier token
    let header = if let Some(brace_idx) = text.find('{') {
        &text[..brace_idx]
    } else {
        text
    };

    let (fn_name, ret_type) = parse_fn_header(header, span);

    Some(Item {
        kind: ItemKind::Function(FunctionItem {
            name: Identifier {
                name: fn_name,
                span,
            },
            generics: None,
            is_variadic: false,
            parameters: Vec::new(),
            return_type: Some(ret_type),
            body: Block {
                statements: Vec::new(),
                span,
            },
        }),
        span,
        visibility: if is_pub { Visibility::Public } else { Visibility::Private },
        attributes: Vec::new(),
    })
}

fn parse_fn_header(header: &str, span: Span) -> (String, Type) {
    let trimmed = header
        .trim_start_matches("pub")
        .trim_start_matches("async")
        .trim();

    if let Some(paren_idx) = trimmed.find('(') {
        let before_paren = trimmed[..paren_idx].trim();
        let parts: Vec<&str> = before_paren.split_whitespace().collect();
        if parts.len() >= 2 {
            let name = parts[parts.len() - 1].to_string();
            let ret_name = parts[..parts.len() - 1].join(" ");
            let ret_type = make_type_from_str(&ret_name, span);
            return (name, ret_type);
        } else if parts.len() == 1 {
            return (parts[0].to_string(), make_primitive_type(PrimitiveType::Void, span));
        }
    }

    ("fn".to_string(), make_primitive_type(PrimitiveType::Void, span))
}

fn lower_struct(node: &NodeRef) -> Option<Item> {
    let span = Span::new(node.span().0, node.span().1);
    let text = node.text();
    let is_pub = text.starts_with("pub");

    let struct_str = text.trim_start_matches("pub").trim_start_matches("struct").trim();
    let name = struct_str
        .split(|c: char| c.is_whitespace() || c == '<' || c == '{')
        .next()
        .unwrap_or("Struct")
        .to_string();

    Some(Item {
        kind: ItemKind::Struct(StructItem {
            name: Identifier {
                name,
                span,
            },
            generics: None,
            fields: Vec::new(),
        }),
        span,
        visibility: if is_pub { Visibility::Public } else { Visibility::Private },
        attributes: Vec::new(),
    })
}

fn lower_enum(node: &NodeRef) -> Option<Item> {
    let span = Span::new(node.span().0, node.span().1);
    let text = node.text();
    let is_pub = text.starts_with("pub");

    let enum_str = text.trim_start_matches("pub").trim_start_matches("enum").trim();
    let name = enum_str
        .split(|c: char| c.is_whitespace() || c == '<' || c == '{' || c == ':')
        .next()
        .unwrap_or("Enum")
        .to_string();

    Some(Item {
        kind: ItemKind::Enum(EnumItem {
            name: Identifier {
                name,
                span,
            },
            generics: None,
            variants: Vec::new(),
        }),
        span,
        visibility: if is_pub { Visibility::Public } else { Visibility::Private },
        attributes: Vec::new(),
    })
}

fn lower_trait(node: &NodeRef) -> Option<Item> {
    let span = Span::new(node.span().0, node.span().1);
    let text = node.text();
    let is_pub = text.starts_with("pub");

    let trait_str = text.trim_start_matches("pub").trim_start_matches("trait").trim();
    let name = trait_str
        .split(|c: char| c.is_whitespace() || c == '<' || c == '{' || c == ':')
        .next()
        .unwrap_or("Trait")
        .to_string();

    Some(Item {
        kind: ItemKind::Trait(TraitItem {
            name: Identifier {
                name,
                span,
            },
            generics: None,
            super_traits: Vec::new(),
            items: Vec::new(),
        }),
        span,
        visibility: if is_pub { Visibility::Public } else { Visibility::Private },
        attributes: Vec::new(),
    })
}

fn lower_impl(node: &NodeRef) -> Option<Item> {
    let span = Span::new(node.span().0, node.span().1);
    let text = node.text();

    let impl_str = text.trim_start_matches("impl").trim();
    let header = if let Some(brace_idx) = impl_str.find('{') {
        &impl_str[..brace_idx]
    } else {
        impl_str
    };

    let (trait_ref, target_type) = if let Some(for_idx) = header.find(" for ") {
        let trait_name = header[..for_idx].trim().to_string();
        let target_name = header[for_idx + 5..].trim().to_string();
        (
            Some(TraitRef {
                path: vec![Identifier {
                    name: trait_name,
                    span,
                }],
                generics: None,
                span,
            }),
            make_type_from_str(&target_name, span),
        )
    } else {
        (None, make_type_from_str(header.trim(), span))
    };

    Some(Item {
        kind: ItemKind::Impl(ImplItem {
            generics: None,
            trait_ref,
            self_type: target_type,
            items: Vec::new(),
            implicit_type_params: Vec::new(),
        }),
        span,
        visibility: Visibility::Private,
        attributes: Vec::new(),
    })
}

fn lower_global_variable(node: &NodeRef) -> Option<Item> {
    let span = Span::new(node.span().0, node.span().1);
    let text = node.text();
    let is_pub = text.starts_with("pub");

    let clean = text
        .trim_start_matches("pub")
        .trim_start_matches("mut")
        .trim_start_matches("const")
        .trim_start_matches("static")
        .trim_start_matches("volatile")
        .trim()
        .trim_end_matches(';')
        .trim();

    let (type_part, name_part) = if let Some(eq_idx) = clean.find('=') {
        let left = clean[..eq_idx].trim();
        let parts: Vec<&str> = left.split_whitespace().collect();
        if parts.len() >= 2 {
            (parts[..parts.len() - 1].join(" "), parts[parts.len() - 1].to_string())
        } else {
            ("i32".to_string(), left.to_string())
        }
    } else {
        let parts: Vec<&str> = clean.split_whitespace().collect();
        if parts.len() >= 2 {
            (parts[..parts.len() - 1].join(" "), parts[parts.len() - 1].to_string())
        } else {
            ("i32".to_string(), clean.to_string())
        }
    };

    Some(Item {
        kind: ItemKind::GlobalVariable(GlobalVariableItem {
            name: Identifier {
                name: name_part,
                span,
            },
            var_type: make_type_from_str(&type_part, span),
            initializer: None,
            is_mutable: text.contains("mut "),
            is_static: text.contains("static "),
            is_volatile: text.contains("volatile "),
        }),
        span,
        visibility: if is_pub { Visibility::Public } else { Visibility::Private },
        attributes: Vec::new(),
    })
}

fn lower_type_alias(node: &NodeRef) -> Option<Item> {
    let span = Span::new(node.span().0, node.span().1);
    let text = node.text();
    let is_pub = text.starts_with("pub");

    let clean = text.trim_start_matches("pub").trim_start_matches("type").trim().trim_end_matches(';').trim();
    let (name, target) = if let Some(eq_idx) = clean.find('=') {
        (clean[..eq_idx].trim().to_string(), clean[eq_idx + 1..].trim().to_string())
    } else {
        ("Alias".to_string(), "void".to_string())
    };

    Some(Item {
        kind: ItemKind::TypeAlias(TypeAliasItem {
            name: Identifier {
                name,
                span,
            },
            type_def: make_type_from_str(&target, span),
        }),
        span,
        visibility: if is_pub { Visibility::Public } else { Visibility::Private },
        attributes: Vec::new(),
    })
}

fn lower_extern(node: &NodeRef) -> Option<Item> {
    let span = Span::new(node.span().0, node.span().1);

    Some(Item {
        kind: ItemKind::ExternBlock(ExternBlockItem {
            linkage: ExternLinkage::C,
            functions: Vec::new(),
            variables: Vec::new(),
        }),
        span,
        visibility: Visibility::Private,
        attributes: Vec::new(),
    })
}

fn lower_macro(node: &NodeRef) -> Option<Item> {
    let span = Span::new(node.span().0, node.span().1);
    let text = node.text();

    let clean = text.trim_start_matches("macro").trim();
    let name = clean
        .split(|c: char| c.is_whitespace() || c == '(' || c == '{')
        .next()
        .unwrap_or("macro")
        .to_string();

    Some(Item {
        kind: ItemKind::Macro(MacroDef {
            name: Identifier {
                name,
                span,
            },
            parameters: Vec::new(),
            body: Block {
                statements: Vec::new(),
                span,
            },
        }),
        span,
        visibility: Visibility::Private,
        attributes: Vec::new(),
    })
}

fn make_primitive_type(prim: PrimitiveType, span: Span) -> Type {
    Type {
        kind: Box::new(TypeKind::Primitive(prim)),
        span,
    }
}

fn make_type_from_str(s: &str, span: Span) -> Type {
    let trimmed = s.trim();
    let prim = match trimmed {
        "i8" => Some(PrimitiveType::I8),
        "i16" => Some(PrimitiveType::I16),
        "i32" => Some(PrimitiveType::I32),
        "i64" => Some(PrimitiveType::I64),
        "i128" => Some(PrimitiveType::I128),
        "u8" => Some(PrimitiveType::U8),
        "u16" => Some(PrimitiveType::U16),
        "u32" => Some(PrimitiveType::U32),
        "u64" => Some(PrimitiveType::U64),
        "u128" => Some(PrimitiveType::U128),
        "f32" => Some(PrimitiveType::F32),
        "f64" => Some(PrimitiveType::F64),
        "f80" => Some(PrimitiveType::F80),
        "bool" => Some(PrimitiveType::Bool),
        "char" => Some(PrimitiveType::Char),
        "str" => Some(PrimitiveType::Str),
        "void" => Some(PrimitiveType::Void),
        _ => None,
    };

    if let Some(p) = prim {
        make_primitive_type(p, span)
    } else {
        Type {
            kind: Box::new(TypeKind::Named(NamedType {
                path: vec![Identifier {
                    name: trimmed.to_string(),
                    span,
                }],
                generics: None,
            })),
            span,
        }
    }
}
