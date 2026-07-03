use agc::parser::ast::{self};
use std::collections::HashMap;

pub(crate) fn format_primitive_type(p: &ast::PrimitiveType) -> &'static str {
    match p {
        ast::PrimitiveType::I8 => "i8",
        ast::PrimitiveType::I16 => "i16",
        ast::PrimitiveType::I32 => "i32",
        ast::PrimitiveType::I64 => "i64",
        ast::PrimitiveType::I128 => "i128",
        ast::PrimitiveType::U8 => "u8",
        ast::PrimitiveType::U16 => "u16",
        ast::PrimitiveType::U32 => "u32",
        ast::PrimitiveType::U64 => "u64",
        ast::PrimitiveType::U128 => "u128",
        ast::PrimitiveType::F32 => "f32",
        ast::PrimitiveType::F64 => "f64",
        ast::PrimitiveType::F80 => "f80",
        ast::PrimitiveType::C32 => "c32",
        ast::PrimitiveType::C64 => "c64",
        ast::PrimitiveType::C80 => "c80",
        ast::PrimitiveType::Bool => "bool",
        ast::PrimitiveType::Str => "str",
        ast::PrimitiveType::Char => "char",
        ast::PrimitiveType::Void => "void",
    }
}

pub(crate) fn format_type(ty: &ast::Type) -> String {
    match &*ty.kind {
        ast::TypeKind::Primitive(p) => format_primitive_type(p).to_string(),
        ast::TypeKind::Named(n) => n
            .path
            .iter()
            .map(|i| i.name.clone())
            .collect::<Vec<_>>()
            .join("::"),
        ast::TypeKind::Generic(g) => g.name.name.clone(),
        ast::TypeKind::Reference(r) => {
            if r.is_mutable {
                format!("&mut {}", format_type(&r.inner))
            } else {
                format!("&{}", format_type(&r.inner))
            }
        }
        ast::TypeKind::Pointer(p) => format!("{}*", format_type(&p.inner)),
        ast::TypeKind::Optional(t) => format!("?{}", format_type(t)),
        ast::TypeKind::Slice(s) => format!("[{}]", format_type(&s.element_type)),
        ast::TypeKind::Array(a) => format!("[{}; {}]", format_type(&a.element_type), a.size),
        ast::TypeKind::Function(f) => {
            let params: Vec<String> = f.parameters.iter().map(|t| format_type(t)).collect();
            format!("fn({}) -> {}", params.join(", "), format_type(&f.return_type))
        }
        ast::TypeKind::Tuple(t) => {
            let items: Vec<String> = t.iter().map(|ty| format_type(ty)).collect();
            format!("({})", items.join(", "))
        }
    }
}

pub(crate) fn format_params(params: &[ast::Parameter]) -> String {
    params
        .iter()
        .map(|p| format!("{}: {}", p.name.name, format_type(&p.param_type)))
        .collect::<Vec<_>>()
        .join(", ")
}

pub(crate) fn format_generics(g: &ast::Generics) -> String {
    if g.params.is_empty() {
        return String::new();
    }
    let items: Vec<String> = g
        .params
        .iter()
        .map(|p| match p {
            ast::GenericParam::Type(tp) => {
                let mut s = tp.name.name.clone();
                if !tp.bounds.is_empty() {
                    let bounds: Vec<String> = tp.bounds.iter()
                        .map(|b| b.trait_ref.path.iter().map(|id| id.name.clone()).collect::<Vec<_>>().join("::"))
                        .collect();
                    s.push_str(&format!(": {}", bounds.join(" + ")));
                }
                s
            }
            ast::GenericParam::Lifetime(lp) => format!("'{}", lp.name.name),
        })
        .collect();
    format!("<{}>", items.join(", "))
}

pub(crate) fn primitive_size(p: &ast::PrimitiveType) -> Option<usize> {
    match p {
        ast::PrimitiveType::I8 | ast::PrimitiveType::U8 | ast::PrimitiveType::Bool | ast::PrimitiveType::Char => Some(1),
        ast::PrimitiveType::I16 | ast::PrimitiveType::U16 => Some(2),
        ast::PrimitiveType::I32 | ast::PrimitiveType::U32 | ast::PrimitiveType::F32 => Some(4),
        ast::PrimitiveType::I64 | ast::PrimitiveType::U64 | ast::PrimitiveType::F64 | ast::PrimitiveType::C32 => Some(8),
        ast::PrimitiveType::I128 | ast::PrimitiveType::U128 | ast::PrimitiveType::F80 | ast::PrimitiveType::C64 => Some(16),
        ast::PrimitiveType::C80 => Some(32),
        ast::PrimitiveType::Str | ast::PrimitiveType::Void => None,
    }
}

pub(crate) fn field_type_size(ty: &ast::Type, struct_map: &HashMap<String, usize>) -> Option<usize> {
    match &*ty.kind {
        ast::TypeKind::Primitive(p) => primitive_size(p),
        ast::TypeKind::Reference(_) | ast::TypeKind::Pointer(_) => Some(8),
        ast::TypeKind::Named(n) => {
            let name: Vec<&str> = n.path.iter().map(|i| i.name.as_str()).collect();
            struct_map.get(&name.join("::")).copied()
        }
        ast::TypeKind::Optional(t) => field_type_size(t, struct_map).map(|s| s + 1),
        ast::TypeKind::Slice(_) => None,
        ast::TypeKind::Array(a) => field_type_size(&a.element_type, struct_map).map(|s| s * a.size as usize),
        _ => None,
    }
}

/// Check if a struct has the `#[packed]` or `#[repr(packed)]` attribute.
pub(crate) fn is_repr_packed(attrs: &[ast::Attribute]) -> bool {
    attrs.iter().any(|a| {
        a.name.name == "packed"
            || (a.name.name == "repr"
                && a.args.iter().any(|arg| matches!(arg, ast::AttributeArg::Identifier(id) if id.name == "packed")))
    })
}

pub(crate) fn compute_struct_size(s: &ast::StructItem, struct_map: &HashMap<String, usize>, packed: bool) -> Option<usize> {
    let mut total = 0usize;
    for field in &s.fields {
        let size = field_type_size(&field.field_type, struct_map)?;
        if packed {
            total += size;
        } else {
            let align = size.next_power_of_two().min(16);
            total = (total + align - 1) / align * align;
            total += size;
        }
    }
    Some(total)
}

pub(crate) fn format_attribute(attr: &ast::Attribute) -> String {
    let name = attr.name.name.clone();
    let args: Vec<String> = attr.args.iter().map(|arg| match arg {
        ast::AttributeArg::Identifier(id) => id.name.clone(),
        ast::AttributeArg::Literal(lit) => format!("{lit:?}"),
    }).collect();
    if args.is_empty() {
        format!("#[{name}]")
    } else {
        format!("#[{}({})]", name, args.join(", "))
    }
}

pub(crate) fn format_struct_hover(
    s: &ast::StructItem,
    attrs: &[ast::Attribute],
    name: &str,
    struct_map: &HashMap<String, usize>,
) -> String {
    let mut parts = Vec::new();
    for attr in attrs {
        parts.push(format_attribute(attr));
    }
    let fields: Vec<String> = s
        .fields
        .iter()
        .map(|f| format!("{}: {}", f.name.name, format_type(&f.field_type)))
        .collect();
    parts.push(format!("struct {} {{ {} }}", name, fields.join("; ")));
    let packed = is_repr_packed(attrs);
    if let Some(size) = compute_struct_size(s, struct_map, packed) {
        if packed {
            parts.push(format!("packed size: {size} bytes"));
        } else {
            parts.push(format!("size: {size} bytes"));
        }
    } else {
        parts.push("size: ? (unsized or opaque)".to_string());
    }
    parts.join("\n")
}

pub(crate) fn format_function_sig_from_parts(
    name: &str,
    generics: Option<&ast::Generics>,
    params: &[ast::Parameter],
    return_type: Option<&ast::Type>,
) -> String {
    let g = generics.map(format_generics).unwrap_or_default();
    let p = format_params(params);
    let ret = return_type
        .map(|t| format_type(t))
        .unwrap_or_else(|| "void".to_string());
    format!("{ret} {name}{g}({p})")
}

pub(crate) fn format_function_sig(f: &ast::FunctionItem) -> String {
    format_function_sig_from_parts(
        &f.name.name,
        f.generics.as_ref(),
        &f.parameters,
        f.return_type.as_ref(),
    )
}

pub(crate) fn format_impl_function_sig(f: &ast::ImplFunction) -> String {
    format_function_sig_from_parts(
        &f.name.name,
        f.generics.as_ref(),
        &f.parameters,
        f.return_type.as_ref(),
    )
}

pub(crate) fn format_extern_function_sig(f: &ast::ExternFunctionItem) -> String {
    let linkage = match f.linkage {
        ast::ExternLinkage::C => "C",
        _ => "unknown",
    };
    let params = format_params(&f.signature.parameters);
    let ret = f
        .signature
        .return_type
        .as_ref()
        .map(|t| format_type(t))
        .unwrap_or_else(|| "void".to_string());
    let variadic = if f.signature.is_variadic { ", ..." } else { "" };
    format!("extern \"{linkage}\" {ret} {}({params}{variadic})", f.name.name)
}

pub(crate) fn format_item_hover(item: &ast::Item) -> Option<String> {
    match &item.kind {
        ast::ItemKind::Function(f) => Some(format_function_sig(f)),
        ast::ItemKind::Macro(m) => {
            let params = format_params(&m.parameters);
            Some(format!("macro {}({})", m.name.name, params))
        }
        ast::ItemKind::GlobalVariable(v) => {
            let ty = format_type(&v.var_type);
            if v.initializer.is_some() {
                Some(format!("{ty} {} = ...", v.name.name))
            } else {
                Some(format!("{ty} {}", v.name.name))
            }
        }
        ast::ItemKind::Enum(e) => {
            let variants: Vec<String> = e
                .variants
                .iter()
                .map(|v| match &v.data {
                    ast::EnumVariantData::Unit => v.name.name.clone(),
                    ast::EnumVariantData::Tuple(types) => {
                        let ts: Vec<String> = types.iter().map(|t| format_type(t)).collect();
                        format!("{}({})", v.name.name, ts.join(", "))
                    }
                    ast::EnumVariantData::Struct(fields) => {
                        let fs: Vec<String> = fields
                            .iter()
                            .map(|f| format!("{}: {}", f.name.name, format_type(&f.field_type)))
                            .collect();
                        format!("{} {{ {} }}", v.name.name, fs.join("; "))
                    }
                })
                .collect();
            Some(format!("enum {} {{ {} }}", e.name.name, variants.join("; ")))
        }
        ast::ItemKind::TypeAlias(a) => {
            Some(format!("type {} = {}", a.name.name, format_type(&a.type_def)))
        }
        ast::ItemKind::ExternFunction(f) => Some(format_extern_function_sig(f)),
        ast::ItemKind::ExternVariable(v) => {
            Some(format!("extern {} {}", format_type(&v.var_type), v.name.name))
        }
        ast::ItemKind::Trait(t) => Some(format!("trait {}", t.name.name)),
        ast::ItemKind::Struct(_) | ast::ItemKind::Impl(_) => None,
        ast::ItemKind::Import(_) | ast::ItemKind::ExternBlock(_) => None,
    }
}
