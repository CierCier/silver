//! Built-in compiler macros and statement template snippets.

use tower_lsp_server::ls_types::*;

pub(crate) const KEYWORDS_TOP_LEVEL: &[&str] = &[
    "struct", "enum", "impl", "trait", "fn", "const", "static", "pub", "extern", "import", "macro",
];

pub(crate) const PRIMITIVE_TYPES: &[&str] = &[
    "i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128", "f32", "f64", "f80",
    "c32", "c64", "c80", "bool", "str", "char", "void",
];

pub(crate) const GENERIC_TYPE_SNIPPETS: &[(&str, &str, &str)] = &[
    ("Vec<T>", "Vec<${1:T}>", "Growable heap-allocated vector"),
    ("Option<T>", "Option<${1:T}>", "Optional value (Some or None)"),
    ("Result<T, E>", "Result<${1:T}, ${2:E}>", "Result type (Ok or Err)"),
    ("HashMap<K, V>", "HashMap<${1:K}, ${2:V}>", "Hash map key-value store"),
    ("HashSet<T>", "HashSet<${1:T}>", "Hash set collection"),
    ("Box<T>", "Box<${1:T}>", "Heap-allocated unique ownership pointer"),
    ("Rc<T>", "Rc<${1:T}>", "Reference-counted shared ownership pointer"),
    ("Deque<T>", "Deque<${1:T}>", "Double-ended queue"),
];

pub(crate) const MACRO_BUILTINS: &[(&str, &str, &str, &str)] = &[
    (
        "@println",
        "@println(\"{}\", val);",
        "println(\"${1:{}\", ${2:val});$0",
        "Prints formatted text followed by a newline to stdout.",
    ),
    (
        "@print",
        "@print(\"{}\", val);",
        "print(\"${1:{}\", ${2:val});$0",
        "Prints formatted text to stdout without a trailing newline.",
    ),
    (
        "@format",
        "@format(\"{}\", val)",
        "format(\"${1:{}\", ${2:val})",
        "Formats arguments into an owned String on the heap.",
    ),
    (
        "@assert",
        "@assert(condition, \"message\");",
        "assert(${1:condition});$0",
        "Asserts a condition in debug builds; aborted with backtrace on failure.",
    ),
    (
        "@size",
        "@size(Type)",
        "size(${1:Type})",
        "Returns the byte size of a type or struct layout.",
    ),
    (
        "@align",
        "@align(Type)",
        "align(${1:Type})",
        "Returns the memory alignment in bytes of a type or struct.",
    ),
    (
        "@json",
        "@json(value)",
        "json(${1:value})",
        "Serializes a struct to a JSON String using synthesized or explicit ToJson.",
    ),
    (
        "@from_json",
        "@from_json<Type>(json_str)",
        "from_json<${1:Type}>(${2:json_str})",
        "Deserializes a JSON string into Result<Type, JsonError>.",
    ),
    (
        "@cfg",
        "@cfg(key)",
        "cfg(${1:debug})",
        "Checks compile-time cfg flag condition.",
    ),
    (
        "@hash",
        "@hash(value)",
        "hash(${1:value})",
        "Computes the 64-bit hash of a value.",
    ),
];

pub(crate) const TOP_LEVEL_SNIPPETS: &[(&str, &str, &str, &str)] = &[
    (
        "fn",
        "fn name(params) ret { ... }",
        "fn ${1:name}(${2:params}) ${3:void} {\n    $0\n}",
        "Function definition",
    ),
    (
        "struct",
        "struct Name { ... }",
        "struct ${1:Name} {\n    ${2:field};\n}",
        "Struct definition",
    ),
    (
        "enum",
        "enum Name { ... }",
        "enum ${1:Name} {\n    ${2:Variant},\n}",
        "Enum definition",
    ),
    (
        "impl",
        "impl Name { ... }",
        "impl ${1:Name} {\n    $0\n}",
        "Methods implementation block",
    ),
    (
        "impl-trait",
        "impl Trait for Type { ... }",
        "impl ${1:Trait} for ${2:Type} {\n    $0\n}",
        "Trait implementation block",
    ),
    (
        "trait",
        "trait Name { ... }",
        "trait ${1:Name} {\n    $0\n}",
        "Trait definition",
    ),
    (
        "test",
        "#[test] void test_name() { ... }",
        "#[test]\nvoid test_${1:name}() {\n    $0\n}",
        "Unit test function",
    ),
];

pub(crate) const CONTROL_FLOW_SNIPPETS: &[(&str, &str, &str, &str)] = &[
    (
        "if",
        "if (condition) { ... }",
        "if (${1:condition}) {\n    $0\n}",
        "If statement",
    ),
    (
        "if-else",
        "if (condition) { ... } else { ... }",
        "if (${1:condition}) {\n    $1\n} else {\n    $0\n}",
        "If-else statement",
    ),
    (
        "while",
        "while (condition) { ... }",
        "while (${1:condition}) {\n    $0\n}",
        "While loop",
    ),
    (
        "for",
        "for (init; cond; step) { ... }",
        "for (${1:i32 i = 0}; ${2:i < count}; ${3:i = i + 1}) {\n    $0\n}",
        "Index-based for loop",
    ),
    (
        "for-in",
        "for (item in iterable) { ... }",
        "for (${1:item} in ${2:iterable}) {\n    $0\n}",
        "Iterator for-in loop",
    ),
    (
        "match",
        "match expr { ... }",
        "match ${1:expr} {\n    $0\n}",
        "Pattern match statement",
    ),
    (
        "defer",
        "defer stmt;",
        "defer ${1:cleanup()};$0",
        "Deferred cleanup execution at scope exit",
    ),
    (
        "return",
        "return expr;",
        "return ${1:0};$0",
        "Return from function",
    ),
];

pub(crate) fn macro_completions(prefix: &str) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    for (name, detail, snippet, doc_str) in MACRO_BUILTINS {
        if name.starts_with(prefix) {
            let insert_snippet = if prefix.starts_with('@') {
                snippet.to_string()
            } else {
                format!("@{}", snippet)
            };
            items.push(CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(detail.to_string()),
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: doc_str.to_string(),
                })),
                insert_text: Some(insert_snippet),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                sort_text: Some(format!("0_{}", name)),
                ..Default::default()
            });
        }
    }
    items
}
