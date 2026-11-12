# tree-sitter-silver

Tree-sitter grammar for [Silver](https://github.com/CierCier/silver).

The grammar parses items precisely: imports, structs, enums, traits, `impl`
blocks, functions, macros, globals, externs, attributes, and top-level
`if (@cfg(...))` blocks, along with their types and parameter lists. Function
bodies and initializers are balanced token groups. Outlines, folding, and
indentation stay exact while a body is half-typed. Expression-level meaning
comes from the language server's semantic tokens.

A small external scanner (`src/scanner.c`) handles what a token rule
cannot. Block comments nest, as in the compiler. `shared` and `lock` are
keywords only by what follows them, `shared T` and `lock place {`, so
`m.lock()` and a variable named `lock` stay identifiers.

```sh
npm ci
npm run generate   # rewrites src/ from grammar.js (tree-sitter CLI 0.26.8, ABI 14)
npm test           # corpus tests in test/corpus, highlight tests in test/highlight
```

Highlight tests use the Zed queries in
[`../zed/languages/silver/highlights.scm`](../zed/languages/silver/highlights.scm).
