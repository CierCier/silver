# User macro contract

Declare a macro with `macro ReturnType name(parameters) { body }` and invoke it
with `@name(arguments)`. `@name<T>(arguments)` supplies explicit type arguments;
otherwise generic inference uses the result context and supported argument forms.
Built-in macros such as `@format` have their own compiler-defined contracts.

## Substitution and evaluation

User macros expand expressions at compile time. They do not evaluate arguments
as a function call would. Each use of a parameter substitutes its argument
expression. Repeating the parameter repeats evaluation; an unused parameter
does not evaluate its argument. An argument used once inside an untaken branch
also does not run. Parameter annotations guide inference and the expanded body;
they do not provide function-style argument validation for unused arguments.

```silver
macro i32 twice(i32 value) { return value + value; }
macro i32 ignore(i32 value) { return 7; }
```

`@twice(next())` calls `next()` twice. `@ignore(next())` does not call it.
Use a normal function when callers require exactly one evaluation. Expansion
does not exempt the resulting code from type, ownership, or borrow checks.

Non-variadic macros require exactly the declared argument count. A variadic
macro requires all fixed arguments and accepts zero or more trailing arguments.
Both compilers reject extra and missing arguments at the call site. Stage0
may constant-fold supported pure expressions; this does not change runtime
side-effect counts.

## Variadics

Only the final parameter may be variadic, written `T... values`.

| Operation | Meaning |
| --- | --- |
| `values.len` | Compile-time argument count; no argument evaluates |
| `values[0]` | Substitute only that argument, using a constant nonnegative index |
| `for value in values { ... }` | Unroll the body in source argument order |

An empty variadic loop emits no iterations. This is not runtime iteration or
general argument splicing. Out-of-range indexing is unsupported; do not depend
on a particular diagnostic across the compilers.

## Compiler limits

Stage1 supports expression macros with a direct return expression, a final
return after supported statements, or an `if`/`else` with returning branches.
Statement macros contain no `return`. Nested macro calls work, with an expansion
depth limit of 64 in stage1 and 128 in stage0. Recursion is bounded compilation,
not a runtime call mechanism.

Stage0 renames local bindings during expansion. Stage1 does not implement full
hygienic renaming; neither compiler promises definition-site name resolution.
Free names resolve in the expanded context, and local shadowing is not a
portable macro technique. Keep macro locals scoped and distinctive. Avoid
depending on stage0's broader compile-time evaluator when targeting stage1.

The stdlib exercises macros through formatting and JSON serialization.
See `tests/format_bytes_test.ag`, `tests/fmt_test.ag`, and `tests/json_test.ag`.
Dedicated language macro fixtures are retired under the stdlib-only test policy.
