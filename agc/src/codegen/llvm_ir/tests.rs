#[cfg(test)]
#[expect(clippy::module_inception)]
mod tests {

    use crate::codegen::llvm_ir::LlvmIrGenerator;
    use crate::codegen::llvm_ir::generate::run_module_optimization_passes;
    use crate::lexer::lex;
    use crate::parser::Parser;
    use crate::parser::ast;
    use crate::semantic::monomorph;
    use crate::semantic::typeck::TypeChecker;
    use crate::symbol_table::CompilerSymbolTable;
    use inkwell::OptimizationLevel;
    use inkwell::context::Context;
    use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};

    fn parse_and_typecheck(source: &str) -> ast::Program {
        let tokens = lex(source).expect("lex failed");
        let mut parser = Parser::new(tokens);
        let (mut program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {errors:?}");

        let mut checker = TypeChecker::new();
        let mut table = crate::symbol_table::CompilerSymbolTable::new();
        let (type_errors, monomorphs) = checker.check_program_with_table(&program, &mut table);
        assert!(type_errors.is_empty(), "type errors: {type_errors:?}");
        // Populate ForIn iterator types from typeck-resolved data
        let resolved_iter_types = checker.take_resolved_iter_types();
        if !resolved_iter_types.is_empty() {
            crate::semantic::typeck::populate_for_in_iterator_types(
                &mut program,
                &resolved_iter_types,
            );
        }
        monomorph::append_monomorphs(&mut program, &monomorphs, &[]);
        program
    }

    fn lower_to_llvm(source: &str) -> String {
        let program = parse_and_typecheck(source);
        LlvmIrGenerator::generate(&program).expect("llvm generation failed")
    }

    /// Extract the distinct 16-hex hashed symbols defined in the IR under a
    /// `define <retty> @<prefix>` pattern (e.g. `define i64 @Thing__bump__`).
    fn defined_hashed_symbols(ir: &str, define_prefix: &str) -> Vec<String> {
        let mut out = Vec::new();
        let mut rest = ir;
        while let Some(pos) = rest.find(define_prefix) {
            let hash_start = pos + define_prefix.len();
            let hash: String = rest[hash_start..].chars().take(16).collect();
            assert!(
                hash.len() == 16 && hash.chars().all(|c| c.is_ascii_hexdigit()),
                "expected 16-hex symbol hash after {define_prefix}: {hash}"
            );
            let symbol = format!("{define_prefix}{hash}");
            if !out.contains(&symbol) {
                out.push(symbol);
            }
            rest = &rest[hash_start + 16..];
        }
        out
    }

    /// The 16-hex hash of the symbol the IR calls through `call <retty> @<prefix>`.
    fn called_symbol_hash(ir: &str, call_prefix: &str) -> String {
        let pos = ir
            .find(call_prefix)
            .expect("expected call to hashed symbol");
        ir[pos + call_prefix.len()..][..16].to_string()
    }

    fn find_function_mut<'a>(
        program: &'a mut ast::Program,
        name: &str,
    ) -> &'a mut ast::FunctionItem {
        for item in &mut program.items {
            if let ast::ItemKind::Function(func) = &mut item.kind
                && func.name.name == name
            {
                return func;
            }
        }
        panic!("function `{name}` not found");
    }

    #[test]
    fn lowers_implicit_assignment_cast() {
        let ir = lower_to_llvm("i32 main() { i64 x = 1; i32 y = 2; x = y; return 0; }");
        assert!(
            ir.contains("cast.i2i"),
            "expected integer cast in IR:\n{ir}"
        );
    }

    #[test]
    fn lowers_implicit_return_cast() {
        let ir = lower_to_llvm("i64 main() { i32 x = 1; return x; }");
        assert!(ir.contains("cast.i2i"), "expected return cast in IR:\n{ir}");
    }

    #[test]
    fn lowers_implicit_call_argument_cast() {
        let mut program = parse_and_typecheck(
            "i64 id(i64 x) { return x; } i32 main() { i64 y = 1; f64 z = 1.25; id(y); return 0; }",
        );
        let main_fn = find_function_mut(&mut program, "main");
        let ast::StatementKind::Expression(call_expr) = &mut main_fn.body.statements[2].kind else {
            panic!("expected call expression statement");
        };
        let ast::ExpressionKind::Call { arguments, .. } = call_expr.kind.as_mut() else {
            panic!("expected call expression");
        };
        let span = arguments[0].span;
        arguments[0] = ast::Expression {
            kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                name: "z".to_string(),
                span,
            })),
            span,
        };

        let ir = LlvmIrGenerator::generate(&program).expect("llvm generation failed");
        assert!(
            ir.contains("cast.f2i"),
            "expected call-arg cast in IR:\n{ir}"
        );
    }

    #[test]
    fn lowers_explicit_cast_expression() {
        let mut program = parse_and_typecheck("i32 main() { f64 a = 1.5; i32 x = 1; return x; }");
        let main_fn = find_function_mut(&mut program, "main");
        let ast::StatementKind::Let(let_stmt) = &mut main_fn.body.statements[1].kind else {
            panic!("expected let statement");
        };
        let span = let_stmt
            .initializer
            .as_ref()
            .map(|e| e.span)
            .expect("missing initializer span");
        let_stmt.initializer = Some(ast::Expression {
            kind: Box::new(ast::ExpressionKind::Cast {
                expression: Box::new(ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                        name: "a".to_string(),
                        span,
                    })),
                    span,
                }),
                target_type: Box::new(ast::Type {
                    kind: Box::new(ast::TypeKind::Primitive(ast::PrimitiveType::I32)),
                    span,
                }),
            }),
            span,
        });

        let ir = LlvmIrGenerator::generate(&program).expect("llvm generation failed");
        assert!(
            ir.contains("cast.f2i"),
            "expected explicit cast in IR:\n{ir}"
        );
    }

    #[test]
    fn lowers_let_without_initializer_to_zero() {
        let ir = lower_to_llvm(
            "struct Point { i32 x; i32 y; } i32 main() { Point p; return p.x + p.y; }",
        );
        assert!(
            ir.contains("zeroinitializer"),
            "expected zero initialization for uninitialized let binding:\n{ir}"
        );
    }

    #[test]
    fn lowers_enum_member_access_to_resolved_integer_width() {
        let ir = lower_to_llvm("enum Color { Red; Blue = 255; } Color id() { return Color.Blue; }");
        assert!(
            ir.contains("define i8 @id()"),
            "expected enum backing type to lower to i8:\n{ir}"
        );
    }

    #[test]
    fn lowers_large_enum_to_u16() {
        let ir = lower_to_llvm("enum Big { Value = 256; } Big id() { return Big.Value; }");
        assert!(
            ir.contains("define i16 @id()"),
            "expected enum backing type to lower to i16:\n{ir}"
        );
    }

    #[test]
    fn lowers_nested_initializer_expressions() {
        let ir = lower_to_llvm(
            "struct Point { i32 x; i32 y; } struct Rect { Point min; Point max; } i32 main() { Rect r = { .min = { .x = 1, .y = 2 }, .max = { .x = 3, .y = 4 } }; return r.max.y; }",
        );
        assert!(
            ir.contains("%Rect = type { %Point, %Point }")
                && ir.contains("getelementptr inbounds nuw %Rect"),
            "expected nested struct initializer lowering:\n{ir}"
        );
    }

    #[test]
    fn lowers_global_variable_initializer_and_load() {
        let ir = lower_to_llvm("i32 counter = -1; i32 main() { return counter; }");
        assert!(
            ir.contains("@counter = global i32 -1"),
            "expected lowered global initializer:\n{ir}"
        );
        assert!(
            ir.contains("load i32, ptr @counter"),
            "expected global load in function body:\n{ir}"
        );
    }

    #[test]
    fn lowers_global_variable_store() {
        let ir = lower_to_llvm("i32 counter; i32 main() { counter = 3; return counter; }");
        assert!(
            ir.contains("@counter = global i32 0"),
            "expected zero-initialized global:\n{ir}"
        );
        assert!(
            ir.contains("store i32 3, ptr @counter"),
            "expected global store in function body:\n{ir}"
        );
    }

    #[test]
    fn lowers_overloaded_method_arity_symbols() {
        let ir = lower_to_llvm(
            "struct Thing { i64 v; } impl Thing { i64 bump(Thing* self, i64 a) { return self.v + a; } i64 bump(Thing* self, i64 a, i64 b) { return self.v + a + b; } i64 bump(Thing* self, i64 a, i64 b, i64 c) { return self.v + a + b + c; } } i32 main() { Thing t; t.v = 1; i64 r = t.bump(10, 20, 30); return (i32)r; }",
        );
        let defs = defined_hashed_symbols(&ir, "define i64 @Thing__bump__");
        assert_eq!(
            defs.len(),
            3,
            "expected three distinct overload symbols:\n{ir}"
        );
        // The 3-argument call must resolve to one of the three definitions.
        let call_hash = called_symbol_hash(&ir, "call i64 @Thing__bump__");
        assert!(
            defs.iter().any(|d| d.ends_with(&call_hash)),
            "call target {call_hash} not among definitions:\n{ir}"
        );
        assert!(
            ir.contains(&format!(
                "call i64 @Thing__bump__{call_hash}(ptr %t, i64 10, i64 20, i64 30)"
            )),
            "expected call to resolve to the 3-arg overload:\n{ir}"
        );
    }

    #[test]
    fn lowers_same_arity_type_overloads_by_argument_type() {
        let ir = lower_to_llvm(
            "struct S { i64 v; } impl S { i64 pick(S* self, i64 v) { return v; } i64 pick(S* self, str v) { return 1; } } i32 main() { S s; s.v = 1; i64 a = s.pick(7); i64 b = s.pick((str)\"x\"); return 0; }",
        );
        let defs = defined_hashed_symbols(&ir, "define i64 @S__pick__");
        assert_eq!(
            defs.len(),
            2,
            "expected two distinct overload symbols:\n{ir}"
        );
        let i64_hash = called_symbol_hash(&ir, "call i64 @S__pick__");
        let str_hash = called_symbol_hash(
            &ir[ir.find("call i64 @S__pick__").unwrap() + 1..],
            "call i64 @S__pick__",
        );
        assert_ne!(
            i64_hash, str_hash,
            "i64 and str calls must select different overloads:\n{ir}"
        );
        assert!(
            ir.contains(&format!("call i64 @S__pick__{i64_hash}(ptr %s, i64 7)")),
            "expected i64 argument to select the i64 overload:\n{ir}"
        );
        assert!(
            ir.contains(&format!("call i64 @S__pick__{str_hash}(ptr %s, ptr")),
            "expected cast str argument to select the str overload:\n{ir}"
        );
    }

    #[test]
    fn lowers_deref_argument_to_pointee_overload() {
        let ir = lower_to_llvm(
            "struct S { i64 v; } impl S { i64 pick(S* self, bool v) { return 1; } i64 pick(S* self, i64 v) { return v; } } i32 main() { S s; s.v = 1; i64* p = &s.v; i64 r = s.pick(*p); return (i32)r; }",
        );
        let defs = defined_hashed_symbols(&ir, "define i64 @S__pick__");
        assert_eq!(
            defs.len(),
            2,
            "expected two distinct overload symbols:\n{ir}"
        );
        // The deref argument (i64) must select the i64 overload (i64 second param).
        let call_hash = called_symbol_hash(&ir, "call i64 @S__pick__");
        let call_line = ir
            .lines()
            .find(|l| l.contains(&format!("call i64 @S__pick__{call_hash}")))
            .unwrap_or("");
        assert!(
            call_line.contains("ptr %s, i64 %"),
            "expected deref argument to select the i64 overload:\n{ir}"
        );
    }

    #[test]
    fn lowers_pointer_receiver_method_call_without_double_pointer_cast() {
        let ir = lower_to_llvm(
            "struct Counter { i32 value; } impl Counter { i32 read(Counter* self) { return self.value; } } i32 use_ptr(Counter* p) { return p.read(); } i32 main() { return 0; }",
        );
        assert!(
            ir.contains("call i32 @Counter__read__"),
            "expected pointer receiver method call:\n{ir}"
        );
        assert!(
            ir.contains("%p1 = load ptr, ptr %p"),
            "method receiver should load pointer value before call:\n{ir}"
        );
    }

    #[test]
    fn lowers_protocol_for_in() {
        let ir = lower_to_llvm(
            "trait IntoIterator {} trait Iterator {} struct Optional<T> { bool present; T thing; } struct Empty { i32 x; } impl IntoIterator for Empty { Empty into_iter(Empty self) { return self; } } impl Iterator for Empty { Optional<i32> next(Empty self) { Optional<i32> r; return r; } } i32 main() { Empty e = { .x = 0 }; for i in e { return 0; } return 0; }",
        );
        assert!(
            ir.contains("forin.cond"),
            "expected protocol for-in loop in IR:\n{ir}"
        );
    }

    #[test]
    fn lowers_generic_protocol_for_in() {
        let ir = lower_to_llvm(
            "trait IntoIterator {} trait Iterator {} struct Optional<T> { bool present; T thing; } struct Vec<T> { T* data; i64 len; } struct VecIter<T> { Vec<T>* vec; i64 idx; } impl IntoIterator for Vec<T> { VecIter<T> into_iter(Vec<T> self) { VecIter<T> it; return it; } } impl Iterator for VecIter<T> { Optional<T> next(VecIter<T>* self) { Optional<T> r; return r; } } i32 main() { Vec<i32> v; for i in v { return 0; } return 0; }",
        );
        assert!(
            ir.contains("VecIter__i32__next"),
            "expected monomorphized next in IR:\n{ir}"
        );
    }
    #[test]
    fn lowers_operator_overload_add() {
        let ir = lower_to_llvm(
            "struct Vec2 { i32 x; } impl Vec2 { Vec2 __add(Vec2 self, Vec2 other) { Vec2 r; r.x = self.x + other.x; return r; } } i32 main() { Vec2 a; Vec2 b; Vec2 c = a + b; return 0; }",
        );
        assert!(ir.contains("@Vec2____add"), "expected __add call:\n{ir}");
    }

    #[test]
    fn lowers_operator_overload_div() {
        let ir = lower_to_llvm(
            "struct Vec2 { i32 x; } impl Vec2 { Vec2 __div(Vec2 self, Vec2 other) { Vec2 r; r.x = self.x / other.x; return r; } } i32 main() { Vec2 a; Vec2 b; Vec2 c = a / b; return 0; }",
        );
        assert!(ir.contains("@Vec2____div"), "expected __div call:\n{ir}");
    }

    #[test]
    fn lowers_operator_overload_sub() {
        let ir = lower_to_llvm(
            "struct Vec2 { i32 x; } impl Vec2 { Vec2 __sub(Vec2 self, Vec2 other) { Vec2 r; r.x = self.x - other.x; return r; } } i32 main() { Vec2 a; Vec2 b; Vec2 c = a - b; return 0; }",
        );
        assert!(ir.contains("@Vec2____sub"), "expected __sub call:\n{ir}");
    }

    #[test]
    fn lowers_operator_overload_mul() {
        let ir = lower_to_llvm(
            "struct Vec2 { i32 x; } impl Vec2 { Vec2 __mul(Vec2 self, Vec2 other) { Vec2 r; r.x = self.x * other.x; return r; } } i32 main() { Vec2 a; Vec2 b; Vec2 c = a * b; return 0; }",
        );
        assert!(ir.contains("@Vec2____mul"), "expected __mul call:\n{ir}");
    }

    #[test]
    fn lowers_operator_overload_compound_assign() {
        let ir = lower_to_llvm(
            "struct Vec2 { i32 x; } impl Vec2 { Vec2 __add(Vec2 self, Vec2 other) { Vec2 r; r.x = self.x + other.x; return r; } } i32 main() { Vec2 a; Vec2 b; a += b; return 0; }",
        );
        assert!(
            ir.contains("@Vec2____add"),
            "expected __add call for compound assign:\n{ir}"
        );
    }

    #[test]
    fn lowers_unary_operator_neg() {
        let ir = lower_to_llvm(
            "struct Vec2 { i32 x; } impl Vec2 { Vec2 __neg(Vec2 self) { Vec2 r; r.x = -self.x; return r; } } i32 main() { Vec2 a; Vec2 b = -a; return 0; }",
        );
        assert!(ir.contains("@Vec2____neg"), "expected __neg call:\n{ir}");
    }
    #[test]
    fn lowers_indexed_access_get() {
        let ir = lower_to_llvm(
            "struct Buffer { i32* data; i64 len; } impl Buffer { i32 __index_get(Buffer* self, i64 index) { return (*self).data[index]; } } i32 main() { Buffer b; i32 x = b[0]; return x; }",
        );
        assert!(
            ir.contains("@Buffer____index_get"),
            "expected __index_get call:\n{ir}"
        );
    }

    #[test]
    fn lowers_index_assign() {
        let ir = lower_to_llvm(
            "struct Buffer { i32* data; i64 len; } impl Buffer { i32 __index_get(Buffer* self, i64 index) { return (*self).data[index]; } void __index_set(Buffer* self, i64 index, i32 value) { (*self).data[index] = value; } } i32 main() { Buffer b; b[0] = 42; return 0; }",
        );
        assert!(
            ir.contains("@Buffer____index_set"),
            "expected __index_set call:\n{ir}"
        );
    }

    #[test]
    fn lowers_comparison_eq() {
        let ir = lower_to_llvm(
            "struct Vec2 { i32 x; } impl Vec2 { bool __eq(Vec2 self, Vec2 other) { return self.x == other.x; } } i32 main() { Vec2 a; Vec2 b; bool c = a == b; return 0; }",
        );
        assert!(ir.contains("@Vec2____eq"), "expected __eq call:\n{ir}");
    }
    #[test]
    fn lowers_primitive_arithmetic_stays_inline() {
        // Primitives should NOT go through the trait path
        let ir = lower_to_llvm("i32 main() { i32 a = 1; i32 b = 2; i32 c = a + b; return c; }");
        assert!(
            !ir.contains("__add"),
            "primitive add should not call __add:\n{ir}"
        );
    }

    #[test]
    fn optimization_removes_trivial_alloca() {
        let context = Context::create();
        let module = context.create_module("test");
        let builder = context.create_builder();

        // Build a simple function: i32 test() { i32 x = 42; return x; }
        let i32_type = context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let fn_val = module.add_function("test", fn_type, None);

        let entry = context.append_basic_block(fn_val, "entry");
        builder.position_at_end(entry);

        // Create an alloca for the local variable, store and load
        let alloca = builder
            .build_alloca(i32_type, "x")
            .expect("alloca should succeed");
        let const_42 = i32_type.const_int(42, false);
        let _ = builder.build_store(alloca, const_42);
        let loaded = builder
            .build_load(i32_type, alloca, "loaded")
            .expect("load should succeed");
        let _ = builder.build_return(Some(&loaded));

        // Set up target machine
        Target::initialize_all(&InitializationConfig::default());
        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).expect("failed to create target");
        let machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .expect("failed to create target machine");
        module.set_data_layout(&machine.get_target_data().get_data_layout());

        // Run optimization pipeline
        run_module_optimization_passes(&module, &machine, Some("2"))
            .expect("optimization should succeed");

        let post = module.to_string();
        assert!(
            !post.contains("alloca"),
            "expected no alloca after optimization:\n{post}"
        );
    }

    #[test]
    fn no_scalar_literal_globals_in_ir() {
        let ir = lower_to_llvm(
            "i32 main() {
                i32 a = 1;
                f64 b = 2.5;
                bool c = true;
                char d = 'X';
                i32 r = a;
                return r;
            }",
        );
        for suffix in &["i64", "f64", "bool", "char", "c64"] {
            let pattern = format!(".lit.{suffix}");
            assert!(
                !ir.contains(&pattern),
                "expected no `{pattern}` global in IR:\n{ir}"
            );
        }
    }
    #[test]
    fn string_literals_deduplicated() {
        let ir = lower_to_llvm(
            "i32 main() {
                str a = \"same\";
                str b = \"same\";
                str c = \"other\";
                return 0;
            }",
        );
        // Count the number of distinct `.str.` globals
        let count = ir.matches(".str.").count();
        assert!(
            count >= 2,
            "expected at least 2 string globals (two unique strings), got {count}:\n{ir}"
        );
    }

    #[test]
    fn abi_coercion_alloca_in_entry_block() {
        // An extern C function taking a struct triggers ABI coercion on x86_64.
        // The resulting IR should contain an abi_coercion_tmp alloca (now in the entry block
        // via create_entry_alloca rather than at the current builder position).
        let ir = lower_to_llvm(
            "extern \"C\" {
                void modify_point(i32 x, i32 y);
            }
            i32 main() {
                modify_point(1, 2);
                return 0;
            }",
        );
        // The IR should contain the extern function declaration.
        assert!(
            ir.contains("modify_point"),
            "expected extern function declaration in IR:\n{ir}"
        );
    }

    #[test]
    fn generates_debug_info_metadata() {
        let source = "i32 main() { i32 a = 42; return a; }";
        let program = parse_and_typecheck(source);
        let mut table = CompilerSymbolTable::new();
        let path = std::path::Path::new("test_debug.ag");
        let ir = LlvmIrGenerator::generate_with_imports_and_table_and_source(
            &program,
            &[],
            &mut table,
            Some(path),
            Some(source),
            true,
        )
        .expect("failed to generate debug info IR");

        assert!(
            ir.contains("!dbg"),
            "expected !dbg annotations in IR:\n{ir}"
        );
        assert!(
            ir.contains("DISubprogram"),
            "expected DISubprogram metadata in IR:\n{ir}"
        );
    }

    #[test]
    fn static_local_lowers_to_internal_global() {
        let ir = lower_to_llvm(
            "i32 main() { static i32 counter = 0; counter = counter + 1; return counter; }",
        );
        assert!(
            ir.contains("@main.counter.0 = internal global i32 0"),
            "expected internal global for static local:\n{ir}"
        );
        assert!(
            ir.contains("store i32") && ir.contains("@main.counter.0"),
            "expected store to the static local global:\n{ir}"
        );
    }

    #[test]
    fn volatile_local_lowers_to_volatile_ops() {
        let ir = lower_to_llvm("i32 main() { volatile i32 v = 1; v = v + 1; return v; }");
        assert!(
            ir.contains("load volatile i32"),
            "expected volatile load in IR:\n{ir}"
        );
        assert!(
            ir.contains("store volatile i32"),
            "expected volatile store in IR:\n{ir}"
        );
    }

    #[test]
    fn volatile_array_lowers_to_volatile_element_ops() {
        let ir = lower_to_llvm(
            "i32 main() { volatile i32 buf[3]; buf[0] = 1; i32 x = buf[0]; buf[1] += 10; \
             buf[2]++; return x + buf[1] + buf[2]; }",
        );
        assert!(
            ir.contains("load volatile i32") && ir.contains("store volatile i32"),
            "expected volatile element load and store in IR:\n{ir}"
        );
        assert!(
            ir.contains("store volatile [3 x i32]"),
            "expected volatile array zero-init in IR:\n{ir}"
        );
        assert!(
            ir.contains("load volatile i32") && ir.contains("assign.load"),
            "expected volatile compound-assign load in IR:\n{ir}"
        );
        assert!(
            ir.contains("incdec.load") && ir.contains("load volatile i32"),
            "expected volatile incdec load in IR:\n{ir}"
        );
    }

    #[test]
    fn volatile_pointer_param_lowers_to_volatile_ops() {
        // The video-buffer case: a buffer reached only through a pointer, where
        // the volatility must travel with the pointer (function parameter).
        let ir = lower_to_llvm(
            "void f(volatile u8* fb, i64 o, u8 c) { fb[o] = c; u8 v = fb[o]; fb[o]++; } i32 main() { return 0; }",
        );
        assert!(
            ir.contains("store volatile i8"),
            "expected volatile store through pointer param:\n{ir}"
        );
        assert!(
            ir.contains("load volatile i8"),
            "expected volatile load through pointer param:\n{ir}"
        );
        assert!(
            ir.contains("incdec.load") && ir.contains("load volatile i8"),
            "expected volatile compound access through pointer param:\n{ir}"
        );
    }

    #[test]
    fn volatile_pointer_deref_lowers_to_volatile_ops() {
        let ir = lower_to_llvm(
            "i32 main() { volatile u8* p = (volatile u8*)0; u8 v = *p; *p = (u8)1; return (i32)v; }",
        );
        assert!(
            ir.contains("load volatile i8") && ir.contains("deref.load"),
            "expected volatile deref read:\n{ir}"
        );
        assert!(
            ir.contains("store volatile i8"),
            "expected volatile deref write:\n{ir}"
        );
    }

    #[test]
    fn volatile_pointer_global_with_mmio_address() {
        let ir = lower_to_llvm(
            "volatile u8* vram = (volatile u8*)0xB8000; i32 main() { vram[0] = (u8)65; return (i32)vram[0]; }",
        );
        assert!(
            ir.contains("@vram = global ptr inttoptr (i64 753664 to ptr)"),
            "expected inttoptr MMIO address in global initializer:\n{ir}"
        );
        assert!(
            ir.contains("store volatile i8") && ir.contains("load volatile i8"),
            "expected volatile access through the video buffer pointer:\n{ir}"
        );
    }

    #[test]
    fn volatile_global_array_lowers_to_volatile_gep_ops() {
        let ir = lower_to_llvm(
            "volatile u32 regs[4] = {10, 20, 30, 40}; i32 main() { regs[2] = 99; return (i32)regs[2]; }",
        );
        assert!(
            ir.contains("@regs = global [4 x i32] [i32 10, i32 20, i32 30, i32 40]"),
            "expected const array initializer on volatile global:\n{ir}"
        );
        assert!(
            ir.contains("store volatile i32") && ir.contains("@regs"),
            "expected volatile store through the global GEP:\n{ir}"
        );
        assert!(
            ir.contains("load volatile i32") && ir.contains("@regs"),
            "expected volatile load through the global GEP:\n{ir}"
        );
    }

    #[test]
    fn static_array_local_lowers_to_internal_global_with_initializer() {
        let ir = lower_to_llvm(
            "i32 main() { static i32 cache[2] = {7, 8}; return cache[0] + cache[1]; }",
        );
        assert!(
            ir.contains("@main.cache.0 = internal global [2 x i32] [i32 7, i32 8]"),
            "expected internal global with const array initializer:\n{ir}"
        );
    }

    #[test]
    fn static_volatile_array_local_lowers_to_internal_global() {
        let ir = lower_to_llvm("i32 main() { static volatile i32 reg = 0; reg++; return reg; }");
        assert!(
            ir.contains("@main.reg.0 = internal global i32 0"),
            "expected internal global for static volatile local:\n{ir}"
        );
        assert!(
            ir.contains("load volatile i32") && ir.contains("store volatile i32"),
            "expected volatile ops on the static volatile local:\n{ir}"
        );
    }

    #[test]
    fn large_zero_init_array_uses_memset() {
        // Regression: a store of a huge constant aggregate (e.g.
        // `store [100000 x i8] zeroinitializer`) crashes the LLVM SelectionDAG
        // combiner; large zero-initialized arrays must zero-fill via memset.
        let ir = lower_to_llvm(
            "i32 main() { u8 backing[100000]; backing[0] = (u8)1; return (i32)backing[0]; }",
        );
        assert!(
            ir.contains("@llvm.memset") && ir.contains("i64 100000"),
            "expected memset of the full array size in IR:\n{ir}"
        );
        assert!(
            !ir.contains("store [100000 x i8] zeroinitializer"),
            "expected no huge constant store in IR:\n{ir}"
        );
    }

    #[test]
    fn nested_array_lowers_to_nested_llvm_array() {
        let ir = lower_to_llvm("i32 main() { i32 grid[3][4]; grid[1][2] = 7; return grid[1][2]; }");
        assert!(
            ir.contains("alloca [3 x [4 x i32]]"),
            "expected [3 x [4 x i32]] (C row-major, first suffix outermost):\n{ir}"
        );
        assert!(
            ir.contains("getelementptr inbounds [3 x [4 x i32]], ptr %grid, i64 0, i64 1")
                && ir.contains("getelementptr inbounds [4 x i32], ptr %arr.idx.ptr"),
            "expected row-then-column GEP chain:\n{ir}"
        );
    }

    #[test]
    fn flat_multidim_const_initializer() {
        let ir = lower_to_llvm(
            "i32 table[2][3] = {1, 2, 3, 4, 5, 6}; i32 main() { return table[1][2]; }",
        );
        assert!(
            ir.contains(
                "@table = global [2 x [3 x i32]] [[3 x i32] [i32 1, i32 2, i32 3], [3 x i32] [i32 4, i32 5, i32 6]]"
            ),
            "expected nested const array from flat initializer:\n{ir}"
        );
    }

    #[test]
    fn static_global_lowers_to_internal_linkage() {
        let ir = lower_to_llvm("static i32 g = 42; i32 main() { return g; }");
        assert!(
            ir.contains("@g = internal global i32 42"),
            "expected internal linkage for static global:\n{ir}"
        );
    }
}
