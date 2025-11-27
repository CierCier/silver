#include "agc/ast_dump.hpp"
#include "agc/codegen.hpp"
#include "agc/lexer.hpp"
#include "agc/parser.hpp"
#include "libag/libag.hpp"
#include "agc/comptime.hpp"
#include "agc/sema.hpp"
#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>
#include <string_view>
#include <vector>
#include <filesystem>
#include <unordered_set>
#include <deque>

static void print_usage(const char *prog) {
  std::cout
      << "agc - Silver compiler\n\n"
      << "USAGE:   " << prog << " [options] file...\n\n"
      << "OPTIONS:\n"
      << "  -h, --help            Display this help and exit\n"
      << "  --version             Display compiler version and exit\n"
      << "  -v                    Show version info (and continue)\n"
      << "  -fsyntax-only         Parse and diagnose only; no output\n"
      << "  -ast-dump             Dump the parsed AST to stdout\n"
      << "  -c                    Compile only; do not link (stub)\n"
      << "  -S                    Compile to assembly (stub)\n"
      << "  -E                    Preprocess only (stub)\n"
      << "  -o <file>             Write output to <file>\n"
      << "  --emit=<backend>      Generate code using a backend (e.g. js, "
         "csharp, llvm)\n"
      << "  -emit-<backend>       Shorthand for --emit=<backend>\n"
      << "  -I <dir>              Add directory to import/include search path\n"
      << "  -D <name>[=<val>]     Define a preprocessor symbol (reserved)\n"
      << "  --                    End of options\n";
}

static std::optional<std::string> resolve_import(const std::vector<std::string>& parts, const std::vector<std::string>& include_paths) {
    std::string relPath;
    for (size_t i = 0; i < parts.size(); ++i) {
        if (i > 0) relPath += "/";
        relPath += parts[i];
    }
    relPath += ".ag";

    for (const auto& inc : include_paths) {
        std::filesystem::path p = inc;
        p /= relPath;
        if (std::filesystem::exists(p)) {
            return std::filesystem::absolute(p).string();
        }
    }
    return std::nullopt;
}

// Helper to replace ExprComptime with evaluated result
static void replace_comptime(agc::ExprPtr &expr, agc::ComptimeEvaluator &evaluator) {
    if (!expr) return;

    // Check if this node is ExprComptime
    if (auto *ct = std::get_if<agc::ExprComptime>(&expr->v)) {
        auto res = evaluator.evaluate(*ct->expr);
        if (res.ok()) {
            // Replace expr with constant
            if (auto *i = std::get_if<agc::ComptimeInt>(&*res.value)) {
                expr->v = agc::ExprInt{static_cast<uint64_t>(i->value)};
            } else if (auto *s = std::get_if<agc::ComptimeStr>(&*res.value)) {
                expr->v = agc::ExprStr{s->value};
            } else if (auto *b = std::get_if<agc::ComptimeBool>(&*res.value)) {
                expr->v = agc::ExprInt{static_cast<uint64_t>(b->value ? 1 : 0)}; // Bool as int for now
            } else {
                std::cerr << "warning: unsupported comptime result type\n";
            }
        } else {
            std::cerr << "error: comptime evaluation failed: " << res.error << "\n";
            exit(1);
        }
        return;
    }

    // Recurse
    std::visit([&](auto &n) {
        using T = std::decay_t<decltype(n)>;
        if constexpr (std::is_same_v<T, agc::ExprUnary>) {
            replace_comptime(n.rhs, evaluator);
        } else if constexpr (std::is_same_v<T, agc::ExprBinary>) {
            replace_comptime(n.lhs, evaluator);
            replace_comptime(n.rhs, evaluator);
        } else if constexpr (std::is_same_v<T, agc::ExprAssign>) {
            replace_comptime(n.lhs, evaluator);
            replace_comptime(n.rhs, evaluator);
        } else if constexpr (std::is_same_v<T, agc::ExprCond>) {
            replace_comptime(n.cond, evaluator);
            replace_comptime(n.thenE, evaluator);
            replace_comptime(n.elseE, evaluator);
        } else if constexpr (std::is_same_v<T, agc::ExprCall>) {
            for (auto &arg : n.args) replace_comptime(arg, evaluator);
        } else if constexpr (std::is_same_v<T, agc::ExprIndex>) {
            replace_comptime(n.base, evaluator);
            replace_comptime(n.index, evaluator);
        } else if constexpr (std::is_same_v<T, agc::ExprMember>) {
            replace_comptime(n.base, evaluator);
        } else if constexpr (std::is_same_v<T, agc::ExprAddressOf>) {
            replace_comptime(n.operand, evaluator);
        } else if constexpr (std::is_same_v<T, agc::ExprDeref>) {
            replace_comptime(n.operand, evaluator);
        }
    }, expr->v);
}

static void visit_stmt_comptime(agc::StmtPtr &stmt, agc::ComptimeEvaluator &evaluator);

static void visit_stmt_comptime(agc::StmtPtr &stmt, agc::ComptimeEvaluator &evaluator) {
    if (!stmt) return;
    std::visit([&](auto &s) {
        using T = std::decay_t<decltype(s)>;
        if constexpr (std::is_same_v<T, agc::StmtExpr>) {
            replace_comptime(s.expr, evaluator);
        } else if constexpr (std::is_same_v<T, agc::StmtReturn>) {
            if (s.expr) replace_comptime(*s.expr, evaluator);
        } else if constexpr (std::is_same_v<T, agc::StmtDecl>) {
            for (auto &pair : s.declarators) {
                if (pair.second && *pair.second) replace_comptime(*pair.second, evaluator);
            }
        } else if constexpr (std::is_same_v<T, agc::StmtIf>) {
            replace_comptime(s.cond, evaluator);
            visit_stmt_comptime(s.thenBranch, evaluator);
            if (s.elseBranch) visit_stmt_comptime(*s.elseBranch, evaluator);
        } else if constexpr (std::is_same_v<T, agc::StmtWhile>) {
            replace_comptime(s.cond, evaluator);
            visit_stmt_comptime(s.body, evaluator);
        } else if constexpr (std::is_same_v<T, agc::StmtBlock>) {
            for (auto &st : s.stmts) visit_stmt_comptime(st, evaluator);
        } else if constexpr (std::is_same_v<T, agc::StmtFor>) {
            if (s.init) visit_stmt_comptime(*s.init, evaluator);
            if (s.cond) replace_comptime(*s.cond, evaluator);
            if (s.iter) replace_comptime(*s.iter, evaluator);
            visit_stmt_comptime(s.body, evaluator);
        }
    }, stmt->v);
}

int main(int argc, char **argv) {
  // ... (Options struct and parsing logic remains same) ...
  struct Options {
    bool help{false};
    bool version{false};
    bool verbose{false};
    bool fsyntax_only{false};
    bool ast_dump{false};
    bool compile_only{false};                // -c
    bool assemble{false};                    // -S
    bool preprocess{false};                  // -E
    std::optional<std::string> emit_backend; // --emit / -emit-<name>
    std::optional<std::string> output;       // -o
    std::vector<std::string> include_paths;  // -I
    std::vector<std::string> defines;        // -D
    std::vector<std::string> inputs;         // files
  } opt;

  // ... (Argument parsing loop) ...
  bool end_of_opts = false;
  for (int i = 1; i < argc; ++i) {
    std::string_view a = argv[i];
    if (!end_of_opts && a == "--") {
      end_of_opts = true;
      continue;
    }
    if (!end_of_opts && a.size() && a[0] == '-') {
      if (a == "-h" || a == "--help") {
        opt.help = true;
        continue;
      }
      if (a == "--version") {
        opt.version = true;
        continue;
      }
      if (a == "-v") {
        opt.verbose = true;
        continue;
      }
      if (a == "-fsyntax-only") {
        opt.fsyntax_only = true;
        continue;
      }
      if (a == "-ast-dump") {
        opt.ast_dump = true;
        continue;
      }
      if (a == "-c") {
        opt.compile_only = true;
        continue;
      }
      if (a == "-S") {
        opt.assemble = true;
        continue;
      }
      if (a == "-E") {
        opt.preprocess = true;
        continue;
      }
      if (a.rfind("--emit=", 0) == 0) {
        opt.emit_backend = std::string(a.substr(7));
        continue;
      }
      if (a == "--emit") {
        if (i + 1 >= argc) {
          std::cerr << "error: missing backend after --emit\n";
          return 1;
        }
        opt.emit_backend = std::string(argv[++i]);
        continue;
      }
      if (a == "-o") {
        if (i + 1 >= argc) {
          std::cerr << "error: missing filename after -o\n";
          return 1;
        }
        opt.output = std::string(argv[++i]);
        continue;
      }
      if (a == "-I") {
        if (i + 1 >= argc) {
          std::cerr << "error: missing path after -I\n";
          return 1;
        }
        opt.include_paths.emplace_back(argv[++i]);
        continue;
      }
      if (a == "-D") {
        if (i + 1 >= argc) {
          std::cerr << "error: missing macro after -D\n";
          return 1;
        }
        opt.defines.emplace_back(argv[++i]);
        continue;
      }
      // Support joined forms -Ipath and -DNAME=VAL
      if (a.rfind("-I", 0) == 0 && a.size() > 2) {
        opt.include_paths.emplace_back(std::string(a.substr(2)));
        continue;
      }
      if (a.rfind("-D", 0) == 0 && a.size() > 2) {
        opt.defines.emplace_back(std::string(a.substr(2)));
        continue;
      }
      if (a.rfind("-emit-", 0) == 0 && a.size() > 6) {
        opt.emit_backend = std::string(a.substr(6));
        continue;
      }

      std::cerr << "error: unknown option '" << a << "'\n";
      print_usage(argv[0]);
      return 1;
    } else {
      opt.inputs.emplace_back(argv[i]);
    }
  }

  if (opt.help) {
    print_usage(argv[0]);
    return 0;
  }
  if (opt.version) {
    std::cout << "agc " << ag::version() << "\n";
    return 0;
  }
  if (opt.verbose) {
    std::cout << "agc version " << ag::version() << "\n";
  }
  if (opt.inputs.empty()) {
    print_usage(argv[0]);
    return 1;
  }

  // Add default include paths
  opt.include_paths.push_back(".");
  opt.include_paths.push_back("/usr/lib/silver/include");
  opt.include_paths.push_back("/lib/silver/include");

  // Warn about stub phases not yet implemented
  if (opt.compile_only || opt.assemble || opt.preprocess) {
    std::cerr << "note: -c/-S/-E are not implemented; ignoring\n";
  }

  // Default to LLVM backend if not specified and not just syntax checking/dumping
  if (!opt.emit_backend && !opt.fsyntax_only && !opt.ast_dump) {
      opt.emit_backend = "llvm";
  }

  int exitCode = 0;
  agc::Program mainProg;
  bool parseError = false;

  std::deque<std::string> queue;
  std::unordered_set<std::string> visited;

  for (const auto& in : opt.inputs) {
      std::filesystem::path p(in);
      if (std::filesystem::exists(p)) {
          auto abs = std::filesystem::absolute(p).string();
          if (visited.find(abs) == visited.end()) {
              visited.insert(abs);
              queue.push_back(abs);
          }
      } else {
          // Let the loop handle the error
          queue.push_back(in);
      }
  }

  while (!queue.empty()) {
    std::string path = queue.front();
    queue.pop_front();

    std::ifstream in(path, std::ios::binary);
    if (!in) {
      std::cerr << "error: cannot open '" << path << "'\n";
      exitCode = 1;
      parseError = true;
      continue;
    }

    std::ostringstream ss;
    ss << in.rdbuf();
    std::string text = ss.str();
    try {
      agc::Lexer lx(text, path);
      auto toks = lx.lex();
      agc::Parser p(toks);
      auto prog = p.parseProgram();
      
      // Scan for imports
      for (const auto& d : prog.decls) {
          if (auto* imp = std::get_if<agc::DeclImport>(&d->v)) {
              auto resolved = resolve_import(imp->path, opt.include_paths);
              if (resolved) {
                  if (visited.find(*resolved) == visited.end()) {
                      visited.insert(*resolved);
                      queue.push_back(*resolved);
                  }
              } else {
                  std::cerr << "error: cannot resolve import '";
                  for(size_t i=0; i<imp->path.size(); ++i) std::cerr << (i?".":"") << imp->path[i];
                  std::cerr << "'\n";
                  exitCode = 1;
                  parseError = true;
              }
          }
      }

      // Merge decls into mainProg
      for (auto &d : prog.decls) {
        mainProg.decls.push_back(std::move(d));
      }

      if (!opt.fsyntax_only && !opt.emit_backend && !opt.ast_dump) {
         std::cout << path << ": parsed OK\n";
      }
    } catch (const std::exception &e) {
      std::cerr << path << ": " << e.what() << "\n";
      exitCode = 1;
      parseError = true;
    }
  }

  if (parseError) return exitCode;

  // Run Semantic Analysis (Const Inference)
  agc::SemanticAnalyzer sema;
  sema.analyze(mainProg);

  // Run Comptime Evaluation
  agc::ComptimeEvaluator evaluator;
  // Register user functions
  for (const auto &d : mainProg.decls) {
      if (auto *f = std::get_if<agc::DeclFunc>(&d->v)) {
          evaluator.registerUserFunc(f->name, f);
      }
  }
  // Traverse and evaluate comptime exprs
  for (auto &d : mainProg.decls) {
      if (auto *f = std::get_if<agc::DeclFunc>(&d->v)) {
          if (f->body) {
              for (auto &s : f->body->stmts) {
                  visit_stmt_comptime(s, evaluator);
              }
          }
      } else if (auto *v = std::get_if<agc::DeclVar>(&d->v)) {
          for (auto &pair : v->declarators) {
              if (pair.second && *pair.second) {
                  replace_comptime(*pair.second, evaluator);
              }
          }
      }
  }

  if (opt.ast_dump) {
    dump(mainProg, std::cout);
    return 0;
  }

  if (opt.emit_backend) {
    auto be = agc::create_backend(*opt.emit_backend);
    if (!be) {
      std::cerr << "error: unknown backend '" << *opt.emit_backend << "'\n";
      return 1;
    }

    // Handle object file generation if backend is LLVM (or supports it)
    bool is_llvm = (*opt.emit_backend == "llvm");
    
    if (is_llvm && !opt.output) {
         opt.output = "a.out";
    }

    if (is_llvm && opt.output) {
         std::string objFile = *opt.output + ".o";
         std::string err;
         if (!be->emit_object_file(mainProg, objFile, err)) {
             std::cerr << "Error emitting object file: " << err << "\n";
             return 1;
         }
         
         // Link
         // We will use 'cc' to link, which includes crt1.o and libc.
         // Assume build/libag contains the library
         std::string cmd = "cc -o " + *opt.output + " " + objFile + " -Lbuild/libag -lag_static";
         int ret = system(cmd.c_str());
         if (ret != 0) {
             std::cerr << "Linker failed: " << cmd << "\n";
             return 1;
         }
         // Cleanup
         std::filesystem::remove(objFile);
    } else {
         std::ostream *out = &std::cout;
         std::ofstream fout;
         if (opt.output) {
           fout.open(*opt.output);
           if (!fout) {
             std::cerr << "error: cannot open output '" << *opt.output << "'\n";
             return 1;
           }
           out = &fout;
         }

         std::string err;
         agc::CodegenOptions cgopt;
         if (!be->generate(mainProg, *out, err, cgopt)) {
           std::cerr << "error: " << err << "\n";
           exitCode = 1;
         }
    }
  }
  return exitCode;
}
