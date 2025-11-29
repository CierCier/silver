#include "agc/driver.hpp"
#include "agc/ast_dump.hpp"
#include "agc/codegen.hpp"
#include "agc/comptime.hpp"
#include "agc/config.hpp"
#include "agc/lexer.hpp"
#include "agc/parser.hpp"
#include "agc/sema.hpp"
#include "libag/libag.hpp"
#include <deque>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>
#include <unordered_set>

#if defined(SILVER_HAS_LLVM) && SILVER_HAS_LLVM
#include <llvm/Config/llvm-config.h>
#endif

namespace agc {

void CompilerDriver::print_usage(const char *prog) {
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
      << "  -l <lib>              Link against library <lib>\n"
      << "  -L <dir>              Add directory to library search path\n"
      << "  -static               Link against static runtime library\n"
      << "  --                    End of options\n";
}

std::optional<std::string>
CompilerDriver::resolve_import(const std::vector<std::string> &parts,
                               const std::vector<std::string> &include_paths) {
  std::string relPath;
  for (size_t i = 0; i < parts.size(); ++i) {
    if (i > 0)
      relPath += "/";
    relPath += parts[i];
  }
  relPath += ".ag";

  for (const auto &inc : include_paths) {
    std::filesystem::path p = inc;
    p /= relPath;
    if (std::filesystem::exists(p)) {
      return std::filesystem::absolute(p).string();
    }
  }
  return std::nullopt;
}

int CompilerDriver::run(int argc, char **argv) {
  CompilerOptions opt;

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
      if (a == "-l") {
        if (i + 1 >= argc) {
          std::cerr << "error: missing library after -l\n";
          return 1;
        }
        opt.link_libs.emplace_back(argv[++i]);
        continue;
      }
      if (a.rfind("-l", 0) == 0 && a.size() > 2) {
        opt.link_libs.emplace_back(std::string(a.substr(2)));
        continue;
      }
      if (a == "-L") {
        if (i + 1 >= argc) {
          std::cerr << "error: missing path after -L\n";
          return 1;
        }
        opt.link_paths.emplace_back(argv[++i]);
        continue;
      }
      if (a.rfind("-L", 0) == 0 && a.size() > 2) {
        opt.link_paths.emplace_back(std::string(a.substr(2)));
        continue;
      }
      if (a.rfind("-emit-", 0) == 0 && a.size() > 6) {
        opt.emit_backend = std::string(a.substr(6));
        continue;
      }

      if (a.rfind("--static", 0) == 0) {
        opt.static_link = true;
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
    std::cout << "agc version " << ag::version_full() << "\n";
#if defined(SILVER_HAS_LLVM) && SILVER_HAS_LLVM
    std::cout << "Target: " << LLVM_DEFAULT_TARGET_TRIPLE << "\n";
#endif
    std::cout << "Default include path: " << config::DEFAULT_INCLUDE_PATH
              << "\n";
    std::cout << "Default library path: " << config::DEFAULT_LIB_PATH << "\n";
    return 0;
  }
  if (opt.verbose) {
    std::cout << "agc version " << ag::version_full() << "\n";
    diags_.setVerbose(true);
  }
  if (opt.inputs.empty()) {
    print_usage(argv[0]);
    return 1;
  }

  // Add default include paths
  opt.include_paths.push_back(".");
  // Use configured default include path from CMake
  if (config::DEFAULT_INCLUDE_PATH[0] != '\0') {
    opt.include_paths.push_back(config::DEFAULT_INCLUDE_PATH);
  }

  // Warn about stub phases not yet implemented
  if (opt.compile_only || opt.assemble || opt.preprocess) {
    std::cerr << "note: -c/-S/-E are not implemented; ignoring\n";
  }

  // Default to LLVM backend if not specified and not just syntax
  // checking/dumping
  if (!opt.emit_backend && !opt.fsyntax_only && !opt.ast_dump) {
    opt.emit_backend = "llvm";
  }

  int exitCode = 0;
  agc::Program mainProg;
  bool parseError = false;

  std::deque<std::string> queue;
  std::unordered_set<std::string> visited;

  for (const auto &in : opt.inputs) {
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
      agc::Parser p(toks, diags_);
      auto prog = p.parseProgram();

      // Scan for imports
      for (const auto &d : prog.decls) {
        if (auto *imp = std::get_if<agc::DeclImport>(&d->v)) {
          auto resolved = resolve_import(imp->path, opt.include_paths);
          if (resolved) {
            if (visited.find(*resolved) == visited.end()) {
              visited.insert(*resolved);
              queue.push_back(*resolved);
            }
          } else {
            std::cerr << "error: cannot resolve import '";
            for (size_t i = 0; i < imp->path.size(); ++i)
              std::cerr << (i ? "." : "") << imp->path[i];
            std::cerr << "'\n";
            exitCode = 1;
            parseError = true;
          }
        }
        if (auto *lnk = std::get_if<agc::DeclLink>(&d->v)) {
          if (opt.verbose)
            std::cout << "Found link: " << lnk->lib << "\n";
          opt.link_libs.push_back(lnk->lib);
        }
      }

      // Merge decls into mainProg
      for (auto &d : prog.decls) {
        mainProg.decls.push_back(std::move(d));
      }

      if (!opt.fsyntax_only && !opt.emit_backend && !opt.ast_dump) {
        std::cout << path << ": parsed OK\n";
      }
    } catch (const agc::ParseError &e) {
      diags_.report(agc::DiagLevel::Error, e.loc, e.what());
      exitCode = 1;
      parseError = true;
    } catch (const std::exception &e) {
      std::cerr << path << ": " << e.what() << "\n";
      exitCode = 1;
      parseError = true;
    }
  }

  if (parseError)
    return exitCode;

  // Run Semantic Analysis (Const Inference)
  agc::SemanticAnalyzer sema(diags_, typeCtx_);
  sema.analyze(mainProg);
  if (diags_.hasErrors())
    return 1;
  // Run Comptime Evaluation
  agc::ComptimeEvaluator evaluator;
  // Register user functions
  for (const auto &d : mainProg.decls) {
    if (auto *f = std::get_if<agc::DeclFunc>(&d->v)) {
      evaluator.registerUserFunc(f->name, f);
    }
  }
  // Traverse and evaluate comptime exprs
  evaluator.evaluateProgram(mainProg);

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

    agc::CodegenOptions cgopt;
    cgopt.diags = &diags_;

    if (is_llvm && opt.output) {
      std::string objFile = *opt.output + ".o";
      std::string err;
      if (!be->emit_object_file(mainProg, objFile, err, cgopt)) {
        std::cerr << "error: " << err << "\n";
        exitCode = 1;
      }
      // Link
      // We will use 'cc' to link, which includes crt1.o and libc.
      std::string cmd = "cc -o " + *opt.output + " " + objFile;

      // Collect library paths (both for linking and runtime)
      std::vector<std::string> libPaths;
      for (const auto &path : opt.link_paths) {
        libPaths.push_back(path);
      }
      // Default libag path - use absolute path for local builds
      std::filesystem::path defaultLibPath =
          std::filesystem::current_path() / "build" / "libag";
      if (std::filesystem::exists(defaultLibPath)) {
        libPaths.push_back(defaultLibPath.string());
      }
      // Use configured default library path from CMake
      if (config::DEFAULT_LIB_PATH[0] != '\0' &&
          std::filesystem::exists(config::DEFAULT_LIB_PATH)) {
        libPaths.push_back(config::DEFAULT_LIB_PATH);
      }

      // Add -L flags for link-time lookup
      for (const auto &path : libPaths) {
        cmd += " -L" + path;
      }

      for (const auto &lib : opt.link_libs) {
        cmd += " -l" + lib;
      }

      if (opt.static_link) {
        cmd += " -lag_static -static";
      } else {
        cmd += " -lag";
        // Add rpath for runtime library lookup (dynamic linking)
        // Use $ORIGIN for relative paths and absolute paths for system
        // locations
        std::string rpath;
        for (const auto &path : libPaths) {
          if (!rpath.empty())
            rpath += ":";
          rpath += path;
        }
        if (!rpath.empty()) {
          cmd += " -Wl,-rpath," + rpath;
        }
      }

      if (opt.verbose) {
        std::cout << "Linking: " << cmd << "\n";
      }

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
      if (!be->generate(mainProg, *out, err, cgopt)) {
        std::cerr << "error: " << err << "\n";
        exitCode = 1;
      }
    }
  }
  return exitCode;
}

} // namespace agc
