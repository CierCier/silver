#pragma once

#include "agc/ast.hpp"
#include "agc/diagnostics.hpp"
#include "agc/type.hpp"
#include <optional>
#include <string>
#include <vector>

namespace agc {

struct CompilerOptions {
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
  std::vector<std::string> link_libs;      // -l
  std::vector<std::string> link_paths;     // -L
  std::vector<std::string> inputs;         // files
  bool static_link{false};                 // --static
};

class CompilerDriver {
public:
  CompilerDriver() = default;

  // Parse arguments and run the compiler
  int run(int argc, char **argv);

  // Run the compilation pipeline for a single file
  // (internal use mostly, but exposed)
  bool compile(const std::string &path, const CompilerOptions &opts);

  DiagnosticEngine &getDiagnostics() { return diags_; }

private:
  void print_usage(const char *prog);
  std::optional<std::string>
  resolve_import(const std::vector<std::string> &parts,
                 const std::vector<std::string> &include_paths);

  DiagnosticEngine diags_;
  TypeContext typeCtx_;
};

} // namespace agc
