#pragma once
#include "agc/ast.hpp"
#include <iosfwd>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

namespace agc {

class DiagnosticEngine;

struct CodegenOptions {
  DiagnosticEngine *diags{nullptr};
  // Placeholder for future toggles (opt level, debug, etc.)
};

class CodegenBackend {
public:
  virtual ~CodegenBackend() = default;
  virtual std::string_view name() const = 0;
  // Generate code for a single translation unit (Program) into the output
  // stream. On error, return false and set error message.
  virtual bool generate(const Program &prog, std::ostream &os,
                        std::string &error, const CodegenOptions &opt) = 0;

  virtual bool emit_object_file(const Program &prog,
                                const std::string &filename, std::string &err,
                                const CodegenOptions &opts = {}) {
    err = "Backend does not support object file generation";
    return false;
  }
};

// Factory to create a backend by name (e.g., "llvm", "js", "csharp").
std::unique_ptr<CodegenBackend> create_backend(std::string_view name);

std::vector<std::string> available_backends();

} // namespace agc
