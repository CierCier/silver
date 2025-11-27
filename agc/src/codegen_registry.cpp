#include "agc/codegen.hpp"
#include <algorithm>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

// forward decls of concrete backends
#ifdef SILVER_HAS_LLVM
namespace agc {
std::unique_ptr<CodegenBackend> create_backend_llvm();
}
#endif
#ifdef SILVER_HAS_JS
namespace agc {
std::unique_ptr<CodegenBackend> create_backend_js();
}
#endif
#ifdef SILVER_HAS_CSHARP
namespace agc {
std::unique_ptr<CodegenBackend> create_backend_csharp();
}
#endif

namespace agc {

std::unique_ptr<CodegenBackend> create_backend(std::string_view name) {
#ifdef SILVER_HAS_LLVM
  if (name == "llvm" || name == "ir")
    return create_backend_llvm();
#endif
#ifdef SILVER_HAS_JS
  if (name == "js" || name == "javascript")
    return create_backend_js();
#endif
#ifdef SILVER_HAS_CSHARP
  if (name == "cs" || name == "csharp" || name == "c#")
    return create_backend_csharp();
#endif
  return {};
}

std::vector<std::string> available_backends() {
  std::vector<std::string> backends;
#ifdef SILVER_HAS_LLVM
  backends.push_back("llvm");
#endif
#ifdef SILVER_HAS_JS
  backends.push_back("js");
#endif
#ifdef SILVER_HAS_CSHARP
  backends.push_back("csharp");
#endif
  return backends;
}

} // namespace agc
