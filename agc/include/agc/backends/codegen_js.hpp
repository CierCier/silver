#pragma once
#include "agc/codegen.hpp"

namespace agc {
std::unique_ptr<CodegenBackend> create_backend_js();
}
