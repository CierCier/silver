#pragma once

namespace agc {

// Helper for std::visit with multiple lambdas
// Usage: std::visit(overloaded{
//            [](const Type1& x) { ... },
//            [](const Type2& x) { ... },
//            ...
//        }, variant);
template <class... Ts> struct overloaded : Ts... {
  using Ts::operator()...;
};

// Deduction guide (required for C++17)
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

} // namespace agc