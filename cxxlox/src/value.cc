#include "value.h"

#include <format>
#include <iostream>

namespace {

std::string to_string(bool value) { return value ? "true" : "false"; }

std::string to_string(Nil) { return "nil"; }

std::string to_string(double value) { return std::format("{:g}", value); }

std::string to_string(std::string value) { return value; }

} // namespace

void print_value(Value value) {
  std::cout << std::visit([](auto &&v) { return to_string(v); }, value);
}

bool values_equal(Value const &a, Value const &b) {
  if (a.index() != b.index()) {
    return false;
  }

  return std::visit(
      [](auto &&va, auto &&vb) -> bool {
        using A = std::decay_t<decltype(va)>;
        using B = std::decay_t<decltype(vb)>;

        if constexpr (std::is_same_v<A, Nil> && std::is_same_v<B, Nil>) {
          return true;
        } else if constexpr (std::is_same_v<A, B>) {
          return va == vb;
        }

        return false;
      },
      a, b);
}
