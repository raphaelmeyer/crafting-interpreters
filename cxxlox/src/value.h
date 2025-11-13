#pragma once

#include <variant>
#include <vector>

struct Nil {};

using Value = std::variant<Nil, bool, double>;

inline Value bool_value(bool value) { return {value}; }

inline Value nil_value() { return Nil{}; }

inline Value number_value(double value) { return {value}; }

inline bool is_bool(Value const &value) {
  return std::holds_alternative<bool>(value);
}

inline bool as_bool(Value const &value) { return std::get<bool>(value); }

inline bool is_nil(Value const &value) {
  return std::holds_alternative<Nil>(value);
}

inline bool is_number(Value const &value) {
  return std::holds_alternative<double>(value);
}

inline double as_number(Value const &value) { return std::get<double>(value); }

using ValueArray = std::vector<Value>;

void print_value(Value value);

bool values_equal(Value const &a, Value const &b);
