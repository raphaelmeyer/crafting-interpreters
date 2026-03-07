#pragma once

#include "object_fwd.h"

#include <string>
#include <variant>
#include <vector>

struct Nil {};

using Value =
    std::variant<Nil, bool, double, std::string, ObjFunction, ObjNative>;

ObjFunction new_function();

inline Value bool_value(bool value) { return {value}; }

inline Value nil_value() { return Nil{}; }

inline Value number_value(double value) { return {value}; }

inline Value string_value(std::string value) { return value; }

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

inline bool is_string(Value const &value) {
  return std::holds_alternative<std::string>(value);
}

inline std::string as_string(Value const &value) {
  return std::get<std::string>(value);
}

inline bool is_function(Value const &value) {
  return std::holds_alternative<ObjFunction>(value);
}

inline ObjFunction as_function(Value const &value) {
  return std::get<ObjFunction>(value);
}

inline bool is_native(Value const &value) {
  return std::holds_alternative<ObjNative>(value);
}

inline ObjNative as_native(Value const &value) {
  return std::get<ObjNative>(value);
}

using ValueArray = std::vector<Value>;

void print_value(Value value);

bool values_equal(Value const &a, Value const &b);
