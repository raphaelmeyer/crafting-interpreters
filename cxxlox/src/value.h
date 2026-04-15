#pragma once

#include "object_fwd.h"

#include <iostream>
#include <string>
#include <variant>
#include <vector>

struct Nil {};

using Value = std::variant<Nil, bool, double, std::string, ObjHandle>;

inline Value bool_value(bool value) { return {value}; }
inline Value nil_value() { return Nil{}; }
inline Value number_value(double value) { return {value}; }
inline Value string_value(std::string value) { return value; }
inline Value obj_value(ObjHandle h) { return h; }

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

inline bool is_obj(Value const &value) {
  return std::holds_alternative<ObjHandle>(value);
}
inline ObjRef as_obj(Value const &value) {
  return std::get<ObjHandle>(value).lock();
}

bool is_class(Value const &value);
bool is_closure(Value const &value);
bool is_function(Value const &value);
bool is_instance(Value const &value);
bool is_native(Value const &value);

Class &as_class(Value const &value);
Closure &as_closure(Value const &value);
Function &as_function(Value const &value);
Instance &as_instance(Value const &value);
Native &as_native(Value const &value);

using ValueArray = std::vector<Value>;

void print_value(std::ostream &out, Value value);

bool values_equal(Value const &a, Value const &b);
