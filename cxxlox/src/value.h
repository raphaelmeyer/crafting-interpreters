#pragma once

#include <vector>

enum class ValueType { BOOL, NIL, NUMBER };

struct Value {
  ValueType type;
  union Storage {
    bool boolean;
    double number;
  } as;
};

inline Value bool_value(bool value) {
  return {ValueType::BOOL, {.boolean = value}};
}

inline Value nil_value() { return {ValueType::NIL, {.number = 0}}; }

inline Value number_value(double value) {
  return {ValueType::NUMBER, {.number = value}};
}

inline bool is_bool(Value const &value) {
  return value.type == ValueType::BOOL;
}

inline bool is_nil(Value const &value) { return value.type == ValueType::NIL; }

inline bool is_number(Value const &value) {
  return value.type == ValueType::NUMBER;
}

using ValueArray = std::vector<Value>;

void print_value(Value value);
