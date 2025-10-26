#include "value.h"

#include <format>
#include <iostream>

void print_value(Value value) {
  switch (value.type) {
  case ValueType::BOOL:
    std::cout << (value.as.boolean ? "true" : "false");
    break;
  case ValueType::NIL:
    std::cout << "nil";
    break;
  case ValueType::NUMBER:
    std::cout << std::format("{:g}", value.as.number);
  }
}

bool values_equal(Value const &a, Value const &b) {
  if (a.type != b.type) {
    return false;
  }
  switch (a.type) {
  case ValueType::BOOL:
    return a.as.boolean == b.as.boolean;
  case ValueType::NIL:
    return true;
  case ValueType::NUMBER:
    return a.as.number == b.as.number;
  default:
    return false;
  }
}
