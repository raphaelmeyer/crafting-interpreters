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
