#include "value.h"

#include <format>
#include <iostream>

void print_value(Value value) { std::cout << std::format("{}", value); }
