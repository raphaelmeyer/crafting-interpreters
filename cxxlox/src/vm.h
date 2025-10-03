#pragma once

#include <memory>
#include <string_view>

enum class InterpretResult { OK, COMPILE_ERROR, RUNTIME_ERROR };

class VM {
public:
  virtual ~VM() = default;
  virtual InterpretResult interpret(std::string_view source) = 0;

  static std::unique_ptr<VM> create();
};
