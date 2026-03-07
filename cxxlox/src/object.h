#pragma once

#include "object_fwd.h"

#include "chunk.h"

#include <cstddef>
#include <functional>
#include <span>
#include <string>

struct Function {
  std::size_t arity;
  Chunk chunk;
  std::string name;
};

using NativeFn = std::function<Value(std::size_t, std::span<Value>)>;

struct Native {
  NativeFn function;
};

ObjFunction new_function();
ObjNative new_native(NativeFn function);

NativeFn as_native(Value const &value);
