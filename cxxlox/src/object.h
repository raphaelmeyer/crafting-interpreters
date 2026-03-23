#pragma once

#include "object_fwd.h"

#include "chunk.h"

#include <cstddef>
#include <functional>
#include <span>
#include <string>
#include <vector>

struct Function {
  std::size_t arity;
  std::size_t upvalue_count;
  Chunk chunk;
  std::string name;
};

using NativeFn = std::function<Value(std::size_t, std::span<Value>)>;

struct Native {
  std::size_t arity;
  NativeFn function;
};

struct Closure {
  ObjFunction function;
  std::vector<ObjUpvalue> upvalues;
};

struct StackSlot {
  std::size_t from_start;
};

struct UpValue {
  std::variant<StackSlot> value;
};

ObjFunction new_function();
ObjNative new_native(std::size_t arity, NativeFn function);
ObjClosure new_closure(ObjFunction function);
ObjUpvalue new_upvalue(std::size_t stack_slot);
