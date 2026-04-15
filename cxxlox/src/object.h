#pragma once

#include "object_fwd.h"

#include "chunk.h"

#include <cstddef>
#include <functional>
#include <span>
#include <string>
#include <unordered_map>
#include <variant>

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
  ObjHandle function;
  std::vector<ObjHandle> upvalues;
};

struct Class {
  std::string name;
};

struct Instance {
  ObjHandle klass;
  std::unordered_map<std::string, Value> fields;
};

struct StackSlot {
  std::size_t from_start;
};

struct Closed {
  Value closed;
};

struct UpValue {
  std::variant<StackSlot, Closed> value;
};

struct Obj {
  bool marked = false;
  std::variant<Function, Native, Closure, Class, Instance, UpValue> data;
};

ObjHandle new_class(ObjList &objects, std::string const &name);
ObjHandle new_closure(ObjList &objects, ObjHandle function);
ObjHandle new_function(ObjList &objects);
ObjHandle new_instance(ObjList &objects, ObjHandle klass);
ObjHandle new_native(ObjList &objects, std::size_t arity, NativeFn function);
ObjHandle new_upvalue(ObjList &objects, std::size_t stack_slot);
