#pragma once

#include "object_fwd.h"

#include "chunk.h"

#include <cstddef>
#include <functional>
#include <span>
#include <string>
#include <unordered_map>
#include <variant>

class GarbageCollector;

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
  ~Obj();

  bool marked = false;
  std::variant<Function, Native, Closure, Class, Instance, UpValue> data;
};

std::string_view obj_type_string(Obj const &obj);

ObjHandle new_class(GarbageCollector &gc, std::string const &name);
ObjHandle new_closure(GarbageCollector &gc, ObjHandle function);
ObjHandle new_function(GarbageCollector &gc);
ObjHandle new_instance(GarbageCollector &gc, ObjHandle klass);
ObjHandle new_native(GarbageCollector &gc, std::size_t arity,
                     NativeFn function);
ObjHandle new_upvalue(GarbageCollector &gc, std::size_t stack_slot);
