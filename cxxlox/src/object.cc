#include "object.h"

#include "value.h"

ObjFunction new_function() { return std::make_shared<Function>(); }

ObjNative new_native(std::size_t arity, NativeFn function) {
  return std::make_shared<Native>(arity, function);
}
