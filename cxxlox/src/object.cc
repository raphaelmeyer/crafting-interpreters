#include "object.h"

#include "value.h"

ObjFunction new_function() { return std::make_shared<Function>(); }

ObjNative new_native(NativeFn function) {
  return std::make_shared<Native>(function);
}

NativeFn as_native(Value const &value) {
  return std::get<ObjNative>(value)->function;
}
