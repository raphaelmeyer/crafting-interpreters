#include "object.h"

#include "value.h"

ObjFunction new_function() { return std::make_shared<Function>(); }

ObjNative new_native(std::size_t arity, NativeFn function) {
  return std::make_shared<Native>(arity, function);
}

ObjClosure new_closure(ObjFunction function) {
  auto closure = std::make_shared<Closure>(
      function, std::vector<ObjUpvalue>{function->upvalue_count, nullptr});
  return closure;
}

ObjClass new_class(std::string const &name) {
  auto klass = std::make_shared<Class>(name);
  return klass;
}

ObjUpvalue new_upvalue(std::size_t stack_slot) {
  return std::make_shared<UpValue>(StackSlot{stack_slot});
}
