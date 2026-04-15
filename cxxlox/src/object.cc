#include "object.h"

ObjHandle new_class(ObjList &objects, std::string const &name) {
  auto obj = std::make_shared<Obj>(Obj{.data = Class{name}});
  objects.push_back(obj);
  return obj;
}

ObjHandle new_closure(ObjList &objects, ObjHandle function) {
  auto const upvalue_count =
      std::get<Function>(function.lock()->data).upvalue_count;
  auto obj = std::make_shared<Obj>(
      Obj{.data = Closure{function, std::vector<ObjHandle>(upvalue_count)}});
  objects.push_back(obj);
  return obj;
}

ObjHandle new_function(ObjList &objects) {
  auto obj = std::make_shared<Obj>(Obj{.data = Function{}});
  objects.push_back(obj);
  return obj;
}

ObjHandle new_instance(ObjList &objects, ObjHandle klass) {
  auto obj = std::make_shared<Obj>(
      Obj{.data = Instance{.klass = klass, .fields = {}}});
  objects.push_back(obj);
  return obj;
}

ObjHandle new_native(ObjList &objects, std::size_t arity, NativeFn function) {
  auto obj = std::make_shared<Obj>(Obj{.data = Native{arity, function}});
  objects.push_back(obj);
  return obj;
}

ObjHandle new_upvalue(ObjList &objects, std::size_t stack_slot) {
  auto obj = std::make_shared<Obj>(Obj{.data = UpValue{StackSlot{stack_slot}}});
  objects.push_back(obj);
  return obj;
}
