#include "object.h"

#include "debug.h"
#include "garbage_collector.h"

#include <format>
#include <iostream>

std::string_view obj_type_string(Obj const &obj) {
  return std::visit(
      [](auto const &data) -> std::string_view {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, Function>) {
          return "function";
        } else if constexpr (std::is_same_v<T, Native>) {
          return "native";
        } else if constexpr (std::is_same_v<T, Closure>) {
          return "closure";
        } else if constexpr (std::is_same_v<T, Class>) {
          return "class";
        } else if constexpr (std::is_same_v<T, Instance>) {
          return "instance";
        } else {
          return "upvalue";
        }
      },
      obj.data);
}

Obj::~Obj() {
  if (Debug::GC_LOG) {
    std::cout << std::format("{} free {}\n", static_cast<void *>(this),
                             obj_type_string(*this));
  }
}

ObjHandle new_class(GarbageCollector &gc, std::string const &name) {
  gc.trigger();
  auto obj = std::make_shared<Obj>(Class{name});
  gc.manage(obj);
  return obj;
}

ObjHandle new_closure(GarbageCollector &gc, ObjHandle function) {
  gc.trigger();
  auto const upvalue_count =
      std::get<Function>(function.lock()->data).upvalue_count;
  auto obj = std::make_shared<Obj>(
      Closure{function, std::vector<ObjHandle>(upvalue_count)});
  gc.manage(obj);
  return obj;
}

ObjHandle new_function(GarbageCollector &gc) {
  gc.trigger();
  auto obj = std::make_shared<Obj>(Function{});
  gc.manage(obj);
  return obj;
}

ObjHandle new_instance(GarbageCollector &gc, ObjHandle klass) {
  gc.trigger();
  auto obj = std::make_shared<Obj>(Instance{.klass = klass, .fields = {}});
  gc.manage(obj);
  return obj;
}

ObjHandle new_native(GarbageCollector &gc, std::size_t arity,
                     NativeFn function) {
  gc.trigger();
  auto obj = std::make_shared<Obj>(Native{arity, function});
  gc.manage(obj);
  return obj;
}

ObjHandle new_upvalue(GarbageCollector &gc, std::size_t stack_slot) {
  gc.trigger();
  auto obj = std::make_shared<Obj>(UpValue{StackSlot{stack_slot}});
  gc.manage(obj);
  return obj;
}
