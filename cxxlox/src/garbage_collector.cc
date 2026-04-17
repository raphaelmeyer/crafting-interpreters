#include "garbage_collector.h"

#include "debug.h"
#include "object.h"

#include <format>
#include <iostream>

void GarbageCollector::trigger() {
  if (Debug::GC_STRESS) {
    collect_garbage();
  }
  if (objects.size() >= next_gc_threshold) {
    collect_garbage();
  }
}

void GarbageCollector::manage(ObjRef const &obj) {
  objects.push_back(obj);

  if (Debug::GC_LOG) {
    std::cout << std::format("{} allocate {}", static_cast<void *>(obj.get()),
                             obj_type_string(*obj))
              << "\n";
  }
}

void GarbageCollector::mark_object(ObjRef const &obj) {
  if ((not obj) or obj->is_marked) {
    return;
  }

  if (Debug::GC_LOG) {
    std::cout << std::format("{} mark ", static_cast<void *>(obj.get()));
    print_value(std::cout, obj_value(obj));
    std::cout << "\n";
  }

  obj->is_marked = true;
  gray_stack.push_back(obj);
}

void GarbageCollector::mark_value(Value const &value) {
  if (is_obj(value)) {
    mark_object(as_obj(value));
  }
}

void GarbageCollector::mark_roots() {
  if (on_mark_roots) {
    on_mark_roots();
  }
}

void GarbageCollector::blacken_object(ObjRef const &obj) {
  if (Debug::GC_LOG) {
    std::cout << std::format("{} blacken ", static_cast<void *>(obj.get()));
    print_value(std::cout, obj_value(obj));
    std::cout << "\n";
  }

  std::visit(
      [this](auto const &data) {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, Function>) {
          mark_values(data.chunk.constants);
        } else if constexpr (std::is_same_v<T, Closure>) {
          mark_object(data.function.lock());
          mark_objects(data.upvalues);
        } else if constexpr (std::is_same_v<T, Instance>) {
          mark_object(data.klass.lock());
          mark_values(data.fields | std::ranges::views::values);
        } else if constexpr (std::is_same_v<T, UpValue>) {
          auto const *closed = std::get_if<Closed>(&data.value);
          if (closed != nullptr) {
            mark_value(closed->closed);
          }
        }
      },
      obj->data);
}

void GarbageCollector::trace_references() {
  while (not gray_stack.empty()) {
    ObjRef obj = gray_stack.back();
    gray_stack.pop_back();
    blacken_object(obj);
  }
}

void GarbageCollector::sweep() {
  std::erase_if(objects, [](ObjRef const &obj) {
    if (obj->is_marked) {
      obj->is_marked = false;
      return false;
    }
    return true;
  });
}

void GarbageCollector::collect_garbage() {
  if (Debug::GC_LOG) {
    std::cout << "-- gc begin" << "\n";
  }

  std::size_t const before = objects.size();

  mark_roots();
  trace_references();
  sweep();

  next_gc_threshold = objects.size() * GC_HEAP_GROW_FACTOR;

  if (Debug::GC_LOG) {
    std::cout << "-- gc end" << "\n";
    std::cout << std::format(
                     "   collected {} objects (from {} to {}) next at {}",
                     before - objects.size(), before, objects.size(),
                     next_gc_threshold)
              << "\n";
  }
}
