#include "garbage_collector.h"

#include "debug.h"
#include "object.h"

#include <format>
#include <iostream>

void GarbageCollector::trigger() {
  if (Debug::GC_STRESS) {
    collect_garbage();
  }
}

void GarbageCollector::manage(ObjRef const &obj) {
  objects.push_back(obj);

  if (Debug::GC_LOG) {
    std::cout << std::format("{} allocate {}\n", static_cast<void *>(obj.get()),
                             obj_type_string(*obj));
  }
}

void GarbageCollector::mark_object(ObjRef const &obj) {
  if ((not obj) or obj->is_marked) {
    return;
  }

  if (Debug::GC_LOG) {
    std::cout << std::format("{} mark {}\n", static_cast<void *>(obj.get()),
                             obj_type_string(*obj));
  }

  obj->is_marked = true;
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

void GarbageCollector::collect_garbage() {
  if (Debug::GC_LOG) {
    std::cout << "-- gc begin\n";
  }

  mark_roots();

  if (Debug::GC_LOG) {
    std::cout << "-- gc end\n";
  }
}
