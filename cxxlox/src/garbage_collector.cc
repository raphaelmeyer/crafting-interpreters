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

void GarbageCollector::collect_garbage() {
  if (Debug::GC_LOG) {
    std::cout << "-- gc begin\n";
  }

  if (Debug::GC_LOG) {
    std::cout << "-- gc end\n";
  }
}
