#pragma once

#include "object_fwd.h"

class GarbageCollector {
public:
  void trigger();
  void manage(ObjRef const &obj);

private:
  void collect_garbage();

  ObjList objects;
};
