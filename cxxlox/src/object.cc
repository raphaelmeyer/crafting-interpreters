#include "object.h"

ObjFunction new_function() { return std::make_shared<Function>(); }
