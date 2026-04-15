#pragma once

#include <memory>
#include <vector>

struct Function;
struct Native;
struct Closure;
struct Class;
struct Instance;

struct Obj;

using ObjRef = std::shared_ptr<Obj>;
using ObjHandle = std::weak_ptr<Obj>;
using ObjList = std::vector<ObjRef>;
