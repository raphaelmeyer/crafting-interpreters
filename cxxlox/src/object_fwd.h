#pragma once

#include <memory>

struct Function;
struct Native;
struct Closure;
struct UpValue;

using ObjFunction = std::shared_ptr<Function>;
using ObjNative = std::shared_ptr<Native>;
using ObjClosure = std::shared_ptr<Closure>;
using ObjUpvalue = std::shared_ptr<UpValue>;
