#pragma once

#include <memory>

struct Function;
struct Native;

using ObjFunction = std::shared_ptr<Function>;
using ObjNative = std::shared_ptr<Native>;
