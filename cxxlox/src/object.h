#pragma once

#include "chunk.h"

#include <cstddef>
#include <memory>
#include <string>

struct Function {
  std::size_t arity;
  Chunk chunk;
  std::string name;
};

using ObjFunction = std::shared_ptr<Function>;

ObjFunction new_function();
