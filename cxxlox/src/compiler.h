#pragma once

#include "object.h"

#include <iostream>
#include <memory>
#include <string_view>

struct Chunk;

class Compiler {
public:
  virtual ~Compiler() = default;

  virtual ObjFunction compile(std::string_view source) = 0;

  static std::unique_ptr<Compiler> create(std::ostream &err = std::cerr);
};
