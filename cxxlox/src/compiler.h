#pragma once

#include "object_fwd.h"

#include <iostream>
#include <memory>
#include <string_view>

struct Chunk;

class Compiler {
public:
  virtual ~Compiler() = default;

  virtual ObjHandle compile(std::string_view source) = 0;

  static std::unique_ptr<Compiler> create(ObjList &objects,
                                          std::ostream &err = std::cerr);
};
