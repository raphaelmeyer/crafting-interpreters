#pragma once

#include <memory>
#include <string_view>

struct Chunk;

class Compiler {
public:
  virtual ~Compiler() = default;

  virtual bool compile(std::string_view source, Chunk &chunk) = 0;

  static std::unique_ptr<Compiler> create();
};
