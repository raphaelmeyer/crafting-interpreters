#pragma once

#include "chunk.h"

#include <string_view>

bool compile(std::string_view source, Chunk &chunk);
