#pragma once

#include "chunk.h"

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

constexpr const size_t UINT8_COUNT = UINT8_MAX + 1;

bool compile(char const *source, Chunk *chunk);

#ifdef __cplusplus
} // extern "C"
#endif
