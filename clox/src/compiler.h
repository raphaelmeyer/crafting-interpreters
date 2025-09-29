#pragma once

#include "chunk.h"

#ifdef __cplusplus
extern "C" {
#endif

bool compile(char const *source, Chunk *chunk);

#ifdef __cplusplus
} // extern "C"
#endif
