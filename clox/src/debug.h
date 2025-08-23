#pragma once

#include "chunk.h"

#ifdef __cplusplus
extern "C" {
#endif

void disassemble_chunk(Chunk const *chunk, char const *name);

#ifdef __cplusplus
} // extern "C"
#endif
