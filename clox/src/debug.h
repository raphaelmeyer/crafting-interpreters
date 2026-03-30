#pragma once

#include "chunk.h"

#ifdef __cplusplus
extern "C" {
#endif

extern bool DEBUG_TRACE_EXECUTION;
extern bool DEBUG_PRINT_CODE;
extern bool DEBUG_STRESS_GC;
extern bool DEBUG_LOG_GC;

void disassemble_chunk(Chunk const *chunk, char const *name);
int32_t disassemble_instruction(Chunk const *chunk, int32_t offset);

#ifdef __cplusplus
} // extern "C"
#endif
