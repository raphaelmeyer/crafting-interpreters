#include "debug.h"

#include <stdio.h>

static int32_t disassemble_instruction(Chunk const *chunk, int32_t offset);
static int32_t simple_instruction(char const *name, int offset);

void disassemble_chunk(Chunk const *chunk, char const *name) {
  printf("== %s ==\n", name);

  for (int32_t offset = 0; offset < chunk->count;) {
    offset = disassemble_instruction(chunk, offset);
  }
}

int32_t disassemble_instruction(Chunk const *chunk, int32_t offset) {
  printf("%04d ", offset);

  uint8_t instruction = chunk->code[offset];
  switch (instruction) {
  case OP_RETURN:
    return simple_instruction("OP_RETURN", offset);
  default:
    printf("Unknown opcode %d\n", instruction);
    return offset + 1;
  }
}

int32_t simple_instruction(char const *name, int offset) {
  printf("%s\n", name);
  return offset + 1;
}
