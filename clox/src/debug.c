#include "debug.h"

#include <stdio.h>

static int32_t disassemble_instruction(Chunk const *chunk, int32_t offset);
static int32_t constant_instruction(char const *name, Chunk const *chunk,
                                    int32_t offset);
static int32_t simple_instruction(char const *name, int offset);

void disassemble_chunk(Chunk const *chunk, char const *name) {
  printf("== %s ==\n", name);

  for (int32_t offset = 0; offset < chunk->count;) {
    offset = disassemble_instruction(chunk, offset);
  }
}

int32_t disassemble_instruction(Chunk const *chunk, int32_t offset) {
  printf("%04d ", offset);

  if (offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1]) {
    printf("   | ");
  } else {
    printf("%4d ", chunk->lines[offset]);
  }

  uint8_t instruction = chunk->code[offset];
  switch (instruction) {
  case OP_CONSTANT:
    return constant_instruction("OP_CONSTANT", chunk, offset);
  case OP_RETURN:
    return simple_instruction("OP_RETURN", offset);
  default:
    printf("Unknown opcode %d\n", instruction);
    return offset + 1;
  }
}

static int32_t constant_instruction(char const *name, Chunk const *chunk,
                                    int32_t offset) {
  uint8_t const constant = chunk->code[offset + 1];
  printf("%-16s %4d '", name, constant);
  print_value(chunk->constants.values[constant]);
  printf("'\n");
  return offset + 2;
}

int32_t simple_instruction(char const *name, int offset) {
  printf("%s\n", name);
  return offset + 1;
}
