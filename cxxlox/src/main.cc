#include "chunk.h"
#include "debug.h"
#include "vm.h"

#include <string_view>

int main(int argc, char *argv[]) {
  for (std::int32_t i = 1; i < argc; ++i) {
    using namespace std::literals;
    if (std::string_view(argv[i]) == "--debug"sv) {
      Debug::TRACE_EXECUTION = true;
    }
  }

  init_vm();

  Chunk chunk{};

  auto const constant = add_constant(chunk, 1.2);
  write_chunk(chunk, OpCode::CONSTANT, 123);
  write_chunk(chunk, constant, 123);

  write_chunk(chunk, OpCode::RETURN, 123);

  disassemble_chunk(chunk, "test chunk");
  interpret(&chunk);

  free_vm();

  return 0;
}
