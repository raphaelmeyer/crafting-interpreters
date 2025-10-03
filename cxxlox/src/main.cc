#include "debug.h"
#include "vm.h"

#include <filesystem>
#include <fstream>
#include <iostream>
#include <optional>
#include <span>
#include <string_view>

namespace {

void usage() { std::exit(64); }

void repl(VM &vm) {
  for (;;) {
    std::cout << "> " << std::flush;

    std::string line{};
    if (not std::getline(std::cin, line)) {
      printf("\n");
      break;
    }

    vm.interpret(line);
  }
}

std::string read_file(std::filesystem::path path) {
  std::ifstream file{path, std::ios::binary | std::ios::in};
  if (not file.is_open()) {
    std::cerr << "Could not open file " << path << ".\n";
    std::exit(74);
  }

  auto const size = std::filesystem::file_size(path);
  std::string source{};
  source.resize(size, '\0');

  file.read(source.data(), source.size());
  if (not file.good()) {
    std::cerr << "Could not read file " << path << ".\n";
    std::exit(74);
  }

  return source;
}

void run_file(std::filesystem::path path, VM &vm) {
  auto const source = read_file(path);
  auto const result = vm.interpret(source);

  if (result == InterpretResult::COMPILE_ERROR) {
    std::exit(65);
  }
  if (result == InterpretResult::RUNTIME_ERROR) {
    std::exit(70);
  }
}

} // namespace

int main(int argc, char *argv[]) {
  std::optional<std::filesystem::path> source_file;

  for (std::string_view const arg : std::span(argv + 1, argc - 1)) {
    using namespace std::literals;

    if (arg.starts_with("--"sv)) {
      if (arg == "--debug"sv) {
        Debug::TRACE_EXECUTION = true;
        Debug::PRINT_CODE = true;
      } else {
        usage();
      }
    } else {
      if (source_file.has_value()) {
        usage();
      }
      source_file = arg;
    }
  }

  auto const vm = VM::create();

  if (source_file.has_value()) {
    run_file(source_file.value(), *vm);
  } else {
    repl(*vm);
  }

  return 0;
}
