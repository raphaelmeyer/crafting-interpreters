
#include "debug.h"
#include "vm.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void usage() {
  fprintf(stderr, "Usage: clox [--debug] [path]\n");
  exit(64);
}

static void repl() {
  char line[1024];
  for (;;) {
    printf("> ");

    if (!fgets(line, sizeof(line), stdin)) {
      printf("\n");
      break;
    }

    interpret(line);
  }
}

static char *read_file(char const *path) {
  FILE *file = fopen(path, "rb");
  if (file == NULL) {
    fprintf(stderr, "Could not open file \"%s\".\n", path);
    exit(74);
  }

  fseek(file, 0, SEEK_END);
  size_t file_size = ftell(file);
  rewind(file);

  char *buffer = (char *)malloc(file_size + 1);
  if (file == NULL) {
    fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
    exit(74);
  }

  size_t bytes_read = fread(buffer, sizeof(char), file_size, file);
  if (bytes_read < file_size) {
    fprintf(stderr, "Could not read file \"%s\".\n", path);
    exit(74);
  }

  buffer[bytes_read] = '\0';

  fclose(file);
  return buffer;
}

static void run_file(char const *path) {
  char *source = read_file(path);
  InterpretResult result = interpret(source);
  free(source);

  if (result == INTERPRET_COMPILE_ERROR) {
    exit(65);
  }
  if (result == INTERPRET_RUNTIME_ERROR) {
    exit(70);
  }
}

int main(int argc, const char *argv[]) {
  char const *source_file = NULL;

  for (int32_t i = 1; i < argc; ++i) {
    if (strncmp(argv[i], "--", 2) == 0) {
      if (strcmp(argv[i], "--debug") == 0) {
        DEBUG_TRACE_EXECUTION = true;
      } else {
        usage();
      }
    } else {
      if (source_file != NULL) {
        usage();
      }
      source_file = argv[i];
    }
  }

  init_vm();

  if (source_file != NULL) {
    run_file(source_file);
  } else {
    repl();
  }

  free_vm();

  return 0;
}
