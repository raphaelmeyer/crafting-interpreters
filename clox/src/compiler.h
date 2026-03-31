#pragma once

#include "object.h"

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

ObjFunction *compile(char const *source);
void mark_compiler_roots();

#ifdef __cplusplus
} // extern "C"
#endif
