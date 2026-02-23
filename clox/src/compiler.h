#pragma once

#include "object.h"

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

ObjFunction *compile(char const *source);

#ifdef __cplusplus
} // extern "C"
#endif
