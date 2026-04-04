#include "memory.h"

#include "compiler.h"
#include "debug.h"
#include "vm.h"

#include <stdio.h>
#include <stdlib.h>

static VM *vm = NULL;

static const size_t GC_HEAP_GROW_FACTOR = 2;

static void free_item(size_t size, void *pointer) {
  reallocate(pointer, size, 0);
}

static void free_items(size_t item_size, void *pointer, size_t count) {
  reallocate(pointer, item_size * count, 0);
}

static void free_string(ObjString *string) {
  reallocate((char *)string->chars, sizeof(char) * (string->length + 1), 0);
  reallocate(string, sizeof(ObjString), 0);
}

static void free_object(Obj *object) {
  if (DEBUG_LOG_GC) {
    printf("%p free type %d\n", (void *)object, object->type);
  }

  switch (object->type) {

  case OBJ_CLOSURE: {
    ObjClosure *closure = (ObjClosure *)object;
    free_items(sizeof(ObjUpvalue *), closure->upvalues, closure->upvalue_count);
    free_item(sizeof(ObjClosure), object);
    break;
  }
  case OBJ_FUNCTION: {
    ObjFunction *function = (ObjFunction *)object;
    free_chunk(&function->chunk);
    free_item(sizeof(ObjFunction), object);
    break;
  }
  case OBJ_NATIVE: {
    free_item(sizeof(ObjNative), object);
    break;
  }
  case OBJ_STRING: {
    free_string((ObjString *)object);
    break;
  }
  case OBJ_UPVALUE: {
    free_item(sizeof(ObjUpvalue), object);
    break;
  }
  }
}

static void mark_array(ValueArray *array) {
  for (int32_t i = 0; i < array->count; ++i) {
    mark_value(array->values[i]);
  }
}

static void blacken_object(Obj *object) {
  if (DEBUG_LOG_GC) {
    printf("%p blacken ", (void *)object);
    print_value(obj_value(object));
    printf("\n");
  }

  switch (object->type) {
  case OBJ_CLOSURE: {
    ObjClosure *closure = (ObjClosure *)object;
    mark_object((Obj *)closure->function);
    for (size_t i = 0; i < closure->upvalue_count; ++i) {
      mark_object((Obj *)closure->upvalues[i]);
    }
    break;
  }
  case OBJ_FUNCTION: {
    ObjFunction *function = (ObjFunction *)object;
    mark_object((Obj *)function->name);
    mark_array(&function->chunk.constants);
    break;
  }
  case OBJ_UPVALUE:
    mark_value(((ObjUpvalue *)object)->closed);
    break;
  case OBJ_NATIVE:
  case OBJ_STRING:
    break;
  }
}

static void mark_roots() {
  for (Value *slot = vm->stack; slot < vm->stack_top; ++slot) {
    mark_value(*slot);
  }

  for (size_t i = 0; i < vm->frame_count; ++i) {
    mark_object((Obj *)vm->frames[i].closure);
  }

  for (ObjUpvalue *upvalue = vm->open_upvalues; upvalue != NULL;
       upvalue = upvalue->next) {
    mark_object((Obj *)upvalue);
  }

  mark_table(&vm->globals);
  mark_compiler_roots();
}

static void trace_references() {
  while (vm->gray_count > 0) {
    Obj *object = vm->gray_stack[--vm->gray_count];
    blacken_object(object);
  }
}

static void sweep() {
  Obj *previous = NULL;
  Obj *object = vm->objects;
  while (object != NULL) {
    if (object->is_marked) {
      object->is_marked = false;
      previous = object;
      object = object->next;
    } else {
      Obj *unreached = object;
      object = object->next;
      if (previous != NULL) {
        previous->next = object;
      } else {
        vm->objects = object;
      }

      free_object(unreached);
    }
  }
}

void collect_garbage() {
  size_t before = 0;
  if (DEBUG_LOG_GC) {
    printf("-- gc begin\n");
    before = vm->bytes_allocated;
  }

  mark_roots();
  trace_references();
  table_remove_white(&vm->strings);
  sweep();

  vm->next_gc = vm->bytes_allocated * GC_HEAP_GROW_FACTOR;

  if (DEBUG_LOG_GC) {
    printf("-- gc end\n");
    printf("   collected %zu bytes (from %zu to %zu) next at %zu\n",
           before - vm->bytes_allocated, before, vm->bytes_allocated,
           vm->next_gc);
  }
}

void *allocate(size_t item_size, size_t count) {
  return reallocate(NULL, 0, item_size * count);
}

void *reallocate(void *pointer, size_t old_size, size_t new_size) {
  vm->bytes_allocated += new_size - old_size;

  if (new_size > old_size) {
    if (DEBUG_STRESS_GC) {
      collect_garbage();
    }

    if (vm->bytes_allocated > vm->next_gc) {
      collect_garbage();
    }
  }

  if (new_size == 0) {
    free(pointer);
    return NULL;
  }

  void *result = realloc(pointer, new_size);
  if (result == NULL) {
    exit(1);
  }

  return result;
}

void mark_object(Obj *object) {
  if (object == NULL) {
    return;
  }
  if (object->is_marked) {
    return;
  }

  if (DEBUG_LOG_GC) {
    printf("%p mark ", (void *)object);
    print_value(obj_value(object));
    printf("\n");
  }

  object->is_marked = true;

  if (vm->gray_capacity < vm->gray_count + 1) {
    vm->gray_capacity = grow_capacity(vm->gray_capacity);
    vm->gray_stack =
        (Obj **)realloc(vm->gray_stack, sizeof(Obj *) * vm->gray_capacity);

    if (vm->gray_stack == NULL) {
      exit(1);
    }
  }

  vm->gray_stack[vm->gray_count++] = object;
}

void mark_value(Value value) {
  if (is_obj(value)) {
    mark_object(value.as.obj);
  }
}

void free_objects(Obj *head) {
  Obj *object = head;
  while (object != NULL) {
    Obj *next = object->next;
    free_object(object);
    object = next;
  }

  free(vm->gray_stack);
}

void init_garbage_collector(VM *vm_instance) { vm = vm_instance; }
