#include "vm.h"

#include "compiler.h"
#include "debug.h"
#include "memory.h"
#include "object.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

static VM vm;

static Value clock_native(int32_t, Value *) {
  return number_value((double)clock() / CLOCKS_PER_SEC);
}

static void reset_stack() {
  vm.stack_top = vm.stack;
  vm.frame_count = 0;
  vm.open_upvalues = NULL;
}

static void runtime_error(char const *format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  for (int32_t i = vm.frame_count - 1; i >= 0; --i) {
    CallFrame const *frame = &vm.frames[i];
    ObjFunction const *function = frame->closure->function;
    size_t instruction = frame->ip - function->chunk.code - 1;
    fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);
    if (function->name == NULL) {
      fprintf(stderr, "script\n");
    } else {
      fprintf(stderr, "%s()\n", function->name->chars);
    }
  }

  reset_stack();
}

static void print_stack() {
  printf("          ");
  for (Value *slot = vm.stack; slot < vm.stack_top; ++slot) {
    printf("[ ");
    print_value(*slot);
    printf(" ]");
  }
  printf("\n");
}

void push(Value value) {
  *vm.stack_top = value;
  vm.stack_top++;
}

Value pop() {
  vm.stack_top--;
  return *vm.stack_top;
}

static Value peek(int distance) { return vm.stack_top[-1 - distance]; }

static void define_native(const char *name, NativeFn function) {
  push(obj_value(copy_string(name, strlen(name))));
  push(obj_value(new_native(function)));
  table_set(&vm.globals, as_string(vm.stack[0]), vm.stack[1]);
}

static bool call(ObjClosure *closure, int arg_count) {
  if (arg_count != closure->function->arity) {
    runtime_error("Expected %d arguments but got %d.", closure->function->arity,
                  arg_count);
    return false;
  }

  if (vm.frame_count == FRAMES_MAX) {
    runtime_error("Stack overflow.");
    return false;
  }

  CallFrame *frame = &vm.frames[vm.frame_count++];
  frame->closure = closure;
  frame->ip = closure->function->chunk.code;
  frame->slots = vm.stack_top - arg_count - 1;
  return true;
}

static bool call_value(Value callee, int arg_count) {
  if (is_obj(callee)) {
    switch (obj_type(callee)) {
    case OBJ_CLASS: {
      ObjClass *klass = as_class(callee);
      vm.stack_top[-arg_count - 1] = obj_value(new_instance(klass));
      return true;
    }
    case OBJ_CLOSURE:
      return call(as_closure(callee), arg_count);
    case OBJ_NATIVE: {
      NativeFn native = as_native(callee);
      Value const result = native(arg_count, vm.stack_top - arg_count);
      vm.stack_top -= arg_count + 1;
      push(result);
      return true;
    }
    default:
      // Non-callable object type
      break;
    }
  }
  runtime_error("Can only call functions and classes.");
  return false;
}

static ObjUpvalue *capture_upvalue(Value *local) {
  ObjUpvalue *previous_upvalue = NULL;
  ObjUpvalue *upvalue = vm.open_upvalues;
  while (upvalue != NULL && upvalue->location > local) {
    previous_upvalue = upvalue;
    upvalue = upvalue->next;
  }

  if (upvalue != NULL && upvalue->location == local) {
    return upvalue;
  }

  ObjUpvalue *created_upvalue = new_upvalue(local);
  created_upvalue->next = upvalue;

  if (previous_upvalue == NULL) {
    vm.open_upvalues = created_upvalue;
  } else {
    previous_upvalue->next = created_upvalue;
  }

  return created_upvalue;
}

static void close_upvalues(Value *last) {
  while (vm.open_upvalues != NULL && vm.open_upvalues->location >= last) {
    ObjUpvalue *upvalue = vm.open_upvalues;
    upvalue->closed = *upvalue->location;
    upvalue->location = &upvalue->closed;
    vm.open_upvalues = upvalue->next;
  }
}

static bool is_falsey(Value value) {
  return is_nil(value) || (is_bool(value) && !value.as.boolean);
}

static void concatenate() {
  ObjString *b = as_string(peek(0));
  ObjString *a = as_string(peek(1));

  size_t length = a->length + b->length;
  char *chars = allocate(sizeof(char), length + 1);
  memcpy(chars, a->chars, a->length);
  memcpy(chars + a->length, b->chars, b->length);
  chars[length] = '\0';

  ObjString *result = take_string(chars, length);
  pop();
  pop();
  push(obj_value(result));
}

void init_vm() {
  reset_stack();
  init_object_allocation(&vm);
  vm.objects = NULL;
  vm.bytes_allocated = 0;
  vm.next_gc = 1024 * 1024;

  vm.gray_count = 0;
  vm.gray_capacity = 0;
  vm.gray_stack = NULL;

  init_table(&vm.globals);
  init_table(&vm.strings);

  define_native("clock", clock_native);
}

void free_vm() {
  free_table(&vm.globals);
  free_table(&vm.strings);
  free_objects(vm.objects);
}

static inline uint8_t read_byte(CallFrame *frame) { return *frame->ip++; }

static inline uint16_t read_short(CallFrame *frame) {
  frame->ip += 2;
  return (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]);
}

static inline Value read_constant(CallFrame *frame) {
  return frame->closure->function->chunk.constants.values[read_byte(frame)];
}

static inline ObjString *read_string(CallFrame *frame) {
  return as_string(read_constant(frame));
}

static Value subtract(double a, double b) { return number_value(a - b); }
static Value multiply(double a, double b) { return number_value(a * b); }
static Value divide(double a, double b) { return number_value(a / b); }
static Value greater(double a, double b) { return bool_value(a > b); }
static Value less(double a, double b) { return bool_value(a < b); }

static inline InterpretResult binary_op(Value (*op)(double, double)) {
  if (!is_number(peek(0)) || !is_number(peek(1))) {
    runtime_error("Operands must be numbers.");
    return INTERPRET_RUNTIME_ERROR;
  }
  double b = pop().as.number;
  double a = pop().as.number;
  push(op(a, b));

  return INTERPRET_OK;
}

static InterpretResult run() {
  CallFrame *frame = &vm.frames[vm.frame_count - 1];

  if (DEBUG_TRACE_EXECUTION) {
    printf("== run vm ==\n");
  }

  for (;;) {
    if (DEBUG_TRACE_EXECUTION) {
      print_stack();
      disassemble_instruction(
          &frame->closure->function->chunk,
          (int32_t)(frame->ip - frame->closure->function->chunk.code));
    }

    uint8_t instruction;
    switch (instruction = read_byte(frame)) {

    case OP_CONSTANT: {
      Value const constant = read_constant(frame);
      push(constant);
      break;
    }

    case OP_NIL:
      push(nil_value());
      break;
    case OP_TRUE:
      push(bool_value(true));
      break;
    case OP_FALSE:
      push(bool_value(false));
      break;

    case OP_POP:
      pop();
      break;

    case OP_GET_LOCAL: {
      uint8_t slot = read_byte(frame);
      push(frame->slots[slot]);
      break;
    }

    case OP_SET_LOCAL: {
      uint8_t slot = read_byte(frame);
      frame->slots[slot] = peek(0);
      break;
    }

    case OP_GET_GLOBAL: {
      ObjString *name = read_string(frame);
      Value value;
      if (!table_get(&vm.globals, name, &value)) {
        runtime_error("Undefined variable '%s'.", name->chars);
        return INTERPRET_RUNTIME_ERROR;
      }
      push(value);
      break;
    }

    case OP_DEFINE_GLOBAL: {
      ObjString *const name = read_string(frame);
      table_set(&vm.globals, name, peek(0));
      pop();
      break;
    }

    case OP_SET_GLOBAL: {
      ObjString *name = read_string(frame);
      if (table_set(&vm.globals, name, peek(0))) {
        table_delete(&vm.globals, name);
        runtime_error("Undefined variable '%s'.", name->chars);
        return INTERPRET_RUNTIME_ERROR;
      }
      break;
    }

    case OP_GET_UPVALUE: {
      uint8_t slot = read_byte(frame);
      push(*frame->closure->upvalues[slot]->location);
      break;
    }

    case OP_SET_UPVALUE: {
      uint8_t slot = read_byte(frame);
      *frame->closure->upvalues[slot]->location = peek(0);
      break;
    }

    case OP_GET_PROPERTY: {
      if (!is_instance(peek(0))) {
        runtime_error("Only instances have properties.");
        return INTERPRET_RUNTIME_ERROR;
      }

      ObjInstance const *instance = as_instance(peek(0));
      ObjString const *name = read_string(frame);

      Value value;
      if (table_get(&instance->fields, name, &value)) {
        pop();
        push(value);
        break;
      }

      runtime_error("Undefined property '%s'.", name->chars);
      return INTERPRET_RUNTIME_ERROR;
    }

    case OP_SET_PROPERTY: {
      if (!is_instance(peek(1))) {
        runtime_error("Only instances have fields.");
        return INTERPRET_RUNTIME_ERROR;
      }

      ObjInstance *instance = as_instance(peek(1));
      table_set(&instance->fields, read_string(frame), peek(0));
      Value value = pop();
      pop();
      push(value);
      break;
    }

    case OP_EQUAL: {
      Value b = pop();
      Value a = pop();
      push(bool_value(values_equal(a, b)));
      break;
    }

    case OP_GREATER: {
      InterpretResult result = binary_op(greater);
      if (result != INTERPRET_OK) {
        return result;
      }
      break;
    }
    case OP_LESS: {
      InterpretResult result = binary_op(less);
      if (result != INTERPRET_OK) {
        return result;
      }
      break;
    }
    case OP_ADD: {
      if (is_string(peek(0)) && is_string(peek(1))) {
        concatenate();
      } else if (is_number(peek(0)) && is_number(peek(1))) {
        const double b = pop().as.number;
        const double a = pop().as.number;
        push(number_value(a + b));
      } else {
        runtime_error("Operands must be two numbers or two strings.");
        return INTERPRET_RUNTIME_ERROR;
      }
      break;
    }
    case OP_SUBTRACT: {
      InterpretResult result = binary_op(subtract);
      if (result != INTERPRET_OK) {
        return result;
      }
      break;
    }
    case OP_MULTIPLY: {
      InterpretResult result = binary_op(multiply);
      if (result != INTERPRET_OK) {
        return result;
      }
      break;
    }
    case OP_DIVIDE: {
      InterpretResult result = binary_op(divide);
      if (result != INTERPRET_OK) {
        return result;
      }
      break;
    }

    case OP_NOT: {
      push(bool_value(is_falsey(pop())));
      break;
    }

    case OP_NEGATE: {
      if (!is_number(peek(0))) {
        runtime_error("Operand must be a number.");
        return INTERPRET_RUNTIME_ERROR;
      }
      push(number_value(-pop().as.number));
      break;
    }

    case OP_PRINT: {
      print_value(pop());
      printf("\n");
      break;
    }

    case OP_JUMP: {
      uint16_t offset = read_short(frame);
      frame->ip += offset;
      break;
    }

    case OP_JUMP_IF_FALSE: {
      uint16_t offset = read_short(frame);
      if (is_falsey(peek(0))) {
        frame->ip += offset;
      }
      break;
    }

    case OP_LOOP: {
      uint16_t offset = read_short(frame);
      frame->ip -= offset;
      break;
    }

    case OP_CALL: {
      int32_t arg_count = read_byte(frame);
      if (!call_value(peek(arg_count), arg_count)) {
        return INTERPRET_RUNTIME_ERROR;
      }
      frame = &vm.frames[vm.frame_count - 1];
      break;
    }

    case OP_CLOSURE: {
      ObjFunction *function = as_function(read_constant(frame));
      ObjClosure *closure = new_closure(function);
      push(obj_value(closure));
      for (size_t i = 0; i < closure->upvalue_count; ++i) {
        uint8_t is_local = read_byte(frame);
        uint8_t index = read_byte(frame);
        if (is_local) {
          closure->upvalues[i] = capture_upvalue(frame->slots + index);
        } else {
          closure->upvalues[i] = frame->closure->upvalues[index];
        }
      }
      break;
    }

    case OP_CLOSE_UPVALUE: {
      close_upvalues(vm.stack_top - 1);
      pop();
      break;
    }

    case OP_RETURN: {
      Value result = pop();
      close_upvalues(frame->slots);
      vm.frame_count--;
      if (vm.frame_count == 0) {
        pop();
        return INTERPRET_OK;
      }

      vm.stack_top = frame->slots;
      push(result);
      frame = &vm.frames[vm.frame_count - 1];
      break;
    }

    case OP_CLASS:
      push(obj_value(new_class(read_string(frame))));
      break;
    }
  }
}

InterpretResult interpret(char const *source) {
  ObjFunction *function = compile(source);
  if (function == NULL) {
    return INTERPRET_COMPILE_ERROR;
  }

  push(obj_value(function));
  ObjClosure *closure = new_closure(function);
  pop();
  push(obj_value(closure));
  call(closure, 0);

  return run();
}
