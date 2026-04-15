#include "value.h"

#include "object.h"

#include <format>

bool is_class(Value const &value) {
  if (not is_obj(value)) {
    return false;
  }
  return std::holds_alternative<Class>(as_obj(value)->data);
}

bool is_closure(Value const &value) {
  if (not is_obj(value)) {
    return false;
  }
  return std::holds_alternative<Closure>(as_obj(value)->data);
}

bool is_function(Value const &value) {
  if (not is_obj(value)) {
    return false;
  }
  return std::holds_alternative<Function>(as_obj(value)->data);
}

bool is_instance(Value const &value) {
  if (not is_obj(value)) {
    return false;
  }
  return std::holds_alternative<Instance>(as_obj(value)->data);
}

bool is_native(Value const &value) {
  if (not is_obj(value)) {
    return false;
  }
  return std::holds_alternative<Native>(as_obj(value)->data);
}

Class &as_class(Value const &value) {
  return std::get<Class>(as_obj(value)->data);
}

Closure &as_closure(Value const &value) {
  return std::get<Closure>(as_obj(value)->data);
}

Function &as_function(Value const &value) {
  return std::get<Function>(as_obj(value)->data);
}

Instance &as_instance(Value const &value) {
  return std::get<Instance>(as_obj(value)->data);
}

Native &as_native(Value const &value) {
  return std::get<Native>(as_obj(value)->data);
}

namespace {

std::string to_string(bool value) { return value ? "true" : "false"; }
std::string to_string(Nil) { return "nil"; }
std::string to_string(double value) { return std::format("{:g}", value); }
std::string to_string(std::string value) { return value; }

std::string to_string(Function const &v) {
  if (not v.name.empty()) {
    return std::format("<fn {}>", v.name);
  }
  return "<script>";
}

std::string to_string(Native const &) { return "<native fn>"; }

std::string to_string(Closure const &closure) {
  return to_string(std::get<Function>(closure.function.lock()->data));
}

std::string to_string(Class const &klass) { return klass.name; }

std::string to_string(Instance const &instance) {
  auto const name = std::get<Class>(instance.klass.lock()->data).name;
  return std::format("{} instance", name);
}

std::string to_string(UpValue const &) { return "upvalue"; }

std::string to_string(ObjHandle handle) {
  auto obj = handle.lock();
  return std::visit([](auto const &item) { return to_string(item); },
                    obj->data);
}

} // namespace

void print_value(std::ostream &out, Value value) {
  out << std::visit([](auto &&v) { return to_string(v); }, value);
}

bool values_equal(Value const &a, Value const &b) {
  if (a.index() != b.index()) {
    return false;
  }

  return std::visit(
      [](auto &&va, auto &&vb) -> bool {
        using A = std::decay_t<decltype(va)>;
        using B = std::decay_t<decltype(vb)>;

        if constexpr (std::is_same_v<A, Nil> && std::is_same_v<B, Nil>) {
          return true;
        } else if constexpr (std::is_same_v<A, ObjHandle> &&
                             std::is_same_v<B, ObjHandle>) {
          return va.lock() == vb.lock();
        } else if constexpr (std::is_same_v<A, B>) {
          return va == vb;
        }

        return false;
      },
      a, b);
}
