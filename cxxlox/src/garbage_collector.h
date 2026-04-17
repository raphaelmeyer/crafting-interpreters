#pragma once

#include "object_fwd.h"
#include "value.h"

#include <functional>
#include <ranges>
#include <vector>

template <typename R>
concept ValueRange = std::ranges::input_range<R> &&
                     std::same_as<std::ranges::range_value_t<R>, Value>;

template <typename R>
concept ObjHandleRange = std::ranges::input_range<R> &&
                         std::same_as<std::ranges::range_value_t<R>, ObjHandle>;

class GarbageCollector {
public:
  explicit GarbageCollector(std::function<void()> on_mark_roots_)
      : on_mark_roots{std::move(on_mark_roots_)} {}

  void trigger();
  void manage(ObjRef const &obj);

  void mark_object(ObjRef const &obj);
  void mark_value(Value const &value);

  void mark_values(ValueRange auto &&range) {
    for (auto const &v : range) {
      mark_value(v);
    }
  }

  void mark_objects(ObjHandleRange auto &&range) {
    for (auto const &h : range) {
      mark_object(h.lock());
    }
  }

private:
  void collect_garbage();
  void mark_roots();
  void blacken_object(ObjRef const &obj);
  void trace_references();
  void sweep();

  static constexpr std::size_t GC_HEAP_GROW_FACTOR = 2;

  std::function<void()> on_mark_roots;
  ObjList objects{};
  std::vector<ObjRef> gray_stack{};
  std::size_t next_gc_threshold{16};
};
