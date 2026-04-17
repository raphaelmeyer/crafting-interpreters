cmake_minimum_required(VERSION 3.25)

project(cxxlox)

include(CTest)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_CXX_FLAGS_DEBUG "-O2 -g")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")

add_library(cxxlox-sources INTERFACE)
target_link_libraries(cxxlox-sources INTERFACE common)
target_sources(cxxlox-sources INTERFACE
  src/chunk.cc
  src/compiler.cc
  src/debug.cc
  src/garbage_collector.cc
  src/object.cc
  src/scanner.cc
  src/value.cc
  src/vm.cc
)

add_executable(cxxlox-test)
target_link_libraries(cxxlox-test PRIVATE cxxlox-sources doctest::doctest)
target_sources(cxxlox-test PRIVATE
  src/main.spec.cc
  src/vm.spec.cc
  src/gc.spec.cc
)
target_compile_options(cxxlox-test PRIVATE "-fsanitize=address" "-fno-omit-frame-pointer")
target_link_options(cxxlox-test PRIVATE "-fsanitize=address")
add_test(NAME "cxxlox-test" COMMAND cxxlox-test)

add_executable(cxxlox)
target_link_libraries(cxxlox PRIVATE cxxlox-sources)
target_sources(cxxlox PRIVATE
  src/main.cc
)

add_library(common INTERFACE)
target_compile_options(common INTERFACE -Wall -Wextra -Werror)

add_subdirectory(vendor)
