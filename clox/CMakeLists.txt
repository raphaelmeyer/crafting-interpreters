cmake_minimum_required(VERSION 3.25)

project(clox)

include(CTest)

set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_C_FLAGS_DEBUG "-O1 -g")
set(CMAKE_C_FLAGS_RELEASE "-O3")

set(CMAKE_CXX_FLAGS_DEBUG "-O1 -g")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")

add_library(clox-sources INTERFACE)
target_link_libraries(clox-sources INTERFACE common)
target_sources(clox-sources INTERFACE
  src/chunk.c
  src/debug.c
  src/memory.c
  src/value.c
  src/vm.c
)

add_executable(clox-test)
target_link_libraries(clox-test PRIVATE clox-sources doctest::doctest)
target_sources(clox-test PRIVATE
  src/main.spec.cc
  src/chunk.spec.cc
  src/value.spec.cc
  src/vm.spec.cc
)
target_compile_options(clox-test PRIVATE "-fsanitize=address" "-fno-omit-frame-pointer")
target_link_options(clox-test PRIVATE "-fsanitize=address")
add_test(NAME "clox-test" COMMAND clox-test)

add_executable(clox)
target_link_libraries(clox PRIVATE clox-sources)
target_sources(clox PRIVATE
  src/main.c
)

add_library(common INTERFACE)
target_compile_options(common INTERFACE -Wall -Wextra -Werror)

add_subdirectory(vendor)
