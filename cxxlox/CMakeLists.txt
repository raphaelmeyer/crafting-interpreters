cmake_minimum_required(VERSION 3.25)

project(cxxlox)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_CXX_FLAGS_DEBUG "-O2 -g")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")

add_executable(cxxlox)
target_compile_options(cxxlox PRIVATE -Wall -Wextra -Werror)
target_sources(cxxlox PRIVATE
  src/main.cc
)
