cmake_minimum_required(VERSION 3.25)

project(clox)

set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

set(CMAKE_C_FLAGS_DEBUG "-O2 -g")
set(CMAKE_C_FLAGS_RELEASE "-O3")

add_executable(clox)
target_link_libraries(clox PRIVATE common)
target_sources(clox PRIVATE src/main.c)

add_library(common INTERFACE)
target_compile_options(common INTERFACE -Wall -Wextra -Werror)
