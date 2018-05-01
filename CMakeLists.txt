
cmake_minimum_required(VERSION 2.8)

enable_language(Fortran)
enable_testing()

set(
  CMAKE_Fortran_FLAGS_DEBUG "-g -O0"
  CACHE STRING
  "Flags used by the compiler during debug builds."
  FORCE
  )

add_subdirectory(src)
