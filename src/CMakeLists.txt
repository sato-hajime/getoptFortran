
include_directories(${CMAKE_CURRENT_BINARY_DIR})

add_library(getoptF
  cgetopt_interface.f90
  getoptF.f90
  )

add_executable(test_getoptF
  test_getoptF.f90
  )

target_link_libraries(test_getoptF
  getoptF
  )
