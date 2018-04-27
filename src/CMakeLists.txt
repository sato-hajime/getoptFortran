
include_directories(${CMAKE_CURRENT_BINARY_DIR})

add_library(cgetopt
  cgetopt.F90
  getoptF.f90
  )

add_executable(test_cgetopt
  test_cgetopt.f90
  )

target_link_libraries(test_cgetopt
  cgetopt
  )
