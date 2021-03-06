
module cgetopt_interface

  use iso_c_binding

  implicit none

  type,bind(C) :: struct_option
     type(c_ptr)    :: name
     integer(c_int) :: has_arg
     type(c_ptr)    :: flag
     integer(c_int) :: val
  end type struct_option
  
  type(c_ptr)   ,bind(c) :: optarg = C_NULL_PTR
  integer(c_int),bind(c) :: optind =  1
  integer(c_int),bind(c) :: opterr =  1
  integer(c_int),bind(c) :: optopt = -1
  
  interface
     
     function getopt &
          (argc, argv, optstring) &
          bind(C, name="getopt")
       import c_int, c_ptr, c_char
       implicit none
       integer(c_int)       :: getopt
       integer(c_int),value :: argc
       type(c_ptr)          :: argv(argc)
       type(c_ptr),value    :: optstring
     end function getopt
     
     function getopt_long &
          (argc, argv, optstring, longopts, longindex) &
          bind(C, name="getopt_long")
       import c_int, c_ptr
       implicit none
       integer(c_int)       :: getopt_long
       integer(c_int),value :: argc
       type(c_ptr)          :: argv(argc)
       type(c_ptr),value    :: optstring
       type(c_ptr),value    :: longopts
       integer(c_int)       :: longindex
     end function getopt_long

     function getopt_long_only &
          (argc, argv, optstring, longopts, longindex) &
          bind(C, name="getopt_long")
       import c_int, c_ptr
       implicit none
       integer(c_int)       :: getopt_long_only
       integer(c_int),value :: argc
       type(c_ptr)          :: argv(argc)
       type(c_ptr),value    :: optstring
       type(c_ptr),value    :: longopts
       integer(c_int)       :: longindex
     end function getopt_long_only
     
  end interface
  
end module cgetopt_interface
