
module cgetopt

  use iso_c_binding

  implicit none

  type,bind(C) :: struct_option
     type(c_ptr)    :: name
     integer(c_int) :: has_arg
     type(c_ptr)    :: flag
     integer(c_int) :: val
  end type struct_option
  
  type(c_ptr)   ,bind(c, name="optarg") :: coptarg
  integer(c_int),bind(c, name="optind") :: coptind
  integer(c_int),bind(c, name="opterr") :: copterr
  integer(c_int),bind(c, name="optopt") :: coptopt

  interface
     function getopt__ &
          (argc, argv, optstring) &
          bind(C, name="getopt")
       import c_int, c_ptr
       integer(c_int)       :: getopt__
       integer(c_int),value :: argc
       type(c_ptr),value    :: argv
       type(c_ptr),value    :: optstring
     end function getopt_original
     
     function getopt_long__ &
          (argc, argv, optstring, longopts, longindex) &
          bind(C, name="getopt_long")
       import c_int, c_ptr
       integer(c_int)       :: getopt_long__
       integer(c_int),value :: argc
       type(c_ptr),value    :: argv
       type(c_ptr),value    :: optstring
       type(c_ptr),value    :: longopts
       integer(c_int)       :: longindex
     end function getopt_long__

     function getopt_long_only__ &
          (argc, argv, optstring, longopts, longindex) &
          bind(C, name="getopt_long")
       import c_int, c_ptr
       integer(c_int)       :: getopt_long__
       integer(c_int),value :: argc
       type(c_ptr),value    :: argv
       type(c_ptr),value    :: optstring
       type(c_ptr),value    :: longopts
       integer(c_int)       :: longindex
     end function getopt_long_only__
     
  end interface

contains

  subroutine getopt(argc, argv, optstring)
    integer,intent(in)      :: argc
    character(*),intent(in) :: argv(:)
    character(*),intent(in) :: optstring
  end subroutine getopt
  
end module cgetopt
