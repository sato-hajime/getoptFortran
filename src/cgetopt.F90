
module cgetopt

  use iso_c_binding

  implicit none

  type,bind(C) :: struct_option
     type(c_ptr)    :: name
     integer(c_int) :: has_arg
     type(c_ptr)    :: flag
     integer(c_int) :: val
  end type struct_option
  
  type(c_ptr)   ,bind(c) :: optarg
  integer(c_int),bind(c) :: optind
  integer(c_int),bind(c) :: opterr
  integer(c_int),bind(c) :: optopt
  
  interface
     
     function strlen__(s) bind(C, name="strlen")
       import c_size_t, c_ptr
       integer(c_size_t) :: strlen
       type(c_ptr),value :: s
     end function strlen__
     
     function getopt__ &
          (argc, argv, optstring) &
          bind(C, name="getopt")
       import c_int, c_ptr
       integer(c_int)       :: getopt__
       integer(c_int),value :: argc
       type(c_ptr)          :: argv(:)
       type(c_ptr),value    :: optstring
     end function getopt__
     
     function getopt_long__ &
          (argc, argv, optstring, longopts, longindex) &
          bind(C, name="getopt_long")
       import c_int, c_ptr
       integer(c_int)       :: getopt_long__
       integer(c_int),value :: argc
       type(c_ptr)          :: argv(:)
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
       type(c_ptr)          :: argv(:)
       type(c_ptr),value    :: optstring
       type(c_ptr),value    :: longopts
       integer(c_int)       :: longindex
     end function getopt_long_only__
     
  end interface

contains

  function strlen(s)
    integer(c_size_t)                     :: strlen
    character(*,c_char),target,intent(in) :: s
    strlen = strlen__(c_loc(s))
  end function strlen
  
  subroutine getopt(argc, argv, optstring)
    integer,intent(in)                  :: argc
    character(:),allocatable,intent(in) :: argv(:)
    character(:),allocatable,intent(in) :: optstring
  end subroutine getopt
  
end module cgetopt
