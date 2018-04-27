
module getoptF
  
  use iso_c_binding
  use cgetopt, only : c_getopt => getopt

  interface getopt
  end interface getopt

  type arg
     character(:),allocatable :: content
  end type arg
  
contains
  
  function getopt(argc, argv, optstring)
    character(1,c_char)  :: getopt
    integer,intent(in)                  :: argc
    type(arg),pointer,intent(in) :: argv(:)
    character(:),pointer :: optstring

    type(c_ptr),allocatable :: c_argv(:)

    allocate( c_argv(0:argc) )

    do i = 0, argc
       c_argv(iarg) = c_loc(argv(iarg)%content)
    end do
    
  end function getopt

  subroutine get_argc_argv(argc, argv)
    intrinsic :: command_argument_count, get_command_argument
    integer,intent(out)               :: argc
    type(arg),allocatable,intent(out) :: argv(:)

    integer :: iarg, length
    
    argc = command_argument_count()
    
    allocate( argv(0:argc) )

    do iarg = 0, argc
       call get_command_argument(iarg, length=length)
       allocate( character(length) :: argv(iarg)%content )
       call get_command_argument(iarg, argv(iarg)%content)
    end do
    
  end subroutine get_argc_argv
  
end module getoptF
