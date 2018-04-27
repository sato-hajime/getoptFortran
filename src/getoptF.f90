
module getoptF

  use iso_c_binding
  use cgetopt, only : c_getopt => getopt

  implicit none
  
  private

  !> element type for jag array of string
  type,public :: arg
     character(:,c_char),allocatable :: content
  end type arg

  
  
  public :: get_argc_argv, getopt, C_NULL_CHAR

contains

  subroutine get_argc_argv(argc, argv)
    integer,intent(out)               :: argc
    type(arg),allocatable,intent(out) :: argv(:)
    integer                  :: iarg
    intrinsic :: command_argument_count, get_command_argument

    argc = command_argument_count()
    allocate(argv(0:argc))
    do iarg = 0, argc
       call setargv_nullterminated
    end do

  contains

    subroutine setargv_nullterminated
      integer                  :: tmpLen
      character(:),allocatable :: tmp
      call get_command_argument(iarg, length=tmpLen)
      allocate( character(tmpLen) :: tmp )
      argv(iarg)%content = tmp // C_NULL_CHAR
    end subroutine setargv_nullterminated
    
  end  subroutine get_argc_argv

  function getopt(argc, argv, optstring)
    character(1,c_char)                     :: getopt
    integer,intent(in)                      :: argc
    type(arg),target,allocatable,intent(in) :: argv(:)
    character(*)                            :: optstring
    type(c_ptr),allocatable                 :: c_argv(:)
    
    character(len_trim(optstring)+1,c_char),target :: c_optstring
    
    integer iarg
    
    allocate( c_argv(0:argc) )
    do iarg = 0, argc
       c_argv(iarg) = c_loc(argv(iarg)%content)
    end do
   
    print *, c_getopt(argc, c_argv, c_loc(c_optstring))
    
  end function getopt
  
end module getoptF
