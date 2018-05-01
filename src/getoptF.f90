
module getoptF

  use iso_c_binding
  use cgetopt_interface, only : c_getopt => getopt

  implicit none
  
  private

  !> element type for jag array of string
  type,public :: arg
     character(:,c_char),allocatable :: content
  end type arg

  public :: get_argc_argv, getopt
  
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
      call get_command_argument(iarg, tmp)
      argv(iarg)%content = tmp // C_NULL_CHAR
    end subroutine setargv_nullterminated
    
  end  subroutine get_argc_argv

  logical function getopt(argc, argv, optstring, opt) result(result)
    integer,intent(in)                      :: argc
    type(arg),target,allocatable,intent(in) :: argv(:)
    character(*)                            :: optstring
    character(1,c_char),intent(out)         :: opt

    type(c_ptr),allocatable                 :: c_argv(:)
    character(len_trim(optstring)+1,c_char),target :: c_optstring
    
    integer iarg, ichar

    c_optstring = optstring // C_NULL_CHAR
    
    allocate( c_argv(0:argc) )
    do iarg = 0, argc
       c_argv(iarg) = c_loc(argv(iarg)%content)
    end do
    
    ichar = c_getopt(argc, c_argv, c_loc(c_optstring))

    opt = ACHAR(ichar)

    result = ichar > 0
    
  end function getopt
  
end module getoptF
