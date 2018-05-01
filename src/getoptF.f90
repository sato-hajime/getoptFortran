
module getoptF

  use iso_c_binding
  
  use cgetopt_interface, only : c_getopt => getopt
  use cgetopt_interface, only : c_optarg => optarg
  use cgetopt_interface, only : optind, opterr, optopt
  
  implicit none
  
  private

  !> element type for jag array of string
  type,public :: arg
     character(:,c_char),allocatable :: content
  end type arg

  interface
     function strlen(s) bind(C)
       import c_size_t, c_ptr
       implicit none
       integer(c_size_t) :: strlen
       type(c_ptr),value :: s
     end function strlen
  end interface

  interface getopt
     module procedure :: getopt_fullspec
     module procedure :: getopt_short
  end interface getopt
  
  public :: get_argc_argv
  public :: get_optarg, optind, opterr, optopt
  public :: getopt
  
contains

  subroutine get_argc_argv(argc, argv)
    integer,intent(out)               :: argc
    type(arg),allocatable,intent(out) :: argv(:)
    integer                  :: iarg
    intrinsic :: command_argument_count, get_command_argument

    argc = command_argument_count() + 1
    allocate(argv(0:argc-1))
    do iarg = lbound(argv,1), ubound(argv,1)
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
  
  logical function getopt_fullspec(argc, argv, optstring, opt) &
       result(result)
    integer,intent(in)                      :: argc
    type(arg),target,allocatable,intent(in) :: argv(:)
    character(*),intent(in)                 :: optstring
    character(1),intent(out)                :: opt

    type(c_ptr),allocatable                 :: c_argv(:)
    character(len_trim(optstring)+1,c_char),target :: c_optstring
    
    integer iarg, ichar

    c_optstring = optstring // C_NULL_CHAR
    
    allocate( c_argv(lbound(argv,1):ubound(argv,1)) )
    do iarg = lbound(argv,1), ubound(argv,1)
       c_argv(iarg) = c_loc(argv(iarg)%content)
    end do
        
    ichar = c_getopt(argc, c_argv, c_loc(c_optstring))

    opt = ACHAR(ichar)
    
    result = ichar > 0
    
  end function getopt_fullspec

  logical function getopt_short(optstring, opt)
    implicit none
    character(*),intent(in)  :: optstring
    character(1),intent(out) :: opt
    integer,save               :: argc = -1
    type(arg),save,allocatable :: argv(:)

    if (argc <= 0) then
       call get_argc_argv(argc, argv)
    end if
    
    getopt_short = getopt_fullspec( &
         argc, argv, optstring, opt)
    
  end function getopt_short
  
  function get_optarg() result(optarg)
    character(:),allocatable :: optarg
    integer(c_size_t) :: length
    if (C_ASSOCIATED(c_optarg)) then
       length = strlen(c_optarg)
       call get_optarg__
    else
       optarg = ""
    end if
  contains
    subroutine get_optarg__
      character(length,c_char),pointer :: f_optarg
      call c_f_pointer(c_optarg, f_optarg)
      optarg = f_optarg
    end subroutine get_optarg__
  end function get_optarg
  
end module getoptF
