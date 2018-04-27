
! module test_cgetopt

!   implicit none

! contains

!   subroutine target()
!     intrinsic :: command_argument_count, get_command_argument 
!     integer :: length
    
!     length = maxLength()

!     call test_getopt(length)
    
!   contains

!     integer function maxLength()
!       integer :: length_arr(0:command_argument_count())
!       integer :: iarg, status

!       do iarg = lbound(length_arr, 1), ubound(length_arr, 1)
!          call get_command_argument &
!               (iarg, length = length_arr(iarg), status = status)
!       end do
      
!       maxLength = maxval(length_arr)

!     end function maxLength

!     subroutine test_getopt(length)
!       integer,intent(in) :: length
!       character(length)  :: argv(0:command_argument_count())
!       integer            :: iarg
      
!       do iarg = lbound(argv, 1), ubound(argv, 1)
!          call get_command_argument(iarg, argv(iarg))
!       end do

!       do iarg = lbound(argv, 1), ubound(argv, 1)
!          print '("''", A, "''")', argv(iarg)
!       end do
      
!     end subroutine test_getopt
    
!   end subroutine target

! end module test_cgetopt

  program main
    
    use getoptf

    implicit none

    integer               :: argc, iarg
    type(arg),allocatable :: argv(:)
    
    call get_argc_argv(argc, argv)

    do iarg = 0,argc
       print '("''", A, "''")', argv(iarg)%content
    end do
    
  end program main
