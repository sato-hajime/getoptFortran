
program main
    
  use getoptf
  
  implicit none
  
  integer               :: argc
  type(arg),allocatable :: argv(:)
  character(1)          :: opt

  integer :: iarg
  
  call get_argc_argv(argc, argv)

  print *, "check command args"
  do iarg = 0, argc
     print '("$", I0, "=''", A, "''")', iarg, argv(iarg)%content
  end do
  
  do while(.true.)
     opt = getopt(argc, argv, "abc")
     print *, opt
     select case(opt)
     case (C_NULL_CHAR)
        print *, "final"
        exit
     case default
        exit
     end select
  end do
  
end program main
