
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

  print *, "parse command arg"
  
  do while(getopt(argc, argv, "abc", opt))
     select case(opt)
     case default
        print '("-", A)', opt
     end select
  end do
  
end program main
