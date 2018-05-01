
program main
    
  use getoptf
  
  implicit none
  
  integer               :: argc
  type(arg),allocatable :: argv(:)
  character(1)          :: opt

  character(:),allocatable :: optarg
  
  integer :: iarg
  
  call get_argc_argv(argc, argv)

  print *, "check command args"
  do iarg = lbound(argv,1), ubound(argv,1)
     print '("$", I0, "=''", A, "''")', iarg, argv(iarg)%content
  end do

  print *, "parse command arg"
  
  do while(getopt(argc, argv, "abcd:e:f:g::h::i::", opt))
     select case(opt)
     case ('a', 'b', 'c')
        print '("#1 -", A)', opt
     case ('d', 'e', 'f')
        optarg = get_optarg()
        print '("#2 -", A, " ''", A, "''")', opt, optarg
     case ('g', 'h', 'i')
        optarg = get_optarg()
        print '("#3 -", A, " ''", A, "''")', opt, optarg
     case default
        print '("#0 -", A)', opt
     end select
  end do
  
end program main
