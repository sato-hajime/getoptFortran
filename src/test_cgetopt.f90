
  program main
    
    use getoptf
    
    implicit none

    integer               :: argc
    type(arg),allocatable :: argv(:)
    character(1)          :: opt
    
    call get_argc_argv(argc, argv)

    do while(.true.)
       opt = getopt(argc, argv, "a")
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
