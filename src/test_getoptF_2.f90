
program main

  use getoptf, only : getopt, get_optarg

  character(1)             :: opt
  character(:),allocatable :: optarg
  
  do while(getopt("abcd:e:f:g::h::i::", opt))
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
