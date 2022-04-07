module modmod
     contains
     subroutine changeorder(a,b)
     integer,INTENT(INOUT)::a,b
     integer::c
     c=a
     a=b
     b=c
     end subroutine changeorder
end module modmod

program example
     use modmod
     implicit none
     integer::a,b
     a=3
     b=5
     print*,"a=",a,"b=",b
     call changeorder(a,b)
     print*,"a=",a,"b=",b
end program example
