module myexample
     contains
     function myfunc1(num) result (mysum)
     integer,intent(in)::num
     integer::i,mysum
     mysum=0
     do i=0,num
          if (mod(i,2)==0) then
               mysum=mysum+i
          end if
     end do
     end function
end module

program myprog
     use myexample
     implicit none
     integer::N,myresult
     N=5
     myresult=myfunc1(N)
     print*,myresult
end program myprog