module mymodule
     contains
     subroutine mysub (vec,num,myres)
     integer,dimension(:)::vec
     integer,intent(in)::num
     integer,intent(out)::myres

     myres=vec(num)
     end subroutine
end module

program myexercise
     use mymodule
     implicit none
     integer::i,t,N,myresult
     integer,dimension(10)::v

     t=0
     N=5
     do i=1,10
          t=t+i
          v(i)=10*t
     end do
     call mysub(v,N,myresult)
     print*,myresult

end program
