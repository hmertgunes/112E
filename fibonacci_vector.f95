program fibo
     implicit none
     integer :: i, t
     INTEGER, DIMENSION(16) :: fib_vec

     fib_vec(1) = 0
     fib_vec(2) = 1
     do i = 3,16
          fib_vec(i) = fib_vec(i-2) + fib_vec(i-1)
     end do
     do t = 1,16
          print*, t, fib_vec(t)
     end do     
end program fibo
