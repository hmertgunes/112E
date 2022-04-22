module myFuncs
     contains
     function myfactorial(number) result(fac_of_nb)
          integer,intent(in) :: number
          real :: fac_of_nb
          integer :: i
          fac_of_nb = 1
          do i = 1, number
               fac_of_nb = fac_of_nb * i
          end do
     end function

     function myownsinus(x, N) result(mysinusvalue) ! x is in radian form.
          integer, intent(in) :: N
          real, intent(in) :: x
          integer :: k
          real :: mysinusvalue

          mysinusvalue = 0
          do k = 0, N
               mysinusvalue = mysinusvalue + (((-1)**k / myfactorial(2*k+1)) * x**(2*k+1))
          end do
     end function
end module myFuncs


program sine

     use myFuncs
     implicit none
     integer :: N
     real :: x, mysinusvalue

     print*, "Number x ="
     read*, x
     print*, "Number N ="
     read*, N

     mysinusvalue = myownsinus(x,N)
     print*,"My sinus value=", mysinusvalue
     print*, "sin(x) - my sinus value = ", sin(x) - mysinusvalue

end program sine
