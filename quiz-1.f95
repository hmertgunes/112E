program CalculatorTheSum
     ! This program calculates the sum of 1*2+2*3+3*4+...+9*10.
     implicit none
     ! Declining variables.
     integer :: i, mysum
     ! Starting loop with mysum equals to 0.
     mysum = 0
     do i = 1,9
          mysum = mysum + i*(i+1)
     end do
     print*, "The result of expression =",mysum ! The expression given above.

end program CalculatorTheSum








