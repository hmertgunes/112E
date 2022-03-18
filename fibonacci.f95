program fibonacci_calculator
     implicit none
     INTEGER :: i
     real :: x, F
     
     print*, "Please give an integer"
     read*, i
     
     x = (1 + sqrt(5.0)) / 2
     F = ((x**i)-(x-sqrt(5.0))**i) / sqrt(5.0) 
     print*, F
end program fibonacci_calculator

