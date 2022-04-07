module mytoolbox1
     contains

     subroutine take_cube(m, n)
          REAL :: m, n
          n = m **3
     end subroutine take_cube     

     function take_square(a) result(b)
          REAL, INTENT(IN) :: a
          REAL :: b
          b = a **2
     end function take_square     
end module mytoolbox1     


program function_example
     use mytoolbox1
     implicit none
     
     real :: mynumber=5.3, mysqnum, mycube
     mysqnum = take_square(mynumber)
     call take_cube(mynumber, mycube)
     print*, mysqnum
     print*, mycube
end program function_example
