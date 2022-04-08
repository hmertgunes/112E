program max_value
     ! The program finds the maximum element of the matrix, 
     ! The row number of the maximum element and the column number of the maximum element
     implicit none
     integer :: row_max, col_max, k, l
     real :: my_max
     real, dimension(6,6) :: M 
     
     ! Filling the matrix M with values which given below
     do k = 1, size(M,1)
          do l = 1, size(M,2)
               M(k,l) = tanh(real(k) / real(l))
          end do     
     end do     
     
     ! Review the matrix
     do k = 1, size(M,1)
          print*, M(k,:)
     end do     
     
     ! A quick checking if the matrix M is correctly printed 
     print*, "3rd row, 4th column =",M(3,4)

     ! Starting by assumings
     my_max = M(1,1)
     row_max = 1
     col_max = 1

     ! Scanning whole matrix elements
     do k = 1, size(M,1)
          do l = 1, size(M,2)
               if (M(k,l) > my_max) then
                    my_max = M(k,l)
                    row_max = k
                    col_max = l 
               end if
          end do     
     end do

     ! Printing results
     print*, "My max value =", my_max
     print*, "My max row =", row_max
     print*, "My max column =", col_max
end program max_value
