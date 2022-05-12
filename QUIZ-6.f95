module func_and_subr
     contains
     function matrixaverage(matrix) result(the_average)
          !Our function should be able to work with matrices of any row/column numbers.
          INTEGER, INTENT(IN), dimension(:,:) :: matrix 
          real :: the_average, sum=0.0
          integer :: i, j, counter=0

          do i = 1, size(matrix, 1)
               do j = 1, size(matrix, 2)
                    counter = counter + 1
                    sum = sum + matrix(i,j)
               end do
          end do
          the_average = sum / counter
     end function

     subroutine maxwhatwhere(matriks, max_element, row_of, col_of)
          !Our subroutine should be able to work with matrices of any row/column numbers.
          INTEGER, INTENT(IN), DIMENSION(:,:) :: matriks
          INTEGER, INTENT(OUT) :: max_element, row_of, col_of
          INTEGER :: m, n, place_holder=0

          ! Finding the max value and it's location 
          do m = 1, size(matriks, 1)
               do n = 1, size(matriks, 2)
                    if (matriks(m,n) > place_holder) then
                         place_holder = matriks(m,n)
                         max_element = place_holder
                         row_of = m
                         col_of = n
                    end if
               end do
          end do

          print*, "The maximum element=", max_element
          print*, "The row number of maximum element=", row_of
          print*, "The column number of maximum element=", col_of
     end subroutine
end module


program quiz
     use func_and_subr ! To be able to call function and subroutine, we must use the "use" command.
     implicit none
     INTEGER, DIMENSION(3,3) :: mat ! Defined 3x3 matrix
     INTEGER :: i, j, max, row, col
     real :: average ! The average value should be real value

     print*, "Please enter a 3x3 matrix element by element."
     
     ! Filling the elements of 3x3 matrix by user
     do i = 1, size(mat,1)
          do j = 1, size(mat,2)
               read*, mat(i,j)
          end do
     end do

     CALL maxwhatwhere(mat, max, row, col) ! Calling the subroutine
     average = matrixaverage(mat) ! Calling the function
     print*, "The average value of matrix=", average
end program quiz

