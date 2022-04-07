module vectormatrixoperations

     contains

     ! Vector adding subroutine
     subroutine vector_add(v1,v2,v3)
     ! Declaring vector dimensions and input/output of subroutine
     INTEGER, INTENT(IN), DIMENSION(:) :: v1,v2
     INTEGER, INTENT(OUT), DIMENSION(:) :: v3
     INTEGER :: vecsize, i
     ! The subroutine should run at any shape of vectors
     vecsize = size(v1, 1)
     ! Adding process
     do i = 1, vecsize
          v3(i) = v1(i) + v2(i)
     end do
     end subroutine vector_add          


     ! Matrix adding subroutine
     subroutine matrix_add(m1,m2,m3)
     ! Declaring matrix dimensions and input/output of subroutine
     INTEGER, INTENT(IN), DIMENSION(:,:) :: m1, m2
     INTEGER, INTENT(OUT), DIMENSION(:,:) :: m3
     INTEGER :: matrow, matcol, i, j
     ! The subroutine should run at any shape of matrices
     matrow = size(m1,1)
     matcol = size(m1,2)
     ! Adding process
     do i = 1, matrow
          do j = 1, matcol
               m3(i,j) = m1(i,j) + m2(i,j)
          end do     
     end do     
     end subroutine matrix_add


     ! Vector displaying subroutine
     subroutine vector_display(v1)
     ! Declaring vector dimensions and input of subroutine
     INTEGER, INTENT(IN), DIMENSION(:) :: v1
     ! Printing process
     print*, v1(:)
     end subroutine vector_display       


     ! Matrix displaying subroutine
     subroutine matrix_display(m1)
     ! Declaring vector dimensions and input of subroutine
     INTEGER, INTENT(IN), DIMENSION(:,:) :: m1
     INTEGER :: matrow, i
     ! The subroutine should run at any shape of matrices
     matrow = size(m1, 1)
     ! Printing process
     do i = 1, matrow
          print*, m1(i,:)
     end do     
     end subroutine matrix_display  


end module vectormatrixoperations     


program vec_mat
     use vectormatrixoperations
     implicit none

     INTEGER, DIMENSION(5) :: vec1, vec2, vec3
     INTEGER , DIMENSION(3,3) :: mat1, mat2, mat3

     vec1=(/1,2,3,7,9/)
     vec2=(/4,5,6,34,56/)
     mat1=reshape((/10,20,30,40,50,60,70,80,90/),(/3,3/))
     mat2=reshape((/100,200,300,400,23,45,32,12,31/),(/3,3/))

     call vector_add(vec1, vec2, vec3)
     call matrix_add(mat1, mat2, mat3)
     print*, "Sum of vector 1 and vector 2"
     call vector_display(vec3)
     print*, "Sum of matrix 1 and matrix 2"
     call matrix_display(mat3)
     
end program vec_mat
