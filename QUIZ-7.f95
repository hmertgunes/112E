module funk
     contains
     function generate_random_integer_in_a_range(a,b) result(myrandomnumber)
          integer,intent(in)::a,b
          integer::myrandomnumber
          real::myrandomrealnumber
          call random_number(myrandomrealnumber)
          !Note: call random_number(x) generates a random real number in [0,1)
          myrandomnumber=a+floor((b+1-a)*myrandomrealnumber)
          !Note: floor(x) gives the greatest integer less than or equal to x.
     end function

     function myfunction(mymat) result(numberoftries)
          integer,dimension(:,:)::mymat
          integer::numberofelements,numberoftries,row,col,mysum
          numberofelements=size(mymat,1)
          numberoftries=1
          do
               row = generate_random_integer_in_a_range(1, numberofelements)
               col = generate_random_integer_in_a_range(1, numberofelements)
               mymat(row, col) = 1

               ! Took the code block exists below from term project and updated it for this task

               if ( row > 1 .and. row < NrowHMG ) then ! Inner elements, there are 5 neighbors
                    if (col > 1 .and. col < NcolHMG ) then 
                         mymat(row+1, col) = -1
                         mymat(row-1, col) = -1
                         mymat(row, col-1) = -1
                         mymat(row, col+1) = -1
                    end if
               end if

               if (row == 1) then  ! Top side of the matrix without corners, there is no row-1 neighbor
                    if (col > 1 .and. col < numberofelements) then 
                         mymat(row-1,col) = -1
                         mymat(row, col-1) = -1
                         mymat(row, col+1) = -1
                    else if (col == 1) then
                         mymat(row, col+1) = -1
                         mymat(row+1, col) = -1
                    else if (col == numberofelements) then
                         mymat(row, col-1) = -1
                         mymat(row+1, col) = -1
                    end if
               else if (row == numberofelements) then ! Bottom side of the matrix without corners, there is no row+1 neighbor
                    if (col > 1 .and. col < numberofelements) then
                         mymat(row+1,col) = -1
                         mymat(row, col-1) = -1
                         mymat(row, col+1) = -1
                    else if (col == numberofelements) then
                         mymat(row, col-1) = -1
                         mymat(row-1, col) = -1
                    else if (col == 1) then
                         mymat(row, col+1) = -1
                         mymat(row-1, col) = -1
                    end if
               else if (col == 1) then ! Left side of the matrix without corners, there is no col-1 neighbor
                    if (row > 1 .and. row < NrowHMG) then
                         mymat(row-1,col) = -1
                         mymat(row+1, col) = -1
                         mymat(row, col+1) = -1
                    end if
               else if (col == numberofelements) then ! Right side of the matrix without corners, there is no col+1 neighbor
                    if (row > 1 .and. row < numberofelements) then
                         mymat(row-1,col) = -1
                         mymat(row+1, col) = -1
                         mymat(row, col-1) = -1
                    end if
               end if
               !--------------------------------------
               mysum=sum(mymat)
               do i = 1, numberofelements
                    print*, mymat(i,:)
               end do
               print*,"current sum:",mysum
               if (mysum==0) then
                    exit
               else
                    numberoftries=numberoftries+1
               end if
          end do
     end function
end module funk

program quiz
     use funk
     implicit none

     INTEGER, PARAMETER :: N=4 ! N is a positive even number
     INTEGER, DIMENSION(N,N) :: mat=0 ! All elements of matrix equals to zero at initial situation
     INTEGER :: result
     
     result = myfunction(mat)
     print*,"Result=", result

end program quiz
