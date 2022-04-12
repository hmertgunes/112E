! This project includes a simple version of "mean field approximation" that can be used when modeling physical systems.
! Created a matrix called avematHMG, which consists of the average of matHMG matrix's each elements and it's nearest neighbors.
! Then, calculated the average of the whole elements of the matrix avematHMG and set it to averageHMG.
! Finally, displayed the value of averageHMG on the screen.

program project
     
     ! XX: HMG, my initials in capitals 
     ! NrowHMG: Number of rows of the matrix matHMG and avematHMG, (The last digit of my student number + 1)*19, which is (8+1)*19 = 171
     ! NcolHMG: Number of columns of the matrix matHMG and avematHMG, (The last digit of my student number + 1)*23, which is (8+1)*23 = 207

     IMPLICIT NONE
     INTEGER, PARAMETER ::  NrowHMG=171, NcolHMG=207 ! "parameter" because I am declaring them as constants
     REAL, DIMENSION(NrowHMG, NcolHMG) :: matHMG, avematHMG ! Declaring the real matrices with NrowHMG by NcolHMG
     REAL :: averageHMG ! averageHMG stores the average of the whole elements of the matrix avematXX 
     INTEGER :: i, j, kHMG, cnt ! i, j and kHMG are needed for our loops as sequence of numbers, cnt is the number of whole elements in matrices


     ! Filling the matrix matHMG such that the ith row and jth column element will be the result of the sum
     ! matHMG(i,j)= ∑ 8.0 / ( 4∗kXX+1) ∗ (4∗kXX+3) ), goes from 0 to i+j
     do i = 1, NrowHMG
          do j = 1, NcolHMG
               matHMG(i,j) = 0 ! At first, matHMG(i,j) had to equals 0 because loop is adding on top of the total
               do kHMG = 0, i+j ! ∑ goes from 0 to i+j, kHMG stores that sequence of numbers
                    matHMG(i,j) = matHMG(i,j) + 8.0 / ((4 * kHMG + 1)*(4 * kHMG + 3))
               end do
          end do
     end do                   


     ! avematHMG will be the average of the ith row and jth column element of matHMG and it's nearest neighbors
     ! avemathHMG ith row and jth column = (the ith row and jth column element itself + nearest neighbors) / number of total elements
     ! I had to seperate filling commands into 2 loop blocks because the number of neighbors may change according to element's location

     ! Block 1: Filling side elements (without corners) of the matrix avematHMG
     do i = 1, NrowHMG
          do j = 1, NcolHMG
               if (i == 1) then  ! Top side of the matrix without corners, there is no i-1 neighbor
                    if (j > 1 .and. j < NcolHMG) then 
                         avematHMG(i,j) = (matHMG(i,j) + matHMG(i+1,j) + matHMG(i, j-1) + matHMG(i,j+1))/4.0 
                    end if
               else if (i == NrowHMG) then ! Bottom side of the matrix without corners, there is no i+1 neighbor
                    if (j > 1 .and. j < NcolHMG) then
                         avematHMG(i,j) = (matHMG(i,j) + matHMG(i-1,j) + matHMG(j+1,j) + matHMG(i,j-1))/4.0 
                    end if
               else if (j == 1) then ! Left side of the matrix without corners, there is no j-1 neighbor
                    if (i > 1 .and. i < NrowHMG) then
                         avematHMG(i,j) = (matHMG(i,j) + matHMG(i-1,j) + matHMG(i+1,j) + matHMG(i,j+1))/4.0
                    end if
               else if (j == NcolHMG) then ! Right side of the matrix without corners, there is no j+1 neighbor
                    if (i > 1 .and. i < NrowHMG) then
                         avematHMG(i,j) = (matHMG(i,j) + matHMG(i-1,j) + matHMG(i+1,j) + matHMG(i,j-1))/4.0
                    end if
               end if
          end do
     end do

     ! Block 2: Filling inner elements and corners of the matrix avematHMG
     do i = 1, NrowHMG
          do j = 1, NcolHMG
               if ( i > 1 .and. i < NrowHMG ) then ! Inner elements, there are 5 neighbors
                    if (j > 1 .and. j < NcolHMG ) then 
                         avematHMG(i,j) = (matHMG(i,j) + matHMG(i+1,j) + matHMG(i-1,j) + matHMG(i,j+1) + matHMG(i,j-1))/5.0
                    end if
               else if (i == 1 .and. j == 1) then ! Top-left corner, there are no i-1 and j-1 neighbors
                    avematHMG(i,j) = (matHMG(i,j) + matHMG(i+1,j) + matHMG(i,j+1))/3.0
               else if (i == 1 .and. j == NcolHMG) then ! Top-right corner, there are no i-1 and j+1 neighbors
                    avematHMG(i,j) = (matHMG(i,j) + matHMG(i+1,j) + matHMG(i,j-1))/3.0
               else if (i == NrowHMG .and. j == 1) then ! Bottom-left corner, there are no i+1 and j-1 neighbors
                    avematHMG(i,j) = (matHMG(i,j) + matHMG(i-1,j) + matHMG(i,j+1))/3.0
               else if (i == NrowHMG .and. j == NcolHMG) then ! Bottom-right corner, there are no i+1 and j+1 neighbors
                    avematHMG(i,j) = (matHMG(i,j) + matHMG(i-1,j) + matHMG(i,j-1))/3.0
               end if
          end do
     end do


     ! This loop calculates the number of whole elements in matrices matHMG and avematHMG
     cnt = 0 ! Starting loop with cnt = 0
     do i = 1, NrowHMG
          do j = 1, NcolHMG
               cnt = cnt + 1
          end do
     end do

     ! averageHMG: The average of the whole elements of the matrix avematHMG
     averageHMG = sum(avematHMG) / cnt ! sum(avematHMG) = 111096.789, cnt = 35397, averageHMG = 3.13859344
     print*, "The value of averageHMG =", averageHMG 


end program project
