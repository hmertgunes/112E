program odd_even
     implicit none
     integer, dimension(4,4) :: vec
     integer :: numodd=0, numeven=0, i, j

     vec = reshape((/1,2,3,4,5,6,7,8,9,34,5,6,7,834,45,43/),(/4,4/))
     
     do i = 1,4
          do j = 1,4
               if  (mod(vec(i,j),2) == 0 ) then
                    numeven = numeven + 1
               else
                    numodd = numodd + 1
               end if          
          end do     
     end do     
     print*, "number of odd =", numodd
     print*, "number of even =", numeven
end program odd_even
