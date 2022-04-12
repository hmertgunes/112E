program find_value
     implicit none
     integer, dimension(7) :: vec
     integer :: i, sayac

     sayac = 0
     do i = 1,7
          print*, "Input new vec value."
          read*, vec(i)
     end do      
     do i = 1,7
          if (vec(i) > 3) then
               sayac = sayac + 1
          end if     
     end do     
     print*, "Number of elements which are greater than 3 =",sayac
end program find_value
