program fileoperations
     implicit none
     integer :: i

     open(unit=1, file="my_file.txt")

     do i = 1,10
          write(1,*) i**2
     end do
     close(1)
     
end program fileoperations