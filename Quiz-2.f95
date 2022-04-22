program devil
     implicit none
     INTEGER :: i

     do
          print*, " " ! For better looking.
          print*, "Please enter an integer number."
          read*, i

          if ( i == 666 ) then
               print*, "I do not like this number"
               exit
          else if (mod(i,2) == 1) then ! mod() = Remainder function. For odd numbers.
               print*, "The square of the number = " ,i**2
               print*, " " ! For better looking.
          else ! For even numbers.
               print*, "The square root of the number = " ,sqrt(real(i)) ! sqrt()'s input must be REAL or COMPLEX.
               print*, " " ! For better looking.
          end if              

     end do
     
end program devil

