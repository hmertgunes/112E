program center_of_mass
     implicit none
     real, dimension(3) :: x, y, m
     real :: x_cm, y_cm

     x = (/4.2,5.3,6.5/)
     y = (/7.6,8.3,9.9/)
     m = (/19.0, 17.4, 18.6/)

     x_cm = dot_product(m, x) / sum(m)
     y_cm = dot_product(m, y) / sum(m)

     print*, x_cm, y_cm
end program center_of_mass
