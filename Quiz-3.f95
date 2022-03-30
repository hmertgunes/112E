program epidemic_disease
     implicit none
     ! S: Susceptible (healthy but tends to get the disease)     
     ! I: Infected (got the disease)
     ! R :Recovered (healthy and can not get the disease again)
     real, dimension(51) :: S, I, R
     real, DIMENSION(50) :: H ! H is vector of days. 1,2,3,...., 50
     real :: a, b
     integer :: t
     a = 0.7 ! contact rate
     b = 0.2 ! recovery rate
     ! when t = 0.day equals to S(1), I(1), R(1), initial values
     S(1) = 0.99
     I(1) = 0.01
     R(1) = 0.00
     do t = 1, 50 ! We must train model 50 day.
          S(t+1) = S(t)-a*I(t)*S(t)
          I(t+1) = I(t)+a*I(t)*S(t)-b*I(t)
          R(t+1) = R(t)+b*I(t)  
          H(t) = t  
     end do     
     ! when t = 50th day equals S(51), I(51), R(51)
     print*, "When t= 0th day  ","S=",S(1),"I=",I(1),"  R=",R(1),"Total=",S(1)+I(1)+R(1)
     print*, "      S     ", "    I    ", "     R", "     which day   "
     print*, "   -------  ", " ------  ", "  ------", "  ----------"
          do t = 1, 50
          write(*, 1) S(t+1), I(t+1), R(t+1), H(t)
          1 format(4f10.4)
     end do     
end program epidemic_disease
