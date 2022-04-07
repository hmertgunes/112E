module trig
     real,public,parameter :: pi=3.141593

     contains

     function degreetoradians(angdeg) result(angrad)
     real,intent(in) :: angdeg
     real :: angrad
     angrad = (pi/180.0)*angdeg
     end function degreetoradians

     function mytan(degree) result(tangentvalue)
     real,intent(in) :: degree
     real::tangentvalue,angleinrad
     angleinrad = degreetoradians(degree)
     tangentvalue = sin(angleinrad)/cos(angleinrad)
     end function mytan

end module trig

program trigex
     use trig
     implicit none
     real :: myresult
     myresult = mytan(60.0)
     print*, myresult
end program trigex