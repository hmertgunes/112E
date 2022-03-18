program average
	implicit none
	integer :: num, sumof, i, avg, grade 
	
	print*, "Please enter the number of students."
	read*, num
	
	sumof = 0
	do i = 1, num
		print*, "Please enter the grade of students whose number is = ",  i
		read*, grade
		sumof = sumof + grade
	end do
	
	avg = sumof / num
	print*, "The average grade of this class is equal to = ", avg
end program average
