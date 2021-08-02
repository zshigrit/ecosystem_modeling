program area_qt

	implicit none
	
	real*8 :: area 
	character :: fig
	real*8, external :: tri,sq
	real*8 :: a,b,c,s
	
	print *, " This program calculates the are of a square/triangle"
	print *, "Type s for a sqaure and type t for triangle"
	
	read *, fig
	
	if (fig =='t' .or. fig=='T') then
		print *, "enter the sides of the triangle"
		read *, a,b,c
		area = tri(a,b,c)
		print *, "The area of the triangle is:", area 
	else if (fig=='s' .or. fig=='S') then
		print *, "enter the dise of the square"
		read *, s
		area = sq(s)
		print *, "The area of the triangle is:", area 
	else
		print *, "invaid option, terminating program"
	end if
	

end program area_qt

real*8 function tri(a,b,c)
	implicit none
	real*8, intent(in) :: a,b,c
	real*8 :: s
	
	s = 0.5 *(a+b+c)
	tri = (s*(s-a)*(s-b)*(s-c))**0.5
	
	print *, "The sides are: ", a, b, c 
	!print *, a, b, c
	
	
end function tri

real*8 function sq(s)

	implicit none
	real*8, intent(in) :: s
	
	print *, "the side of the square is:", s
	
	sq=s*s
	
end function sq
