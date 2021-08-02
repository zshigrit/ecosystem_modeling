
subroutine A

	implicit none
	print *, "we are subroutine A"
	print *, "calling subroutine B"
	call B
	print *, "we are back to subroutine A"
	
end subroutine A

subroutine B

	implicit none
	print *, "we are subroutine B"
	print *, "calling subroutine C"
	call C
	print *, "we are back to subroutine B"
	
end subroutine B

subroutine C

	implicit none
	print *, "we are subroutine C"
	print *, "calling subroutine D"
	call D
	print *, "we are back to subroutine C"
	
end subroutine C

subroutine D

	implicit none
	print *, "we are subroutine D"
	print *, "we are back to subroutine B"
	
end subroutine D
