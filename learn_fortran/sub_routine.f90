program sub_ex

	implicit none
	
	print *, "we are inside main program"
	print *, "calling subroutine A"
	call A
	print *, "we are back to the main program and exiting"
    
end program sub_ex

