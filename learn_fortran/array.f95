program array

implicit none
!integer, parameter :: ikind=3
!double precision, dimension(ikind) :: x 

real, allocatable, dimension(:) :: x 
integer :: elements

elements = 3 
 
allocate (x(elements))

x(1) = 1.0
x(2) = 3.0
x(3) = 4.0

print *, x 
deallocate(x)

end program array 