program mat_slice

	implicit none
	real*8, dimension (3,3) :: mat
	real*8, dimension (2,2,2) :: mat1
	real*8, dimension (2,2) :: slice
	logical, dimension (3,3) :: mask
	integer :: i=3, j=3
	
	mat = reshape((/1,4,7,2,5,8,3,6,9/),(/3,3/))
	mat1 = reshape((/1,4,7,2,5,8,3,6/),(/2,2,2/))
	print*,mat1(2,2,2)
	
	mask = .true.
	mask(i,:)=.false.
	mask(:,j)=.false.
	
	slice = reshape(pack(mat,mask),(/2,2/))
	
	print *, "test is"
	
	!write (*,*,advance='yes'), slice
	
	do i=1,3
		do j=1,3
			write(*,'(f8.3,t3)',advance='no') mat(i,j)
		end do
		!print *,new_line('a')
		!write(*,*)
	end do
	
	
end program mat_slice
