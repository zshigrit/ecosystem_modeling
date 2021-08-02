program trigs

    implicit none
    integer :: i 
    integer, parameter :: n = 100
    real (kind=8), dimension(0:n) :: x, sinx, cosx, secx, tanx, cscx, &
    cotx, sin2x, cos2x, tan2x, sec2x, csc2x, cot2x
    real, parameter :: pi = 2*asin(1.0)
    real :: increment
    
    increment = 2*pi/(real(n))
    
    do i = 0,n
        x(i) = i*increment
    end do
    
    sinx = sin(x)
    cosx = cos(x)
    !tanx = tan(x)
    tanx = sinx/cosx
    cscx = 1.0/sinx
    secx = 1.0/cosx
    cotx = 1.0/tanx
    
    !15 format ("The Sine of ", f10.4, " is: ", f10.4)
    !10 format (f10.4, 5x, f10.4)
    
    !open (unit=1,file="sinx.txt")
    
!    do i=0,n
!        print 15, x(i),sinx(i)
!        write(unit=1,fmt=10) (x(i)/pi)*180, sinx(i)
!    end do
    
    x(0:n) = [(i*increment, i=0,n)]
    
    print *, x
    
    !close(unit=1)
	
end program trigs
