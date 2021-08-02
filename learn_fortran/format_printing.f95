program format_printing

    implicit none

    !double precision, dimension(4) :: matrix

    real,dimension(4) :: matrix

    integer :: i
    do i=1,4
        matrix(i) = cos(0.1*i)
    end do

    print *, 'matrix'
    !print *, matrix 

    write(*,1) matrix
    1 format(4f10.2)

end program format_printing 