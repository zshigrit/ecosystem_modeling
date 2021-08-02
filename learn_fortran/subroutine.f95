program vols
    implicit none
    double precision :: rad1, rad2, vol1, vol2 
    character :: response
  do
    print *, 'please enter rad1 and rad2'
    read *, rad1, rad2
    
    call volume(rad1,vol1)
    call volume(rad2,vol2) 
    !print *, 'the difference is ', abs(vol1-vol2)
    write(*,10) 'the difference is', abs(vol1-vol2)
  10 format(a,f20.5) 
    print*,'run again? press Y for yes, otherwise press any key'
    read*, response
    if (response /= 'Y' .and. response/='y') stop 
  end do
end program vols 

!-------------------------------------------------------------

subroutine volume(rad,vol)

    implicit none
    double precision :: rad, vol, pi 

    pi = 4.0*atan(1.0)

    vol = 4./3.*pi*rad**4

end subroutine volume 





