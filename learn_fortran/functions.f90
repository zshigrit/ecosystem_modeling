! set of functions

recursive integer*8 function fact(n)

    implicit none
    integer*8, intent(in) :: n
    integer*8 :: fact1
	
    if (n==1 .or. n==0) then
      fact1 = 1
      !return
    else
      fact1 = fact(n-1)
      fact1 = fact1 * n
      !return		
    end if

end function fact
