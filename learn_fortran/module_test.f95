module test
    implicit none
    
contains 

    real function diff(a,b) 

        real :: a, b

        diff = a - b 

    end function diff 

end module test