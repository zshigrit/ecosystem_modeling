program sum 

    implicit none
    !declare variable
    real :: x,y,answer

    print *, 'enter 2 variables'
    read *, x
    read *, y
    answer = x+y

    print *, 'the total is: ', answer

end program sum