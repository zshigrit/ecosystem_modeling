program test_mod
    use test, only: diff 

    implicit none

    real :: test1

    test1 = diff(3.0,5.0)

    print *, test1 

end program test_mod