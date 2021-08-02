! heron's formular: perimeter and area of a triangle

program triangle_area
    
    implicit none
    real :: a, b, c, s, p, Area

    !a = 3
    !b = 4
    !c = 5
    
    print *, 'the sides are:'
    read *, a, b, c
    p = a+b+c
    s = p/2

    Area = (s*(s-a)*(s-b)*(s-c))**0.5
    
    print *, "the area of the triangle is:", area

end program triangle_area