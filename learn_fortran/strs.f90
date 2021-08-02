program strs

    implicit none
    character :: name1*10 = " Fortran ", name2*10="C++ ", name3*10, name4*15

    name3 = name1//name2 
    name4 = trim(name1)//trim(name2) 

    print *, name3
    print *, name4


end program strs 