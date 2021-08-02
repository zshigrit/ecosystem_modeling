program read_write

    implicit none 

    real :: x,y,z

    x=23
    y = 45
    z = 89
    open(10,file='writtendata.txt')
    write(10,*) x,y,z 
    print *, x,y,z

end program read_write 