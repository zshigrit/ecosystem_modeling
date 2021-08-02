program namer
    ! declare the variable
    character :: name*18 
    ! ask me to write my name
    print *,"what is your name?"
    ! recognize the typing from terminal
    read *, name 
    ! print my name
    print *, 'my name is ', name 
end program namer 