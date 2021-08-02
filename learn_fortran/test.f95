program test 

implicit none
real :: x,y,answer
integer :: choice

print *, 'please choose an option'
print *, '1 addition'
print *, '2 multiply'
print *, '3 divide'

x = 12
y = 23

read *, choice 

if (choice ==1) then
    answer = x + y
    print *, 'answer is ',answer
end if 

if (choice ==2) then
    answer = x * y
    print *, 'answer is ',answer
end if 

if (choice ==3) then
    answer = x / y
    print *, 'answer is ',answer
end if 



end program test