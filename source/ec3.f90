module ec3
contains 
function f(t,y)
    real*8,intent(in)::t,y 
    f=y*t+1
return 
end function
end module 