module ec2
contains 
function f(t,x)   
    real*8,intent(in)::t,x
        f= -0.4*x + 0.2
        return
end function   
end module 


