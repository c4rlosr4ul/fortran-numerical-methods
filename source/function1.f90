module funcion1
contains 
function f(t,x)   
    real*8,intent(in)::x,t
        f=-x**1.5 +1 
        return
end function   
end module 