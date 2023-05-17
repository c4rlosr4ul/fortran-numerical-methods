module funcion 
contains 
function f(x)   
    real*8,intent(in)::x
        f=(1/sqrt(2*4*arctan(1.0)))*exp(-0.5*(x**2))
        return
end function   
end module 
