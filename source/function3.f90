module funcion3
contains 
function f(x,y)   
    real*8,intent(in)::x,y 
        f=-y**1.5 +0*x+ 1  
        return
end function   
end module 


