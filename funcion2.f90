module funcion2
contains 
function f(y,y_1)   
    real*8,intent(in)::y,y_1
        f=0.05*y_1 -0.15*y 
        return
end function   
end module 


 