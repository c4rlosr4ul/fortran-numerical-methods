module funciones
contains  
function df(x)
    real*8, intent(in)::x
    df = 3*x**2
    return 
    end function
function f2(x)
    real*8, intent(in)::x
    f2=(1.4e-5)*x**(1.5) + (1.15e-5)*x**(2) - (2*(9.81))/1000
    return
    end function
subroutine fc(nume,x,f)
    implicit none 
    real*8, intent(in) ::x,nume
    real*8, intent(out)::f
    f = x**3 - nume
    return 
    end subroutine
    !Lokita :u se profe xD 
end module funciones
