program main_integracion
use intengracion_numerica
    
    implicit none 
    real(8) :: xi, xf, rsl
    integer :: n
    
    xi = 0.0d0
    xf = 3.0d0
    n = 100000
    
    call m_trapecio(f, xi, xf, n, rsl)
    write(*, *) "La integracion de f desde xi hasta xf es", rsl

contains
    function f(x)
        real(8), intent(in) :: x
        real(8) :: f
            f = x ** 2
    end function

end program main_integracion
