program main_integracion
use intengracion_numerica
    
    implicit none 
    real(8) :: xi, xf, rsl
    integer :: n
    
    xi = 0.0d0
    xf = 3.0d0
    n = 100000

    write(*, *) "La integracion de f desde xi hasta xf con"
    call m_trapecio(f, xi, xf, n, rsl)
    write(*, *) "Metedo del trapecio: ", rsl

    call m_simpson_un_tercio(f, xi, xf, n, rsl)
    write(*, *) "Metedo de simpson 1/3: ", rsl

    call m_simpson_tres_octavos(f, xi, xf, n, rsl)
    write(*, *) "Metedo de simpson 3/8: ", rsl

    n = 10

    call m_romberg(f, xi, xf, n, rsl)
    write(*, *) "Metedo de Romberg: ", rsl


contains
    function f(x)
        real(8), intent(in) :: x
        real(8) :: f
            f = x ** 2
    end function

end program main_integracion
