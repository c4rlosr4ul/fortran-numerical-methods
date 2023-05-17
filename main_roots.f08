program main_raices
    use raices_de_ecuaciones
    implicit none
    real(8) :: x_0_aprox, tol, x0, raiz, a, c

    x_0_aprox = 7.5 
    tol = 1e-8
    
    call m_newton(f, d_f, x_0_aprox, tol, raiz)
    write(*, *) "La raiz con M. Newton es", raiz
   
    a = 7.5d0
    c = 8.0d0
    
    call m_biseccion(f, a, c, tol, raiz)
    write(*, *) "La raiz con M. Biseccion es", raiz


    call m_secante(f, a, c, tol, raiz)
    write(*, *) "La raiz con M. Secente es", raiz


contains 

    function f(x)
        real(8), intent(in) :: x
        real(8) :: f
               f = cos(x) * cosh(x) + 1
    end function

    function d_f(x)
        real(8), intent(in) :: x
        real(8) :: d_f
            d_f = -sin(x) * cosh(x) + cos(x) * sinh(x)
    end function

end program main_raices

