module raices_de_ecuaciones

    implicit none
    private
    public :: m_newton, m_biseccion, m_secante

contains

    subroutine m_biseccion(f, a, c, tol, x0)

     interface
            real(8) function f(x)
                real(8), intent(in) :: x
            end function f
    end interface

        real(8), intent(in) :: a, c, tol
        real(8), intent(out) :: x0
        real(8) :: x, xi, xf

        xi = a; xf = c

        do while (abs(xf - xi) > tol) 
            x = (xi + xf)/2
                if(f(xi) * f(x) > 0 .and. f(xf) * f(x) < 0) then
                    xi = x
                else if(f(xi) * f(x) < 0 .and. f(xf) * f(x) > 0) then
                    xf = x
                end if
        end do
        
        x0 = x

    end subroutine m_biseccion

    subroutine m_newton(func, d_func, x_0_aprox, tol, x0)

        interface
            real(8) function func(x)
                real(8), intent(in) :: x
            end function func

            real(8) function d_func(x)
                real(8), intent(in) :: x
            end function d_func
        end interface

        real(8), intent(in) :: x_0_aprox, tol
        real(8), intent(out) :: x0
        real(8) :: x

        x0 = x_0_aprox

        do  
            if(abs(func(x0)) < tol) exit
            x = x0 - (func(x0) / d_func(x0))
            x0 = x
        end do

    end subroutine m_newton

    subroutine m_secante(func, x0i, x0f, tol, x0)

    interface
        real(8) function func(x)
            real(8), intent(in) :: x
        end function func
    end interface

    real(8), intent(in) :: x0i, x0f, tol
    real(8), intent(out) :: x0
    real(8) :: xi, xf
    
    xi = x0i
    xf = x0f

    do  
    x0 = xf-((func(xf))*((xf-xi)/(func(xf)-func(xi))))
        if(abs(func(x0)) < tol) exit
        xi = xf
        xf = x0
    end do

end subroutine m_secante
    
end module raices_de_ecuaciones
