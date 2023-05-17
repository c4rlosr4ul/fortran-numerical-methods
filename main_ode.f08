program main_edo
use solucion_edo

    implicit none
    real(8) :: x0, y0, dy0, x, y, y1, y2
    real(8) :: t0, x10, x20, t, x1, x2
    integer :: n
    character(len=25) :: filename

! Solucion de ecuaciones diferenciales de primer orden

    x0 = 0.0d0
    y0 = 1.0d0
    n = 100
        x = 5.0d0
    print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    print *, "- La solucion numerica es de la EDO de primer orden es:"

    call m_euler(dy, x0, y0, n, x, y)
        print *, "M. Euler: "
        write(*,'(A4,ES15.4,1X,A7,ES20.10)') "| x=",x ,"| y(x)=",y 


    call m_runge_kutta(dy, x0, y0, n, x, y)
        print *, "M. Runge Kutta: "
        write(*,'(A4,ES15.4,1X,A7,ES20.10)') "| x=",x ,"| y(x)=",y 

! Solucion de ecuaciones diferenciales de segundo orden

    x0 = 0.0d0
    y0 = 1.0d0
    dy0 = 2.0d0
    n = 10
        x = 1.0d0 

    print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    print *, "- La solucion numerica es de la EDO de 2do orden es:"
     
    call m_euler_ad(d2y, x0, y0, dy0, n, x, y, y1, y2)
        print *, "M. Euler:"
        write(*,'(A3,1X,A1,1X,ES14.8,6X,A7,1X,A1,1X,ES14.8)') "| x", "=", x, "| y(x)", "=", y
        write(*, '(A7,1X,A1,1X,ES14.8,3X,A8,1X,A1,1X,ES14.8)') "| y'(x)", "=", y1, "| y''(x)", "=", y2

    call m_runge_kutta_4or(d2y, x0, y0, dy0, n, x, y, y1)
        print *, "M. Runge Kutta 4to orden:"
        write(*,'(A3,1X,A1,1X,ES14.8,6X,A7,1X,A1,1X,ES14.8)') "| x", "=", x, "| y(x)", "=", y
        write(*, '(A7,1X,A1,1X,ES14.8,3X,A8,1X,A1,1X,ES14.8)') "| y'(x)", "=", y1, "| y''(x)", "=", y2

    n = 1000
    call m_verlet(d2y, x0, y0, dy0, n, x, y, y1)
        write(*, *) "M. Verlet"
        write(*, '(A3,1X,A1,1X,ES14.8,3X,A7,1X,A1,1X,ES14.8)') "| x", "=", x, "| y(x)", "=", y
        write(*, '(A7,1X,A1,1X,ES14.8)') "| y'(x)", "=", y1

! Sistema de edos de primer orden

    t0 = 0.0d0
    x10 = 1.0d0
    x20 = 0.0d0
    n = 100
        t = 5.0d0

    print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    print* , "- La solucion del sistema de ecuaciones difrenciales"
    
    call se_m_eulercromer(dx1, dx2, t0, x10, x20, n, t, x1, x2)
        write(*, *)"M. EulerCromer: "
        write(*, '(A3,1X,A1,1X,ES14.8)') "| t", "=", t
        write(*, '(A7,1X,A1,1X,ES16.8,1X,A7,1X,A1,1X,ES16.8)') "| x1(t)", "=", x1 , "| x2(t)", "=", x2
    call se_m_rkutta4or(dx1, dx2, t0, x10, x20, n, t, x1, x2)
        write(*, *)"M. RungeKutta4or: "
        write(*, '(A3,1X,A1,1X,ES14.8)') "| t", "=", t
        write(*, '(A7,1X,A1,1X,ES16.8,1X,A7,1X,A1,1X,ES16.8)') "| x1(t)", "=", x1 , "| x2(t)", "=", x2

contains 

    function d2y(x, y, dy)
        real(8), intent(in) :: x, y, dy
        real(8) :: d2y
            !d2y = exp(x) + dy
            d2y = 4 * dy - 4 * y
    end function

    function dy(x, y)
        real(8), intent(in) :: x, y
        real(8) :: dy
            dy = 1 - x * y
    end function

    function dx1(t, x1, x2)
        real(8), intent(in) :: t, x1, x2
        real(8) :: dx1
!            dx1 = -0.005*sqrt(x1**2 + x2**2)*x1
            dx1 = x2
    end function

    function dx2(t, x1, x2)
        real(8), intent(in) :: t, x1, x2
        real(8) :: dx2
!            dx2 = -9.81 - 0.005*sqrt(x1**2 + x2**2)*x2
            dx2 = -x1
    end function

end program main_edo
