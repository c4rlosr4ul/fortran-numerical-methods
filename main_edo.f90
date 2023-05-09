program main_edo
use solucion_edo

    implicit none
    real(8) :: x0, y0, dy0, x, y, y1, y2
    integer :: n
    character(len=25) :: filename

! Solucion de ecuaciones diferenciales de primer orden
    x0 = 0.0d0
    y0 = 1.0d0
    n = 100
        x = 1.0d0
    call m_runge_kutta(dy, x0, y0, n, x, y)

    print *, "La solucion numerica es de la EDO de primer orden es:"
    print *, "M. Runge Kutta: "
    write(*,'(A4,ES15.4,1X,A7,ES20.10)') "| x=",x ,"| y(x)=",y 

! Solucion de ecuaciones diferenciales de segundo orden

   x0 = 0.0d0
    y0 = 1.0d0
    dy0 = 2.0d0
    n = 10
        x = 1.0d0 

    print *, "La solucion numerica es de la EDO de 2do orden es:"
     
    call m_euler_ad(d2y, x0, y0, dy0, n, x, y, y1, y2)
    print *, "M. Euler:"
    write(*,'(A4,1X,A1,1X,ES14.8,3X,A7,1X,A1,1X,ES14.8)') "| x", "=", x, "| y(x)", "=", y
    write(*, '(A7,1X,A1,1X,ES14.8,3X,A8,1X,A1,1X,ES14.8)') "| y'(x)", "=", y1, "| y''(x)", "=", y2

    call m_runge_kutta_4or(d2y, x0, y0, dy0, n, x, y, y1)
    print *, "M. Runge Kutta 4to orden:"
    write(*,'(A4,1X,A1,1X,ES14.8,3X,A7,1X,A1,1X,ES14.8)') "| x", "=", x, "| y(x)", "=", y
    write(*, '(A7,1X,A1,1X,ES14.8,3X,A8,1X,A1,1X,ES14.8)') "| y'(x)", "=", y1, "| y''(x)", "=", y2



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
            dy = exp(x) - y
    end function

end program main_edo
