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

    print *, "La solucion numerica es de la EDO:"
    write(*,'(A4,ES15.4,1X,A7,ES20.10)') "| x=",x ,"| y(x)=",y 

! Solucion de ecuaciones diferenciales de segundo orden
    ! Primera edo, funciona
!    x0 = 0.0d0
!    y0 = 1.0d0
!    dy0 = 10.0d0
!    ! n = 1000000 equivalente a precision de 4 puntos flotantes
!    n = 10
!        x = 1.0d0 
!    
    !Segunda edo, tambien funciona (todo bien)
    x0 = 0.0d0
    y0 = 1.0d0
    dy0 = 2.0d0
    ! n = 1000000 equivalente a precision de 4 puntos flotantes
    n = 100
        x = 1.0d0 

    call m_euler_ad(d2y, x0, y0, dy0, n, x, y, y1, y2)
   
    print *, "La solucion numerica es de la EDO:"
    write(*,'(A4,ES15.4,1X,A7,ES20.10,1X,A8,ES20.10,1X,A9,ES20.10)') "| x=",x ,"| y(x)=",y ,"| y'(x)=", y1,"| y''(x)=", y2 

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
            dy = exp(x) - dy
    end function

end program main_edo
