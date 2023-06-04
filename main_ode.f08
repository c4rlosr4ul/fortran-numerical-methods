program main_edo
use solucion_edo

    implicit none
    real(8) :: x0, y0, dy0, x, y, y1, y2
    real(8) :: t0, x10, x20, t, x1, x2
    real(8) :: dx10, dx20, d1x1, d1x2
    integer :: n
    character(len=25) :: filename
!
!! Solucion de ecuaciones diferenciales de primer orden
!
!    x0 = 0.0d0
!    y0 = 1.0d0
!    n = 100
!        x = 1.0d0
!
!    print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
!    print *, "- La solucion numerica es de la EDO de primer orden es:"
!
!    call m1o_euler(dy, x0, y0, n, x, y)
!        print *, "M. Euler: "
!        write(*,'(A4,ES15.4,1X,A7,ES20.10)') "| x=",x ,"| y(x)=",y 
!
!    call m1o_runge_kutta_2or(dy, x0, y0, n, x, y)
!        print *, "M. Runge Kutta 2do orden: "
!        write(*,'(A4,ES15.4,1X,A7,ES20.10)') "| x=",x ,"| y(x)=",y 
!
!    call m1o_runge_kutta_4or(dy, x0, y0, n, x, y)
!        print *, "M. Runge Kutta 4t0 orden: "
!        write(*,'(A4,ES15.4,1X,A7,ES20.10)') "| x=",x ,"| y(x)=",y 
!
! Solucion de ecuaciones diferenciales de segundo orden

    x0 = 0.0d0
    y0 = 1.0d0
    dy0 = 0.0d0
    n = 1000
        x = 15.0d0 

    print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    print *, "- La solucion numerica es de la EDO de 2do orden es:"
     
    call m2o_euler(d2y, x0, y0, dy0, n, x, y, y1, y2)
        print *, "M. Euler:"
        write(*,'(A3,1X,A1,1X,F20.8,6X,A7,1X,A1,1X,F20.8)') "| x", "=", x, "| y(x)", "=", y
        write(*, '(A7,1X,A1,1X,F20.8,3X,A8,1X,A1,1X,F20.8)') "| y'(x)", "=", y1, "| y''(x)", "=", y2

    call m2o_verlet(d2y, x0, y0, dy0, n, x, y, y1)
        write(*, *) "M. Verlet"
        write(*,'(A3,1X,A1,1X,F20.8,6X,A7,1X,A1,1X,F20.8)') "| x", "=", x, "| y(x)", "=", y
        write(*, '(A7,1X,A1,1X,F20.8,3X,A8,1X,A1,1X,F20.8)') "| y'(x)", "=", y1, "| y''(x)", "=", y2

    call m2o_runge_kutta_2or(d2y, x0, y0, dy0, n, x, y, y1)
        print *, "M. Runge Kutta 2do orden:"
        write(*,'(A3,1X,A1,1X,F20.8,6X,A7,1X,A1,1X,F20.8)') "| x", "=", x, "| y(x)", "=", y
        write(*, '(A7,1X,A1,1X,F20.8,3X,A8,1X,A1,1X,F20.8)') "| y'(x)", "=", y1, "| y''(x)", "=", y2
   
    call m2o_runge_kutta_4or(d2y, x0, y0, dy0, n, x, y, y1)
        print *, "M. Runge Kutta 4to orden:"
        write(*,'(A3,1X,A1,1X,F20.8,6X,A7,1X,A1,1X,F20.8)') "| x", "=", x, "| y(x)", "=", y
        write(*, '(A7,1X,A1,1X,F20.8,3X,A8,1X,A1,1X,F20.8)') "| y'(x)", "=", y1, "| y''(x)", "=", y2
   

!! Sistema de edos de primer orden
!
!    t0 = 0.0d0
!    x10 = 1.0d0
!    x20 = 0.0d0
!    n = 100
!        t = 5.0d0
!
!    print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
!    print* , "La solucion del sistema de ecuaciones diferenciales de primer grado es:"
!    
!    call se_m_eulercromer(dx1, dx2, t0, x10, x20, n, t, x1, x2)
!        write(*, *)"M. EulerCromer: "
!        write(*, '(A3,1X,A1,1X,ES14.8)') "| t", "=", t
!        write(*, '(A7,1X,A1,1X,ES16.8,1X,A7,1X,A1,1X,ES16.8)') "| x1(t)", "=", x1 , "| x2(t)", "=", x2
!
!    call se_m_rk2or(dx1, dx2, t0, x10, x20, n, t, x1, x2)
!        write(*, *)"M. RungeKutta de 2do order: "
!        write(*, '(A3,1X,A1,1X,ES14.8)') "| t", "=", t
!        write(*, '(A7,1X,A1,1X,ES16.8,1X,A7,1X,A1,1X,ES16.8)') "| x1(t)", "=", x1 , "| x2(t)", "=", x2
!
!    call se_m_rk4or(dx1, dx2, t0, x10, x20, n, t, x1, x2)
!        write(*, *)"M. RungeKutta de 2do orden: "
!        write(*, '(A3,1X,A1,1X,ES14.8)') "| t", "=", t
!        write(*, '(A7,1X,A1,1X,ES16.8,1X,A7,1X,A1,1X,ES16.8)') "| x1(t)", "=", x1 , "| x2(t)", "=", x2
!!
!  Sistema de edos de segundo grado :P

    t0 = 0.0d0
    x10 = 0.50d0
    x20 = 0.50d0
    dx10 = 0.0d0
    dx20 = 0.0d0
    n = 100
        t = 10.0d0

    print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    print* , "La solucion del sistema de ecuaciones diferenciales de segundo grado es:"

    call se2o_m_rk_2or(d2x1, d2x2, t0, x10, x20, dx10, dx20, n, t, x1, x2, d1x1, d1x2)
    write(*, '(A3,1X,A1,1X,ES20.8)') "| t", "=", t
    write(*, '(A8,1X,A1,1X,ES20.8,1X,A8,1X,A1,1X,ES20.8)') "| x1(t)", "=", x1 , "| x2(t)", "=", x2
    write(*, '(A8,1X,A1,1X,ES20.8,1X,A8,1X,A1,1X,ES20.8)') "| x1'(t)", "=", d1x1 , "| x2'(t)", "=", d1x2

    call se2o_m_rk_4or(d2x1, d2x2, t0, x10, x20, dx10, dx20, n, t, x1, x2, d1x1, d1x2)
    write(*, '(A3,1X,A1,1X,ES20.8)') "| t", "=", t
    write(*, '(A8,1X,A1,1X,ES20.8,1X,A8,1X,A1,1X,ES20.8)') "|  x1(t)", "=", x1 , "| x2(t)", "=", x2
    write(*, '(A8,1X,A1,1X,ES20.8,1X,A8,1X,A1,1X,ES20.8)') "| x1'(t)", "=", d1x1 , "| x2'(t)", "=", d1x2


contains 

    function dy(x, y)
        real(8), intent(in) :: x, y
        real(8) :: dy
!             dy = (x * y)**3 - (y/x)**2
            dy = -2*y*x
    end function

    function d2y(x, y, dy)
        real(8), intent(in) :: x, y, dy
        real(8) :: d2y
        real(8) :: b, g, w, k, m
!            d2y = -81.0d0 * y
!            d2y = -0.2*dy**3 -y 
        m = 20.0d0; b = 5.0d0; k = 20.0d0; g = b/m; w = sqrt(k/m)  
            d2y = -g*dy - w**2 * y
    end function

! Para el sistema ED 1er grado
!
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

    ! Error porque lo uso como funcion y como variables
!
!! Para el sistema ED 2do grado
!
    function d2x1(t, x1, x2, dx1, dx2)
        real(8), intent(in) :: t, x1, x2, dx1, dx2
        real(8) :: d2x1
        real(8) :: m1, m2, k1, k2
!!        real(8) :: g, m1, m2, l1, l2
!!            g = 9.81; m1 = 1; m2 = 1; l1 = 1; l2 =1
!!            d2x1 = (m2*l1*(dx1**2)*sin(x1-x2) + m2*g*sin(x2)*cos(x1-x2) + m2*l2*(dx2**2)*sin(x1-x2) - (m1+m2)*g*sin(x1)) / ((m1+m2)*l1 - m2*l1*cos(x1-x2)**2)
            m1 = 1; m2 = 1; k1 = 1; k2 = 1
                d2x1 = (-k1/m1)*x1 + (k2/m1)*(x2 - x1)
    end function 

    function d2x2(t, x1, x2, dx1, dx2)
        real(8), intent(in) :: t, x1, x2, dx1, dx2
        real(8) :: d2x2
        real(8) :: m1, m2, k1, k2
!        real(8) :: g, m1, m2, l1, l2
!            g = 9.81; m1 = 1; m2 = 1; l1 = 1; l2 =1
!            d2x2 = (-m2*l2*(dx2**2)*sin(x1-x2)*cos(x1-x2) + (m1+m2)*(g*sin(x1)*cos(x1-x2) + l1*(dx1**2)*sin(x1-x2) - g*sin(x2))) / ((m1+m2)*l2 - m2*l2*cos(x1-x2)**2)
        m1 = 1; m2 = 1; k1 = 1; k2 = 1
            d2x2 = (-k2/m2)*x2 + (k1/m2)*(x1 - x2)
    end function

end program main_edo
