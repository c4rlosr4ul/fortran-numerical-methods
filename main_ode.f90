!-----------------------------------------------------------------------
! Program: main_edo
! Description: Solves first-order and second-order differential equations
!              and systems of differential equations numerically.
! Author: Carlos Raul 
! Date: 06/10/23
!-----------------------------------------------------------------------

program main_edo
    use edo_solution
    implicit none

    ! Variables for first-order ODE
    real(8) :: x0, y0, dy0, t, x, y, y1, y2
    real(8) :: t0, x10, x20, x1, x2
    real(8) :: dx10, dx20, d1x1, d1x2
    integer :: n
    character(len=30) :: filename

    ! Solve first-order differential equations
    x0 = 0.0d0
    y0 = 1.0d0
    n = 1000
    x = 10.0d0

    print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    print *, "- The numerical solution of the first-order ODE is:"

    ! Euler's method
    call m1o_euler(dy, x0, y0, n, x, y)
    print *, "Euler's method:"
    write(*, '(A4,ES15.4,1X,A7,ES20.10)') "| x=",x ,"| y(x)=",y

    ! Runge-Kutta 2nd order method
    call m1o_runge_kutta_2or(dy, x0, y0, n, x, y)
    print *, "Runge-Kutta 2nd order method:"
    write(*, '(A4,ES15.4,1X,A7,ES20.10)') "| x=",x ,"| y(x)=",y

    ! Runge-Kutta 4th order method
    call m1o_runge_kutta_4or(dy, x0, y0, n, x, y)
    print *, "Runge-Kutta 4th order method:"
    write(*, '(A4,ES15.4,1X,A7,ES20.10)') "| x=",x ,"| y(x)=",y

    ! Solve second-order differential equations
    x0 = 0.0d0
    y0 = 1.0d0
    dy0 = 0.0d0
    n = 100
    x = 15.0d0 

    print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    print *, "- The numerical solution of the second-order ODE is:"

    ! Euler's method
    call m2o_euler(d2y, x0, y0, dy0, n, x, y, y1, y2)
    print *, "Euler's method:"
    write(*, '(A3,1X,A1,1X,F20.8,6X,A7,1X,A1,1X,F20.8)') "| x", "=", x, "| y(x)", "=", y
    write(*, '(A7,1X,A1,1X,F20.8,3X,A8,1X,A1,1X,F20.8)') "| y'(x)", "=", y1, "| y''(x)", "=", y2

    ! Verlet method
    call m2o_verlet(d2y, x0, y0, dy0, n, x, y, y1)
    print *, "Verlet method:"
    write(*, '(A3,1X,A1,1X,F20.8,6X,A7,1X,A1,1X,F20.8)') "| x", "=", x, "| y(x)", "=", y
    write(*, '(A7,1X,A1,1X,F20.8,3X,A8,1X,A1,1X,F20.8)') "| y'(x)", "=", y1, "| y''(x)", "=", y2

    ! Runge-Kutta 2nd order method
    call m2o_runge_kutta_2or(d2y, x0, y0, dy0, n, x, y, y1)
    print *, "Runge-Kutta 2nd order method:"
    write(*, '(A3,1X,A1,1X,F20.8,6X,A7,1X,A1,1X,F20.8)') "| x", "=", x, "| y(x)", "=", y
    write(*, '(A7,1X,A1,1X,F20.8,3X,A8,1X,A1,1X,F20.8)') "| y'(x)", "=", y1, "| y''(x)", "=", y2
   
    ! Runge-Kutta 4th order method
    call m2o_runge_kutta_4or(d2y, x0, y0, dy0, n, x, y, y1)
    print *, "Runge-Kutta 4th order method:"
    write(*, '(A3,1X,A1,1X,F20.8,6X,A7,1X,A1,1X,F20.8)') "| x", "=", x, "| y(x)", "=", y
    write(*, '(A7,1X,A1,1X,F20.8,3X,A8,1X,A1,1X,F20.8)') "| y'(x)", "=", y1, "| y''(x)", "=", y2

    ! System of first-order differential equations
    t0 = 0.0d0
    x10 = 1.0d0
    x20 = 0.0d0
    n = 100
    t = 5.0d0


    print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    print* , "The numerical solution of the system of first-order differential equations is:"
    
    ! Euler-Cromer method
    call se_m_eulercromer(dx1, dx2, t0, x10, x20, n, t, x1, x2)
    write(*, *)"Euler-Cromer method:"
    write(*, '(A3,1X,A1,1X,ES14.8)') "| t", "=", t
    write(*, '(A7,1X,A1,1X,ES16.8,1X,A7,1X,A1,1X,ES16.8)') "| x1(t)", "=", x1 , "| x2(t)", "=", x2

    ! Runge-Kutta 2nd order method
    call se_m_rk2or(dx1, dx2, t0, x10, x20, n, t, x1, x2)
    write(*, *)"Runge-Kutta 2nd order method:"
    write(*, '(A3,1X,A1,1X,ES14.8)') "| t", "=", t
    write(*, '(A7,1X,A1,1X,ES16.8,1X,A7,1X,A1,1X,ES16.8)') "| x1(t)", "=", x1 , "| x2(t)", "=", x2

    ! Runge-Kutta 4th order method
    call se_m_rk4or(dx1, dx2, t0, x10, x20, n, t, x1, x2)
    write(*, *)"Runge-Kutta 4th order method:"
    write(*, '(A3,1X,A1,1X,ES14.8)') "| t", "=", t
    write(*, '(A7,1X,A1,1X,ES16.8,1X,A7,1X,A1,1X,ES16.8)') "| x1(t)", "=", x1 , "| x2(t)", "=", x2

    ! System of second-order differential equations
    t0 = 0.0d0
    x10 = 0.50d0
    x20 = 0.50d0
    dx10 = 0.0d0
    dx20 = 0.0d0
    n = 100
    t = 10.0d0

    print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    print* , "The numerical solution of the system of second-order differential equations is:"

    ! Runge-Kutta 2nd order method
    call se2o_m_rk_2or(d2x1, d2x2, t0, x10, x20, dx10, dx20, n, t, x1, x2, d1x1, d1x2)
    write(*, '(A3,1X,A1,1X,ES20.8)') "| t", "=", t
    write(*, '(A8,1X,A1,1X,ES20.8,1X,A8,1X,A1,1X,ES20.8)') "| x1(t)", "=", x1 , "| x2(t)", "=", x2
    write(*, '(A8,1X,A1,1X,ES20.8,1X,A8,1X,A1,1X,ES20.8)') "| x1'(t)", "=", d1x1 , "| x2'(t)", "=", d1x2

    ! Runge-Kutta 4th order method
    call se2o_m_rk_4or(d2x1, d2x2, t0, x10, x20, dx10, dx20, n, t, x1, x2, d1x1, d1x2)
    write(*, '(A3,1X,A1,1X,ES20.8)') "| t", "=", t
    write(*, '(A8,1X,A1,1X,ES20.8,1X,A8,1X,A1,1X,ES20.8)') "|  x1(t)", "=", x1 , "| x2(t)", "=", x2
    write(*, '(A8,1X,A1,1X,ES20.8,1X,A8,1X,A1,1X,ES20.8)') "| x1'(t)", "=", d1x1 , "| x2'(t)", "=", d1x2

contains 

    ! Function for the first-order ODE
    real(8) function dy(x, y)
        real(8), intent(in) :: x, y
        real(8) :: m, b, g, w, F0
            m = 1.0d0; g = 9.81d0; w = 2.0d0; F0 = 1.2d0
        dy = F0*cos(w*x)/m - g*sin(y)

    end function

    ! Function for the second-order ODE
    real(8) function d2y(x, y, dy)
        real(8), intent(in) :: x, y, dy
        real(8) :: m, b, g, w, k
            m = 20.0d0; b = 5.0d0; k = 20.0d0; g = b / m; w = sqrt(k / m)
        d2y = -g * dy - w**2 * y
    end function

    ! Function for the system of first-order ODEs
    real(8) function dx1(t, x1, x2)
        real(8), intent(in) :: t, x1, x2
        dx1 = -0.005*sqrt(x1**2 + x2**2)*x1
    end function

    real(8) function dx2(t, x1, x2)
        real(8), intent(in) :: t, x1, x2
        dx2 = -9.81 - 0.005*sqrt(x1**2 + x2**2)*x2
    end function

    ! Function for the system of second-order ODEs
    real(8) function d2x1(t, x1, x2, dx1, dx2)
        real(8), intent(in) :: t, x1, x2, dx1, dx2
        real(8) :: m1, m2, k1, k2
            m1 = 1; m2 = 1; k1 = 1; k2 = 1
        d2x1 = (-k1 / m1) * x1 + (k2 / m1) * (x2 - x1)
    end function 

    real(8) function d2x2(t, x1, x2, dx1, dx2)
        real(8), intent(in) :: t, x1, x2, dx1, dx2
        real(8) :: m1, m2, k1, k2
            m1 = 1; m2 = 1; k1 = 1; k2 = 1
        d2x2 = (-k2 / m2) * x2 + (k1 / m2) * (x1 - x2)
    end function

end program main_edo

!   Examples

!   real(8) :: m, b, g, w, F0
!   m = 1.0d0; g = 9.81d0; w = 2.0d0; F0 = 1.2d0
!   dy = F0*cos(w*x)/m - g*sin(y)

!    real(8) :: g, m1, m2, l1, l2
!    g = 9.81; m1 = 1; m2 = 1; l1 = 1; l2 =1
!    d2x2 = (-m2*l2*(dx2**2)*sin(x1-x2)*cos(x1-x2) + (m1+m2)*(g*sin(x1)*cos(x1-x2) + l1*(dx1**2)*sin(x1-x2) - g*sin(x2))) / ((m1+m2)*l2 - m2*l2*cos(x1-x2)**2)

!    real(8) :: g, m1, m2, l1, l2
!    g = 9.81; m1 = 1; m2 = 1; l1 = 1; l2 =1
!    d2x1 = (m2*l1*(dx1**2)*sin(x1-x2) + m2*g*sin(x2)*cos(x1-x2) + m2*l2*(dx2**2)*sin(x1-x2) - (m1+m2)*g*sin(x1)) / ((m1+m2)*l1 - m2*l1*cos(x1-x2)**2)
