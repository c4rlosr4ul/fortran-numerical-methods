module solucion_edo
    implicit none
    private
    public :: m_euler, m_euler_ad, m_runge_kutta, m_runge_kutta_4or, m_verlet, se_m_eulercromer, se_m_rkutta4or

contains

! Solucion de edo's de primer orden
    subroutine m_euler(dy, x0, y0, n, x, y)
      interface
        real(8) function dy(x, y)
        real(8), intent(in) :: x, y
        end function dy
      end interface
        
        real(8), intent(in) :: x0, y0, x
        integer, intent(in) :: n
        real(8), intent(out) :: y
        real(8) :: norma, xt, h
        integer :: i
        character(len=25) :: filename

        filename = "data/x_y.dat"

        open(unit=90, file=filename, status="unknown", action="write")

        xt = x0
        y = y0
        norma = abs(x - x0)/n
        h = norma

        do i = 1, n
            y = y + h*dy(xt, y)
            xt = xt + h
        write(90, *) xt, y
        end do


    end subroutine m_euler

    subroutine m_runge_kutta(dy, x0, y0, n, x, y)

      interface
        real(8) function dy(x, y)
        real(8), intent(in) :: x, y
        end function dy
      end interface
        
        real(8), intent(in) :: x0, y0, x
        integer, intent(in) :: n
        real(8), intent(out) :: y
        real(8) :: norma, xt, h, k1, k2, k3, k4
        integer :: i
        character(len=25) :: filename

        filename = "data/x_y_y1.dat"

        open(unit=100, file=filename, status="unknown", action="write")

        xt = x0
        y = y0
        norma = abs(x - x0)/n
        h = norma

        do i = 1, n, 1
            xt = xt + h
                k1 = h * dy(xt, y)
                k2 = h * dy(xt + 0.5* h, y + 0.5*k1)
                k3 = h * dy(xt + h*0.5, y + k2 *0.5)
                k4 = h * dy(xt + h, y + k3)
            y = y + (k1 + 2*k2 + 2*k3 + k4)/6
        write(100, *) xt, y
        end do

    end subroutine m_runge_kutta

! Solucion de edo's de segundo orden
    subroutine m_euler_ad(d2y, x0, y0, dy0, n, x, y, y1, y2)

    interface
        real(8) function d2y(x, y, dy)
        real(8), intent(in) :: x, y, dy
        end function d2y
    end interface
        
        real(8), intent(in) :: x0, y0, dy0, x
        integer, intent(in) :: n
        real(8), intent(out) :: y, y1, y2
        real(8) :: norma, xt
        character(len=25) :: filename

        filename = "data/x_y_y1_y2.dat"

        open(unit=110, file=filename, status="unknown", action="write")

        norma =  abs(x - x0)/n
        xt = x0
        y = y0
        y1 = dy0

        do while (abs(xt) <= abs(x))
            xt = xt + norma
            y2 = d2y(xt, y, y1) 
            y = y + norma * y1
            y1 = y1 + norma * y2      
            write(110, *) xt, y, y1, y2
        end do

    end subroutine m_euler_ad


    subroutine m_verlet(d2y, x0, y0, dy0, n, x, y, y1)
        interface
            real(8) function d2y(x, y, dy)
            real(8), intent(in) :: x, y, dy
            end function d2y
        end interface
            
        real(8), intent(in) :: x0, y0, dy0, x
        integer, intent(in) :: n
        real(8), intent(out) :: y, y1
        real(8) :: h, xt, dyk, dyk_new
        integer :: i
        character(len=25) :: filename

        filename = "data/rk-x_y_dy.dat"

        open(unit=120, file=filename, status="unknown", action="write")

        h = abs(x - x0)/n
        xt = x0
        y = y0
        y1 = dy0

        do i = 1, n
            dyk = d2y(xt, y, y1)
            y = y + h*y1 + 0.5*h**2*dyk
            dyk_new = d2y(xt+h, y, y1)
            y1 = y1 + 0.5*h*(dyk_new + dyk)
            xt = xt + h
        end do

        close(unit=120)

    end subroutine
           
    subroutine m_runge_kutta_4or(d2y, x0, y0, dy0, n, x, y, y1)
    interface
        real(8) function d2y(x, y, dy)
        real(8), intent(in) :: x, y, dy
        end function d2y
    end interface
        
        real(8), intent(in) :: x0, y0, dy0, x
        integer, intent(in) :: n
        real(8), intent(out) :: y, y1
        real(8) :: h, xt, k1, l1, k2, l2, k3, l3, k4, l4
        integer :: i
        character(len=25) :: filename

        filename = "data/rk-x_y_dy.dat"

        open(unit=120, file=filename, status="unknown", action="write")

        h = abs(x - x0)/n
        xt = x0
        y = y0
        y1 = dy0

        do i = 1, n
            xt = xt + h

            k1 = h * y1
            l1 = h * d2y(xt, y, y1)

            k2 = h * (y1 + l1*0.5)
            l2 = h * d2y(xt + h*0.5, y + k1*0.5, y1 + l1*0.5)

            k3 = h * (y1 + l2*0.5)
            l3 = h * d2y(xt + h*0.5, y + k2*0.5, y1 + l2*0.5)

            k4 = h * (y1 + l3)
            l4 = h * d2y(xt + h, y + k3, y1 + l3)

        y = y + (k1 + 2*k2 + 2*k3 + k4)/6
        y1 = y1 + (l1 + 2*l2 + 2*l3 + l4)/6
            
        write(120, *) xt, y, y1

        end do

    end subroutine m_runge_kutta_4or

!Solucion de sistemas de EDos de primer grado

    subroutine se_m_eulercromer(dx1, dx2, t0, x10, x20, n, t, x1, x2)
    interface
        real(8) function dx1(t, x1, x2)
        real(8), intent(in) :: t, x1, x2
        end function dx1

        real(8) function dx2(t, x1, x2)
        real(8), intent(in) :: t, x1, x2
        end function dx2
    end interface
        
        real(8), intent(in) :: t0, x10, x20, t
        integer, intent(in) :: n
        real(8), intent(out) :: x1, x2
        real(8) :: h, tt
        integer :: i
        character(len=25) :: filename

        filename = "data/se-t-x1-x2.dat"

        open(unit=130, file=filename, status="unknown", action="write")
        
        h = abs(t - t0)/n
        tt = t0
        x1 = x10
        x2 = x20
        write(130, *) tt, x1, x2

        do i = 1, n
            tt = tt + h
            x1 = x1 + h*dx1(tt, x1, x2)
            x2 = x2 + h*dx2(tt, x1, x2)
        write(130, *) tt, x1, x2
        end do

    end subroutine se_m_eulercromer

    subroutine se_m_rkutta4or(dx1, dx2, t0, x10, x20, n, t, x1, x2)  
    interface
        real(8) function dx1(t, x1, x2)
        real(8), intent(in) :: t, x1, x2
        end function dx1

        real(8) function dx2(t, x1, x2)
        real(8), intent(in) :: t, x1, x2
        end function dx2
    end interface

        real(8), intent(in) :: t0, x10, x20, t
        integer, intent(in) :: n
        real(8), intent(out) :: x1, x2
        real(8) :: h, tt, l1, k1, l2, k2, l3, k3, l4, k4
        integer :: i
        character(len=25) :: filename

        filename = "data/rk-t-x1-x2.dat"

        open(unit=140, file=filename, status="unknown", action="write")
        
        h = abs(t - t0)/n
        tt = t0
        x1 = x10
        x2 = x20
        write(140, *) tt, x1, x2

        do i = 1, n
            tt = tt + h
                k1 = h*dx1(tt, x1, x2)
                l1 = h*dx2(tt, x1, x2)
                k2 = h*dx1(tt + h*0.5, x1 + k1*0.5, x2 + l1*0.5)
                l2 = h*dx2(tt + h*0.5, x1 + k1*0.5, x2 + l1*0.5)
                k3 = h*dx1(tt + h*0.5, x1 + k2*0.5, x2 + l2*0.5)
                l3 = h*dx2(tt + h*0.5, x1 + k2*0.5, x2 + l2*0.5)
                k4 = h*dx1(tt + h, x1 + k3, x2 + l3)
                l4 = h*dx2(tt + h, x1 + k3, x2 + l3)
            x1 = x1 + (k1 + 2*k2 + 2*k3 + k4)/6
            x2 = x2 + (l1 + 2*l2 + 2*l3 + l4)/6
        write(140, *) tt, x1, x2
        end do

    end subroutine se_m_rkutta4or

end module solucion_edo
