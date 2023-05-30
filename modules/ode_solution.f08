module solucion_edo
    implicit none
    private
    public :: m1o_euler, m1o_runge_kutta_2or, m1o_runge_kutta_4or
    public :: m2o_euler, m2o_verlet,  m2o_runge_kutta_2or, m2o_runge_kutta_4or
    public :: se_m_eulercromer, se_m_rk2or, se_m_rk4or
    public :: se2o_m_rk_2or, se2o_m_rk_4or

contains

! Solucion de edo's de primer orden
    subroutine m1o_euler(dy, x0, y0, n, x, y)
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

        filename = "data/m1o-e-x_y.dat"

        open(unit=90, file=filename, status="unknown", action="write")

        xt = x0
        y = y0
        norma = abs(x - x0)/n
        h = norma
        write(90, *) xt, y

        do i = 1, n
            y = y + h*dy(xt, y)
            xt = xt + h
        write(90, *) xt, y
        end do


    end subroutine m1o_euler

    subroutine m1o_runge_kutta_2or(dy, x0, y0, n, x, y)

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
        character(len=30) :: filename

        filename = "data/m1o-rk2or-x_y_y1.dat"

        open(unit=95, file=filename, status="unknown", action="write")

        xt = x0
        y = y0
        norma = abs(x - x0)/n
        h = norma
        write(95, *) xt, y

        do i = 1, n, 1
                k1 = h * dy(xt, y)
            xt = xt + h
                k2 = h * dy(xt, y + k1)
            y = y + (k1 + k2)/2
        write(95, *) xt, y
        end do

    end subroutine m1o_runge_kutta_2or

    subroutine m1o_runge_kutta_4or(dy, x0, y0, n, x, y)

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
        character(len=30) :: filename

        filename = "data/m1o-rk4or_x_y_y1.dat"

        open(unit=100, file=filename, status="unknown", action="write")

        xt = x0
        y = y0
        norma = abs(x - x0)/n
        h = norma
        write(100, *) xt, y

        do i = 1, n, 1
            xt = xt + h
                k1 = h * dy(xt, y)
                k2 = h * dy(xt + 0.5* h, y + 0.5*k1)
                k3 = h * dy(xt + h*0.5, y + k2 *0.5)
                k4 = h * dy(xt + h, y + k3)
            y = y + (k1 + 2*k2 + 2*k3 + k4)/6
        write(100, *) xt, y
        end do

    end subroutine m1o_runge_kutta_4or

! Solucion de edo's de segundo orden
    subroutine m2o_euler(d2y, x0, y0, dy0, n, x, y, y1, y2)

    interface
        real(8) function d2y(x, y, dy)
        real(8), intent(in) :: x, y, dy
        end function d2y
    end interface
        
        real(8), intent(in) :: x0, y0, dy0, x
        integer, intent(in) :: n
        real(8), intent(out) :: y, y1, y2
        real(8) :: norma, xt, h
        integer :: i
        character(len=30) :: filename

        filename = "data/m2o-e-x_y_y1_y2.dat"

        open(unit=110, file=filename, status="unknown", action="write")

        norma = abs(x - x0)/n
        h = norma
        xt = x0
        y = y0
        y1 = dy0
        write(110, *) xt, y, y1

        do i = 1, n
            y2 = d2y(xt, y, y1)
            y1 = y1 + h * y2
            y = y + h * y1
            xt = xt + h
            write(110, *) xt, y, y1, y2
        end do

    end subroutine m2o_euler

    subroutine m2o_verlet(d2y, x0, y0, dy0, n, x, y, y1)
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

        filename = "data/m2o-mv-x_y_dy.dat"

        open(unit=120, file=filename, status="unknown", action="write")

        h = abs(x - x0)/n
        xt = x0
        y = y0
        y1 = dy0
        write(120, *) xt, y, y1

        do i = 1, n
            dyk = d2y(xt, y, y1)
            y = y + h*y1 + 0.5*h**2*dyk
            dyk_new = d2y(xt+h, y, y1)
            y1 = y1 + 0.5*h*(dyk_new + dyk)
            xt = xt + h
            write(120, *) xt, y, y1
        end do

        close(unit=120)

    end subroutine m2o_verlet
           
   subroutine m2o_runge_kutta_2or(d2y, x0, y0, dy0, n, x, y, y1)
    interface
        real(8) function d2y(x, y, dy)
        real(8), intent(in) :: x, y, dy
        end function d2y
    end interface
        
        real(8), intent(in) :: x0, y0, dy0, x
        integer, intent(in) :: n
        real(8), intent(out) :: y, y1
        real(8) :: h, xt, k1, l1, k2, l2
        integer :: i
        character(len=25) :: filename

        filename = "data/m2o-rk2or-x_y_dy.dat"

        open(unit=125, file=filename, status="unknown", action="write")

        h = abs(x - x0)/n
        xt = x0
        y = y0
        y1 = dy0
        write(125, *) xt, y, y1

        do i = 1, n
            xt = xt + h

            k1 = h * y1
            l1 = h * d2y(xt, y, y1)

            k2 = h * (y1 + l1)
            l2 = h * d2y(xt + h, y + k1, y1 + l1)

        y = y + (k1 + k2)/2
        y1 = y1 + (l1 + l2)/2

        write(125, *) xt, y, y1

        end do

    end subroutine m2o_runge_kutta_2or

    subroutine m2o_runge_kutta_4or(d2y, x0, y0, dy0, n, x, y, y1)
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

        filename = "data/m2o-rk4or-x_y_dy.dat"

        open(unit=120, file=filename, status="unknown", action="write")

        h = abs(x - x0)/n
        xt = x0
        y = y0
        y1 = dy0
        write(120, *) xt, y, y1

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

    end subroutine m2o_runge_kutta_4or

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

        filename = "data/se-ec-t-x1-x2.dat"

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

    subroutine se_m_rk2or(dx1, dx2, t0, x10, x20, n, t, x1, x2)  
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

        filename = "data/se-rk2or-t-x1-x2.dat"

        open(unit=135, file=filename, status="unknown", action="write")
        
        h = abs(t - t0)/n
        tt = t0
        x1 = x10
        x2 = x20
        write(135, *) tt, x1, x2

        do i = 1, n
            tt = tt + h
                k1 = h*dx1(tt, x1, x2)
                l1 = h*dx2(tt, x1, x2)
                k2 = h*dx1(tt + h, x1 + k1, x2 + l1)
                l2 = h*dx2(tt + h, x1 + k1, x2 + l1)
            x1 = x1 + (k1 + k2)/2
            x2 = x2 + (l1 + l2)/2
            write(135, *) tt, x1, x2
        end do

    end subroutine se_m_rk2or

    subroutine se_m_rk4or(dx1, dx2, t0, x10, x20, n, t, x1, x2)  
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

        filename = "data/se-rk4or-t-x1-x2.dat"

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

    end subroutine se_m_rk4or

!Solucion de sistemas de EDos de segundo grado

   subroutine se2o_rk_2or(d2x1, d2x2, t0, x10, x20, dx10, dx20, n, t, x1, x2, dx1, dx2 )
    interface
        real(8) function d2x1(t, x1, x2, dx1, dx2)
        real(8), intent(in) :: t, x1, x2, dx1, dx2
        end function dx1

        real(8) function d2x2(t, x1, x2, dx1, dx2))
        real(8), intent(in) :: t, x1, x2, dx1, dx2
        end function dx2
    end interface
        
        real(8), intent(in) :: t0, x10, x20, dx10, dx20
        integer, intent(in) :: n
        real(8), intent(out) ::  x1, x2, dx1, dx2
        real(8) :: h, xt, k1, l1, k2, l2
        integer :: i
        character(len=25) :: filename

        filename = "data/se2or-rk2or-t_x1_x2_dx1_dx2.dat"

        open(unit=150, file=filename, status="unknown", action="write")

        h = abs(t - t0)/n
        tt = t0; x1 = x10; x2 = x20; dx1 = dx10; dx2 = dx20

        do i = 1, n
            tt = tt + h
                k1x1 = h * dx1
                l1x1 = h * d2x1(tt, x1, x2, dx1, dx2) 

                k1x2 = h * dx2
                l1x2 = h * d2x2(tt, x1, x2, dx1, dx2) 

                k2x1 = h * (dx1 + lx1)
                l2x1 = h * d2x1(tt + h, x1 + k1x1, dyx1 + l1x1)

                k2x2 = h * (dx2 + lx2)
                l2x2 = h * d2x2(tt + h, x1 + k1x2, dyx2 + l1x2)


            x1 = x1 + (k1x1 + k2x1)/2
            x2 = x2 + (k1x2 + k2x2)/2

            dx1 = dx1 + (l1x1 + l2x1)/2
            dx2 = dx2 + (l1x1 + l2x1)/2

            write(150, *) tt, x1, x2, dx1, dx2

        end do

    end subroutine m2o_runge_kutta_2or

end module solucion_edo
