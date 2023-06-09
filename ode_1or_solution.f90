MODULE ode_1or_solution
    implicit none
    private
    public :: m1o_euler, m1o_runge_kutta_2or, m1o_runge_kutta_4or

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

        open(unit=10, file=filename, status="unknown", action="write")

        xt = x0
        y = y0
        norma = abs(x - x0)/n
        h = norma
        write(10, *) xt, y

        do i = 1, n
            y = y + h*dy(xt, y)
            xt = xt + h
        write(10, *) xt, y
        end do
        close(10)

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

        open(unit=25, file=filename, status="unknown", action="write")

        xt = x0
        y = y0
        norma = abs(x - x0)/n
        h = norma
        write(20, *) xt, y

        do i = 1, n, 1
                k1 = h * dy(xt, y)
            xt = xt + h
                k2 = h * dy(xt, y + k1)
            y = y + (k1 + k2)/2
        write(20, *) xt, y
        end do
        close(20)
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

        open(unit=30, file=filename, status="unknown", action="write")

        xt = x0
        y = y0
        norma = abs(x - x0)/n
        h = norma
        write(30, *) xt, y

        do i = 1, n, 1
            xt = xt + h
                k1 = h * dy(xt, y)
                k2 = h * dy(xt + 0.5* h, y + 0.5*k1)
                k3 = h * dy(xt + h*0.5, y + k2 *0.5)
                k4 = h * dy(xt + h, y + k3)
            y = y + (k1 + 2*k2 + 2*k3 + k4)/6
        write(30, *) xt, y
        end do
        close(30)

    end subroutine m1o_runge_kutta_4or


END MODULE ode_1or_solution
