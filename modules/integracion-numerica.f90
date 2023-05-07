module intengracion_numerica
implicit none
    PRIVATE
    PUBLIC :: m_trapecio
contains
    subroutine m_trapecio(f, xi, xf, n, rsl)

    interface
            real(8) function f(x)
                real(8), intent(in) :: x
            end function f
    end interface

        real(8), intent(in) :: xi, xf
        integer, intent(in) :: n
        real(8), intent(out) :: rsl
        real(8) :: x , h
        integer :: i

        h = abs(xi - xf)/n
        rsl = 0.0d0

        do i = 1, n
            rsl = rsl + h *(f(x + (i - 1)*h) + f(x + (i)*h))/2
        end do

    end subroutine m_trapecio


end module intengracion_numerica
