module intengracion_numerica
implicit none
    PRIVATE
    PUBLIC :: m_trapecio, m_simpson_un_tercio, m_simpson_tres_octavos, m_romberg
    interface
            real(8) function f(x)
                real(8), intent(in) :: x
            end function f
    end interface

contains
    subroutine m_trapecio(f, xi, xf, n, rsl)

        procedure(f), pointer :: f
        real(8), intent(in) :: xi, xf
        integer, intent(in) :: n
        real(8), intent(out) :: rsl
        real(8) :: x , h
        integer :: i

        h = abs(xi - xf)/n
        x = xi
        rsl = 0.0d0

        do i = 1, n
            rsl = rsl + h *(f(x + (i - 1)*h) + f(x + (i)*h))/2
        end do

    end subroutine m_trapecio

    subroutine m_simpson_un_tercio(f, xi, xf, n, rsl)
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
        x = xi
        rsl = 0.0d0

        do i = 1, n-1, 2
            rsl = rsl + (h/3)*(2*f(x + h*(i+1)) + 4*f(x + i*h))
        end do 

        rsl = rsl + (h/3)* (f(xi) - f(xf))

    end subroutine m_simpson_un_tercio

    subroutine m_simpson_tres_octavos(f, xi, xf, n, rsl)
        interface
            real(8) function f(x)
                real(8), intent(in) :: x
            end function f
        end interface

        real(8), intent(in) :: xi, xf
        integer, intent(in) :: n
        real(8), intent(out) :: rsl
        real(8) :: x, h
        integer :: i

        h = abs(xf - xi) / n
        x = xi
        rsl = 0.0d0

        do i = 1, n - 1
            if (mod(i, 3) == 0) then
                rsl = rsl + 2.0d0 * f(x + i * h)
            else
                rsl = rsl + 3.0d0 * f(x + i * h)
            end if
        end do
        rsl = 0.3750d0 * h  * (f(xi) + f(xf) + rsl)

    end subroutine m_simpson_tres_octavos
    
    subroutine m_romberg(f, xi, xf, n, rsl)
    interface
            real(8) function f(x)
                real(8), intent(in) :: x
            end function f
        end interface

        real(8), intent(in) :: xi, xf
        integer, intent(in) :: n
        real(8), intent(out) :: rsl
        real(8), allocatable :: R(:, :)
        real(8) :: x, h, sum
        integer :: i, j, k, m

        allocate(R(n, n))
        
        R(1,1) = (xf - xi) * 0.5 * (f(xi) + f(xf))
      
          do i = 2, n 
            sum = 0
            do j = 1, (2**(i-2)), 1
              sum = sum + f(xi + ((xf - xi) / 2**(i-2)) * (j - 0.5))
            end do
            R(i,1) = 0.5 * (R(i-1,1) + ((xf - xi) / (2**(i-2))) * sum)
          end do
          
          do k = 2, n
            do m = k, n
              R(m,k) = ((4**(k-1)) * R(m,k-1) - R(m-1,k-1)) / (4**(k-1) - 1)
            end do
          end do
        rsl =  R(n,n)

    end subroutine m_romberg
    
end module intengracion_numerica
