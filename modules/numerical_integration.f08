
module numerical_integration
    implicit none
    PRIVATE
    PUBLIC :: trapezoidal_method, simpson_one_third_method, simpson_three_eighths_method, romberg_method

contains

    subroutine trapezoidal_method(f, lower_limit, upper_limit, n, result)

    interface
            real(8) function f(x)
                real(8), intent(in) :: x
            end function f
    end interface

        real(8), intent(in) :: lower_limit, upper_limit
        integer, intent(in) :: n
        real(8), intent(out) :: result
        real(8) :: x , h
        integer :: i

        h = abs(lower_limit - upper_limit)/n
        x = lower_limit
        result = 0.0d0

        do i = 1, n
            result = result + h *(f(x + (i - 1)*h) + f(x + (i)*h))/2
        end do

    end subroutine trapezoidal_method

    subroutine simpson_one_third_method(f, lower_limit, upper_limit, n, result)

    interface
            real(8) function f(x)
                real(8), intent(in) :: x
            end function f
    end interface

        real(8), intent(in) :: lower_limit, upper_limit
        integer, intent(in) :: n
        real(8), intent(out) :: result
        real(8) :: x , h 
        integer :: i

        h = abs(lower_limit - upper_limit)/n
        x = lower_limit
        result = 0.0d0

        do i = 1, n-1, 2
            result = result + (h/3)*(2*f(x + h*(i+1)) + 4*f(x + i*h))
        end do 

        result = result + (h/3)* (f(lower_limit) - f(upper_limit))

    end subroutine simpson_one_third_method

    subroutine simpson_three_eighths_method(f, lower_limit, upper_limit, n, result)

    interface
            real(8) function f(x)
                real(8), intent(in) :: x
            end function f
        end interface

        real(8), intent(in) :: lower_limit, upper_limit
        integer, intent(in) :: n
        real(8), intent(out) :: result
        real(8) :: x, h
        integer :: i

        h = abs(upper_limit - lower_limit) / n
        x = lower_limit
        result = 0.0d0

        do i = 1, n - 1
            if (mod(i, 3) == 0) then
                result = result + 2.0d0 * f(x + i * h)
            else
                result = result + 3.0d0 * f(x + i * h)
            end if
        end do
        result = 0.3750d0 * h  * (f(lower_limit) + f(upper_limit) + result)

    end subroutine simpson_three_eighths_method
    
    subroutine romberg_method(f, lower_limit, upper_limit, n, result)

    interface
            real(8) function f(x)
                real(8), intent(in) :: x
            end function f
        end interface

        real(8), intent(in) :: lower_limit, upper_limit
        integer, intent(in) :: n
        real(8), intent(out) :: result
        real(8), allocatable :: R(:, :)
        real(8) :: x, h, sum
        integer :: i, j, k, m

        allocate(R(n, n))
        
        R(1,1) = (upper_limit - lower_limit) * 0.5 * (f(lower_limit) + f(upper_limit))
      
          do i = 2, n 
            sum = 0
            do j = 1, (2**(i-2)), 1
              sum = sum + f(lower_limit + ((upper_limit - lower_limit) / 2**(i-2)) * (j - 0.5))
            end do
            R(i,1) = 0.5 * (R(i-1,1) + ((upper_limit - lower_limit) / (2**(i-2))) * sum)
          end do
          
          do k = 2, n
            do m = k, n
              R(m,k) = ((4**(k-1)) * R(m,k-1) - R(m-1,k-1)) / (4**(k-1) - 1)
            end do
          end do
        result =  R(n,n)

    end subroutine romberg_method
    
end module numerical_integration

