
program main_integration
    use numerical_integration
    
    implicit none 
    real(8) :: lower_limit, upper_limit, result
    integer :: n
    
    lower_limit = 0.0d0
    upper_limit = 1.0d0
    n = 100000

    write(*, *) "Integration of f from lower limit to upper limit using:"
    call trapezoidal_method(f, lower_limit, upper_limit, n, result)
    write(*, *) "Trapezoidal Method: ", result

    call simpson_one_third_method(f, lower_limit, upper_limit, n, result)
    write(*, *) "Simpson's 1/3 Method: ", result

    call simpson_three_eighths_method(f, lower_limit, upper_limit, n, result)
    write(*, *) "Simpson's 3/8 Method: ", result

    n = 15

    call romberg_method(f, lower_limit, upper_limit, n, result)
    write(*, *) "Romberg's Method: ", result
    ! The best

contains
    function f(x)
        real(8), intent(in) :: x
        real(8) :: f
            f = exp(-1 * x ** 2)
    end function

end program main_integration
