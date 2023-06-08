PROGRAM main_integration
    USE numerical_integration
    IMPLICIT NONE
    REAL(8) :: lower_limit, upper_limit, result
    INTEGER :: n

    lower_limit = 0.0d0
    upper_limit = 1.0d0
    n = 100

    CALL trapezoidal_method(f, lower_limit, upper_limit, n, result)
    PRINT*, "Trapezoidal method: ", result

    CALL simpson_one_third_method(f, lower_limit, upper_limit, n, result)
    PRINT*, "Simpson's 1/3 method: ", result

    CALL simpson_three_eighths_method(f, lower_limit, upper_limit, n, result)
    PRINT*, "Simpson's 3/8 method: ", result

    n = 16
    CALL romberg_method(f, lower_limit, upper_limit, n, result)
    PRINT*, "Romberg method: ", result

CONTAINS

    REAL(8) FUNCTION f(x)
        REAL(8), INTENT(IN) :: x
            f = exp(-1 * x ** 2)
    end function

END PROGRAM main_integration
