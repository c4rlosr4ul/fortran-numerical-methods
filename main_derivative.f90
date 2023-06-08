PROGRAM main_derivative
    USE numerical_derivative
    IMPLICIT NONE
    REAL(8) :: x, h, df

    x = 1.0
    h = 0.0001

    CALL forward_difference(f, x, h, df)
    PRINT*, "Forward difference: ", df

    CALL backward_difference(f, x, h, df)
    PRINT*, "Backward difference: ", df

    CALL centered_difference(f, x, h, df)
    PRINT*, "Centered difference: ", df

CONTAINS

    REAL(8) FUNCTION f(x)
        REAL(8), INTENT(IN) :: x
        f = x**2
    END FUNCTION f

END PROGRAM main_derivative
