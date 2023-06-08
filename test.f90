MODULE numerical_derivative
    INTERFACE
        FUNCTION func_interface(x)
            REAL(8), INTENT(IN) :: x
            REAL(8) :: func_interface
        END FUNCTION func_interface
    END INTERFACE

    ABSTRACT INTERFACE
        FUNCTION f(x)
            REAL(8), INTENT(IN) :: x
            REAL(8) :: f
        END FUNCTION f
    END INTERFACE

    CONTAINS

    SUBROUTINE forward_difference(f, x, h, df)
        PROCEDURE(func_interface), INTENT(IN) :: f
        REAL(8), INTENT(IN) :: x, h
        REAL(8), INTENT(OUT) :: df
        df = (f(x + h) - f(x)) / h
    END SUBROUTINE forward_difference

    SUBROUTINE backward_difference(f, x, h, df)
        PROCEDURE(func_interface), INTENT(IN) :: f
        REAL(8), INTENT(IN) :: x, h
        REAL(8), INTENT(OUT) :: df
        df = (f(x) - f(x - h)) / h
    END SUBROUTINE backward_difference

    SUBROUTINE centered_difference(f, x, h, df)
        PROCEDURE(func_interface), INTENT(IN) :: f
        REAL(8), INTENT(IN) :: x, h
        REAL(8), INTENT(OUT) :: df
        df = (f(x + h) - f(x - h)) / (2 * h)
    END SUBROUTINE centered_difference

END MODULE numerical_derivative

REAL(8) FUNCTION f(x)
    REAL(8), INTENT(IN) :: x
    f = x**2
END FUNCTION f

PROGRAM main_derivative
    USE numerical_derivative
    IMPLICIT NONE
    REAL(8) :: x, h, df
    TYPE(func_wrapper) :: f_wrap

    x = 1.0
    h = 0.0001

    NULLIFY(f_wrap%f)
    f_wrap => f

    CALL forward_difference(f_wrap, x, h, df)
    PRINT*, "Forward difference: ", df

    CALL backward_difference(f_wrap, x, h, df)
    PRINT*, "Backward difference: ", df

    CALL centered_difference(f_wrap, x, h, df)
    PRINT*, "Centered difference: ", df

END PROGRAM main_derivative
