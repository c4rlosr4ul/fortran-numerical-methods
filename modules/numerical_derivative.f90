MODULE numerical_derivative
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: forward_difference, backward_difference, centered_difference

CONTAINS

    SUBROUTINE forward_difference(f, x, h, df)
        INTERFACE
            REAL(8) FUNCTION f(x)
                REAL(8), INTENT(IN) :: x
            END FUNCTION f
        END INTERFACE
        REAL(8), INTENT(IN) :: x, h
        REAL(8), INTENT(OUT) :: df
        df = (f(x + h) - f(x)) / h
    END SUBROUTINE forward_difference

    SUBROUTINE backward_difference(f, x, h, df)
        INTERFACE
            REAL(8) FUNCTION f(x)
                REAL(8), INTENT(IN) :: x
            END FUNCTION f
        END INTERFACE
        REAL(8), INTENT(IN) :: x, h
        REAL(8), INTENT(OUT) :: df
        df = (f(x) - f(x - h)) / h
    END SUBROUTINE backward_difference

    SUBROUTINE centered_difference(f, x, h, df)
        INTERFACE
            REAL(8) FUNCTION f(x)
                REAL(8), INTENT(IN) :: x
            END FUNCTION f
        END INTERFACE
        REAL(8), INTENT(IN) :: x, h
        REAL(8), INTENT(OUT) :: df
        df = (f(x + h) - f(x - h)) / (2 * h)
    END SUBROUTINE centered_difference

END MODULE numerical_derivative
