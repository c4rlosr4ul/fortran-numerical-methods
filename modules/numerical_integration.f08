MODULE numerical_integration

    IMPLICIT NONE
    PRIVATE
    PUBLIC :: trapezoidal_method, simpson_one_third_method, simpson_three_eighths_method, romberg_method

CONTAINS

    SUBROUTINE trapezoidal_method(f, lower_limit, upper_limit, n, result)
        INTERFACE
            FUNCTION f(x)
                REAL(8), INTENT(IN) :: x
                REAL(8) :: f
            END FUNCTION f
        END INTERFACE
        REAL(8), INTENT(IN) :: lower_limit, upper_limit
        INTEGER, INTENT(IN) :: n
        REAL(8), INTENT(OUT) :: result
        REAL(8) :: x, h
        INTEGER :: i

        h = abs(lower_limit - upper_limit) / n
        x = lower_limit
        result = 0.0d0

        DO i = 1, n
            result = result + h * (f(x + (i - 1) * h) + f(x + i * h)) / 2
        END DO

    END SUBROUTINE trapezoidal_method

    SUBROUTINE simpson_one_third_method(f, lower_limit, upper_limit, n, result)
        INTERFACE
            FUNCTION f(x)
                REAL(8), INTENT(IN) :: x
                REAL(8) :: f
            END FUNCTION f
        END INTERFACE
        REAL(8), INTENT(IN) :: lower_limit, upper_limit
        INTEGER, INTENT(IN) :: n
        REAL(8), INTENT(OUT) :: result
        REAL(8) :: x, h
        INTEGER :: i

        h = abs(lower_limit - upper_limit) / n
        x = lower_limit
        result = 0.0d0

        DO i = 1, n - 1, 2
            result = result + (h / 3) * (2 * f(x + h * (i + 1)) + 4 * f(x + i * h))
        END DO

        result = result + (h / 3) * (f(lower_limit) + f(upper_limit))

    END SUBROUTINE simpson_one_third_method

    SUBROUTINE simpson_three_eighths_method(f, lower_limit, upper_limit, n, result)
        INTERFACE
            FUNCTION f(x)
                REAL(8), INTENT(IN) :: x
                REAL(8) :: f
            END FUNCTION f
        END INTERFACE
        REAL(8), INTENT(IN) :: lower_limit, upper_limit
        INTEGER, INTENT(IN) :: n
        REAL(8), INTENT(OUT) :: result
        REAL(8) :: x, h
        INTEGER :: i

        h = abs(upper_limit - lower_limit) / n
        x = lower_limit
        result = 0.0d0

        DO i = 1, n - 1
            IF (MOD(i, 3) == 0) THEN
                result = result + 2.0d0 * f(x + i * h)
            ELSE
                result = result + 3.0d0 * f(x + i * h)
            END IF
        END DO

        result = 0.3750d0 * h * (f(lower_limit) + f(upper_limit) + result)

    END SUBROUTINE simpson_three_eighths_method

    SUBROUTINE romberg_method(f, lower_limit, upper_limit, n, result)
        INTERFACE
            FUNCTION f(x)
                REAL(8), INTENT(IN) :: x
                REAL(8) :: f
            END FUNCTION f
        END INTERFACE
        REAL(8), INTENT(IN) :: lower_limit, upper_limit
        INTEGER, INTENT(IN) :: n
        REAL(8), INTENT(OUT) :: result
        REAL(8), ALLOCATABLE :: R(:, :)
        REAL(8) :: x, h, sum
        INTEGER :: i, j, k, m

        ALLOCATE(R(n, n))

        R(1, 1) = (upper_limit - lower_limit) * 0.5d0 * (f(lower_limit) + f(upper_limit))

        DO i = 2, n
            sum = 0.0d0
            DO j = 1, 2**(i - 2), 1
                sum = sum + f(lower_limit + ((upper_limit - lower_limit) / 2**(i - 2)) * (j - 0.5d0))
            END DO
            R(i, 1) = 0.5d0 * (R(i - 1, 1) + ((upper_limit - lower_limit) / (2**(i - 2))) * sum)
        END DO

        DO k = 2, n
            DO m = k, n
                R(m, k) = ((4**(k - 1)) * R(m, k - 1) - R(m - 1, k - 1)) / (4**(k - 1) - 1)
            END DO
        END DO

        result = R(n, n)

        DEALLOCATE(R)

    END SUBROUTINE romberg_method

END MODULE numerical_integration
