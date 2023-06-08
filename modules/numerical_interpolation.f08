MODULE numerical_interpolation
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: lagrange_interpolation, newton_forward_interpolation, newton_backward_interpolation

CONTAINS

    SUBROUTINE lagrange_interpolation(x, y, n, xi, yi)
        REAL(8), DIMENSION(:), INTENT(IN) :: x, y
        REAL(8), INTENT(IN) :: xi
        INTEGER, INTENT(IN) :: n
        REAL(8), INTENT(OUT) :: yi
        REAL(8) :: ssum, prod
        INTEGER :: i, j

        yi = 0.0d0

        DO i = 1, n
            prod = 1.0d0
            DO j = 1, n
                IF (i /= j) THEN
                    prod = prod * (xi - x(j)) / (x(i) - x(j))
                END IF
            END DO
            yi = yi + prod * y(i)
        END DO

    END SUBROUTINE lagrange_interpolation

    SUBROUTINE newton_forward_interpolation(x, y, n, xi, yi)
        REAL(8), DIMENSION(:), INTENT(IN) :: x, y
        REAL(8), INTENT(IN) :: xi
        INTEGER, INTENT(IN) :: n
        REAL(8), INTENT(OUT) :: yi
        REAL(8), ALLOCATABLE, DIMENSION(:, :) :: F
        REAL(8) :: s, norm
        INTEGER :: j, k, m, l

        norm = ABS(x(n) - x(1)) / REAL(n - 1)

        s = (xi - x(1)) / norm

        ALLOCATE(F(n, n))

        DO j = 1, n
            F(j, 1) = y(j)
        END DO

        DO k = 2, n
            DO m = 1, n - k + 1
                F(m, k) = F(m + 1, k - 1) - F(m, k - 1)
            END DO
        END DO

        yi = F(1, 1)

        DO l = 2, n
            yi = yi + combination(s, l - 1) * F(1, l)
        END DO

        DEALLOCATE(F)

    END SUBROUTINE newton_forward_interpolation

    SUBROUTINE newton_backward_interpolation(x, y, n, xi, yi)
        REAL(8), DIMENSION(:), INTENT(IN) :: x, y
        REAL(8), INTENT(IN) :: xi
        INTEGER, INTENT(IN) :: n
        REAL(8), INTENT(OUT) :: yi
        REAL(8), DIMENSION(:, :), ALLOCATABLE :: F
        REAL(8) :: s, norm
        INTEGER :: i, j

        norm = (x(n) - x(1)) / REAL(n - 1)
        s = (xi - x(n)) / norm

        ALLOCATE(F(n, n))

        F(:, 1) = y(:)

        DO j = 2, n
            DO i = n, j, -1
                F(i, j) = (F(i, j - 1) - F(i - 1, j - 1)) / (x(i) - x(i - j + 1))
            END DO
        END DO

        yi = F(n, n)

        DO i = n - 1, 1, -1
            yi = yi * (s - x(i)) + F(i, i)
        END DO

        DEALLOCATE(F)
    END SUBROUTINE newton_backward_interpolation

    FUNCTION factorial(n)
        IMPLICIT NONE
        INTEGER :: factorial, n, i

        IF (n == 1) THEN
            factorial = 1
        ELSE
            factorial = 1
            DO i = 1, n
                factorial = factorial * i
            END DO
        END IF

    END FUNCTION factorial

    FUNCTION combination(s, i)
        IMPLICIT NONE
        REAL(8) :: s, combination
        INTEGER :: i, k

        combination = 1.0d0
        DO k = 0, i - 1
            combination = combination * (s - k)
        END DO
        combination = combination / factorial(i)
    END FUNCTION combination

END MODULE numerical_interpolation
