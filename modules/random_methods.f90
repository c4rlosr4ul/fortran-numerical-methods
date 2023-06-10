MODULE random_methods
    IMPLICIT NONE
    
CONTAINS

    SUBROUTINE calculate_pi(n, pi)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: n
        REAL(8), INTENT(OUT) :: pi
        INTEGER :: i, countInside
        REAL(8) :: x, y
        
        countInside = 0
        DO i = 1, n
            CALL random_number(x)
            CALL random_number(y)
            IF (x**2 + y**2 <= 1.0) THEN
               countInside = countInside + 1
            END IF
        END DO
        
        pi = 4.0 * countInside / n
    END SUBROUTINE calculate_pi
    
    SUBROUTINE monte_carlo_integration(f, x0, x1, n, rsl)
        INTERFACE
            REAL(8) FUNCTION f(x)
                REAL(8), INTENT(IN) :: x
            END FUNCTION f
        END INTERFACE
        
        IMPLICIT NONE
        REAL(8), INTENT(IN) :: x0, x1
        INTEGER, INTENT(IN) :: n
        REAL(8), INTENT(OUT) :: rsl
        INTEGER :: i
        REAL(8) :: rn, rm, r, s, rx, frx, rfx, p
        
        p = 0.0
        
        DO i = 1, n
            CALL random_number(rn)
            CALL random_number(rm)
            r = rn
            s = rm
            rx = r * (x1 - x0) + x0
            frx = f(rx)
            rfx = s * (f(x1) - f(x0)) + f(x0)
            IF (0 <= rfx .AND. rfx <= frx) THEN
                p = p + 1
            END IF
        END DO
        
        rsl = (x1 - x0) * f(x1) * p / n
    END SUBROUTINE monte_carlo_integration
    
    SUBROUTINE double_integral_monte_carlo(f, x0, x1, y0, y1, n, rsl)
        INTERFACE
            REAL(8) FUNCTION f(x, y)
                REAL(8), INTENT(IN) :: x, y
            END FUNCTION f
        END INTERFACE
        
        IMPLICIT NONE
        REAL(8), INTENT(IN) :: x0, x1, y0, y1
        INTEGER, INTENT(IN) :: n
        REAL(8), INTENT(OUT) :: rsl
        INTEGER :: i
        REAL(8) :: rxx, ryy, rzz, x, y, z, in_
        
        in_ = 0.0
        
        DO i = 1, n
            CALL random_number(rxx)
            CALL random_number(ryy)
            CALL random_number(rzz)
            
            x = rxx * (x1 - x0) + x0
            y = ryy * (y1 - y0) + y0
            z = rzz * (f(x1, y1) - f(x0, y0)) + f(x0, y0)
            
            IF (f(x, y) >= z) THEN
                in_ = in_ + 1
            END IF
        END DO
        
        rsl = (x1 - x0) * (y1 - y0) * (f(x1, y1) - f(x0, y0)) * (in_ / n)
    END SUBROUTINE double_integral_monte_carlo
    
    SUBROUTINE calculate_volumes(f, x0, x1, y0, y1, z0, z1, n, rsl)
        INTERFACE
            REAL(8) FUNCTION f(x, y, z)
                REAL(8), INTENT(IN) :: x, y, z
            END FUNCTION f
        END INTERFACE
        
        IMPLICIT NONE
        REAL(8), INTENT(IN) :: x0, x1, y0, y1, z0, z1
        INTEGER, INTENT(IN) :: n
        REAL(8), INTENT(OUT) :: rsl
        INTEGER :: i
        REAL(8) :: rxx, ryy, rzz, x, y, z, in_
        
        in_ = 0.0
        
        DO i = 1, n
            CALL random_number(rxx)
            CALL random_number(ryy)
            CALL random_number(rzz)
            
            x = rxx * (x1 - x0) + x0
            y = ryy * (y1 - y0) + y0
            z = rzz * (z1 - z0) + z0
            
            IF (f(x, y, z) <= 0.0) THEN
                in_ = in_ + 1
            END IF
        END DO
        
        rsl = (x1 - x0) * (y1 - y0) * (z1 - z0) * (in_ / n)
    END SUBROUTINE calculate_volumes
   
END MODULE random_methods
