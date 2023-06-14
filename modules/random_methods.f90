MODULE random_methods
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: calculate_pi 
    PUBLIC :: monte_carlo_integration, double_integral_monte_carlo
    PUBLIC :: calculate_volumes
CONTAINS

    SUBROUTINE calculate_pi(n, pi)
        IMPLICIT NONE
        INTEGER(8), INTENT(IN) :: n
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
    
    SUBROUTINE monte_carlo_integration(f, x, fmax, n, rsl)
        INTERFACE
            REAL(8) FUNCTION f(x)
                REAL(8), INTENT(IN) :: x
            END FUNCTION f
        END INTERFACE
        
        REAL(8), DIMENSION(2), INTENT(IN) :: x
        REAL(8), INTENT(IN) :: fmax
        INTEGER(8), INTENT(IN) :: n
        REAL(8), INTENT(OUT) :: rsl
        INTEGER(8) :: i, p
        REAL(8) :: rn, rm, rx, frx, rfx
        CHARACTER(LEN=32) :: filename
            filename = 'data/mc-integration_x_fx.dat'
            p = 0
            open(unit=10, file=filename, status="unknown", action="write")
        DO i = 1, n
            CALL random_number(rn)
            CALL random_number(rm)
            rx = rn * (x(2) - x(1)) + x(1)
            frx = f(rx)
            rfx = rm * fmax
            IF (0 <= rfx .AND. rfx <= frx) THEN
                p = p + 1
                WRITE(10, *) rx, rfx
            END IF
        END DO
        
        rsl = (x(2) - x(1)) * fmax * p / n
    END SUBROUTINE monte_carlo_integration
    
    SUBROUTINE double_integral_monte_carlo(f, x1, x2, fmax, n, rsl)
        INTERFACE
            REAL(8) FUNCTION f(x, y)
                REAL(8), INTENT(IN) :: x, y
            END FUNCTION f
        END INTERFACE
        
        REAL(8), DIMENSION(2) :: x1, x2
        REAL(8), INTENT(IN) ::  fmax
        INTEGER(8), INTENT(IN) :: n
        REAL(8), INTENT(OUT) :: rsl
        INTEGER(8) :: i, in_
        REAL(8) :: rxx, ryy, rzz, x, y, z
        CHARACTER(LEN=64) :: filename
            filename = 'data/mc-doble_integration_x_y_fxy.dat'
            in_ = 0
            open(unit=20, file=filename, status="unknown", action="write")
        DO i = 1, n
            CALL random_number(rxx)
            CALL random_number(ryy)
            CALL random_number(rzz)
            x = rxx * (x1(2) - x1(1)) + x1(1)
            y = ryy * (x2(2) - x2(1)) + x2(1)
            z = rzz * fmax
                IF (z <= f(x, y)) THEN
                    in_ = in_ + 1
                    WRITE(20, *) x, y, z
                END IF
        END DO
!        ratio = real(in_, kind=8) / n
        rsl = (x1(2) - x1(1)) * (x2(2) - x2(1)) * fmax * in_ /n
    END SUBROUTINE double_integral_monte_carlo
    
    SUBROUTINE calculate_volumes(x1, x2, x3, n, rsl)
        
        REAL(8), DIMENSION(2) :: x1, x2, x3
        INTEGER(8), INTENT(IN) :: n
        REAL(8), INTENT(OUT) :: rsl
        INTEGER(8) :: i, in_
        REAL(8) :: rxx, ryy, rzz, x, y, z, c1, c2
        CHARACTER(LEN=32) :: filename
            filename = 'data/mc-volumen-conditions.dat'
            in_ = 0
            open(unit=30, file=filename, status="unknown", action="write")
        DO i = 1, n
            CALL random_number(rxx)
            CALL random_number(ryy)
            CALL random_number(rzz)
            x = rxx * (x1(2) - x1(1)) + x1(1)
            y = ryy * (x2(2) - x2(1)) + x2(1)
            z = rzz * (x3(2) - x3(1)) + x3(1)
            
       !Condition
       c1 = x**2 + SIN(y) - z 
       c2 = x - z + EXP(y) - 1.d0

       ! We need to know waht surface is up the the other to make all rigthjjjj 
            IF (c1 <= 0.0d0 .and. c2 <= 0.d0) THEN
                in_ = in_ + 1
                WRITE(30, *) x, y, z
            END IF
        END DO
        
        rsl = (x1(2) - x1(1)) * (x2(2) - x2(1)) * (x3(2) - x3(1)) * in_ / n
    END SUBROUTINE calculate_volumes
   
END MODULE random_methods
