PROGRAM main_random_methods
    USE random_methods
    IMPLICIT NONE
    
    REAL(8) :: pi, rsl
    INTEGER(8) :: n
    REAL(8) :: x0, x1, y0, y1, f1max, f2max, z0, z1 
    REAL(8), DIMENSION(2) :: x, y, z

    ! Calculate pi using the Monte Carlo method
    n = 100000
    CALL calculate_pi(n, pi)
    WRITE(*, *) "Estimated value of pi: ", pi

    ! Monte Carlo integration
    x = [0.0d0, 3.0d0]
    f1max = f1(x(2))
    n = 100000
    CALL monte_carlo_integration(f1, x, f1max, n, rsl)
    WRITE(*, *) "Integration of f1(x) result: ", rsl

    ! Double integral Monte Carlo (Calculate the volume beteewn the surface f2 and the plane xY)
    n = 100000
    ! Define the range of the base
    x = [0.0d0, 1.0d0]
    y = [0.0d0, 1.0d0]
    f2max = 1.0d0 ! Determine the maximum value of the function f2 within this region
    CALL double_integral_monte_carlo(f2, x, y, f2max, n, rsl)
    WRITE(*, *) "Double Integration of the function f2(x,y): ", rsl

    ! Calculation of volumes with inequalities
    n = 1000000
    ! Define the range of the parallelepiped for the integration
    x = [0.0d0, 1.0d0]
    y = [0.0d0, 1.0d0]
    z = [0.0d0, 1.0d0]

    CALL calculate_volumes(x, y, z, n, rsl)
    WRITE(*, *) "Volume within the specified conditions: ", rsl

CONTAINS

    REAL(8) FUNCTION f1(x)
        IMPLICIT NONE
        REAL(8), INTENT(IN) :: x
        f1 = x**2
    END FUNCTION f1
    
    REAL(8) FUNCTION f2(x, y)
        IMPLICIT NONE
        REAL(8), INTENT(IN) :: x, y
        f2 = SQRT(SIN(LOG(x + y + 1.0)))
    END FUNCTION f2
    
END PROGRAM main_random_methods
