PROGRAM main_random_methods
    USE random_methods
    IMPLICIT NONE
    
    REAL(8) :: pi, rsl
    INTEGER :: n
    REAL(8) :: x0, x1, y0, y1, z0, z1 

    ! Calculate pi using the Monte Carlo method
    CALL calculate_pi(100000, pi)
    WRITE(*, *) "Estimated value of pi: ", pi

    ! Monte Carlo integration
    x0 = 0.0d0
    x1 = 3.0d0
    n = 100000
    CALL monte_carlo_integration(f1, x0, x1, n, rsl)
    WRITE(*, *) "Integration of f1(x) result: ", rsl

    ! Double integral Monte Carlo
    n = 100000
    ! Define the range of the parallelepiped
    x0 = 0.0d0
    y0 = 0.0d0
    z0 = 0.0d0 ! Integration above z = 0.0
    ! Define the integration region (parallelepiped)
    x1 = 1.0d0
    y1 = 1.0d0
    z1 = f2(x1, y1) ! Determine the maximum value of the function f2 within this region
    CALL double_integral_monte_carlo(f2, x0, x1, y0, y1, n, rsl)
    WRITE(*, *) "Double Integration of the function f2(x,y): ", rsl

    ! Calculation of volumes with inequalities
    n = 1000000
    ! Define the range of the parallelepiped for the integration
    x0 = 0.0d0
    y0 = 0.0d0
    z0 = 0.0d0
    ! Define the region for the volume calculation
    x1 = 1.0d0
    y1 = 1.0d0
    z1 = 1.0d0
    CALL calculate_volumes(x0, x1, y0, y1, z0, z1, n, rsl)
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
