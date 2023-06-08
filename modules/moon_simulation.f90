MODULE moon_simulation
    ! In development, do not use
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: moon_x_dx_d2x

CONTAINS 

    ! Simulates the motion of the Moon in the x-y plane
    SUBROUTINE moon_x_dx_d2x(tmax, vy0, tl, xl, yl)
        USE, INTRINSIC :: iso_fortran_env, ONLY: dp => real64
        IMPLICIT NONE
        REAL(dp), INTENT(IN) :: tmax, vy0
        REAL(dp), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: tl, xl, yl
        REAL(dp) :: GM, pos(2), vel(2), acel(2), acelt, r, dt, t
        INTEGER :: i, n
        
        ! Determine constants
        GM = 4.0_dp * pi**2
        
        ! Initial conditions
        pos = [1.0_dp, 0.0_dp]
        vel = [0.0_dp, vy0]
        acel = [0.0_dp, 0.0_dp]
        t = 0.0_dp
        dt = 0.002_dp
        n = CEILING(tmax / dt)
        ALLOCATE(tl(n), xl(n), yl(n))

        i = 1

        DO WHILE (t <= tmax)
            r = SQRT(SUM(pos**2))
            acelt = -GM * pos(i) / r**3
            pos(i) = pos(i) + vel(i) * dt + 0.5_dp * acelt * dt * dt
            acel(i) = -GM * pos(i) / r**3
            vel(i) = vel(i) + dt * (acel(i) + acelt) * 0.5_dp

            tl(i) = t 
            xl(i) = pos(1)
            yl(i) = pos(2)
            t = t + dt
            i = i + 1
        END DO
    END SUBROUTINE 

END MODULE moon_simulation
