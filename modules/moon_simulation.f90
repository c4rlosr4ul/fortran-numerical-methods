MODULE moon_simulation
    ! In development, do not use
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: moon_sim_ev

CONTAINS 

! Simulates the motion of the Moon in the x-y plane
SUBROUTINE moon_sim_ev(n, t0, t, r, v)
    USE, INTRINSIC :: iso_fortran_env, ONLY: dp => real64
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n
    REAL(dp), INTENT(IN) :: t0, t
    REAL(dp), DIMENSION(:), INTENT(IN) :: r, v
    REAL(dp), DIMENSION(:), ALLOCATABLE, :: tl, xl, yl
    REAL(dp) :: GM, pos(2), vel(2), acel(2), acelt, r_temp, dt, time
    INTEGER :: i, n_steps
    character(len=32) :: filename

        filename = "data/moon_sim_ev.dat"
        open(unit=10, file=filename, status="unknown", action="write")

    ! Determine constants
    GM = 4.0_dp * pi
    
    ! Initial conditions
    pos = r
    vel = v
    acel = [0.0_dp, 0.0_dp]
    time = t0
    dt = t / REAL(n)
    n_steps = n
    
    ALLOCATE(tl(n_steps), xl(n_steps), yl(n_steps))

    DO i = 1, n_steps
        r_temp = SQRT(SUM(pos**2))
        acelt = -GM * pos(1) / r_temp**3
        pos(1) = pos(1) + vel(1) * dt + 0.5_dp * acelt * dt * dt
        acel(1) = -GM * pos(1) / r_temp**3
        vel(1) = vel(1) + dt * (acel(1) + acelt) * 0.5_dp

        tl(i) = time
        xl(i) = pos(1)
        yl(i) = pos(2)
        WRITE(10, *) tl(i), xl(i), yl(i)
        time = time + dt

    END DO
END SUBROUTINE

END MODULE moon_simulation
