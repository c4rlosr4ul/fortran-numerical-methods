
program moon_simulation_program
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    integer :: n
    real(dp) :: t0, t
    real(dp), dimension(2) :: r, v
    real(dp), parameter :: pi = 4.0_dp * atan(1.0_dp)
    real(dp) :: gm, pos(2), vel(2), acel(2), acelt(2), r_temp, dt, time
    integer :: i, j, n_steps
    character(len=32) :: filename

    ! assign initial values
    n = 10000
    t0 = 0.0_dp
    t = 10000.0_dp
    r = [2.0_dp, 0.0_dp]
    v = [0.0_dp, pi]

    ! determine constants
    gm = 4.0_dp * pi

    ! initial conditions
    pos = r
    vel = v
    acel = [0.0_dp, 0.0_dp]
    time = t0
    dt = t / real(n)
    n_steps = n

    filename = "data/moon_sim_ev.dat"
    open(unit=10, file=filename, status="unknown", action="write")

    do i = 1, n_steps
        r_temp = sqrt(sum(pos**2))
        acelt = -gm * pos / r_temp**3
        pos = pos + vel * dt + 0.5_dp * acelt * dt * dt
        acel = -gm * pos / r_temp**3
        vel = vel + dt * (acel + acelt) * 0.5_dp
        write(10, *) time, pos(1), pos(2)
        time = time + dt
    end do

    close(10)

end program moon_simulation_program

