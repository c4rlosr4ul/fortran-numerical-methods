
module moon_simulation
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    private
    public :: moon_sim_ev

contains

    ! Simulates the motion of the Moon in the x-y plane
    subroutine moon_sim_ev(n, t0, t, r, v)
        implicit none
        integer, intent(in) :: n
        real(dp), intent(in) :: t0, t
        real(dp), dimension(:), intent(in) :: r, v
        real(dp), dimension(:), allocatable :: tl, xl, yl
        real(dp) :: GM, pos(2), vel(2), accel(2), dt, time
        integer :: i, n_steps, unit
        character(len=32) :: filename

        ! Constants
        real(dp), parameter :: pi = 3.14159265358979323846_dp
        GM = 4902.8_dp  ! Assuming units are km^3/s^2, adjust as necessary

        ! File setup
        filename = "data/moon_sim_ev.dat"
        open(newunit=unit, file=filename, status="replace", action="write")

        ! Initial conditions
        pos = r
        vel = v
        time = t0
        dt = t / real(n, dp)
        n_steps = n

        allocate(tl(n_steps), xl(n_steps), yl(n_steps))

        do i = 1, n_steps
            accel = -GM * pos / sum(pos**2)**1.5_dp
            pos = pos + vel * dt + 0.5_dp * accel * dt**2
            vel = vel + accel * dt

            tl(i) = time
            xl(i) = pos(1)
            yl(i) = pos(2)
            write(unit, *) tl(i), xl(i), yl(i)

            time = time + dt
        end do

        close(unit)
    end subroutine moon_sim_ev

end module moon_simulation

