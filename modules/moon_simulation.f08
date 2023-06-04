module moon_simulation
    implicit none
    private 
    public :: moon_x_dx_d2x

contains 

    subroutine moon_x_dx_d2x(tmax, vy0, tl, xl, yl)
        use, intrinsic :: iso_fortran_env, only: dp => real64
        implicit none
        real(dp), intent(in) :: tmax, vy0
        real(dp), dimension(:), allocatable, intent(out) :: tl, xl, yl
        real(dp) :: GM, pos(2), vel(2), acel(2), acelt, r, dt, t
        integer :: i, n
!   Determinacion de constates
        GM = 4.0_dp * pi**2
!   Condiciones iniciales
        pos = [1.0_dp, 0.0_dp]
        vel = [0.0_dp, vy0]
        acel = [0.0_dp, 0.0_dp]
        t = 0.0_dp
        dt = 0.002_dp
        n = ceiling(tmax / dt)
        allocate(tl(n), xl(n), yl(n))

        i = 1

        do while (t <= tmax)
            r = sqrt(sum(pos**2))
                acelt = -GM * pos(i) / r**3
            pos(i) = pos(i) + vel(i) * dt + 0.5_dp * acelt * dt * dt
                acel(i) = -GM * pos(i) / r**3
            vel(i) = vel(i) + dt * (acel(i) + acelt) * 0.5_dp


            tl(i) = t 
            xl(i) = pos(1)
            yl(i) = pos(2)
            t = t + dt
            i = i + 1
        end do
    end subroutine 

end module moon_simulation
