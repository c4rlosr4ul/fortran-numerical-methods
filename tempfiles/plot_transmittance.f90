program plot_transmittance
    implicit none 
    real(8) :: E
    integer :: i, n
    character(len=64) :: filename

    filename = "../data/plot_transmittance.dat"
    open(unit=10, file=filename, status="replace", action="write")

    E = 1.0d-2  ! Start slightly above 0 to avoid division by zero at x=0
    n = 100
    do i = 1, n
        E = E + 0.01d0
        write(10, *) E, T(E)
    end do

    close(10)

contains

    real(8) function T(E)
        real(8), intent(in) :: E
        real(8) :: V_0, m, b, a, h_bar, x, k
        m = 1d0; a = 1d0; b = 2d0; h_bar = 1d0
        V_0 = 1d0
        x = E / V_0

        ! Check to prevent invalid operations
        if (x <= 0d0 .or. x >= 1d0) then
            T = 0d0  ! Set T to 0 or another appropriate value for these cases
        else
            k = sqrt((2d0 * m * V_0 / h_bar) * (1d0 - x))
            ! Use max function to avoid negative argument for sinh
            T = 1d0 / (1d0 + (sinh(max(k * (b - a), 0d0)) / (4d0 * x * (1d0 - x)))**2)
        end if
    end function T

end program plot_transmittance

