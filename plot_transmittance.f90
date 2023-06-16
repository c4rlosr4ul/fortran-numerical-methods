program plot_transmittance
    implicit none 
    real(8) :: x, r1, r2, r3
    integer :: i, n
    character(len=64) :: filename

        filename = "data/plot_transmittance.dat"
        open(unit=10, file=filename, status="unknown", action="write")

    n = 100000
    do i = 1, n
        x = x + 0.001
        r1 = 0.2; r2 = 0.5; r3 = 0.9
        write(10, *)  x, T(r1,x), T(r2, x), T(r3, x) 
    end do

contains

    real(8) function T(r, x)
        real(8), INTENT(IN) :: r, x
            T = 1/(1 + ( (4*r**2)/(1 - r**2)**2 * (sin(0.5*x))**2 ) )
    end function

end program plot_transmittance
