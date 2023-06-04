program main_simulations

    implicit none
    real(8) :: tmp, o, p, do, dp
    real(8) :: t0, o0, p0, do0, dp0
    real(8) :: pi
    integer :: n  
    real(8), parameter :: GM = 4.0d0 * 3.1415 **2 !pi **2 !3.986e14 no sense

    ! Parametrizacion en polares
    ! Condiciones iniciales, todos en unidades internacionales
    t0 = 0.0d0
    o0 = 0.0d0
    p0 = 384.400!0d0
    do0 = 1.0d0
    dp0 = 0.0d0
    n = 1000
    tmp = 100.0d0

    call se2o_m_rk_2or(d2o, d2p, t0, o0, p0, do0, dp0, n, tmp, o, p, do, dp) 
    write(*, *) "El angulo, la distacia radial entre la tierra y la luna, la velocidad angular y radial de la luna es:"
    write(*, *) tmp, o, p, do, dp

contains

    function d2o(t, o, p, do, dp)
        real(8), intent(in) :: t, o, p, do, dp
        real(8) :: d2o
            d2o = -2.0d0*dp*do/p
    end function d2o

    function d2p(t, o, p, do, dp)
        real(8), intent(in) :: t, o, p, do, dp
        real(8) :: d2p
            d2p = -GM/p**2 + dp*o**2
    end function d2p

    subroutine se2o_m_rk_2or(d2x1, d2x2, t0, x10, x20, dx10, dx20, n, t, x1, x2, dx1, dx2 )
        interface
            real(8) function d2x1(t, x1, x2, dx1, dx2)
            real(8), intent(in) :: t, x1, x2, dx1, dx2
            end function d2x1

            real(8) function d2x2(t, x1, x2, dx1, dx2)
            real(8), intent(in) :: t, x1, x2, dx1, dx2
            end function d2x2
        end interface
            
        real(8), intent(in) :: t0, x10, x20, dx10, dx20
        integer, intent(in) :: n
        real(8), intent(out) ::  x1, x2, dx1, dx2
        real(8) :: h, tt, t,  k1x1, k1x2, l1x1, l1x2, k2x1, k2x2, l2x1, l2x2
        integer :: i
        character(len=40) :: filename

        filename = "data/moon-simulation_t-o-p-do-dp.dat"

        open(unit=10, file=filename, status="unknown", action="write")

        h = abs(t - t0)/n
        tt = t0; x1 = x10; x2 = x20; dx1 = dx10; dx2 = dx20
        write(10, *) tt, x1, x2, dx1, dx2

        do i = 1, n
            tt = tt + h
                k1x1 = h * dx1
                l1x1 = h * d2x1(tt, x1, x2, dx1, dx2) 

                k1x2 = h * dx2
                l1x2 = h * d2x2(tt, x1, x2, dx1, dx2) 

                k2x1 = h * (dx1 + l1x1)
                l2x1 = h * d2x1(tt + h, x1 + k1x1, x2 + k1x2, dx1 + l1x1, dx2 + l1x2)

                k2x2 = h * (dx2 + l1x2)
                l2x2 = h * d2x2(tt + h, x1 + k1x1, x2 + k1x2, dx1 + l1x1, dx2 + l1x2)

            x1 = x1 + (k1x1 + k2x1)/2
            x2 = x2 + (k1x2 + k2x2)/2

            dx1 = dx1 + (l1x1 + l2x1)/2
            dx2 = dx2 + (l1x2 + l2x2)/2

            write(10, *) tt, x1, x2, dx1, dx2
        end do

        close(10)

    end subroutine se2o_m_rk_2or

end program main_simulations
