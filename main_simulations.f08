PROGRAM main_simulations
    ! In development
    IMPLICIT NONE
    
    REAL(8) :: tmp, o, p, d_o, d_p
    REAL(8) :: t0, o0, p0, d_o0, d_p0
    REAL(8) :: pi
    INTEGER :: n  
    REAL(8), PARAMETER :: GM = 4.0d0 * 3.1415 ** 2 ! pi ** 2 ! 3.986e14 no sense

    ! Parametrization in polar coordinates
    ! Initial conditions, all in international units    t0 = 0.0d0
    o0 = 0.0d0
    p0 = 384.400 ! 0.0d0
    d_o0 = 1.0d0
    d_p0 = 0.0d0
    n = 1000
    tmp = 100.0d0

    CALL se2o_m_rk_2or(d2o, d2p, t0, o0, p0, d_o0, d_p0, n, tmp, o, p, d_o, d_p) 
    WRITE(*, *) "The angle, radial distance between the Earth and the Moon, and the angular and radial velocity of the Moon are:"
    WRITE(*, *) tmp, o, p, d_o, d_p

CONTAINS

    FUNCTION d2o(t, o, p, d_o, d_p)
        REAL(8), INTENT(IN) :: t, o, p, d_o, d_p
        REAL(8) :: d2o
        d2o = -2.0d0 * d_p * d_o / p
    END FUNCTION d2o

    FUNCTION d2p(t, o, p, d_o, d_p)
        REAL(8), INTENT(IN) :: t, o, p, d_o, d_p
        REAL(8) :: d2p
        d2p = -GM / p ** 2 + d_p * o ** 2
    END FUNCTION d2p

    SUBROUTINE se2o_m_rk_2or(d2x1, d2x2, t0, x10, x20, dx10, dx20, n, t, x1, x2, dx1, dx2 )
        INTERFACE
            REAL(8) FUNCTION d2x1(t, x1, x2, dx1, dx2)
                REAL(8), INTENT(IN) :: t, x1, x2, dx1, dx2
            END FUNCTION d2x1

            REAL(8) FUNCTION d2x2(t, x1, x2, dx1, dx2)
                REAL(8), INTENT(IN) :: t, x1, x2, dx1, dx2
            END FUNCTION d2x2
        END INTERFACE
            
        REAL(8), INTENT(IN) :: t0, x10, x20, dx10, dx20
        INTEGER, INTENT(IN) :: n
        REAL(8), INTENT(OUT) :: x1, x2, dx1, dx2
        REAL(8) :: h, tt, t, k1x1, k1x2, l1x1, l1x2, k2x1, k2x2, l2x1, l2x2
        INTEGER :: i
        CHARACTER(len=40) :: filename

        filename = "data/moon-simulation_t-o-p-do-dp.dat"

        OPEN(unit=10, file=filename, status="unknown", action="write")

        h = ABS(t - t0) / n
        tt = t0
        x1 = x10
        x2 = x20
        dx1 = dx10
        dx2 = dx20
        WRITE(10, *) tt, x1, x2, dx1, dx2

        DO i = 1, n
            tt = tt + h
            k1x1 = h * dx1
            l1x1 = h * d2x1(tt, x1, x2, dx1, dx2) 

            k1x2 = h * dx2
            l1x2 = h * d2x2(tt, x1, x2, dx1, dx2) 

            k2x1 = h * (dx1 + l1x1)
            l2x1 = h * d2x1(tt + h, x1 + k1x1, x2 + k1x2, dx1 + l1x1, dx2 + l1x2)

            k2x2 = h * (dx2 + l1x2)
            l2x2 = h * d2x2(tt + h, x1 + k1x1, x2 + k1x2, dx1 + l1x1, dx2 + l1x2)

            x1 = x1 + (k1x1 + k2x1) / 2
            x2 = x2 + (k1x2 + k2x2) / 2

            dx1 = dx1 + (l1x1 + l2x1) / 2
            dx2 = dx2 + (l1x2 + l2x2) / 2

            WRITE(10, *) tt, x1, x2, dx1, dx2
        END DO

        CLOSE(10)

    END SUBROUTINE se2o_m_rk_2or

END PROGRAM main_simulations
