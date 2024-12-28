
MODULE gauss_legendre_module
  IMPLICIT NONE
CONTAINS

  SUBROUTINE gauss_legendre_nodes_and_weights(n, x, w)
    IMPLICIT NONE
    INTEGER, INTENT(IN)          :: n
    REAL(KIND=8), INTENT(OUT)    :: x(n), w(n)

    INTEGER :: i, j, m
    REAL(KIND=8) :: z, z1, p1, p2, p3, pp
    REAL(KIND=8) :: pi
    pi = 3.141592653589793d0

    ! Number of roots we only need to find in [0,1] due to symmetry
    m = (n + 1) / 2

    DO i = 1, m
       ! Approximate the i-th root via an initial guess
       z = COS( pi * (REAL(i,8) - 0.25d0) / (REAL(n,8) + 0.5d0) )

       ! Newton's method to refine the root
       DO
          p1 = 1.0d0
          p2 = 0.0d0
          ! Evaluate the Legendre polynomial P_n(z) using recursion
          DO j = 1, n
             p3 = p2
             p2 = p1
             p1 = ((2.0d0*REAL(j,8)-1.0d0)*z*p2 - (REAL(j,8)-1.0d0)*p3) / REAL(j,8)
          END DO

          ! Derivative of P_n(z)
          pp = REAL(n,8)*( z*p1 - p2 ) / ( z*z - 1.0d0 )

          z1 = z
          z = z1 - p1/pp   ! Newton step

          IF (ABS(z - z1) < 1.0d-14) EXIT
       END DO

       ! Store the root and its symmetric counterpart
       x(i)          = -z
       x(n+1 - i)    =  z

       ! Compute the weights
       w(i)          = 2.0d0 / ( (1.0d0 - z*z)*pp*pp )
       w(n+1 - i)    = w(i)
    END DO

  END SUBROUTINE gauss_legendre_nodes_and_weights


  REAL(KIND=8) FUNCTION gauss_legendre_integration(f, n, a, b) RESULT(integral)
    IMPLICIT NONE

    INTERFACE
       FUNCTION f(x) RESULT(val)
         USE, INTRINSIC :: ISO_C_BINDING, ONLY : c_double
         IMPLICIT NONE
         REAL(KIND=8), INTENT(IN) :: x
         REAL(KIND=8) :: val
       END FUNCTION f
    END INTERFACE

    INTEGER, INTENT(IN)       :: n
    REAL(KIND=8), INTENT(IN)  :: a, b

    ! NOTE: do *not* redeclare 'integral' here since it is the function name
    REAL(KIND=8), ALLOCATABLE :: x(:), w(:)
    REAL(KIND=8)              :: xm, xr, xx
    INTEGER                   :: k

    ! Allocate arrays for nodes and weights
    ALLOCATE(x(n), w(n))

    ! Get the Gauss–Legendre nodes and weights on [-1,1]
    CALL gauss_legendre_nodes_and_weights(n, x, w)

    ! Initialize
    integral = 0.0d0

    ! Midpoint and half-length for [a,b]
    xm = 0.5d0*(b + a)
    xr = 0.5d0*(b - a)

    DO k = 1, n
       xx = xm + xr*x(k)
       integral = integral + w(k) * f(xx)
    END DO

    ! Multiply by the half-length
    integral = integral * xr

    ! Deallocate
    DEALLOCATE(x, w)
  END FUNCTION gauss_legendre_integration

END MODULE gauss_legendre_module


!======================================================================
! Main Program
!======================================================================
PROGRAM test_gauss_legendre
  USE gauss_legendre_module
  IMPLICIT NONE

  REAL(KIND=8) :: result
  INTEGER :: n
  REAL(KIND=8) :: a, b

  ! Here we choose the number of integration points and interval
  n = 5
  a = 0.0d0
  b = 1.0d0

  ! Use the local function my_function via the Gauss–Legendre integrator
  result = gauss_legendre_integration(my_function, n, a, b)

  WRITE(*,*) "Testing Gauss–Legendre Quadrature on [0,1] with f(x)=exp(x)."
  WRITE(*,'(A,F16.8)') "Approx: ", result
  WRITE(*,'(A,F16.8)') "Exact : ", EXP(1.0d0) - 1.0d0

CONTAINS

  REAL(KIND=8) FUNCTION my_function(x) RESULT(val)
    IMPLICIT NONE
    REAL(KIND=8), INTENT(IN) :: x
    val = EXP(x)
  END FUNCTION my_function

END PROGRAM test_gauss_legendre

