program main_random_methods
    implicit none
    real(8) :: x, y, pi, x0, x1, p, q, r, s, rsl
    REAl(8) :: rx, frx, rfx
    real(8) :: y0, y1, z, z0, z1, c1, c2
    real :: rn, rm, rxx, ryy, rzz, frs 
    integer :: i, n, countInside
    real(8) :: in_, out_
    character(len=48) :: filename

    ! Calculate pi using Monte Carlo method
    n = 100000
    countInside = 0
    do i = 1, n
        call random_number(x)
        call random_number(y)
        if (x**2 + y**2 <= 1.0) then
           countInside = countInside + 1
        end if
    end do
    pi = 4.0 * countInside / n
    write(*, *) "Estimated pi: ", pi

    ! Monte Carlo integration

    x0 = 0.0d0
    x1 = 3.0d0
    n = 100000
    p = 0.0d0
    q = 0.0d0

    filename = "data/mc-integration_x_fx.dat"

    open(unit=10, file=filename, status="unknown", action="write")


    do i = 1, n
        call random_number(rn)
        call random_number(rm)
        r = rn
        s = rm
        rx = r*(x1-x0) + x0
        frx = f1(r*(x1-x0) + x0)
        rfx = s*(f1(x1)-f1(x0)) + f1(x0)
        if (0 <= rfx .and. rfx <= frx) then
            p = p + 1
            write(10, *) rx, rfx
        end if
    end do

    ! For increasing functions
    rsl = (x1 - x0) * f1(x1) * p / n 
    write(*, *) "Integration of f1(x) result: ", rsl

! Double integral Monte Carlo
    n = 100000
    ! Paralepipede of range
    x0 = 0.0d0
    y0 = 0.0d0
    z0 = 0.0d0 ! Integration abaove z = 0.d0
    !The paralalepipede of integration
    x1 = 1.0d0
    y1 = 1.0d0
    z1 = f2(x1,y1) !For function x,y crecents in gere extreme put a max of the funcitoin in it's region

    in_ = 0.0d0

    filename = "data/mc-doble_integration_x_y_fxy.dat"

    open(unit=20, file=filename, status="unknown", action="write")


    do i = 1, n
        call random_number(rxx)
        call random_number(ryy)
        call random_number(rzz)
        
        x = rxx * (x1 - x0) + x0
        y = ryy * (y1 - y0) + y0
        z = rzz * (z1 - z0) + z0 ! (f(x1, y1)  fmax and z0 fmin in the paralepided
        
        !Condition
        c1 = (x - 0.5d0)**2 + (y -0.5d0)**2 - 0.5d0
        
        if (c1 <= 0.d0 .and. z <= f2(x,y)) then
            in_ = in_ + 1
            write(20, *) x, y, z
        end if

    end do

    ! For increasing functions
    rsl = (x1 - x0) * (y1 - y0) * (f2(x1, y1) - z0)  * (in_ / n)
    write(*, *) "Doble Integration of the function f2(x,y) is: ", rsl

! Calculation of volumns with inequalities`

    n = 1000000
    ! Paralepipede of range to the integration
    x0 = 0.0d0
    y0 = 0.0d0
    z0 = 0.0d0

    x1 = 1.0d0
    y1 = 1.0d0
    z1 = 1.0d0

    in_ = 0.0d0

    filename = "data/mc-volumen-conditions_x_y_fxy.dat"

    open(unit=30, file=filename, status="unknown", action="write")
    
    do i = 1, n
        call random_number(rxx)
        call random_number(ryy)
        call random_number(rzz)
        
        x = rxx * (x1 - x0) + x0
        y = ryy * (y1 - y0) + y0
        z = rzz * (z1 - z0) + z0 ! (f(x1, y1)  fmax and z0 fmin in the paralepided
        
       !Condition
       c1 = x**2 + SIN(y) - z 
       c2 = x - z + EXP(y) - 1.d0
       ! We need to know waht surface is up the the other to make all rigthjjjj 
 
        if (c1 <= 0.0d0 .and. c2 <= 0.d0) then
!        if (c1 <= z .and. z <= c2) then
            in_ = in_ + 1
            write(30, *) x, y, z
        end if
    end do

    ! For increasing functions
    rsl = (x1 - x0) * (y1 - y0) * (z1 - z0) * (in_ / n)
    write(*, *) "The volumen in the conditions is: ", rsl

contains 
    real(8) function f1(x)
        real(8), intent(in) :: x
        f1 = x**2
    end function f1

    REAL(8) FUNCTION f2(x, y)
        REAL(8), INTENT(IN) :: x, y
        f2 = sqrt(sin(log(x + y + 1)))
    END FUNCTION f2
end program main_random_methods

