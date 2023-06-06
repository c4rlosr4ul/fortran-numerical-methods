program main_random_methods
    implicit none
    real(8) :: x, y, pi, x0, x1, p, q, r, s, rsl, yr, ry
    real :: rn, rm
    integer :: i, n, countInside

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
    n = 10000000
    p = 0.0d0
    q = 0.0d0
    do i = 1, n
        call random_number(rn)
        call random_number(rm)
        r = rn
        s = rm
        yr = f(r*(x1-x0) + x0)
        ry = s*(f(x1)-f(x0)) + f(x0)
        if (0 <= yr .and. yr <= ry) then
            p = p + 1
        else 
            q = q + 1
        end if
    end do

    ! For increasing functions
    rsl = (x1 - x0) * f(x1) / (1 + q/p) 
    write(*, *) "Integration result: ", rsl

contains 
    function f(x)
        real(8), intent(in) :: x
        real(8) :: f
        f = x**2
    end function f
end program main_random_methods

