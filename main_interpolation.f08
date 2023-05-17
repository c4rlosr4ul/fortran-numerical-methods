
program interpolation
    use numerical_interpolation
    implicit none
    integer :: n, i, error_status
    real(8), allocatable, dimension(:) :: x, y, temp
    real(8) ::  xi, yi
    character(len=32) :: filename

    filename = "data/points_pol_1_xy.dat"

    open(unit=10, file=filename, status="old", action="read", iostat=error_status)
    if (error_status /= 0) then
        print *, "Error opening the file."
        stop
    end if

    n = 0
    error_status = 0
    do while (error_status == 0)
        read(10, *, iostat=error_status) temp
        if (error_status == 0) n = n + 1  
    end do

    allocate(x(n), y(n))

    rewind(10)

    do i = 1, n
        read(10, *) x(i), y(i) 
        !print *, x(i), y(i)
    end do

    close(10)

    xi = -1.5d0

    call lagrange_interpolation(x, y, n, xi, yi)

    write(*, *) "The interpolated point with Lagrange Method", xi, yi
    
    call newton_forward_interpolation(x, y, n, xi, yi)

    write(*, *) "The interpolated point with Newton's Forward Method", xi, yi

    call newton_backward_interpolation(x, y, n, xi, yi)

    write(*, *) "The interpolated point with Newton's Backward Method", xi, yi

end program interpolation
