program interpolacion
use interpolacion_numerica
   implicit none
    integer :: n, i, ierr
    real(8), allocatable, dimension(:) :: x, y, temp
    real(8) ::  xi, yi
    character(len=25) :: filename

    filename = "data/points_pol_1.dat"

    open(unit=10, file=filename, status="old", action="read", iostat=ierr)
    if (ierr /= 0) then
        print *, "Error al abrir el archivo."
        stop
    end if

    n = 0
    ierr = 0
    do while (ierr == 0)
        read(10, *, iostat=ierr) temp
        if (ierr == 0) n = n + 1  
    end do

    allocate(x(n), y(n))

    rewind(10)

    do i = 1, n
        read(10, *) x(i), y(i) 
        !print *, x(i), y(i)
    end do

    close(10)

    xi = -1.5d0

    call i_lagrange(x, y, n, xi, yi)

    write(*, *) "El punto interpolado con M. Lagrange", xi, yi
    
    call i_newton_ad(x, y, n, xi, yi)

    write(*, *) "El punto interpolado con M. Newton adelante", xi, yi

    call i_newton_at(x, y, n, xi, yi)

    write(*, *) "El punto interpolado con M. Newton atras", xi, yi

end program interpolacion
