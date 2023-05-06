! Interpolacion de Newton hacia adelante
program newton_method
    implicit none
    integer :: n, i, j, k, m, ierr, l
    real*8 :: Ti, s, Ri, norma
    real*8, allocatable, dimension(:) :: x, y, T
    real*8, allocatable :: F(:, :)
    character(len=30) :: filename

    filename = "data/puntos.dat"

    open(unit=15, file=filename, status="old", action="read", iostat=ierr)
    if (ierr /= 0) then
        print *, "Error al abrir el archivo."
        stop
    end if

    n = 0
    ierr = 0
    do while (ierr == 0)
        read(15, *, iostat=ierr) x, y
        if (ierr == 0) n = n + 1  
    end do

    allocate(x(n), y(n), T(n))

    rewind(15) 
        
    do i = 1, n
        read(15, *) x(i), y(i)
    end do
    close(15)

    allocate(F(n, n))

    write(*, *) "Ingrese el Ti a interpolar"
    read(*, *) Ti
    
    write(*, *) "La norma de la particion de la distribucion del dominio es"
    
    norma = abs(x(n) - x(1))/(n-1)
        
    write(*, *) norma 
    
    s = (Ti-x(1))/norma   

    do j = 1, n
        T(j) = x(j)
        F(j,1) = y(j)  
    end do

    do k = 2, n
        do m = 1, n-k+1
            F(m,k) = F(m+1,k-1) - F(m,k-1)
        end do
    end do

    Ri = F(1,1)
    do l = 2, n
        Ri = Ri + fcombinat(s,l-1)*F(1,l)
    end do
    print *, "El valor interpolado es", Ri

contains

    function factorial(n)
        implicit none
        integer :: factorial, n, i

        if (n == 1) then
            factorial = 1
        else
            factorial = 1
            do i = 1, n
                factorial = factorial * i
            end do
        end if
    end function

    function fcombinat(s,i)
        implicit none
        real*8 :: s, fcombinat
        integer :: i, k

        fcombinat = 1.0
        do k = 0, i-1
            fcombinat = fcombinat * (s-k)
        end do
        fcombinat = fcombinat / factorial(i)
    end function

end program newton_method
