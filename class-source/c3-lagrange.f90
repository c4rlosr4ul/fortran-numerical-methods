program interpolacion_lagrange 
    implicit none
    integer :: n, i, ierr
    real*8, allocatable, dimension(:) :: x, y, xi, yi
    character(len=25) :: filename

    ! Nombre del archivo con los puntos a interpolar
    filename = "data/points_cos_xy.dat"

    ! Leer el número de filas (puntos) en el archivo
    open(unit=10, file=filename, status="old", action="read", iostat=ierr)
    if (ierr /= 0) then
        print *, "Error al abrir el archivo."
        stop
    end if

    n = 0
    ierr = 0
    do while (ierr == 0)
        read(10, *, iostat=ierr) xi
        if (ierr == 0) n = n + 1  ! El valor de ierr es 0, eso significa que se leyó exitosamente una línea
    end do
    ! Cuando no haya más filas con valores en el archivo, la operación de lectura producirá un valor diferente de 0 para iostat, lo que hará que el bucle termine.

    ! Asignar memoria a los arrays x e y
    allocate(x(n), y(n))

    ! Leer los datos del archivo
    rewind(10) !Este comando rebobina el archivo asociado con la unidad 10 (en este caso, "puntos.dat") al principio del archivo. De esta forma, podemos leer los datos de los puntos desde el inicio del archivo.

    do i = 1, n
        read(10, *) x(i), y(i)
    end do
    close(10)

    ! Realizar la interpolación de Lagrange y mostrar el resultado
    call lagrange_interpolation(x, y, n)

contains

    subroutine lagrange_interpolation(x, y, n)
        real*8, dimension(:), intent(in) :: x, y
        integer, intent(in) :: n
        integer :: i, j
        real*8 :: xi, yi, Li

        print *, "Ingrese el x a interpolar (x, f(x))" 
        read(*,*) xi
        print *, "Resultado de la interpolacion de Lagrange al considerar", n, "puntos es:"
        
            yi = 0.0
            do i = 1, n
                Li = 1.0
                do j = 1, n
                    if (i /= j) then 
                    Li = Li * (xi - x(j)) / (x(i) - x(j))
                    end if
                end do
                yi = yi + y(i) * Li
            end do
            !print '(1A,F10.5,1A,F10.5,1A)', "(", xi,",",  yi, ")"
            print *, xi, yi

    end subroutine lagrange_interpolation

end program interpolacion_lagrange
