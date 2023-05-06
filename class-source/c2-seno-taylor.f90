program taylor_series_of_sin
    implicit none
    integer :: n, i, factorial
    real*8 :: x, sum, term, taylor_sin

    ! Leer el punto x y el grado n de la expansión
    write(*,*) "Se aproximara el seno(x) mediante serie de taylor hasta n terminos"
    write(*,*) "Ingrese el valor de x:"
    read(*,*) x
    write(*,*) "Ingrese el grado n de la expansion:"
    read(*,*) n

    sum = 0.0

    ! Bucle para calcular la expansión de la serie de Taylor
    do i = 0, n
        term = ((-1)**i) * (x**(2*i+1)) / factorial(2*i+1)
        sum = sum + term
    end do

    taylor_sin = sum

    ! Mostrar el resultado
    write(*,*) "La expansion de la serie de Taylor del seno hasta el grado", n, "es:", taylor_sin

   end program taylor_series_of_sin

     ! Subrutina para calcular el factorial de un número
    integer function factorial(n) 
    implicit none

        integer, intent(in) :: n
        integer :: m, i, fact
        m = n
        fact = 1
        do i = 1, m
            fact = fact * i
        end do
    factorial = fact
    end function factorial


