program biseccion 
!all biseccion
    implicit none 
    real*8 :: a, b, c, fa, fb, fc, tol

!    a = -10.d0; c = 10.d0
    tol = 0.000001

    write(*, *) "Ingrese el intervalo de raiz a, c"
    read(*, *) a, c

    do while (abs(c - a) > tol) 
        b = (a + c)/2

!        if(f(a) * f(b) > 0 .and. f(c) * f(b) > 0) then
!            write(*, *) "No hay raiz en dicho intervalo" 
        if(f(a) * f(b) > 0 .and. f(c) * f(b) < 0) then
            a = b
        else if(f(a) * f(b) < 0 .and. f(c) * f(b) > 0) then
            c = b
        end if

    end do
    
    write(*, *) "La raiz obteninda con el metodo de la biseccion es ", b
    
contains

function f(x)
    implicit none
    real*8 :: x, f
    f = x * sin(x) - 0.1
end function

! Entre 0 y 10
end program biseccion
