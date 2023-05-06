program diferencias_finitas
    real*8, allocatable :: V(:, :)


!!! Diferencia finita hacia adelante
! Ejemplo 1 
!   Sea f(x) , x=pi/2 y h = pi/6
! fk = f(xk) = f(x0 + k * h)



end program diferencias_finitas

function Delta_r(f,k)
    implicit none
    real*8 :: f
    integer :: k
    
    Delta_r = f(k + 1) - f(k) 

    do i = 1, r

    Delta_r = (-1) ** i * c(r, i)

    end do

end function

function c(r, i)
    implicit none
    integer :: r, i, l1, l2, l3
    
    do l1 = 1, r
    c = c * l1
    end do 
    do l2 = 1, i 
    c = c / l2
    end do
    do l3 = 1, (r - i)
    c = c / l3 
    end do 

end function

function f(x)
    implicit none       
    real*8 :: x 

    f = x ** 3 * cos(x)

end function
