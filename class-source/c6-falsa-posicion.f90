program falsa_posicion

!subroutine fl(xi,xf,iteraciones,xc) 

    implicit none 
    real*8 :: xi, xf, xc, d, e, x, tol, fxc
    integer :: iteraciones, j, i
    
    write(*, *) "Ingrese el intervalo de raiz xi, xf"
    read(*, *) xi, xf
    !write(*, *) "Ingrese la presicion o tolerancia de aproximacion"
    !read(*, *) tol
    
    tol = 1e-05
    fxc = 1.0 

    do while (abs(fxc) > tol)         
        
    xc = ((xi * f(xf)) - (xf * f(xi)))/(f(xf) - f(xi))
    !print *, xc, f(xc)
    
        if(f(xf) * f(xc) > 0 .and. f(xi) * f(xc) < 0)then
            xf = xc
        else if(f(xf) * f(xc) < 0 .and. f(xi) * f(xc) > 0) then
            xi = xc
        end if 
        
    fxc = f(xc)

    end do 

    write(*, *) "La raiz obteninda con el metodo de la biseccion es ", xc

contains 

! function f(x)
!     implicit none
!     real*8 :: x, f
!     f = -x ** 3 + x + 1
! end function
!! 
!
 function f(x)
     implicit none
     real*8 :: x, f
         f = tan(x) - x + 1
 end function

end program
