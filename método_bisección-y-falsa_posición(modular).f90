program MBs
use biseccion
use falsa 
implicit none 
integer iteraciones,i
real*4:: xi,xf,xc,error,xcet,err
character:: r
write(*,*)'Sea x^4+3x^3-2, se calculara la raiz de este polinomio'
write (*,*)"Ingrese el intervalo de que contenga una raiz"
read(*,*)xi,xf
write (*,*) "Ingrese el metodo a utilizar"
123 continue 
write(*,*)"Digete F para el metodo de falsa posicion o digete B para el metodo de biseccion"
read(*,*)r
if(r=="f".or.r=="F") then
write(*,*)"Ingrese el numero de iteraciones"
read(*,*)iteraciones
call fl(xi,xf,iteraciones,xc)
    xcet=xc
    write (*,*) 'La raiz obtenida con el método de &
    &falsa posicion es ',xcet
else if(r=="b".or.r=="B") then
write (*,*)"Ingrese en el error de tolerancia"
read(*,*)error
call bs(xi,xf,error,xc,i)
    xcet=xc
    err=error
    write (*,*) 'La raiz obtenida con el método de bisección es ',xcet
    write (*,*)'Con un rango de error de aproxim.' ,err
    write(*,*)"El numero de iteraciones&
    & con el error brindado fue: ",i
else if(r/="f".or.r/="F".or.r/="b".or.r/="B") then
write (*,*) "Ingrese un metodo admisible "
go to 123
end if
end program MBs 