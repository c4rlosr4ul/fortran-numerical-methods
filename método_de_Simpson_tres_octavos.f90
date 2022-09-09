program integracion
use funcion 
implicit none 
real*8::a,x0,b,IS1,sumatoria1,norma
integer::i,n 
WRITE(*,*)'Ingrese el intervalo'
READ(*,*)a,b 
x0=a
WRITE(*,*)'Ingrese el numero de subintervalos'
READ(*,*)n
norma=((b-a)/n)
sumatoria1=0
do i=1,n-1,1
sumatoria1=sumatoria1+f(a+i*norma)
end do
IS1=norma*(f(x0)+(3*sumatoria1)+f(b))*0.375
write(*,*)'El valor de la integral es: ',IS1
end program integracion
