program integracion
implicit none
integer n,i
real*8::a,b,h,rsl,y,x,f,fa,fb,x1,x0
real*8,dimension(101)::alm
write(*,*)'ingrese el intervalo'
read(*,*)a,b
x0=a ; x1=b
write(*,*)'Ingrese el valor de la particion'
read(*,*)n
h=abs((b-a)/n) !paso de integracion numerica
y=0
do i=1,n-1
a=a+h
call int(a,f)
y=y+f
end do
call int(x0,fa)
call int(x1,fb)
rsl=(h*(fa+fb)/2)+y
123 format(A,ES15.5,A,ES15.5,A,ES15.5,A)
write(*,123)'La integracion de 3x^2 desde ',x0,' hasta',x1,'es',rsl,'.'
end program integracion
subroutine int(x,f)
real(8),intent(in)::x
real(8),intent(out)::f 
f=3*x**2
return
end subroutine 



