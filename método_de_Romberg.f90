program romberg
use funcion
implicit none 
real*8::a,b,h,suma_i
real*8, allocatable::R(:,:)
integer::i,j,k,n
write(*,*)'Integracion de numerica con el metodo de Romberg'
write(*,*)'ingrese el intervalo de menor a mayor'
    read(*,*)a,b
write(*,*)'Ingrese J tal que 2^(J-1) sean los subintervalos'
read(*,*)n
allocate(R(n,n))
R(1,1)=(b-a)*0.5*(f(a)+f(b))
write(*,*)"----------------------------------------------------------------"
write(*,*)'i=',1,"j=",1,'R(i,j)=',R(1,1)
write(*,*)"----------------------------------------------------------------"
do i=2,n 
    suma_i=0
    do k=1,(2**(i-2)),1
    suma_i=suma_i+f(a+((b-a)/2**(i-2))*(k -0.5))
    end do
    R(i,1)=0.5*(R(i-1,1)+(((b-a)/(2**(i-2)))*suma_i))
    write(*,*)'i=',i,"j=",1,'R(i,j)=',R(i,1)
end do
write(*,*)"----------------------------------------------------------------"
do j=2,n
    do i=j,n
        R(i,j)=((4**(j-1))*R(i,j-1)-R(i-1,j-1))/(4**(j-1)-1)
        write(*,*)"i=",i,'j=',j,"R(i,j)=",R(i,j)
    end do
write(*,*)"----------------------------------------------------------------"
end do
write(*,*)'La integral mediante el método de Romberg es',R(n,n)
end program romberg





! Referencia http://www3.fi.mdp.edu.ar/metodos/apuntes/romberg_richardson.pdf