program integracionconsimpson1_3 ! Y posterior calculo del volumen del tanque
implicit none
real*8,allocatable::R2(:,:)
real*8::a,b,h,rsl,sumapar, sumaimp
integer::n,k,i,j,p

open(64,file="data.dat",status="old")
write(*,*)"Ingrese el numero de puntos del plano R2"
write(*,*)"Para realizar la aproximacion del area que se encuentra debajo" !Asumiendo f(x)>0 
write(*,*)"De la funcion aproximada mediante el método de simpson1_3" !Osea n=11
read(*,*)n
allocate(R2(n,2))
do k=1,n
read(64,*)R2(k,1),R2(k,2)
end do
do p=1,n
write(*,*)R2(p,1),R2(p,2)
end do
a=R2(1,1); b=R2(n,1)
h=abs((b-a)/(n-1)) !Se asume una partición constante
do i=1+1,(n-1),2
    sumaimp=sumaimp+4*R2(i,2)
    write(*,*)sumaimp
end do 
do j=2+1,(n-2),2
    sumapar=sumapar+2*R2(j,2)
    write(*,*)sumapar
end do
    rsl=(h/3)*(R2(1,2) + R2(n,2) + sumapar + sumaimp)
    write(*,*)'El area trasvensal del tanque es de aproximadamente en (mm)^2'
    write(*,*)rsl
!Conversion de mm^2 a m^2 y el calculo del volumen del tanque de combustible
rsl=rsl*1e-06 !Area en m^2
rsl=(rsl*3)*1000 !Area por la profundidad del tanque en litros
write(*,*)"Finalmente el volumen del tanque es aproximadadmente"
write(*,*)rsl," litros"
end program integracionconsimpson1_3
