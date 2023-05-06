program integracionconsimpson
use funcion
implicit none
integer n,i,j
real*8::a,b,h,rsl,x1,x0,y0,y1,nrm,sumapar, sumaimp
    write(*,*)'ingrese el intervalo'
    read(*,*)a,b
    write(*,*)'Ingrese el valor de la particion par'
    read(*,*)n
        h=(b-a)/n !paso de integracion numerica simpson 1/3
        nrm=h; sumaimp=0;sumapar=0
    do i=1,n-1,2
        sumaimp=sumaimp+4*f(a+i*h)
        write(*,*)sumaimp
    end do 
    do j=2,n-2,2
        sumapar=sumapar+2*f(a+j*h)
        write(*,*)sumapar
    end do
        rsl=(nrm/3)*(f(a)+f(b)+ sumapar +sumaimp)
    123 format(A,ES15.5,A,ES15.5,A)
    write(*,*)'La integracion de (1/sqrt(2*4*arctan(1.0))))*exp(-0.5*x**2) desde con el la regla de Simposon 1/3'
    write(*,123)'Desde ',a,' hasta',b,' es'
    write(*,*)rsl
end program integracionconsimpson
