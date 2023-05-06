program runge2do2or
implicit none
real*8::x_i,x_f,y_n,y_n1,k1,k2,h,t_n,l1,l2,z_n,z_n1,f,g
integer::n,i
character::X
t_n=0.d0
write(*,*)"Ingrese el intervalo de aproximación x_inicial<=t<=x_final de menor a mayor"
read(*,*)x_i,x_f
101 continue
write(*,*)"Desea ingresar la norma de la particion o el numero de intervalos de partición?"
    write(*,*)"Elegir (h) o (n), donde"
    write(*,*)"h : Norma de la partición"
    write(*,*)"n : Numero de intevalos de la partición"
    read(*,*)X
    if(X=="h".or.X=="H")then
    write(*,*)"Ingrese la norma de la partición"
    read(*,*)h
    else if(X=="n".or.X=="N")then
    write(*,*)"Ingrese el numero de iteraciones"
    read(*,*)n
    h=abs((x_f- x_i)/n)
    go to 100
    else 
    write(*,*)"Ingrese una opción valida"
    go to 101
    end if
n=int((x_f - x_i)/h)
write(*,*)"-----------------------------------------------------------------------------------------"
100 continue
write(*,'(1X,A,ES15.9,A,ES15.9,A)')"Ingrese las condiciónes iniciales y(",x_i,"), y'(",x_i,")"
read(*,*)y_n,z_n
    do i=1,n,1
        t_n=t_n+h
        k1=h*f(y_n,z_n,t_n)
        l1=h*g(y_n,z_n,t_n)
        k2=h*f(y_n+k1,z_n+l1,t_n+h)
        l2=h*g(y_n+k1,z_n+l1,t_n+h)
    y_n1=y_n+(k1+k2)/2
    z_n1=z_n+(l1+l2)/2
    write(*,"(A5,1X,ES11.4,1X,A5,1X,ES11.4,1X,A5,1X,ES11.4,1X,A5,1X,ES11.4)")"\ k1=",k1,"\ l1=",l1,"\ k2=",k2,"\ l2=",l2
    write(*,"(A2,1X,I5,1X,A3,ES12.4,1X,A7,1X,ES24.16,1X,A7,1X,ES24.16)")"n=",i,"|t=",t_n,"|Y_n+1=",y_n1,"|Z_n+1=",z_n1
    write(*,*)"-----------------------------------------------------------------------------------------"
    y_n=y_n1
    z_n=z_n1
    end do
write(*,'(1X,A,ES15.9,A,ES15.9,A)')"Por lo tanto las aproximaciones de y(",x_f,"), y'(",x_f,") son respectivamente"
write(*,"(A,ES15.9,A,E24.16)")"y(",x_f,")=",y_n1
write(*,"(A,ES15.9,A,E24.16)")"y'(",x_f,")=",z_n1
end program runge2do2or
function f(y,z,t)
    implicit none
    real*8, intent(in)::y,z,t 
    real*8::f
    f=z
end function
function g(y,z,t)
    implicit none
    real*8, intent(in)::y,z,t 
    real*8::g
    g=-20*z-200*y
end function