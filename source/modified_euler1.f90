program modificado1
use funcion3
implicit none 
real*8 ::x__1,x_0,y__1,y_0,y_1,h,t,t_k
write(*,*)"Sea y'=-y**1.5 +0*x+ 1 "
write(*,*)"Ingrese la condicion inicial y(0)=y(t_0)"
read(*,*)y_0
write(*,*)"Ingrese la norma h "
read(*,*)h
write(*,*)"Ingrese t_k tal que y(t_k)=y_k se intenta aproximar mediante el método de euler modificado"
read(*,*)t_k
t=0; x_0=0; y__1=y_0
write(*,'(A2,ES15.5,1X,A7,ES20.10)')"t=",0.d0,"| y_x_t=",y_0
do while (t+h<=t_k)
t=t+h
    do 
    y_1=y_0+(f(x_0,y_0)+f(x__1,y__1))*h*0.5
    if (abs(y__1-y_1)<=1.0e-03)exit !Corrigiendo aproximación
    y__1=y_1
    x__1=x__1+h 
    end do
x_0=x__1
y_0=y_1
write(*,'(A2,ES15.5,1X,A7,ES20.10)')"t=",t,"| y_x_t=",y_1
end do
write(*,'(A,ES11.5,A,ES22.16)')"Por lo tanto el valor aproximado de x(",t_k,")=",y_0
end program modificado1
