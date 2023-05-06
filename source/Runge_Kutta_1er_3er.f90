program runge4_1_3
use ec3
implicit none
real*8::x_i,x_f,y_n,y_n1,k1,k2,k3,h,t_n,k4
integer::n,i
write(*,*)"Ingrese el intervalo de aproximación x_i<=t<=x_f"
read(*,*)x_i,x_f
write(*,*)"Ingrese la norma de la partición"
read(*,*)h
write(*,'(A,ES15.9,A)')"Ingrese la condición inicial y(",x_i,")"
read(*,*)y_n
t_n=0
n=int((x_f - x_i)/h)
do i=1,n,1
    t_n=t_n+h
    k1=h*f(t_n,y_n)
    k2=h*f(t_n+0.5*h,y_n+0.5*k1)
    k3=h*f(t_n+h*0.5,y_n+k2*0.5)
    k4=h*f(t_n+h,y_n+k3)
y_n1=y_n+(k1+2*k2+2*k3+k4)/6
write(*,"(A2,1X,I5,1X,A3,ES12.4,1X,A7,1X,ES22.16)")"n=",i,"|t=",t_n,"|I_n+1=",y_n1
y_n=y_n1
end do
end program runge4_1_3