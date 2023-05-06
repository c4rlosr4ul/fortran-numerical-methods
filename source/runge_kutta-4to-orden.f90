program runge4
implicit none 
real*8::x_i,x_f,y_n,y_n1,k1,k2,k3,h,t_n,k4,f
integer::n,i,j,k
CHARACTER(LEN=92) :: fmt
fmt="(A2,1X,I5,1X,A3,ES12.4,1X,A4,1X,ES12.4,1X,A4,ES12.4,1X,A4,ES12.4,1X,A4,ES12.4,1X,A3,ES22.16)"

t_n=0; h=0.1; x_i=0; x_f=1.0; y_n=10
n=int((x_f - x_i)/h)
write(*,*)"Para h=0.1"
do i=1,n+1,1
    t_n=t_n+h
    k1=h*f(t_n,y_n)
    k2=h*f(t_n+0.5*h,y_n+0.5*k1)
    k3=h*f(t_n+h*0.5,y_n+k2*0.5)
    k4=h*f(t_n+h,y_n+k3)
y_n1=y_n+(k1+2*k2+2*k3+k4)/6
write(*,fmt)"n=",i,"|t=",t_n,"|k1=",k1,"|k2=",k2,"|k3=",k3,"|k4=",k4,"|y=",y_n1
y_n=y_n1
end do
!---
t_n=0; h=0.05; x_i=0.d0; x_f=1.0; y_n=1
n=int((x_f - x_i)/h)
write(*,*)"Para h=0.05"
do i=1,n+1,1
    t_n=t_n+h
    k1=h*f(t_n,y_n)
    k2=h*f(t_n+0.5*h,y_n+0.5*k1)
    k3=h*f(t_n+h*0.5,y_n+k2*0.5)
    k4=h*f(t_n+h,y_n+k3)
y_n1=y_n+(k1+2*k2+2*k3+k4)/6
write(*,fmt)"n=",i,"|t=",t_n,"|k1=",k1,"|k2=",k2,"|k3=",k3,"|k4=",k4,"|y=",y_n1
y_n=y_n1
end do
!----
t_n=0; h=0.025; x_i=0; x_f=1.0; y_n=1
n=int((x_f - x_i)/h)
write(*,*)"Para h=0.0.25"
do i=1,n+1,1
    t_n=t_n+h
    k1=h*f(t_n,y_n)
    k2=h*f(t_n+0.5*h,y_n+0.5*k1)
    k3=h*f(t_n+h*0.5,y_n+k2*0.5)
    k4=h*f(t_n+h,y_n+k3)
y_n1=y_n+(k1+2*k2+2*k3+k4)/6
write(*,fmt)"n=",i,"|t=",t_n,"|k1=",k1,"|k2=",k2,"|k3=",k3,"|k4=",k4,"|y=",y_n1
y_n=y_n1
end do
end program runge4

function f(t,x)
    implicit none
    real*8,intent(in)::x,t
    real*8::f
    f=(5*t**2-x)/exp(x+t)
end function   