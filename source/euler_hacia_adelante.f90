program adelante 
use funcion2
implicit none 
real*8::h,t,t_,y,y_1,y_2
integer::paso
write(*,*)"Ingrese la norma "
read(*,*)h
write(*,*)"Ingrese las conciones iniciales y'(0)=y'(t_0) & y(0)=y(t_0) respectivamente"
read(*,*)y_1,y
write(*,*)"Ingrese l tal que t es menor igual que l"
read(*,*)t_
t=0; paso=0
do while (t<=t_) 
paso=paso+1
t=t+h
y_2=f(y,y_1)
y=y+h*y_1
y_1=y_1+h*y_2
write(*,'(A4,I4,1X,A4,ES15.4,1X,A3,ES20.10,1X,A5,ES20.10,1X,A5,ES20.10)')"Paso",paso,"|t_=",t,"|y=",y,"|y_1=",y_1,"|y_2=",y_2  
end do
end program adelante  
 