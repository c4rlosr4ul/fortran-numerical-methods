program g 
implicit none 
!real*8
integer x,y,z
!Primero crear{e una cadena de datos
!Fibonacci 1
x=0
y=1
open(98,file='Datos1.dat', status='unknown')
!open(99,file='Datos2.dat', status='unknown')
!open(100,file='Datos3.dat', status='unknown')
write(*,*) x
write(98,*) x
123 continue

z=x+y
x=z+y
y=x+z
write(98,*)z
write(98,*)x
write(98,*)y
write(*,*)z
write(*,*)x
write(*,*)y
if(y<1000000000) goto 123

end program g