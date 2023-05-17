program Matrices 
implicit none 
integer fil,col,i,j
real,allocatable,dimension(:,:) :: m1
real, dimension(3,3) :: A, B, C
write(*,*) 'Ingrese los elemetos de la matriz A 3x3'
read(*,*)A(1,1),A(1,2),A(1,3),A(2,1),A(2,2),A(2,3),A(3,1),A(3,2),A(3,3)
write(*,*)A(1,1),A(1,2),A(1,3)
write(*,*)A(2,1),A(2,2),A(2,3)
WRITE(*,*)A(3,1),A(3,2),A(3,3)
!Ahora que uno ingrese las dimensiones de la matriz A nxm
write(*,*)"Ingrese cuantas filas a la matriz m1"
read(*,*)i 
write(*,*)"Ingrese cuantas columnas a la matriz m1"
read(*,*)j
allocate(m1(i,j))
write(*,*)'Ingrese los elementos de la matriz m1'
read(*,*)m1
write(*,*)"La matriz de m1 es:"
write(*,*)m1

end program Matrices