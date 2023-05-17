program pt
implicit none
integer::j,rslt
write(*,*)"Ingrese cuantos impares quiere sumar"
read(*,*)j
call suma(j,rslt)
write(*,*)"La suma es: ",rslt
end program pt
subroutine suma(x,y)
implicit none
integer x,y,i,j,k
j=0
do i=1,x
k=2*i-1
j=j+k
end do
y=j
end subroutine suma





