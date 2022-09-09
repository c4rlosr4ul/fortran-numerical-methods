program E
! Declarar variables.
implicit none
real*8 h, m, g
character:: r
!Asignaci�n de variables
g=9.81



DO
write(*,*)"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
write(*,*)"Calculadora de energia potencial"
write(*,*)"Todo valor ingresado debe ser adimendional, se tomaran en unidades del S.I"
write(*,*)"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"


do 
	write(*,*) "Digite la altura"
	read(*,*) h
	if(h>0)exit
	write(*,*) "Ingrese una altura valida"
end  do
do
	write(*,*) "Digite la masa"
	read(*,*) m
	if(m>0)exit
	write(*,*) "Ingrese una masa valida"


  end do
	write(*,*) "El valor de la energia potencial gravitatoria", &
&" de su sistema es igual", h*m*g ," Julios"


write(*,*) "Quiere usar otra vez la calculadora?,Escriba S para continuar"
read(*,*) r

If(r/="S".and.r/="s")stop
END DO



  end program E

