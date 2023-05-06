program errores_calculo

implicit none

! Declaracion de varaiables

real*8 x, seno_x, e_truncamiento, e_redondeo, seno_aprox
integer*4 i, n

! Asignacion de valores iniciales

x= 1.0d0
n= 5

! Calculo del seno de x mediante la aproximacion de series de taylor

seno_x =0.0d0

do i = 0, n
    seno_x = seno_x + (-1)**i * x**(2*i+1) / dble(product([2*i+1], dim =1))
end do

! Calculo del error de truncamiento en la aproximacion
e_truncamiento = abs(sin(x) - seno_x)
    
! Calculo del error de redondeo en la aproximacion 
seno_aprox = 0.0d0

do i = 0, n
    seno_aprox = seno_aprox + (-1)**i * x**(2*i+1) / dble(product([2*i+1], dim=1))
end do

e_redondeo = abs(seno_x - seno_aprox)

! Impresión de resultados
write(*,*) "Seno de x mediante la aproximación de series de Taylor:", seno_x
write(*,*) "Error de truncamiento en la aproximación:", e_truncamiento
write(*,*) "Error de redondeo en la aproximación:", e_redondeo

end program errores_calculo 
