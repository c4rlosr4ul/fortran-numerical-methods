program romberg

  ! Declaración de variables
  use funcion  ! Módulo que contiene la función a integrar
  implicit none 
  real*8 :: a, b, h, suma_i
  real*8, allocatable :: R(:,:)  ! Matriz para almacenar los resultados
  integer :: i, j, k, n
  
  ! Lectura de datos del usuario
  write(*,*) 'Integración numérica con el método de Romberg'
  write(*,*) 'Ingrese el intervalo de menor a mayor'
  read(*,*) a, b
  write(*,*) 'Ingrese J tal que 2^(J-1) sean los subintervalos'
  read(*,*) n
  
  ! Asignación dinámica de memoria a la matriz R
  allocate(R(n,n))
  
  ! Cálculo del primer elemento de la matriz usando la fórmula del trapecio
  R(1,1) = (b-a) * 0.5 * (f(a) + f(b))
  
  ! Impresión de la matriz en la primera columna
  write(*,*)"----------------------------------------------------------------"
  write(*,*) 'i = ', 1, 'j = ', 1, 'R(i,j) = ', R(1,1)
  write(*,*)"----------------------------------------------------------------"
  
  ! Cálculo de los elementos de la matriz en la primera columna usando la fórmula de Richardson
  do i = 2, n 
    suma_i = 0
    do k = 1, (2**(i-2)), 1
      suma_i = suma_i + f(a + ((b-a) / 2**(i-2)) * (k - 0.5))
    end do
    R(i,1) = 0.5 * (R(i-1,1) + ((b-a) / (2**(i-2))) * suma_i)
    
    ! Impresión de la matriz en la columna actual
    write(*,*) 'i = ', i, 'j = ', 1, 'R(i,j) = ', R(i,1)
  end do
  
  ! Cálculo de los elementos de la matriz en las columnas restantes usando la fórmula de Richardson
  do j = 2, n
    do i = j, n
      R(i,j) = ((4**(j-1)) * R(i,j-1) - R(i-1,j-1)) / (4**(j-1) - 1)
      
      ! Impresión de la matriz en la columna actual
      write(*,*) 'i = ', i, 'j = ', j, 'R(i,j) = ', R(i,j)
    end do
    
    ! Línea divisoria entre columnas
    write(*,*)"----------------------------------------------------------------"
  end do
  
  ! Impresión del valor de la aproximación más precisa de la integral
  write(*,*) 'La integral mediante el método de Romberg es ', R(n,n)
  
end program romberg






! Referencia http://www3.fi.mdp.edu.ar/metodos/apuntes/romberg_richardson.pdf
