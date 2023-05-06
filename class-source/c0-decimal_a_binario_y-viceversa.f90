!-----------------------------------------------------------------------
! Autor: Carlos Raul
! Copyright (C) 2023
! Licencia: GPL v3 o posterior
! GitHub: https://github.com/c4rlosr4ul
!-----------------------------------------------------------------------

! Programa para convertir números decimales a binarios y viceversa
program conversion
    implicit none
    ! Definir las variables
    integer*32 :: opcion, decimal_int
    real*8 :: decimal, decimal_real
    character(len=32) :: representacion_binaria
    character(len=*), allocatable  :: binario

    ! Mostrar las opciones al usuario
    write(*,*) "Elige una opcion"
    write(*,*) "1. Convertir de decimal a binario"
    write(*,*) "2. Convertir de binario a decimal"
    ! Leer la opción del usuario
    read(*,*) opcion

    ! Ejecutar la opción seleccionada
    select case (opcion)
    case (1)
        ! Caso 1: Convertir de decimal a binario
        write(*,*) "Introduzca un numero decimal:"
        read (*,*) decimal
        
        ! Comprobar si el número decimal tiene parte fraccionaria
        if (decimal - real(int(decimal), kind=4) /= 0) then
            ! Caso de números con punto flotante positivos o negativos
            call float_to_ieee754(decimal, representacion_binaria)
            print *, "La representación binaria IEEE 754 de precisión simple es: ", representacion_binaria 
        else
            ! Caso de números enteros
            decimal = int(decimal)
            call DecimalEnteroABinario(decimal)
        end if

    case (2)
        ! Caso 2: Convertir de binario a decimal
        write(*,*) "Introduzca un numero binario:"
        read(*,*) binario
        print*, binario
        ! Verificar si el número binario representa un número real en formato IEEE 754
        if (len_trim(binario) == 32) then
            call ieee754_to_float(binario, decimal_real)
            write(*,*) "El numero en decimal es (real):", decimal_real
        else
            call BinarioEnteroADecimal(binario, decimal_int)
            write(*,*) "El numero en decimal es (entero):", decimal_int
        end if

    case default
        ! Caso por defecto: Opción inválida
        write(*,*) "Opcion invalida."
    end select

    ! Subrutinas y funciones
    contains

    ! Subrutina para convertir números decimales enteros a binarios
    subroutine DecimalEnteroABinario(decimal)
        real*4, intent(in) :: decimal
        integer :: residuo, cociente, exponente, decimal_temp, decimal_int

        exponente = 0
        binario = 0
        decimal_int = int(decimal)
        decimal_temp = decimal_int

        ! Calcular la representación binaria del número entero
        do while (decimal_temp > 0)
            residuo = MOD(decimal_temp, 2)
            cociente = decimal_temp / 2
            binario = binario + (residuo * (10 ** exponente))
            exponente = exponente + 1
            decimal_temp = cociente
        end do

        ! Imprimir el resultado
        write(*,*) "El numero en binario es:", binario
    end subroutine DecimalEnteroABinario    

    ! Subrutina para convertir números decimales con punto flotante a binarios (representación IEEE 754)
    subroutine float_to_ieee754(decimal, binario)
        real, intent(in) :: decimal
        real :: decimal_temp
        character(len=32), intent(out) :: binario
        integer :: i, exponente, temp_int
        real :: mantissa, temp_decimal

        decimal_temp = decimal
        ! Determinar el bit de signo
        if (decimal < 0.0) then
            binario(1:1) = "1"
            decimal_temp = -decimal_temp
        else
            binario(1:1) = "0"
        end if

        ! Normalizar el número y encontrar el exponente
        exponente = 0
        do while (decimal_temp >= 2.0 .or. decimal_temp < 1.0)
            if (decimal_temp >= 2.0) then
                decimal_temp = decimal_temp / 2.0
                exponente = exponente + 1
            else
                decimal_temp = decimal_temp * 2.0
                exponente = exponente - 1
            end if
        end do

        ! Agregar el bias al exponente y convertirlo a binario
        exponente = exponente + 127
        binario(2:9) = decAbin(exponente, 8)

        ! Calcular la mantisa
        mantissa = decimal_temp - 1.0

        ! Convertir la mantisa a binario
        do i = 10, 32
            mantissa = mantissa * 2.0
            temp_int = int(mantissa)
            binario(i:i) = itoa(temp_int)
            mantissa = mantissa - temp_int
        end do
    end subroutine float_to_ieee754

    ! Función para convertir un número decimal a binario
    function decAbin(decimal, num_bits)
        integer, intent(in) :: decimal, num_bits
        character(len=num_bits) :: decAbin
        integer :: i, temp_decimal
        
        temp_decimal = decimal
        do i = num_bits, 1, -1
            if (mod(temp_decimal, 2) == 1) then
                decAbin(i:i) = "1"
            else
                decAbin(i:i) = "0"
            end if
            temp_decimal = temp_decimal / 2
        end do
    end function decAbin

    ! Función para convertir un entero a su representación en caracter
    function itoa(i)
        integer, intent(in) :: i
        character :: itoa

        select case (i)
            case (0)
                itoa = '0'
            case (1)
                itoa = '1'
        end select
    end function itoa

    ! Subrutina para convertir un número binario que representa un número entero a decimal
    subroutine BinarioEnteroADecimal(binario, decimal)
        character(len=*), intent(in) :: binario
        integer, intent(out) :: decimal
        integer :: digito, exponente, binario_temp

        exponente = 0
        decimal = 0
        !read(binario, *) binario_temp !Novedosa asignacion
        read(binario, '(I32)') binario_temp

        do while (binario_temp > 0)
            digito = MOD(binario_temp, 10)
            decimal = decimal + (digito * (2 ** exponente))
            exponente = exponente + 1
            binario_temp = binario_temp / 10
        end do
    end subroutine BinarioEnteroADecimal

    ! Subrutina para convertir un número binario en formato IEEE 754 de precisión simple a decimal
    subroutine ieee754_to_float(binario, decimal)
        character(len=*), intent(in) :: binario
        real*8, intent(out) :: decimal
        integer :: signo, exponente, i
        real :: mantisa, base

        if (binario(1:1) == "1") then
            signo = -1
        else
            signo = 1
        end if

        exponente = 0
        do i = 2, 9
            exponente = exponente * 2 + int(binario(i:i), kind=32) - int("0", kind=32)
        end do
        exponente = exponente - 127

        mantisa = 1.0
        base = 0.5
        do i = 10, 32
            if (binario(i:i) == "1") then
                mantisa = mantisa + base
            end if
            base = base / 2.0
        end do

        decimal = signo * mantisa * (2.0 ** exponente)
    end subroutine ieee754_to_float

end program conversion
