PROGRAM BaseConversion
    IMPLICIT NONE
    INTEGER :: base_input, base_output, num_digits
    CHARACTER(LEN=36) :: digits
    CHARACTER(LEN=128) :: num_input, num_output

    digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    WRITE (*,*) "Ingrese el numero que desea convertir:"
    READ (*,*) num_input

    WRITE (*,*) "Ingrese la base del numero ingresado:"
    READ (*,*) base_input

    WRITE (*,*) "Ingrese la base a la que desea convertir el numero:"
    READ (*,*) base_output

    CALL ConvertBase(num_input, base_input, base_output, num_output)

    WRITE (*,*) "El numero convertido es:", num_output
        
CONTAINS

    SUBROUTINE ConvertBase(num_in, base_in, base_out, num_out)
        CHARACTER(LEN=*), INTENT(IN) :: num_in
        INTEGER, INTENT(IN) :: base_in, base_out
        CHARACTER(LEN=*), INTENT(OUT) :: num_out

        INTEGER :: decimal_val, index, power, temp_base_out
        CHARACTER(LEN=1) :: current_digit

        decimal_val = 0
        power = 0

        ! Convierte el numero de la base de entrada a base decimal
        DO index = LEN_TRIM(num_in), 1, -1
            current_digit = num_in(index:index)
            decimal_val = decimal_val + INDEX(digits, current_digit) * (base_in ** power)
            power = power + 1
        END DO

        num_out = ""
        temp_base_out = decimal_val

        ! Convierte el numero decimal a la base de salida
        IF (decimal_val == 0) THEN
            num_out = "0"
        ELSE
            DO WHILE (temp_base_out > 0)
                num_out = digits(MOD(temp_base_out, base_out):MOD(temp_base_out, base_out)) // num_out
                temp_base_out = temp_base_out / base_out
            END DO
        END IF

    END SUBROUTINE ConvertBase
END PROGRAM BaseConversion
