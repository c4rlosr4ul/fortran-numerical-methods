PROGRAM main_simulations
    IMPLICIT NONE
    INTEGER, PARAMETER :: n = 100000, n_col = 6
    INTEGER :: i, cara_aleatoria, n_condicionado
    REAL(8), DIMENSION(n_col) :: prob, cum_prob, nuevas_probabilidades
    REAL(8) :: cara_aleatoria_temp
    CHARACTER(LEN=32) :: filename

    filename = "data/s-dados_condicionado.dat"
    OPEN(UNIT=10, FILE=filename, STATUS="unknown", ACTION="write")
    
    prob = [0.2d0, 0.14d0, 0.22d0, 0.26d0, 0.17d0, 0.11d0]
    cum_prob = 0.0d0
    nuevas_probabilidades = 0.0d0

    ! Calcular la función de distribución acumulada
    cum_prob(1) = prob(1)
    DO i = 2, n_col
        cum_prob(i) = cum_prob(i-1) + prob(i)
    END DO

    n_condicionado = 0
    DO i = 1, n ! Simular n lanzamientos de dado
    CALL RANDOM_NUMBER(cara_aleatoria_temp)
    cara_aleatoria = BUSCAR_CARA(cum_prob, cara_aleatoria_temp)
        nuevas_probabilidades(cara_aleatoria) = nuevas_probabilidades(cara_aleatoria) + 1.0d0
        n_condicionado = n_condicionado + 1
        WRITE(10, *) cara_aleatoria, nuevas_probabilidades(cara_aleatoria)
    END DO

    nuevas_probabilidades = nuevas_probabilidades / REAL(n_condicionado)

    DO i = 1, n_col
        WRITE(*,*) "Probabilidad de cara ", i, ": ", nuevas_probabilidades(i)
    END DO

    WRITE(*,*) "Suma de las probabilidades: ", SUM(nuevas_probabilidades)
    
    CLOSE(10)
    
CONTAINS

    FUNCTION BUSCAR_CARA(cum_prob, aleatorio) RESULT(cara)
        IMPLICIT NONE
        REAL(8), DIMENSION(:), INTENT(IN) :: cum_prob
        REAL(8), INTENT(IN) :: aleatorio
        INTEGER :: i, cara
        
        DO i = 1, SIZE(cum_prob)
            IF (aleatorio <= cum_prob(i)) THEN
                cara = i
                RETURN
            END IF
        END DO
    END FUNCTION BUSCAR_CARA

END PROGRAM main_simulations
