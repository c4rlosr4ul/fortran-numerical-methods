MODULE equation_roots

    IMPLICIT NONE
    PRIVATE
    PUBLIC :: newton_method, bisection_method, secant_method

CONTAINS

    SUBROUTINE bisection_method(f, a, c, tol, root)

        INTERFACE
            REAL(8) FUNCTION f(x)
                REAL(8), INTENT(IN) :: x
            END FUNCTION f
        END INTERFACE

        REAL(8), INTENT(IN) :: a, c, tol
        REAL(8), INTENT(OUT) :: root
        REAL(8) :: x, x_initial, x_final

        x_initial = a
        x_final = c

        DO WHILE (ABS(x_final - x_initial) > tol) 
            x = (x_initial + x_final)/2.0
            IF(f(x_initial) * f(x) > 0.0 .AND. f(x_final) * f(x) < 0.0) THEN
                x_initial = x
            ELSE IF(f(x_initial) * f(x) < 0.0 .AND. f(x_final) * f(x) > 0.0) THEN
                x_final = x
            END IF
        END DO
        
        root = x

    END SUBROUTINE bisection_method

    SUBROUTINE newton_method(func, d_func, initial_approximation, tol, root)

        INTERFACE
            REAL(8) FUNCTION func(x)
                REAL(8), INTENT(IN) :: x
            END FUNCTION func

            REAL(8) FUNCTION d_func(x)
                REAL(8), INTENT(IN) :: x
            END FUNCTION d_func
        END INTERFACE

        REAL(8), INTENT(IN) :: initial_approximation, tol
        REAL(8), INTENT(OUT) :: root
        REAL(8) :: x

        root = initial_approximation

        DO  
            IF(ABS(func(root)) < tol) EXIT
            x = root - (func(root) / d_func(root))
            root = x
        END DO

    END SUBROUTINE newton_method

    SUBROUTINE secant_method(func, initial_root, final_root, tol, root)

        INTERFACE
            REAL(8) FUNCTION func(x)
                REAL(8), INTENT(IN) :: x
            END FUNCTION func
        END INTERFACE

        REAL(8), INTENT(IN) :: initial_root, final_root, tol
        REAL(8), INTENT(OUT) :: root
        REAL(8) :: x_initial, x_final
        
        x_initial = initial_root
        x_final = final_root

        DO  
            root = x_final - ((func(x_final)) * ((x_final - x_initial) / (func(x_final) - func(x_initial))))
            IF(ABS(func(root)) < tol) EXIT
            x_initial = x_final
            x_final = root
        END DO

    END SUBROUTINE secant_method
    
END MODULE equation_roots
