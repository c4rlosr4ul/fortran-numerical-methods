PROGRAM main_roots

    USE equation_roots
    IMPLICIT NONE

    REAL(8) :: initial_approximation, tol, root, lower_limit, upper_limit

    initial_approximation = 8.0 
    tol = 1e-8
    
    CALL newton_method(f, df, initial_approximation, tol, root)
    WRITE(*, *) "The root found using Newton's Method is", root
   
    lower_limit = 8.0
    upper_limit = 9.5
    
    CALL bisection_method(f, lower_limit, upper_limit, tol, root)
    WRITE(*, *) "The root found using Bisection Method is", root

    CALL secant_method(f, lower_limit, upper_limit, tol, root)
    WRITE(*, *) "The root found using Secant Method is", root

CONTAINS 

    FUNCTION f(x)
        REAL(8), INTENT(IN) :: x
        REAL(8) :: f
        f = 2.0 * ATAN(1.0) * x**2 * (9.0 - x) - 30.0
    END FUNCTION f

    FUNCTION df(x)
        REAL(8), INTENT(IN) :: x
        REAL(8) :: df
        df = 2.0 * ATAN(1.0) * (18.0 * x - 3.0 * x**2)  
    END FUNCTION df

END PROGRAM main_roots
