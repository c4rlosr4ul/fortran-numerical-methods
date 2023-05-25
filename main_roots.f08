
program main_roots
    use equation_roots
    implicit none
    real(8) :: initial_approximation, tol, root, lower_limit, upper_limit

    initial_approximation = 8.5 
    tol = 1e-8
    
    call newton_method(f, df, initial_approximation, tol, root)
    write(*, *) "The root found using Newton's Method is", root
   
    lower_limit = 7.5d0
    upper_limit = 8.0d0
    
    call bisection_method(f, lower_limit, upper_limit, tol, root)
    write(*, *) "The root found using Bisection Method is", root

    call secant_method(f, lower_limit, upper_limit, tol, root)
    write(*, *) "The root found using Secant Method is", root

contains 

    function f(x)
        real(8), intent(in) :: x
        real(8) :: f
               f =  2 * atan(1) * x**2 * (9-x)
    end function

    function df(x)
        real(8), intent(in) :: x
        real(8) :: df
            df = 2 * atan(1) * (6*x - x**2)  
    end function

end program main_roots
