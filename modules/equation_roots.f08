
module equation_roots

    implicit none
    private
    public :: newton_method, bisection_method, secant_method

contains

    subroutine bisection_method(f, a, c, tol, root)

        interface
            real(8) function f(x)
                real(8), intent(in) :: x
            end function f
        end interface

        real(8), intent(in) :: a, c, tol
        real(8), intent(out) :: root
        real(8) :: x, x_initial, x_final

        x_initial = a; x_final = c

        do while (abs(x_final - x_initial) > tol) 
            x = (x_initial + x_final)/2
                if(f(x_initial) * f(x) > 0 .and. f(x_final) * f(x) < 0) then
                    x_initial = x
                else if(f(x_initial) * f(x) < 0 .and. f(x_final) * f(x) > 0) then
                    x_final = x
                end if
        end do
        
        root = x

    end subroutine bisection_method

    subroutine newton_method(func, d_func, initial_approximation, tol, root)

        interface
            real(8) function func(x)
                real(8), intent(in) :: x
            end function func

            real(8) function d_func(x)
                real(8), intent(in) :: x
            end function d_func
        end interface

        real(8), intent(in) :: initial_approximation, tol
        real(8), intent(out) :: root
        real(8) :: x

        root = initial_approximation

        do  
            if(abs(func(root)) < tol) exit
            x = root - (func(root) / d_func(root))
            root = x
        end do

    end subroutine newton_method

    subroutine secant_method(func, initial_root, final_root, tol, root)

        interface
            real(8) function func(x)
                real(8), intent(in) :: x
            end function func
        end interface

        real(8), intent(in) :: initial_root, final_root, tol
        real(8), intent(out) :: root
        real(8) :: x_initial, x_final
        
        x_initial = initial_root
        x_final = final_root

        do  
            root = x_final - ((func(x_final)) * ((x_final - x_initial) / (func(x_final) - func(x_initial))))
            if(abs(func(root)) < tol) exit
            x_initial = x_final
            x_final = root
        end do

    end subroutine secant_method
    
end module equation_roots
