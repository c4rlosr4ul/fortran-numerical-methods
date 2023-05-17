
program main_derivative
    use numerical_derivative
    implicit none
    real(8) :: x, h, df
    type(func_wrapper) :: f_wrap

    x = 1.0
    h = 0.0001

    f_wrap%f => f

    call forward_difference(f_wrap, x, h, df)
    print*, "Forward difference: ", df

    call backward_difference(f_wrap, x, h, df)
    print*, "Backward difference: ", df

    call centered_difference(f_wrap, x, h, df)
    print*, "Centered difference: ", df

contains

    function f(x)
        real(8), intent(in) :: x
        real(8) :: f
        f = x**2
    end function f

end program main_derivative

