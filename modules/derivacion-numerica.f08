
module derivacion_numerica
    implicit none
    private
    public :: forward_difference, backward_difference, centered_difference, func_type, func_wrapper

    abstract interface
        real(8) function func_type(x)
            real(8), intent(in) :: x
        end function func_type
    end interface

    type func_wrapper
        procedure(func_type), pointer, nopass :: f
    end type func_wrapper

contains

    subroutine forward_difference(f_wrap, x, h, df)
        type(func_wrapper), intent(in) :: f_wrap
        real(8), intent(in) :: x, h
        real(8), intent(out) :: df
        df = (f_wrap%f(x + h) - f_wrap%f(x)) / h
    end subroutine forward_difference

    ! Diferencias finitas hacia atrás
    subroutine backward_difference(f, x, h, df)
        procedure(func_type), intent(in) :: f
        real(8), intent(in) :: x, h
        real(8), intent(out) :: df
        df = (f_wrap%f(x) - f_wrap%f(x - h)) / h
    end subroutine backward_difference

    ! Diferencias finitas centradas
    subroutine centered_difference(f, x, h, df)
        procedure(func_type), intent(in) :: f
        real(8), intent(in) :: x, h
        real(8), intent(out) :: df
        df = (f_wrap%f(x + h) - f_wrap%f(x - h)) / (2 * h)
    end subroutine centered_difference
end module derivacion_numerica

