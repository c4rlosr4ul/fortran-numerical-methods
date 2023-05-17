
module numerical_interpolation
    implicit none
    private 
    public  :: lagrange_interpolation, newton_forward_interpolation, newton_backward_interpolation

contains
    
    subroutine lagrange_interpolation(x, y, n, xi, yi)
    
        real(8), dimension(:), intent(in) :: x, y 
        real(8), intent(in) :: xi
        integer, intent(in) :: n
        real(8), intent(out) :: yi
        real(8) ::  sum,  product
        integer :: i, j

         do i = 1, n
            product = 1.0d0
            do j = 1, n
              if (i /= j) then
              product = product * (xi -x(j))/(x(i)-x(j))
              end if
            end do
            yi = yi + product * y(i)
         end do

    end subroutine 


    subroutine newton_forward_interpolation(x, y, n, xi, yi)

        real(8), dimension(:), intent(in) :: x, y 
        real(8), intent(in) :: xi
        integer, intent(in) :: n
        real(8), intent(out) :: yi
        real(8), allocatable, dimension(:, :) :: F
        real(8) ::  s, norm
        integer :: j, k, m, l

        norm = abs(x(n) - x(1))/(n-1)

        s = (xi-x(1))/norm   
        
        allocate(F(n, n))

        do j = 1, n
            F(j, 1) = y(j)  
        end do

        do k = 2, n
            do m = 1, n-k+1
                F(m,k) = F(m+1,k-1) - F(m,k-1)
            end do
        end do

        yi = F(1,1)

        do l = 2, n
            yi = yi + combination(s,l-1) * F(1,l)
        end do    

    end subroutine newton_forward_interpolation

    subroutine newton_backward_interpolation(x, y, n, xi, yi)

        real(8), dimension(:), intent(in) :: x, y 
        real(8), intent(in) :: xi
        integer, intent(in) :: n
        real(8), intent(out) :: yi
        real(8), allocatable, dimension(:, :) :: F
        real(8) ::  s, norm
        integer :: i, j, k, m, l

        norm = abs(x(n) - x(1))/(n-1)

        s = (xi - x(n))/norm   
            
        allocate(F(n, n))
             
        do j = 1, n
            F(j, 1) = y(j)  
        end do 

        do j = 2, n
            do i = 1, n-j+1
                F(i,j) = F(i+1,j-1) - F(i,j-1)
            end do
        end do

        yi = F(1, n)

        do k = 1, n-1
            yi = yi + combination(s,k) * F(1, n-k)
        end do

    end subroutine newton_backward_interpolation

    function factorial(n)
    implicit none
    integer :: factorial ,n ,i
        if (n==1) then
            factorial = 1
        else
            factorial = 1
            do i= 1, n
            factorial = factorial * i
            end do
        end if
    end function

    function combination(s,i)
        implicit none
        real(8) :: s, combination
        integer :: i, k
        combination = 1.0d0
        do k = 0, i-1
         combination = combination * (s-k)
        end do
        combination = combination/factorial(i)
     end function

end module numerical_interpolation
