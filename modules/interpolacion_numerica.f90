module interpolacion_numerica
    implicit none
    private 
    public  :: i_lagrange, i_newton_ad, i_newton_at

contains
    
    subroutine i_lagrange(x, y, n, xi, yi)
    
        real(8), dimension(:), intent(in) :: x, y 
        real(8), intent(in) :: xi
        integer, intent(in) :: n
        real(8), intent(out) :: yi
        real(8) ::  sum,  f
        integer :: i, j

         do i = 1, n
            f = 1.0d0
            do j = 1, n
              if (i /= j) then
              f = f * (xi -x(j))/(x(i)-x(j))
              end if
            end do
            yi = yi + f * y(i)
         end do

    end subroutine 


    subroutine i_newton_ad(x, y, n, xi, yi)

        real(8), dimension(:), intent(in) :: x, y 
        real(8), intent(in) :: xi
        integer, intent(in) :: n
        real(8), intent(out) :: yi
        real(8), allocatable, dimension(:, :) :: F
        real(8) ::  s, norma
        integer :: j, k, m, l

        norma = abs(x(n) - x(1))/(n-1)

        s = (xi-x(1))/norma   
        
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
            yi = yi + fcombinat(s,l-1) * F(1,l)
        end do    

    end subroutine i_newton_ad

    subroutine i_newton_at(x, y, n, xi, yi)

    real(8), dimension(:), intent(in) :: x, y 
    real(8), intent(in) :: xi
    integer, intent(in) :: n
    real(8), intent(out) :: yi
    real(8), allocatable, dimension(:, :) :: F
    real(8) ::  s, norma
    integer :: i, j, k, m, l

        norma = abs(x(n) - x(1))/(n-1)

        s = (x(n) - xi)/norma   
        
        allocate(F(n, n))
         
        do j = 1, n
            F(j, 1) = y(j)  
        end do 

        do j = 2, n
            do i = j, n
                F(i,j) = F(i,j-1) - F(i-1,j-1)
            end do
        end do

        yi = F(n, 1)

        do k = 2, n
            yi = yi + fcombinat(s, k) * F(n, k)
        end do

    end subroutine i_newton_at

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

    function fcombinat(s,i)
        implicit none
        real(8) :: s, fcombinat
        integer :: i, k
        fcombinat = 1.0d0
        do k = 0, i-1
         fcombinat = fcombinat * (s-k)
        end do
        fcombinat = fcombinat/factorial(i)
     end function

end module interpolacion_numerica

