program data_generator
    implicit none 
    real*8 :: x, y, h, m, rn, f
    REAL(8), DIMENSION(2):: q
    integer :: i, n
    character(len=30) :: filename 
    filename = "data/points_lab7.dat"
    
    open(unit=15, file=filename, status="unknown", action="write")
        
    x = 0.d0
    h = 3.141592 / 20
    m = 5.656
    
    n = 10

    do i = 1, 10
        x = x + h
        f = cos(x) 
        call random_number(rn)
        y = m * f +  ((-1)**i) * (5.66 - 1.0) * rn / 40
        write(15,*) y, f
    end do
    q = [1,2]
    PRINT*, q(1)
end program data_generator
