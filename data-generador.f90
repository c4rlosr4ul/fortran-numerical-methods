program data_generator
    implicit none 
    real*8 :: x, y, h 
    integer :: i
    character(len=25) :: filename 
    filename = "data/points_pol_1.dat"
    
    open(unit=15, file=filename, status="new", action="write")
        
    x = -10
    h = 0.5

    do i = 1, 20
        x = x + h
        y = x ** 3 + x ** 2 + x + 1
        write(15,*) x, y
    end do


end program data_generator
