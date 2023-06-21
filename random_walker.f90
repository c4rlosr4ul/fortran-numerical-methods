program random_walker
    implicit none
        real(8), dimension(:), allocatable :: prob, prob_pos, r_n, center, r
        ! 1D
        real(8) :: r_num
        integer :: x0, x1, x, c, n, n_dim, n_walk
        integer :: i, j, k
        character(len=64) :: filename_1, filename_2
        ! 2D
        real(8), DIMENSION(:,:), ALLOCATABLE :: esp
        integer :: y0, y1 


        filename_1 = "data/random_walker.dat"
        filename_2 = "data/random_walker_distribution.dat"

        open(unit=10, file=filename_1, status="unknown", action="write")
        open(unit=20, file=filename_2, status="unknown", action="write")

    n = 1000
    x0 = -40; x1 = 40
    n_dim = 1
    n_walk = x1 - x0
    allocate(prob(n_dim), prob_pos(n_walk))

    prob = 0.50d0 ! Probabilitie to up move left  0.5 and to right (1 - 0.5)
    x = 0
    do i = 1, n ! Number of walk simulations
        call random_number(r_num)
        c = (x1 - x0) / 2
            if (r_num < prob(1)) then
                x = x - 1
            else if (r_num > prob(1)) then
                x = x + 1
            end if
            prob_pos(x + c) = prob_pos(x + c) + 1

            if (x >= x1) then
                x = x - 1
            else if (x <= x0) then
                x = x + 1
            end if
        write(10, *) i, x 
    end do
    
        prob_pos = prob_pos / real(n)

    do j = x0, x1, 1
        write(20, *) j, prob_pos(c + j)
    end do

    deallocate(prob, prob_pos)

! 2 dimension random walk
    filename_1 = "data/random_walker_2D.dat"
    open(unit=30, file=filename_1, status="unknown", action="write")

    n = 10000
    n_dim = 2

    allocate(esp(n_dim, n_dim), prob(n_dim), r_n(n_dim), r(n_dim))
    esp = reshape([-40, 40, -40, 40], shape=[2,2])    ! x0 = -40; x1 = 40 & y0 = -40; y1 = 40

    prob = [0.5, 0.5] ! Probability to move left: 0.5 and to move right: (1 - 0.5)

    do i = 1, n ! Number of walk simulations
        do j = 1, n_dim
            call random_number(r_n(j))
            if (r_n(j) < prob(j)) then
                r(j) = r(j) - 1
            else if (r_n(j) > prob(j)) then
                r(j) = r(j) + 1
            end if

            if (r(j) >= esp(j, 2)) then
                r(j) = r(j) - 1
            else if (r(j) <= esp(j, 1)) then
                r(j) = r(j) + 1
            end if
        end do
        write(30, *) i, r(1), r(2)
    end do


end program random_walker

! Next class mean square displacement 
! To do, create a randow wlaker generalizator :P

!    center(1) = (esp(1,1) - esp(1:2)) / 2
!    center(2) = (esp(2,1) - esp(2,2)) / 2
!    allocate( prob_pos(esp(1,1) - esp(1:2), esp(2,1) - esp(2,2) )


