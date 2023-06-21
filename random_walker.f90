program random_walker
    implicit none
        real(8), dimension(:), allocatable :: prob, prob_pos, r_n, center
        ! 1D
        real(8) :: r_num
        integer :: x0, x1, x, c, n, n_dim, n_walk
        integer :: i, j, k
        character(len=64) :: filename_1, filename_2
        ! 2D
        INTEGER, DIMENSION(:), ALLOCATABLE :: r
        real(8), DIMENSION(:,:), ALLOCATABLE :: esp
        real(8) :: r_old
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
    close(10); close(20)

! 2 dimension random walk

    filename_1 = "data/random_walker_2D.dat"
    open(unit=30, file=filename_1, status="unknown", action="write")

    n = 10000
    n_dim = 2

    allocate(esp(n_dim, 2), prob(n_dim), r_n(n_dim), r(n_dim))
    esp = reshape([-40, -40, 40, 40], shape=[2,2])    ! x0 = -40,  y0 = -40; ; x1 = 40, y1 = 40
    prob = [0.7, 0.9] ! Probability to move left: 0.5 and to move right: (1 - 07); to move to down 0.3 and to move to up (1 -0.3)

    r = [0, 0] ! Initialize walker position

    do i = 1, n ! Number of walk simulations
        do j = 1, n_dim
            call random_number(r_n(j))
            r_old = r(j)
            if (r_n(j) < prob(j)) then
                r(j) = r(j) - 1
            else
                r(j) = r(j) + 1
            end if
             ! Ensure r is within the limits
            if (r(j) > esp(j, 2) .or. r(j) < esp(j, 1)) then
                r(j) = r_old  ! Reject the step
            end if
        end do
    write(30, *) i, r(1), r(2)
    end do

    deallocate(esp, prob, r_n, r)
    close(30) ! Close the file

! 3 dimension random walk

    filename_1 = "data/random_walker_3D.dat"
    open(unit=40, file=filename_1, status="unknown", action="write")

    n = 100000
    n_dim = 3

    allocate(esp(n_dim, 2), prob(n_dim), r_n(n_dim), r(n_dim))
    esp = reshape([-40, -40, -40, 40, 40, 40], shape=[3,2])    ! x0 = -40; y0 = -40, z0 = -40; x1 = 40, y1=40, z1 = 40

    prob = [0.5d0, 0.5d0, 0.5d0] ! Probabilities for left/right, down/up, backward/forward
    r = [0, 0, 0] ! Initialize walker position for each walk

do i = 1, n ! Number of walk simulations
    do j = 1, n_dim, 1
        r_old = r(j)  ! Remember the old position
        call random_number(r_num)  ! Generate a new random number for each dimension
        if (r_num <= prob(j)) then
            r(j) = r(j) - 1
        else
            r(j) = r(j) + 1
        end if

        ! Ensure r is within the limits
        if (r(j) > esp(j, 2) .or. r(j) < esp(j, 1)) then
            r(j) = r_old  ! Reject the step
        end if
    end do
    write(40, *) i, r(1), r(2), r(3)
end do

    close(40) ! Close the file    

end program random_walker

! Next class mean square displacement 
! To do, create a randow wlaker generalizator :P

!    center(1) = (esp(1,1) - esp(1:2)) / 2
!    center(2) = (esp(2,1) - esp(2,2)) / 2
!    allocate( prob_pos(esp(1,1) - esp(1:2), esp(2,1) - esp(2,2) )

!    print*, esp(1,1), esp(1,2)
!    print*, esp(2,1), esp(2,2)

