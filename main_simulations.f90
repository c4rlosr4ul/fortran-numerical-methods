program main_simulation
    implicit none
    
    real(8) :: prob, prob_rand, rand_temp
    integer :: die, rand_die
    integer :: n, i, j, k, seed, n_faces, n_cond
    real(8), dimension(:), allocatable :: p, faces, cum_p
    integer, dimension(1:6) :: results
    real(8), dimension(:), allocatable :: new_p
    character(len=32) :: file_name

    ! Initialize the random seed
    seed = 8
    call random_seed(seed)

    ! Initialize the results to zero
    results = 0

    ! Number of die throws
    n = 10000

    ! Simulate 10000 die throws
    do i = 1, n
        call roll_die(die)
        results(die) = results(die) + 1
    end do

    ! Print the results
    print *, "Results of rolling a die 10000 times:"
    do i = 1, 6
        prob = 1.0d0 * results(i) / n
        print *, "Face ", i, ": ", results(i), "Probability", prob
    end do

! Conditional dice simulation
    file_name = "data/s-dice_conditional.dat"
    open(unit=10, file=file_name, status="unknown", action="write")
    n = 100000     ! Number of iterations
    n_cond = 0     ! Number of conditional dice throw simulations
    n_faces = 6     ! Number of faces on the die
    allocate(faces(n_faces), p(n_faces), new_p(n_faces), cum_p(n_faces))

    ! Set initial probabilities for each face
    p = [0.2d0, 0.14d0, 0.22d0, 0.26d0, 0.17d0, 0.11d0]
    cum_p = 0.0d0
    new_p = 0.0d0

    ! Calculate the cumulative distribution function
    cum_p(1) = p(1)
    do i = 2, n_faces
        cum_p(i) = cum_p(i-1) + p(i)
    end do

    ! Initialize the number of conditional simulations
    n_cond = 0
    do i = 1, n
        call random_number(rand_temp)
        rand_die = find_face(cum_p, rand_temp)
        new_p(rand_die) = new_p(rand_die) + 1.0d0
        n_cond = n_cond + 1
        write(10, *) rand_die, new_p(rand_die)
    end do

    ! Calculate new probabilities
    new_p = new_p / real(n_cond)

    ! Print face probabilities
    do i = 1, n_faces
        write(*,*) "Face probability ", i, ": ", new_p(i)
    end do

    ! Print sum of probabilities
    write(*,*) "Sum of probabilities: ", sum(new_p)
    
    close(10)
    deallocate(faces, p, new_p)

contains

    ! Subroutine to simulate a dice roll
    subroutine roll_die(result)
        implicit none
        real :: rand
        integer, intent(out) :: result

        call random_number(rand)
        result = int(rand * 6) + 1
    end subroutine roll_die

    ! Function to find the face based on random number and cumulative probability
    function find_face(cum_p, rand) result(face)
        implicit none
        real(8), dimension(:), intent(in) :: cum_p
        real(8), intent(in) :: rand
        integer :: i, face
        
        do i = 1, size(cum_p)
            if (rand <= cum_p(i)) then
                face = i
                return
            end if
        end do
    end function find_face

end program main_simulation
