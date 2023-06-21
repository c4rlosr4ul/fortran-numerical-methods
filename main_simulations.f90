program main_simulation
    use ode_solution
    implicit none
    
    real(8) :: prob, prob_rand, rand_temp
    integer :: die, rand_die, face
    integer :: n, i, j, k, seed, n_faces, n_cond
    real(8), dimension(:), allocatable :: p, cum_p
    integer, dimension(:), allocatable :: faces
    integer, dimension(1:6) :: results
    real(8), dimension(:), allocatable :: new_p
    character(len=32) :: file_name
    real(8) :: t0, t
    real(8), dimension(2) :: r, v, a 
    real(8), PARAMETER :: pi = 4.d0 * atan(1.d0)
    INTEGER :: error_status
    real(8) :: temp
    real(8), DIMENSION(:), ALLOCATABLE :: a_ , T2_a3

! Dice simulation
    print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

    ! Initialize the random seed
    seed = 8
    call random_seed(seed)

    ! Initialize the results to zero
    results = 0

    ! Number of die throws
    n = 1000

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
    print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    file_name = "data/s-dice_conditional.dat"
    open(unit=10, file=file_name, status="unknown", action="write")
    n = 10000    ! Number of iterations
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
    deallocate(faces, p, new_p, cum_p)

! Conditioanl dice simulation: 2nd way
    print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    file_name = "data/s-dice_conditional_v2.dat"
    open(unit=20, file=file_name, status="unknown", action="write")
    n = 10000    ! Number of simulations
    n_faces = 6     ! Number of faces on the die
    allocate(faces(n_faces), p(n_faces), new_p(n_faces), cum_p(n_faces))

    ! Set initial probabilities for each face
    faces = [1, 2, 3, 4, 5, 6]
    p = [0.182d0, 0.127d0, 0.2d0, 0.236d0, 0.151d0, 0.109d0]
    new_p = 0.0d0

    ! Normalize the probabilities
    p = p / sum(p)
!    PRINT*, p

    ! Calculate the cumulative distribution function
    cum_p(1) = p(1)
    do i = 2, n_faces
        cum_p(i) = cum_p(i-1) + p(i)
    end do
!    PRINT*, cum_p

    do i = 1, n ! n simulations
        call random_number(prob_rand)
        face = 1
        do j = 1, n_faces
            if(prob_rand <= cum_p(j)) then
                face = faces(j)
                new_p(j) = new_p(j) + 1
                exit
            end if
        end do
        write(20,*) face
    end do

    new_p = new_p / n

    ! Print face probabilities
    do i = 1, n_faces
        write(*,*) "Face probability ", i, ": ", new_p(i)
    end do

    ! Print sum of probabilities
    write(*,*) "Sum of probabilities: ", sum(new_p)

    close(20)

deallocate(faces, p, new_p, cum_p)

! Planet simulation moon respect earth
    print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    ! Moon respect earth, used astronomic units
  ! In cartesian coordinates 
    n = 10000
    t0 = 0.d0 !Anio 0
    t = 100.d0
    r = [2.d0, 0.d0]
    v = [0.d0, pi]

    call se2o_m_rk_4or(a_x, a_y, t0 , r(1), r(2), v(1), v(2), n, t, r(1), r(2), v(1), v(2))
    write(*,*) "The new position of the earth is", r
    write(*,*) "The new speed of the earth is", v

    call se2o_m_rk_2or(a_x, a_y, t0 , r(1), r(2), v(1), v(2), n, t, r(1), r(2), v(1), v(2))
    write(*,*) "The new position of the earth is", r
    write(*,*) "The new speed of the earth is", v

! Simple euler verlet simulation of the moon 
    !CALL moon_sim_ev(n, t0, t, r, v)

! Calculate of the gravitational constants

    file_name = "data/a-vs-T2_a3.dat"
    open(unit=30, file=file_name, status="old", action="read", iostat=error_status) 
    if (error_status /= 0) then
        print *, "Error opening the file for reading."
        stop
    end if

    n = 0
    error_status = 0
    do while (error_status == 0)
        read(30, *, iostat=error_status) temp
        if (error_status == 0) n = n + 1  
    end do

    allocate(a_(n), T2_a3(n))

    rewind(30)

    do i = 1, n
        read(30, *) a_(i), T2_a3(i) 
    end do

    close(30)

    open(unit=30, file=file_name, status="replace", action="write", iostat=error_status)
    if (error_status /= 0) then
        print *, "Error opening the file for writing."
        stop
    end if

    do i = 1, n
        temp = SQRT(T2_a3(i)*a_(i)**3)
!                       a, T^3 / a^3,    T,     log(a), log(T)
        write(30, *) a_(i), T2_a3(i), temp, log(a_(i)), log(temp)
        print*, log(a_(i)), log(temp)
! Recta interpolada 1.49928 x - 0.000213816
    end do

    close(30)

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

    real(8) function a_x(t, x, y, vx, vy)
        real(8), intent(in) :: t, x, y, vx, vy
        a_x = -4*pi*x/(x**2+y**2)**0.5
    end function

    real(8) function a_y(t, x, y, vx, vy)
        real(8), intent(in) :: t, x, y, vx, vy
        a_y = -4*pi*y/(x**2+y**2)**0.5
    end function

end program main_simulation
