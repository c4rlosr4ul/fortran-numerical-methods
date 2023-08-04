
program IsingModel
    implicit none
    integer, dimension(:,:),allocatable :: lattice 
    integer, dimension(:), allocatable :: pbc ! Boundary conditions
    integer :: N, i, j, steps, max_steps, equil_steps
    real(8) :: T, K_B, delta_E, r, E, m, E_total, m_total
    character(len=32) :: filename

    filename = "data/ising_model_2D.dat"
    open(unit=10, file=filename, status="unknown", action="write")

    ! Ising model 2D

    N = 30
    allocate(lattice(n, n)) 

    call random_seed()
    do i = 1, N
        do j = 1, N
            lattice(i,j) = 2*int(2.0*rand()) - 1
        end do
    end do

    T = 2.0
    ! condiciones iniciales ne los bordes para poder consertrace solo en la parte interna
    ! Subsistema lattitance S_I; H = \sum_{<i,j>}JS_{(i)}S_{j}, Sea E_S_i = 4J y E_S_f = -4J, entonces \delta E =  8J, si 
    !Notas: Como J = 1 se puede ignorar; Lo que interesa \delta E = -2S[S(pbc(x+1)))] .. 
    ! se hace simplemente crear las condiciones de las fronter
    ! Equilibration steps

    equil_steps = 5000
    do steps = 1, equil_steps
        do i = 1, N
            do j = 1, N
                delta_E = 2.0 * lattice(i,j) * (lattice(mod(i,N)+1,j) + lattice(i,mod(j,N)+1))
                if (delta_E < 0.0 .or. exp(-delta_E/T) > rand()) then ! Maybe exp(-\delta_E/K_B*T?)
                    lattice(i,j) = -lattice(i,j)
                end if
            end do
        end do
    end do

    ! Calculate and record observables
    E = 0; m = 0
        do i = 1, N
            do j = 1, N
                E = -lattice(i,j) * lattice(j,i) + E
                m = lattice(i,j) + m
            end do
        end do
    end do

    ! Print the final state of the lattice
    do i = 1, N
        write(10,*) (lattice(i,j), j=1,N)
    end do

    ! Print the average observables
    write(*, *) E/max_steps, m/max_steps

end program IsingModel

function rand()
    implicit none
    real(8) :: rand
    call random_number(rand)
end function rand

