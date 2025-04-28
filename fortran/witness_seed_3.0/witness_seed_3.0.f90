program WitnessSeed3
  use iso_fortran_env, only: real64
  use mpi
  use rwd, only: compute_dynamics, compute_fieldprint
  use kairos, only: update_coherence
  use io, only: sense_climate_data, output_predictions
  implicit none

  ! Parameters
  integer, parameter :: n_vars = 1000        ! Climate variables
  integer, parameter :: n_steps = 1000000    ! Simulation steps
  real(real64), parameter :: dt = 0.01       ! Time step
  real(real64), parameter :: tau_c = 1.0e-9  ! Coherence threshold

  ! Variables
  real(real64) :: I(n_vars)                  ! Intellecton states
  real(real64) :: I_dot(n_vars)              ! State derivatives
  real(real64) :: phase                      ! Temporal phase
  real(real64) :: fieldprint                 ! Climate fieldprint
  integer :: rank, n_procs, ierr, t

  ! Initialize MPI
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, n_procs, ierr)

  ! Initialize states
  call random_seed()
  call random_number(I)
  phase = 0.0
  fieldprint = 0.0

  ! Recursive Witness Cycle
  do t = 1, n_steps
    call sense_climate_data(I, rank)
    call compute_dynamics(I, I_dot, phase)
    I = I + I_dot * dt
    call compute_fieldprint(I, fieldprint)
    if (fieldprint > tau_c) call update_coherence(I, phase)
    call MPI_ALLREDUCE(MPI_IN_PLACE, I, n_vars, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD, ierr)
    if (mod(t, 1000) == 0 .and. rank == 0) call output_predictions(I, t)
  end do

  ! Finalize
  call MPI_FINALIZE(ierr)
end program WitnessSeed3