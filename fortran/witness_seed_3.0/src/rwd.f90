module rwd
  use iso_fortran_env, only: real64
  implicit none
  real(real64), parameter :: omega = 1.0  ! Base frequency
  real(real64), parameter :: K = 0.1      ! Coupling strength
contains
  subroutine compute_dynamics(I, I_dot, phase)
    real(real64), intent(in) :: I(:)
    real(real64), intent(out) :: I_dot(:)
    real(real64), intent(inout) :: phase
    integer :: i, j
    do i = 1, size(I)
      I_dot(i) = omega * I(i)
      do j = 1, size(I)
        I_dot(i) = I_dot(i) + K * sin(I(j) - I(i))
      end do
    end do
    phase = phase + dt * sum(sin(I))
  end subroutine

  subroutine compute_fieldprint(I, fieldprint)
    real(real64), intent(in) :: I(:)
    real(real64), intent(out) :: fieldprint
    fieldprint = sum(abs(I)) / size(I)
  end subroutine
end module rwd