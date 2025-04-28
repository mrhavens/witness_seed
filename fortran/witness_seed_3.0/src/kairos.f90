module kairos
  use iso_fortran_env, only: real64
  implicit none
contains
  subroutine update_coherence(I, phase)
    real(real64), intent(inout) :: I(:)
    real(real64), intent(in) :: phase
    I = I * cos(phase)
  end subroutine
end module kairos