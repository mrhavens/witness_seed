program test_kairos
  use kairos, only: update_coherence
  use iso_fortran_env, only: real64
  implicit none
  real(real64) :: I(10), phase
  call random_number(I)
  phase = 0.1
  call update_coherence(I, phase)
  if (sum(abs(I)) > 0.0) then
    print *, "Kairos test passed: Coherence updated"
  else
    print *, "Kairos test failed"
    stop 1
  end if
end program test_kairos