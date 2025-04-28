program test_rwd
  use rwd, only: compute_dynamics, compute_fieldprint
  use iso_fortran_env, only: real64
  implicit none
  real(real64) :: I(10), I_dot(10), phase, fieldprint
  call random_number(I)
  phase = 0.0
  call compute_dynamics(I, I_dot, phase)
  call compute_fieldprint(I, fieldprint)
  if (fieldprint > 0.0) then
    print *, "RWD test passed: Fieldprint =", fieldprint
  else
    print *, "RWD test failed"
    stop 1
  end if
end program test_rwd