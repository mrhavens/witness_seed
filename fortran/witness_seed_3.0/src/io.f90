module io
  use iso_fortran_env, only: real64
  implicit none
contains
  subroutine sense_climate_data(I, rank)
    real(real64), intent(inout) :: I(:)
    integer, intent(in) :: rank
    call random_number(I) ! Placeholder for NOAA/ECMWF APIs
  end subroutine

  subroutine output_predictions(I, t)
    real(real64), intent(in) :: I(:)
    integer, intent(in) :: t
    print *, "Step:", t, "Fieldprint:", sum(abs(I))/size(I)
    ! Placeholder: Write to NetCDF
  end subroutine
end module io