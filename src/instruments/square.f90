module square_instrument
!==============================================================================#
! SQUARE_INSTRUMENT
!------------------------------------------------------------------------------#
! Author:  Edward Higgins <ed.higgins@york.ac.uk>
!------------------------------------------------------------------------------#
! Version: 0.1.1, 2025-01-02
!------------------------------------------------------------------------------#
! This code is distributed under the MIT license.
!==============================================================================#
  use instruments
  use constants
  use parameters
  use notes
  implicit none

  private
  type, public, extends(instrument_t) :: square_instrument_t
  contains
    procedure :: play => square_play
  end type square_instrument_t

contains

  subroutine square_play(this, note, buffer, params)
    class(square_instrument_t), intent(in)    :: this
    type(note_t),               intent(in)    :: note
    real(wp), allocatable,      intent(inout) :: buffer(:)
    type(parameters_t),         intent(in)    :: params

    real(wp) :: f, t
    integer :: i

    f = note%frequency()
    t = 0
    i = nint(note%on * params%sample_rate / (params%tempo/60))
    do while (t <= note%length/(params%tempo/60))
      buffer(i) = buffer(i) + note%velocity * sign(1.0_wp, sin(2*pi*f*t)) * this%ad_envelope(t)
      i = i + 1
      t = t + 1.0_wp / params%sample_rate
    end do

  end subroutine square_play
end module square_instrument
