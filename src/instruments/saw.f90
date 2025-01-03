module saw_instrument
!==============================================================================#
! SAW_INSTRUMENT
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
  type, public, extends(instrument_t) :: saw_instrument_t
  contains
    procedure :: play => saw_play
  end type saw_instrument_t

contains

  subroutine saw_play(this, note, buffer, params)
    class(saw_instrument_t), intent(in)    :: this
    type(note_t),               intent(in)    :: note
    real(wp), allocatable,      intent(inout) :: buffer(:)
    type(parameters_t),         intent(in)    :: params

    real(wp) :: f, t, val, cycle_time
    integer :: i

    f = note%frequency()
    t = 0
    cycle_time = 0
    i = nint(note%on * params%sample_rate / (params%tempo/60))
    val = 1
    do while (t <= note%length/(params%tempo/60))
      buffer(i) = buffer(i) + note%velocity * (2*val-1) * this%ad_envelope(t)
      val = val * 0.1_wp ** (f/params%sample_rate)
      i = i + 1
      t = t + 1.0_wp / params%sample_rate
      cycle_time = cycle_time + 1.0_wp / params%sample_rate
      if (cycle_time > 1/f) then
        cycle_time = 0
        val = 1
      end if
    end do

  end subroutine saw_play
end module saw_instrument
