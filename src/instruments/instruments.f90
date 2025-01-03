module instruments
!==============================================================================#
! INSTRUMENTS
!------------------------------------------------------------------------------#
! Author:  Edward Higgins <ed.higgins@york.ac.uk>
!------------------------------------------------------------------------------#
! Version: 0.1.1, 2025-01-02
!------------------------------------------------------------------------------#
! This code is distributed under the MIT license.
!==============================================================================#
  use notes
  use parameters
  use constants
  implicit none

  public

  type, abstract :: instrument_t
    real(wp) :: attack = 0.001_wp
    real(wp) :: decay = 0
  contains
    procedure(instrument_play), deferred :: play
    procedure :: ad_envelope => instrument_ad_envelope

  end type instrument_t

  abstract interface
    subroutine instrument_play(this, note, buffer, params)
      use notes
      use parameters
      use constants
      import :: instrument_t
      class(instrument_t),  intent(in)    :: this
      type(note_t),         intent(in)    :: note
      real(wp), allocatable,intent(inout) :: buffer(:)
      type(parameters_t),   intent(in)    :: params
    end subroutine instrument_play
  end interface

contains
  function instrument_ad_envelope(this, t)
    class(instrument_t), intent(in) :: this
    real(wp), intent(in) :: t
    real(wp) :: instrument_ad_envelope

    instrument_ad_envelope = (1-exp(-t/this%attack)) * exp(-this%decay *  t)

  end function instrument_ad_envelope
end module instruments
