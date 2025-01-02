module notes
!==============================================================================#
! NOTES
!------------------------------------------------------------------------------#
! Author:  Ed Higgins <ed.j.higgins@gmail.com>
!------------------------------------------------------------------------------#
! Version: 0.1.1, 2025-01-01
!------------------------------------------------------------------------------#
! This code is distributed under the MIT license.
!==============================================================================#
  use constants
  implicit none

  private

  
  type, public :: note_t
    character(len=3) :: pitch
    real(wp)         :: on
    real(wp)         :: length
    real(wp)         :: velocity
  contains
    procedure :: dump => note_dump
    procedure :: frequency => note_frequency
  end type note_t

contains

  subroutine note_dump(this)
    class(note_t), intent(in) :: this
    print *, "    Pitch: ", this%pitch
    print *, "    On: ", this%on
    print *, "    Length: ", this%length
    print *, "    Velocity: ", this%velocity
  end subroutine note_dump

  function note_frequency(this) result(f)
    class(note_t), intent(in) :: this
    real(wp) :: f
    real(wp) :: a

    f = 55.0_wp
    select case (this%pitch(1:1))
      case('C')
        f = f * 2.0_wp ** (-9.0_wp/12)
      case('D')
        f = f * 2.0_wp ** (-7.0_wp/12)
      case('E')
        f = f * 2.0_wp ** (-5.0_wp/12)
      case('F')
        f = f * 2.0_wp ** (-4.0_wp/12)
      case('G')
        f = f * 2.0_wp ** (-2.0_wp/12)
      case('B')
        f = f * 2.0_wp ** (2.0_wp/12)
    end select

    if (this%pitch(2:2) == "b") then
      f = f * 2.0_wp ** (-1.0_wp/12)
      f = f * 2 ** (iachar(this%pitch(3:3)) - iachar('1'))

    else if (this%pitch(2:2) == "#") then
      f = f * 2.0_wp ** (-1.0_wp/12)
      f = f * 2 ** (iachar(this%pitch(3:3)) - iachar('1'))

    else
      f = f * 2 ** (iachar(this%pitch(2:2)) - iachar('1'))
    end if

  end function note_frequency
end module notes

