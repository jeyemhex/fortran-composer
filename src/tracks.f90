module tracks
!==============================================================================#
! TRACKS
!------------------------------------------------------------------------------#
! Author:  Ed Higgins <ed.j.higgins@gmail.com>
!------------------------------------------------------------------------------#
! Version: 0.1.1, 2025-01-01
!------------------------------------------------------------------------------#
! This code is distributed under the MIT license.
!==============================================================================#
  use constants
  use parameters
  use notes
  implicit none

  private

  type, public :: track_t
    character(len=64)     :: name
    character(len=64)     :: instrument_name
    real(wp)              :: volume
    real(wp), allocatable :: buffer(:)
    type(note_t), allocatable :: seq(:)
  contains
    procedure :: dump => track_dump
    procedure :: generate => track_generate
  end type track_t

contains

  subroutine track_dump(this)
    class(track_t), intent(in) :: this
    integer :: i_note

    print *, "  Name: " // this%name
    print *, "  Instrument: " // this%instrument_name
    print *, "  Volume: ", this%volume

    print *, "  Sequence:"
    do i_note=1, size(this%seq)
      call this%seq(i_note)%dump()
    end do
  end subroutine track_dump

  subroutine track_generate(this, params)
    class(track_t), intent(inout) :: this
    type(parameters_t), intent(in)    :: params
    integer :: i_note, i
    real(wp) :: f, t

    allocate(this%buffer(ceiling(params%length * params%sample_rate)))

    do i_note = 1, size(this%seq)
      f = this%seq(i_note)%frequency()
      print *, "Found note " // this%seq(i_note)%pitch // ", f = ", f
      t = 0
      i = nint(this%seq(i_note)%on * params%sample_rate / (params%tempo/60))
      do while (t <= this%seq(i_note)%length/(params%tempo/60))
        this%buffer(i) = this%seq(i_note)%velocity * sin(2*pi*f*t) * exp(-t)
        i = i + 1
        t = t + 1.0_wp / params%sample_rate
      end do
    end do

  end subroutine track_generate

end module tracks
