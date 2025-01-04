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
  use instruments
  use parameters
  use notes
  implicit none

  private

  type, public :: track_t
    character(len=64)     :: name
    real(wp)              :: volume = 1.0_wp
    real(wp), allocatable :: buffer(:)
    type(note_t), allocatable :: seq(:)
    class(instrument_t), allocatable :: instrument
  contains
    procedure :: dump => track_dump
    procedure :: generate => track_generate
  end type track_t

contains

  subroutine track_dump(this)
    class(track_t), intent(in) :: this
    integer :: i_note

    print *, "  Name: " // this%name
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

    allocate(this%buffer(0:ceiling(params%length * params%sample_rate)))

    this%buffer =  0
    do i_note = 1, size(this%seq)
      call this%instrument%play(this%seq(i_note), this%buffer, params)
    end do

  end subroutine track_generate

end module tracks
