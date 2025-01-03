module composition
!==============================================================================#
! COMPOSITION
!------------------------------------------------------------------------------#
! Author:  Ed Higgins <ed.j.higgins@gmail.com>
!------------------------------------------------------------------------------#
! Version: 0.1.1, 2025-01-01
!------------------------------------------------------------------------------#
! This code is distributed under the MIT license.
!==============================================================================#
  use iso_fortran_env, only: int16
  use constants
  use parameters
  use instruments
  use sine_instrument
  use saw_instrument
  use square_instrument
  use tracks
  use regex
  implicit none

  private

  type, public :: composition_t
    real(wp)                 :: sample_rate = 44100
    real(wp)                 :: length 
    real(wp)                 :: tempo 
    real(wp), allocatable    :: buffer(:)
    character(len=64)        :: name
    class(track_t), allocatable :: tracks(:)
  contains
    procedure :: mix => composition_mix
    procedure :: play => composition_play
    procedure :: write_wav => composition_write_wav
  end type composition_t

contains

  subroutine composition_mix(this)
    class(composition_t), intent(inout) :: this
    integer :: i

    allocate(this%buffer(size(this%tracks(1)%buffer)))

    this%buffer = 0
    do i=1, size(this%tracks)
      this%buffer = this%buffer + this%tracks(i)%volume * this%tracks(i)%buffer
    end do

  end subroutine composition_mix

  subroutine composition_play(this)
    class(composition_t), intent(inout) :: this

    integer :: audio, i

    call execute_command_line("rm -f .tmp.raw")

    open(newunit=audio, file = ".tmp.raw", access = 'stream', action = 'write')

    do i=1, size(this%buffer)
      write(audio)  int(0.5_wp*huge(1_int16) * this%buffer(i), kind=int16)
    end do

    call execute_command_line("ffplay -loglevel quiet -autoexit -nodisp -ar 44100 -f s16le .tmp.raw")
    call execute_command_line("rm -f .tmp.raw")

    close(audio)

  end subroutine composition_play

  subroutine composition_write_wav(this)
    class(composition_t), intent(inout) :: this

    integer :: audio, raw_data, i

    open(newunit=raw_data, file = ".tmp.raw", access = 'stream', action = 'write')
    do i=1, size(this%buffer)
      write(raw_data)  int(0.5_wp*huge(1_int16) * this%buffer(i), kind=int16)
    end do
    close(raw_data)

    call execute_command_line("sox -r 44100 -e signed -b 16 -c 1 .tmp.raw " // trim(this%name) //".wav")
    call execute_command_line("rm .tmp.raw")
  end subroutine composition_write_wav

end module composition
