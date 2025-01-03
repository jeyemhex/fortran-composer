module parameters
!==============================================================================#
! PARAMETERS
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

  type, public :: parameters_t
    character(len=64) :: src_file
    character(len=16) :: mode = "write"
    real(wp)                 :: sample_rate = 44100
    real(wp)                 :: length 
    real(wp)                 :: tempo 
    character(len=64)        :: name

  contains
    procedure :: init => parameters_init
  end type parameters_t

contains

  subroutine parameters_init(this)
    class(parameters_t), intent(out) :: this
    integer :: num_args

    num_args = command_argument_count()
    if (num_args < 1 .or. num_args > 2) error stop "Usage: `composer <src-file> [<mode>]`"

    call get_command_argument(1, this%src_file)

    if (num_args == 2) call get_command_argument(2, this%mode)

  end subroutine parameters_init
end module parameters
