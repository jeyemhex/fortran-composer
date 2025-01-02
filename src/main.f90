program composer
!==============================================================================#
! MAIN
!------------------------------------------------------------------------------#
! Author:  Ed Higgins <ed.j.higgins@gmail.com>
!------------------------------------------------------------------------------#
! Version: 0.1.1, 2025-01-01
!------------------------------------------------------------------------------#
! This code is distributed under the MIT license.
!==============================================================================#
  use parameters
  use composition
  implicit none

  type(parameters_t)  :: params
  type(composition_t) :: composition

  integer :: i

  call params%init()
  call composition%init(params)


  do i = 1, size(composition%tracks)
    call composition%tracks(i)%generate(params)
  end do

  call composition%mix()
  call composition%play()

end program composer
