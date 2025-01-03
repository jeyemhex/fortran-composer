module parser
!==============================================================================#
! PARSER
!------------------------------------------------------------------------------#
! Author:  Ed Higgins <ed.j.higgins@gmail.com>
!------------------------------------------------------------------------------#
! Version: 0.1.1, 2025-01-03
!------------------------------------------------------------------------------#
! composition code is distributed under the MIT license.
!==============================================================================#
  use iso_fortran_env, only: error_unit
  use constants
  use parameters
  use instruments
  use sine_instrument
  use saw_instrument
  use square_instrument
  use composition
  use tracks
  use regex
  implicit none

  private

  public :: parse_input

  integer :: src, ios
  character(len=256) :: line
  integer :: i_char
  character(len=16) :: mode(8)
  integer :: mode_head
  character(len=64), allocatable :: tokens(:)
  integer :: i_track, num_tracks

contains

  subroutine parse_input(composition, params)
    type(composition_t),  intent(inout) :: composition
    type(parameters_t),   intent(inout) :: params

    open(newunit=src, file=params%src_file)

    mode_head = 1
    mode(1) = "composition"

    num_tracks = 0
    i_track = 0
    i_note = 0
    do
      ! Read the line in
      read(src,'(a)', iostat=ios) line
      if (ios /= 0) exit

      ! Handle comments and whitespace
      line = adjustl(line)
      i_char = index(line, "!")
      if (i_char > 0) line(i_char:) = " "
      if (len_trim(line) == 0) cycle

      select case(mode(mode_head))
        case ("composition")
          call parse_composition(composition)

        case ("track")
          call parse_track(composition%tracks(i_track))

        case ("sequence")
          call parse_sequence(composition%tracks(i_track)%seq)

        case ("instrument")
          call parse_instrument(composition%tracks(i_track)%instrument)

        case default
          error stop "Invalid mode"
      end select
    end do

   close(src)

   ! Update params
   params%sample_rate = composition%sample_rate
   params%length = composition%length
   params%tempo = composition%tempo

  end subroutine parse_input

  subroutine parse_composition(composition)
    type(composition_t),  intent(inout) :: composition

    if (re_match("\w \s* : .*", line)) then
      call re_split("\s*:\s*", line, tokens)
      select case (tokens(1))
        case("name")
          if (.not. re_match("\w+", tokens(2))) error stop "Invalid name"
          composition%name = tokens(2)

        case("length")
          if (.not. re_match("\d+ (\. \d+)?", tokens(2))) error stop "Invalid length"
          read(tokens(2), *) composition%length

        case("tempo")
          if (.not. re_match("\d+ (\. \d+)?", tokens(2))) error stop "Invalid tempo"
          read(tokens(2), *) composition%tempo

        case("sample_rate")
          if (.not. re_match("\d+ (\. \d+)?", tokens(2))) error stop "Invalid sample_rate"
          read(tokens(2), *) composition%sample_rate

        case("num_tracks")
          if (.not. re_match("\d+", tokens(2))) error stop "Invalid num_tracks"
          read(tokens(2), *) num_tracks
          allocate(composition%tracks(num_tracks))

        case default
          error stop "Invalid input parameter"
      end select

    else if (re_match("track \s* {", line)) then
      i_track=i_track+1
      mode_head=mode_head+1
      mode(mode_head) = re_match_str("\w+", line)

    else if (re_match("}", line)) then
      error stop "Unmatched bracket"

    else
      error stop "Unrecognised line in composition"
    end if

  end subroutine parse_composition

  subroutine parse_track(track)
    type(track_t),      intent(inout) :: track

    if (re_match("\w \s* : .*", line)) then
      call re_split("\s*:\s*", line, tokens)
      select case (tokens(1))
        case("name")
          if (.not. re_match("\w+", tokens(2))) error stop "Invalid track name"
          track%name = tokens(2)

        case("volume")
          if (.not. re_match("\d+ (\. \d+)?", tokens(2))) error stop "Invalid volume"
          read(tokens(2), *) track%volume

        case default
          error stop "Invalid track parameter"
      end select

    else if (re_match("(instrument | sequence) \s* {", line)) then
      mode_head=mode_head+1
      mode(mode_head) = re_match_str("\w+", line)

    else if (re_match("}", line)) then
      mode(mode_head) = ""
      mode_head=mode_head-1

    else
      error stop "Unrecognised line in track"
    end if

  end subroutine parse_track

  subroutine parse_sequence(seq)
    type(note_t), allocatable,  intent(inout) :: seq(:)

    type(note_t), allocatable :: tmp_seq(:)
    integer :: i_note, num_notes

    allocate(seq(4))

    if (re_match("\w \s* : .*", line)) then
      call re_split("\s*:\s*", line, tokens)
      select case (tokens(1))
        case("num_notes")
          write(error_unit,*) "num_notes depreciated"

        case default
          error stop "Invalid sequence parameter"
      end select

    else if (re_match("\w+ (\s+ \d+(\.\d+)?)+", line)) then
      call re_split("\s+", line, tokens)
      i_note=i_note+1
      if (i_note > num_notes) error stop "Too many notes in sequence"
      seq(i_note)%pitch = tokens(1)
      read(tokens(2), *) seq(i_note)%on
      read(tokens(3), *) seq(i_note)%length
      read(tokens(4), *) seq(i_note)%velocity

    else if (re_match("}", line)) then
      mode(mode_head) = ""
      mode_head=mode_head-1

    else
      error stop "Unrecognised line in sequence"
    end if

  end subroutine parse_sequence

  subroutine parse_instrument(instrument)
    class(instrument_t),  allocatable,  intent(inout) :: instrument

    if (re_match("\w \s* : .*", line)) then
      call re_split("\s*:\s*", line, tokens)
      select case (tokens(1))
        case("type")
          if (.not. re_match("\w+", tokens(2))) error stop "Invalid track name"
          select case (tokens(2))
            case("sine")
              allocate(sine_instrument_t :: instrument)

            case("square")
              allocate(square_instrument_t :: instrument)

            case("saw")
              allocate(saw_instrument_t :: instrument)

            case default
              error stop "Invalid instrument"
          end select

        case("decay")
          if (.not. re_match("\d+ (\.\d+)?", tokens(2))) error stop "Invalid decay length"
          read(tokens(2),*) instrument%decay

        case("attack")
          if (.not. re_match("\d+ (\.\d+)?", tokens(2))) error stop "Invalid attack length"
          read(tokens(2),*) instrument%attack

        case default
          error stop "Invalid instrument parameter"
      end select

    else if (re_match("}", line)) then
      mode(mode_head) = ""
      mode_head=mode_head-1

    else
      error stop "Unrecognised line in instrument"
    end if

  end subroutine parse_instrument

end module parser
