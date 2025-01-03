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
    procedure :: init => composition_init
    procedure :: read_src => composition_read_src
    procedure :: mix => composition_mix
    procedure :: play => composition_play
    procedure :: write_wav => composition_write_wav
  end type composition_t

contains

  subroutine composition_init(this, params)
    class(composition_t), intent(inout) :: this
    type(parameters_t),   intent(inout) :: params

    call this%read_src(params)

  end subroutine composition_init

  subroutine composition_read_src(this, params)
    class(composition_t), intent(inout) :: this
    type(parameters_t),   intent(inout) :: params

    integer :: src, ios
    character(len=256) :: line
    integer :: i_char
    character(len=16) :: mode(8)
    integer :: mode_head
    character(len=64), allocatable :: tokens(:)
    integer :: i_track, num_tracks
    integer :: i_note, num_notes

    open(newunit=src, file=params%src_file)

    mode_head = 1
    mode(1) = "main"

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
        case ("main")
          if (re_match("\w \s* : .*", line)) then
            call re_split("\s*:\s*", line, tokens)
            select case (tokens(1))
              case("name")
                if (.not. re_match("\w+", tokens(2))) error stop "Invalid name"
                this%name = tokens(2)

              case("length")
                if (.not. re_match("\d+ (\. \d+)?", tokens(2))) error stop "Invalid length"
                read(tokens(2), *) this%length

              case("tempo")
                if (.not. re_match("\d+ (\. \d+)?", tokens(2))) error stop "Invalid tempo"
                read(tokens(2), *) this%tempo

              case("sample_rate")
                if (.not. re_match("\d+ (\. \d+)?", tokens(2))) error stop "Invalid sample_rate"
                read(tokens(2), *) this%sample_rate

              case("num_tracks")
                if (.not. re_match("\d+", tokens(2))) error stop "Invalid num_tracks"
                read(tokens(2), *) num_tracks
                allocate(this%tracks(num_tracks))

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
            error stop "Unrecognised line in main"
          end if

        case ("track")

          if (re_match("\w \s* : .*", line)) then
            call re_split("\s*:\s*", line, tokens)
            select case (tokens(1))
              case("name")
                if (.not. re_match("\w+", tokens(2))) error stop "Invalid track name"
                this%tracks(i_track)%name = tokens(2)

              case("volume")
                if (.not. re_match("\d+ (\. \d+)?", tokens(2))) error stop "Invalid volume"
                read(tokens(2), *) this%tracks(i_track)%volume

              case default
                error stop "Invalid track parameter"
            end select

          else if (re_match("sequence \s* {", line)) then
            mode_head=mode_head+1
            mode(mode_head) = re_match_str("\w+", line)

          else if (re_match("instrument \s* {", line)) then
            mode_head=mode_head+1
            mode(mode_head) = re_match_str("\w+", line)

          else if (re_match("}", line)) then
            mode(mode_head) = ""
            mode_head=mode_head-1

          else
            error stop "Unrecognised line in track"
          end if

        case ("sequence")

          if (re_match("\w \s* : .*", line)) then
            call re_split("\s*:\s*", line, tokens)
            select case (tokens(1))
              case("num_notes")
                if (.not. re_match("\d+", tokens(2))) error stop "Invalid num_notes"
                read(tokens(2), *) num_notes
                allocate(this%tracks(i_track)%seq(num_notes))
                i_note = 0

              case default
                error stop "Invalid sequence parameter"
            end select

          else if (re_match("\w+ (\s+ \d+(\.\d+)?)+", line)) then
            call re_split("\s+", line, tokens)
            i_note=i_note+1
            if (i_note > num_notes) error stop "Too many notes in sequence"
            this%tracks(i_track)%seq(i_note)%pitch = tokens(1)
            read(tokens(2), *) this%tracks(i_track)%seq(i_note)%on
            read(tokens(3), *) this%tracks(i_track)%seq(i_note)%length
            read(tokens(4), *) this%tracks(i_track)%seq(i_note)%velocity

          else if (re_match("}", line)) then
            mode(mode_head) = ""
            mode_head=mode_head-1

          else
            error stop "Unrecognised line in sequence"
          end if

        case ("instrument")

          if (re_match("\w \s* : .*", line)) then
            call re_split("\s*:\s*", line, tokens)
            select case (tokens(1))
              case("type")
                if (.not. re_match("\w+", tokens(2))) error stop "Invalid track name"
                select case (tokens(2))
                  case("sine")
                    allocate(sine_instrument_t :: this%tracks(i_track)%instrument)

                  case("square")
                    allocate(square_instrument_t :: this%tracks(i_track)%instrument)

                  case("saw")
                    allocate(saw_instrument_t :: this%tracks(i_track)%instrument)

                  case default
                    error stop "Invalid instrument"
                end select

              case("decay")
                if (.not. re_match("\d+ (\.\d+)?", tokens(2))) error stop "Invalid decay length"
                read(tokens(2),*) this%tracks(i_track)%instrument%decay

              case("attack")
                if (.not. re_match("\d+ (\.\d+)?", tokens(2))) error stop "Invalid attack length"
                read(tokens(2),*) this%tracks(i_track)%instrument%attack

              case default
                error stop "Invalid instrument parameter"
            end select

          else if (re_match("}", line)) then
            mode(mode_head) = ""
            mode_head=mode_head-1

          else
            error stop "Unrecognised line in track"
          end if

        case default
          error stop "Invalid mode"
      end select
    end do

   close(src)

   ! Update params
   params%sample_rate = this%sample_rate
   params%length = this%length
   params%tempo = this%tempo


  end subroutine composition_read_src

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
