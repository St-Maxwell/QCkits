module QCkits_utils
    use QCkits_global, only: output_unit, optval
    implicit none
    private
    public :: locate_label
    public :: progress_bar_t

    type :: progress_bar_t
        private
        integer :: length = 40
        integer :: total_times
    contains
        procedure, pass, public :: init
        procedure, pass, public :: update
    end type

contains

    subroutine locate_label(status, unit, label, rewind, backspace, maxline)
        logical, intent(inout) :: status
        integer, intent(in) :: unit
        character(len=*), intent(in) :: label
        logical, intent(in), optional :: rewind
            !! default value is .true.
        logical, intent(in), optional :: backspace
            !! default value is .true.
        integer, intent(in), optional :: maxline

        !! locals
        character(len=100) :: buffer
        integer :: ierr
        integer :: i

        status = .false.

        !! rewind is default
        if (optval(rewind, .true.)) rewind(unit)

        if (present(maxline)) then
            do i = 1, maxline
                read(unit,"(A)",iostat=ierr) buffer
                if (ierr /= 0) return ! return and let locate_label .false.
                if (index(buffer, label) /= 0) exit ! found
            end do
            !! loop stops, label not found, and status remains .false.
            return
        else !! default is searching the whole file
            do
                read(unit,"(A)",iostat=ierr) buffer
                if (ierr /= 0) return ! return and let locate_label .false.
                if (index(buffer, label) /= 0) exit ! found
            end do
        end if

        !! backspace is default
        if (optval(backspace, .true.)) backspace(unit)

        status = .true.

    end subroutine locate_label

!===============================================================================

    subroutine init(this, times)
        class(progress_bar_t), intent(inout) :: this
        integer, intent(in) :: times

        this%total_times = times

    end subroutine init


    subroutine update(this, current)
        class(progress_bar_t), intent(inout) :: this
        integer, intent(in) :: current

        !! locals
        character(len=1), parameter :: escape = achar(27)
        character(len=2), parameter :: code_start = escape // '['
        character(len=1), parameter :: code_end = 'm'
        character(len=4), parameter :: code_clear = code_start // '0' // code_end
        character(len=1), parameter :: cr = char(13)
        character(len=*), parameter :: blank = code_start // "30;1" // code_end // '-' // code_clear
        character(len=*), parameter :: fill = code_start // "31;1" // code_end // '-' // code_clear
        character(len=*), parameter :: head = code_start // "32;1" // code_end // " Processing " // code_clear
        character(len=16), save :: percent = code_start // "35;1" // code_end // " ???%" // code_clear
        character(len=:), allocatable :: bar
        integer :: progress
        integer :: nf, nb

        progress = (current * 100) / this%total_times
        write(percent(9:11),"(I3)") progress !! replace "???" with number
        nf = (current * this%length) / this%total_times
        nb = this%length - nf - 1

        if (current < this%total_times) then
            bar = head // repeat(fill,nf) // ' ' // repeat(blank,nb) // percent
            write(output_unit,"(2A)",advance="no") bar, cr
        else
            bar = head // repeat(fill,nf) // percent
            write(output_unit,"(A)") bar
        end if

    end subroutine update

end module QCkits_utils