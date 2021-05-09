module QCkits_utils
    use QCkits_global, only: output_unit, optval, fp
    implicit none
    private
    public :: locate_label, lower_case, print_matrix, remove_char
    public :: progress_bar_t
    public :: fill_lower_triangular

    type :: progress_bar_t
        private
        integer :: length = 40
        integer :: total_times
    contains
        procedure, pass, public :: init => progress_bar_init
        procedure, pass, public :: update => progress_bar_update
    end type

    interface print_matrix
        module procedure :: print_matrix_real
        module procedure :: print_matrix_cmplx
    end interface print_matrix

contains

    subroutine locate_label(status, unit, label, rewind, backspace, maxline, matchcase)
        logical, intent(inout) :: status
        integer, intent(in) :: unit
        character(len=*), intent(in) :: label
        logical, intent(in), optional :: rewind
            !! default value is .true.
        logical, intent(in), optional :: backspace
            !! default value is .true.
        integer, intent(in), optional :: maxline
        logical, intent(in), optional :: matchcase
            !! default is .true.

        !! locals
        character(len=100) :: buffer
        character(len=:), allocatable :: label_
        integer :: ierr
        integer :: i

        status = .false.

        !! rewind is default
        if (optval(rewind, .true.)) rewind(unit)

        if (.not. optval(matchcase, .true.)) then
            label_ = lower_case(label)
        else
            label_ = label
        end if

        if (present(maxline)) then
            do i = 1, maxline
                read(unit,"(A)",iostat=ierr) buffer
                if (ierr /= 0) return ! return and let locate_label .false.
                if (.not. optval(matchcase, .true.)) buffer = lower_case(buffer)
                if (index(buffer, label_) /= 0) exit ! found
            end do
            !! loop stops, label not found, and status remains .false.
            return
        else !! default is searching the whole file
            do
                read(unit,"(A)",iostat=ierr) buffer
                if (ierr /= 0) return ! return and let locate_label .false.
                if (.not. optval(matchcase, .true.)) buffer = lower_case(buffer)
                if (index(buffer, label_) /= 0) exit ! found
            end do
        end if

        !! backspace is default
        if (optval(backspace, .true.)) backspace(unit)

        status = .true.

    end subroutine locate_label


    function lower_case(string)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: lower_case

        !! locals
        integer :: code
        integer :: i

        allocate(lower_case, source=string)
        do i = 1, len(string)
            code = iachar(string(i:i))
            select case (code)
            case (65:90)
                lower_case(i:i) = char(code + 32)
            end select
        end do

    end function lower_case


    subroutine remove_char(string, remove_list)
        character(len=*), intent(inout) :: string
        character(len=*), intent(in) :: remove_list

        !! locals
        integer :: i

        do i = 1, len(string)
            if (index(remove_list, string(i:i)) /= 0) string(i:i) = ' '
        end do

    end subroutine remove_char


    subroutine print_matrix_real(mat, unit, title)
        real(kind=fp), dimension(:,:), intent(in) :: mat
        integer, intent(in) :: unit
        character(len=*), intent(in), optional :: title

        !! locals
        integer :: i

        if (present(title)) write(unit,"(A)") title
        do i = 1, size(mat, dim=1)
            write(unit, "(*(F15.9,1X))") mat(i,:)
        end do
        write(unit,*)
    end subroutine print_matrix_real


    subroutine print_matrix_cmplx(mat, unit, title)
        complex(kind=fp), dimension(:,:), intent(in) :: mat
        integer, intent(in) :: unit
        character(len=*), intent(in), optional :: title

        !! locals
        integer :: i

        if (present(title)) write(unit,"(A)") title
        do i = 1, size(mat, dim=1)
            write(unit, "(*(:,' (',F15.9,',',F15.9,') '))") mat(i,:)
        end do
        write(unit,*)

    end subroutine print_matrix_cmplx

!===============================================================================

    subroutine progress_bar_init(this, times)
        class(progress_bar_t), intent(inout) :: this
        integer, intent(in) :: times

        this%total_times = times

    end subroutine progress_bar_init


    subroutine progress_bar_update(this, current)
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

    end subroutine progress_bar_update

!===============================================================================

    subroutine fill_lower_triangular(mat)
        complex(kind=fp), dimension(:,:), intent(inout) :: mat

        !! locals
        integer :: i, j

        do i = 1, size(mat, dim=1)
            do j = i + 1, size(mat, dim=2)
                mat(j,i) = mat(i,j)
            end do
        end do

    end subroutine fill_lower_triangular

end module QCkits_utils
