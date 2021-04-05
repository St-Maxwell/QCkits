module QCkits_utils
    use QCkits_global, only: optval
    implicit none
    private
    public :: locate_label

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

end module QCkits_utils