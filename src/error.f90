module qckits_error
    use iso_fortran_env, only: output_unit, error_unit
    implicit none
    private
    public :: qckits_success, qckits_failure
    public :: terminate

    integer, parameter :: qckits_success = 0
    integer, parameter :: qckits_failure = 1

contains

    subroutine terminate(status, msg)
        integer, intent(in) :: status
        character(len=*), intent(in), optional :: msg

        select case (status)
        case (qckits_success)
            if (present(msg)) write(output_unit,"(A)") msg
            stop
        case (qckits_failure)
            if (present(msg)) write(error_unit,"('Error: ',A)") msg
            error stop
        end select

    end subroutine terminate

end module qckits_error