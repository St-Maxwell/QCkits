module QCkits_global
    use iso_fortran_env, only: real64, output_unit, input_unit, error_unit
    implicit none
    private
    public :: fp, output_unit, input_unit, error_unit
    public :: optval

    integer, parameter :: fp = real64
        !! default real kind

    interface optval
        module procedure :: optval_int
        module procedure :: optval_real
        module procedure :: optval_char
        module procedure :: optval_logic
    end interface

contains

!===========================================================
! from https://github.com/fortran-lang/stdlib/issues/62
! return default value if opt is not presented
    function optval_int(opt, default) result(val)
        integer, intent(in), optional :: opt
        integer, intent(in) :: default
        integer :: val

        if (present(opt)) then
            val = opt
        else
            val = default
        end if

    end function optval_int

    function optval_real(opt, default) result(val)
        real(kind=fp), intent(in), optional :: opt
        real(kind=fp), intent(in) :: default
        real(kind=fp) :: val

        if (present(opt)) then
            val = opt
        else
            val = default
        end if

    end function optval_real

    function optval_char(opt, default) result(val)
        character(len=*), intent(in), optional :: opt
        character(len=*), intent(in) :: default
        character(len=:), allocatable :: val

        if (present(opt)) then
            val = opt
        else
            val = default
        end if

    end function optval_char

    function optval_logic(opt, default) result(val)
        logical, intent(in), optional :: opt
        logical, intent(in) :: default
        logical :: val

        if (present(opt)) then
            val = opt
        else
            val = default
        end if

    end function optval_logic

end module QCkits_global