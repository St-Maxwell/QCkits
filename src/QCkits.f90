module QCkits
    use iso_fortran_env
    implicit none
    private
    public :: fp
    public :: qckits_instance
    public :: initialize_qckits
    
    integer, parameter :: fp = real64
        !! default real kind

    type :: qckits_t
        character(len=:), allocatable :: file
    end type

    type(qckits_t), pointer :: qckits_instance => null()
        !! singleton

contains
    
    subroutine initialize_qckits(file)
        character(len=*), intent(in) :: file

        if (associated(qckits_instance)) error stop

        allocate(qckits_instance)
        qckits_instance%file = file

    end subroutine initialize_qckits

end module QCkits