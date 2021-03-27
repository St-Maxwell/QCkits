module QCkits
    use iso_fortran_env
    use qckits_error
    implicit none
    private
    public :: fp
    public :: qckits_instance
    public :: initialize_qckits
    
    integer, parameter :: fp = real64
        !! default real kind

    type :: qckits_t
        private
        character(len=:), allocatable :: file
    contains
        private
        procedure, public :: load_file
    end type

    type(qckits_t), pointer :: qckits_instance => null()
        !! singleton

contains
    
    subroutine initialize_qckits(file)
        character(len=*), intent(in) :: file

        if (associated(qckits_instance)) call terminate(qckits_failure, &
                                                    "Trying to initialize the initialized qckits_instance.")

        allocate(qckits_instance)
        qckits_instance%file = file

    end subroutine initialize_qckits


    subroutine load_file(this)
        class(qckits_t), intent(in) :: this

        !! locals
        logical :: is_exist

        inquire(file=this%file, exist=is_exist)
        if (.not. is_exist) call terminate(qckits_failure, trim(this%file) // " not exists.")

        write(output_unit,"('Load ',A,' successfully.')") trim(this%file)

    end subroutine load_file



end module QCkits