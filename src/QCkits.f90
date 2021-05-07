module QCkits
    use QCkits_global
    use QCkits_error
    implicit none
    private
    public :: qckits_instance
    public :: initialize_qckits, destory_qckits

    type :: qckits_t
        private
        character(len=:), allocatable :: file
    contains
        private
        procedure, pass, public :: load_file
        procedure, pass, public :: get_file
    end type

    type(qckits_t), pointer, protected :: qckits_instance => null()
        !! singleton

contains

    subroutine initialize_qckits(file)
        character(len=*), intent(in) :: file

        if (associated(qckits_instance)) &
            call terminate(qckits_failure, "Initializing the initialized qckits_instance.")

        allocate(qckits_instance)
        qckits_instance%file = file

    end subroutine initialize_qckits


    subroutine destory_qckits()

        deallocate(qckits_instance)

    end subroutine destory_qckits


    subroutine load_file(this)
        class(qckits_t), intent(in) :: this

        !! locals
        logical :: is_exist

        inquire(file=this%file, exist=is_exist)
        if (.not. is_exist) call terminate(qckits_failure, trim(this%file) // " not exists.")

        write(output_unit,"('Load ',A,' successfully.')") trim(this%file)

    end subroutine load_file


    function get_file(this) result(file)
        class(qckits_t), intent(in) :: this
        character(len=:), allocatable :: file

        file = trim(this%file)

    end function get_file

end module QCkits