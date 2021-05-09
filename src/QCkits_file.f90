module QCkits_file
    use QCkits, only: qckits_instance
    implicit none
    private
    public :: qchem_file
    public :: initialize_qchem_file, destroy_qchem_file

    type :: qckits_file_t
        private
        character(len=:), allocatable :: filename
        integer :: fileunit
    contains
        private
        procedure, pass, public :: open => open_file
        procedure, pass, public :: close => close_file
        procedure, pass, public :: get_unit
        procedure, pass, public :: find => find_label
        procedure, pass, public :: find_next => find_next_label
    end type

    type(qckits_file_t), pointer, protected :: qchem_file => null()

contains

    subroutine initialize_qchem_file()

        allocate(qchem_file)
        qchem_file%filename = qckits_instance%get_file()

    end subroutine initialize_qchem_file


    subroutine destroy_qchem_file()
        deallocate(qchem_file)
    end subroutine destroy_qchem_file


    subroutine open_file(this)
        class(qckits_file_t), intent(inout) :: this

        open(newunit=this%fileunit, file=this%filename, action="read", status="old")

    end subroutine open_file


    subroutine close_file(this)
        class(qckits_file_t), intent(in) :: this

        close(this%fileunit)

    end subroutine close_file


    function get_unit(this) result(unit)
        class(qckits_file_t), intent(in) :: this
        integer :: unit

        unit = this%fileunit

    end function get_unit


    subroutine find_label(this, label, found)
        use QCkits_utils, only: locate_label
        class(qckits_file_t), intent(in) :: this
        character(len=*), intent(in) :: label
        logical, intent(inout) :: found

        call locate_label(found, this%fileunit, label)

    end subroutine find_label


    subroutine find_next_label(this, label, found)
        use QCkits_utils, only: locate_label
        class(qckits_file_t), intent(in) :: this
        character(len=*), intent(in) :: label
        logical, intent(inout) :: found

        call locate_label(found, this%fileunit, label, rewind=.false.)

    end subroutine find_next_label

end module QCkits_file
