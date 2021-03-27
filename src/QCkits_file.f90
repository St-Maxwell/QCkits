module QCkits_file
    use QCkits
    implicit none
    private
    public :: QCkits_output
    
    type :: QCkits_output
        private
        character(len=:), allocatable :: filename
        integer :: fileunit
    contains
        private
        procedure, pass, public :: init => init_from_qckits
        procedure, pass, public :: open => open_file
        procedure, pass, public :: close => close_file
        procedure, pass, public :: find => find_label
        procedure, pass, public :: find_next => find_next_label
    end type

contains
    
    subroutine init_from_qckits(this)
        class(QCkits_output), intent(inout) :: this

        this%filename = QCkits_instance%file

    end subroutine init_from_qckits


    subroutine open_file(this)
        class(QCkits_output), intent(inout) :: this

        open(newunit=this%fileunit, file=this%filename, action="read", status="old")

    end subroutine open_file


    subroutine close_file(this)
        class(QCkits_output), intent(in) :: this

        close(this%fileunit)

    end subroutine close_file


    subroutine find_label(this, label, found)
        use QCkits_utils, only: locate_label
        class(QCkits_output), intent(in) :: this
        character(len=*), intent(in) :: label
        logical, intent(inout) :: found

        call locate_label(found, this%fileunit, label)

    end subroutine find_label


    subroutine find_next_label(this, label, found)
        use QCkits_utils, only: locate_label
        class(QCkits_output), intent(in) :: this
        character(len=*), intent(in) :: label
        logical, intent(inout) :: found

        call locate_label(found, this%fileunit, label, rewind=.false.)

    end subroutine find_next_label

end module QCkits_file