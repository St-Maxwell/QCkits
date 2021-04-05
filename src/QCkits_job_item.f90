module QCkits_job_item
    use QCkits_global, only: output_unit, input_unit
    use QCkits_error, only: qckits_failure, terminate
    implicit none
    private
    public :: qckits_menu_t, new_job

    type, abstract :: qckits_item_t
        !! the base class of QCkits working menu
        character(len=:), allocatable :: description
    contains
        procedure(qckits_item_run), deferred :: run
    end type

    abstract interface
        subroutine qckits_item_run(this)
            import :: qckits_item_t
            class(qckits_item_t), intent(inout) :: this
        end subroutine qckits_item_run
    end interface

!===============================================================================

    type :: item_list
        !! a sorted linked list
        private
        type(item_list_node), pointer :: head => null()
    contains
        private
        procedure, pass, public :: insert_item
        procedure, pass :: choose => choose_item
        final :: destruct_item_list
    end type

    type :: item_list_node
        private
        integer :: index
        class(qckits_item_t), pointer :: item => null()
        class(item_list_node), pointer :: next => null()
    contains
        final :: destruct_item_list_node
    end type

!===============================================================================

    type, extends(qckits_item_t) :: qckits_menu_t
        private
        type(item_list) :: items
    contains
        private
        procedure, pass, public :: init => init_menu
        procedure, pass, public :: run => run_menu
        procedure, pass :: add_menu_option_menu
        procedure, pass :: add_menu_option_job
        generic, public :: add_option => add_menu_option_menu, add_menu_option_job
        procedure, pass :: show => show_menu
    end type

!===============================================================================

    type, extends(qckits_item_t) :: qckits_job_t
        procedure(qckits_job), pointer, nopass :: job
    contains
        procedure, pass, public :: run => run_job
    end type

    abstract interface
       subroutine qckits_job()
       end subroutine qckits_job
    end interface

contains

!===============================================================================

    subroutine init_menu(this, description)
        class(qckits_menu_t), intent(inout) :: this
        character(len=*), intent(in) :: description

        this%description = description

    end subroutine init_menu


    subroutine run_menu(this)
        !! print items
        !! and wait for input
        class(qckits_menu_t), intent(inout) :: this

        !! locals
        class(qckits_item_t), pointer :: choice
        character(len=100) :: buffer
        integer :: number
        integer :: ierr

        do
            call this%show()

            write(output_unit,"('Enter a number to select: ')",advance="no")
            read(input_unit,*) buffer

            !! if is q
            if (buffer == 'q') exit

            !! if is number
            read(buffer,*,iostat=ierr) number

            !! else
            if (ierr /=0) cycle

            choice => this%items%choose(number)
            if (.not. associated(choice)) cycle

            call choice%run()

        end do

    end subroutine run_menu


    subroutine add_menu_option_menu(this, menu, index)
        class(qckits_menu_t), intent(inout) :: this
        type(qckits_menu_t), intent(in), pointer :: menu
        integer, intent(in) :: index

        call this%items%insert_item(menu, index)

    end subroutine add_menu_option_menu


    subroutine add_menu_option_job(this, job, index)
        class(qckits_menu_t), intent(inout) :: this
        type(qckits_job_t), intent(in), pointer :: job
        integer, intent(in) :: index

        call this%items%insert_item(job, index)

    end subroutine add_menu_option_job


    subroutine show_menu(this)
        class(qckits_menu_t), intent(inout) :: this

        !! locals
        type(item_list_node), pointer :: next

        write(output_unit,"('')")
        write(output_unit,"('=== ',g0,' ===')") this%description

        next => this%items%head
        do
            if (.not. associated(next)) exit
            write(output_unit,"(' [',g0,'] ',g0)") next%index, next%item%description
            next => next%next
        end do

        write(output_unit,"('')")

    end subroutine show_menu

!===============================================================================

    function new_job(job, description)
        procedure(qckits_job) :: job
        character(len=*), intent(in) :: description
        type(qckits_job_t), pointer :: new_job

        allocate(new_job)
        new_job%job => job
        new_job%description = description

    end function new_job


    subroutine run_job(this)
        !! do the concrete job
        class(qckits_job_t), intent(inout) :: this

        call this%job()

    end subroutine run_job

!===============================================================================

    subroutine insert_item(this, item, index)
        class(item_list), intent(inout) :: this
        class(qckits_item_t), intent(in), pointer :: item
        integer, intent(in) :: index

        !! locals
        type(item_list_node), pointer :: curr
        type(item_list_node), pointer :: new_item
        allocate(new_item)
        new_item%item => item
        new_item%index = index

        if (.not. associated(this%head)) then
            !! head is null
            this%head => new_item
            return
        end if

        if (index < this%head%index) then
            !! new_item -> head -> ...
            new_item%next => this%head
            this%head = new_item
            return

        else if (index > this%head%index) then
            curr => this%head

            do
                if (.not. associated(curr%next)) exit !! curr -> null
                if (index < curr%next%index .and. index > curr%index) then
                    !! curr     next -> ...
                    !!   |       ^
                    !!   V       |
                    !! new_item -+
                    new_item%next => curr%next
                    curr%next => new_item
                    return

                else if (index == curr%next%index .or. index == curr%index) then
                    deallocate(new_item)
                    call terminate(qckits_failure, "Menu index conflict")
                end if
                curr => curr%next
            end do

            curr%next => new_item
        else
            deallocate(new_item)
            call terminate(qckits_failure, "Menu index conflict")
        end if

    end subroutine insert_item


    function choose_item(this, index) result(choice)
        class(item_list), intent(in) :: this
        integer, intent(in) :: index
        class(qckits_item_t), pointer :: choice

        !! locals
        type(item_list_node), pointer :: next

        choice => null()
        next => this%head
        do
            if (.not. associated(next)) exit
            if (next%index == index) then
                choice => next%item
                return
            end if
            next => next%next
        end do

    end function choose_item


    subroutine destruct_item_list(this)
        type(item_list), intent(inout) :: this

        !! locals
        type(item_list_node), pointer :: curr

        do
            if (.not. associated(this%head%next)) exit
            curr => this%head%next
            deallocate(this%head)
            this%head => curr
        end do

        deallocate(this%head)

    end subroutine destruct_item_list


    subroutine destruct_item_list_node(this)
        type(item_list_node), intent(inout) :: this

        deallocate(this%item%description)
        deallocate(this%item)

    end subroutine destruct_item_list_node

end module QCkits_job_item