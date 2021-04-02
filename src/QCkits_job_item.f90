module QCkits_job_item
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
        integer :: num_items = 0
        type(item_list_node), pointer :: head => null()
    contains
        procedure, pass, public :: insert_item
    end type

    type :: item_list_node
        integer :: index
        class(qckits_item_t), pointer :: item => null()
        class(item_list_node), pointer :: next => null()
    end type

!===============================================================================

    type, extends(qckits_item_t) :: qckits_menu_t
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

        call this%show()

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

        write(*,*) this%description

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

        write(*,*) this%description

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
            allocate(this%head)
            this%head = new_item
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
                    stop
                end if
                curr => curr%next
            end do

            curr%next => new_item
        else
            deallocate(new_item)
            stop
        end if

    end subroutine insert_item


end module QCkits_job_item