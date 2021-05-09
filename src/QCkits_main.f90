module QCkits_main
    use QCkits_global
    use QCkits
    use QCkits_file
    use QCkits_excited_state
    implicit none
    private
    public :: QCkits_run

contains

    subroutine QCkits_run()
        !! locals
        character(len=40) :: file

        call parse_cmd_args(file)
        call initialize_qckits(file)
        call qckits_instance%load_file()

        call initialize_qchem_file()

        !! if interactive
        call QCkits_interactive()
        !! or silent
        !! to-do, need cmd parse

    end subroutine QCkits_run


    subroutine parse_cmd_args(input_file)
        !! the simplest implement
        character(len=*), intent(inout) :: input_file

        !! locals
        character(len=40) :: args

        call get_command_argument(1, args)
        input_file = args

    end subroutine parse_cmd_args


    subroutine QCkits_interactive()
        use QCkits_job_tree

        call init_menu()

        call qckits_main_menu%run()

        call destroy_menu()

    end subroutine QCkits_interactive

end module QCkits_main
