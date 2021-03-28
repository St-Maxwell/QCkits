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
        type(QCkits_output) :: the_output
        character(len=40) :: file

        call parse_cmd_args(file)
        call initialize_qckits(file)
        call qckits_instance%load_file()

        call the_output%init()

        !! if interactive
        call QCkits_interactive(the_output)
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


    subroutine QCkits_interactive(output)
        type(QCkits_output), intent(inout) :: output

        !! locals
        integer, parameter :: excited_state_analysis = 1
        integer :: iutil
        
        write(output_unit,"('Interactive Mode:')")
        write(output_unit,"('1. Excited State Analysis')")
        write(output_unit,"('Please enter a number to select: ')", advance="no")
        read(input_unit,*) iutil

        select case (iutil)
            case (excited_state_analysis)
                call QCkits_excite_state_analysis(output)
            case default
        end select

    end subroutine QCkits_interactive

end module QCkits_main