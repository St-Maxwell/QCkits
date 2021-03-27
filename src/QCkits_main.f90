module QCkits_main
    use QCkits
    implicit none
    
contains
    
    subroutine parse_cmd_args(input_file)
        !! the simplest implement
        character(len=*), intent(inout) :: input_file

        !! locals
        character(len=40) :: args

        call get_command_argument(1, args)
        input_file = args

    end subroutine parse_cmd_args

end module QCkits_main