module QCkits_excited_state
    use QCkits_global, only: fp, output_unit, input_unit
    use QCkits_utils, only: locate_label
    use QCkits_error
    use QCkits_file, only: QCkits_output
    implicit none
    private
    public :: QCkits_excite_state_analysis

    integer, parameter :: cis_wavefunction = 1
    integer, parameter :: rpa_wavefunction = 2

contains
    
    subroutine QCkits_excite_state_analysis(output)
        type(QCkits_output), intent(inout) :: output

        !! locals
        integer, parameter :: simplify_cis_ampl_print = 1
        integer :: iutil

        call output%open()

        write(output_unit,"('Excited State Analysis')")
        write(output_unit,"('1. Simplify CIS Amplitude')")
        write(output_unit,"('Please enter a number to select: ')", advance="no")
        read(input_unit,*) iutil

        select case (iutil)
            case (simplify_cis_ampl_print)
                block
                    integer :: iwf_type, threshold
                    write(output_unit,"('Select wavefunction (1: CIS, 2: RPA): ')", advance="no")
                    read(input_unit,*) iwf_type
                    write(output_unit,"('Threshold (values greater than 0.001 * threshold will be printed): ')", &
                                       advance="no")
                    read(input_unit,*) threshold
                    call QCkits_simplify_cis_ampl_print(output, iwf_type, threshold)
                end block
            case default
        end select

        call output%close()

    end subroutine QCkits_excite_state_analysis


    subroutine QCkits_simplify_cis_ampl_print(output, wf_type, threshold)
        !! read orbital pairs contribution in excited state,
        !! and print the most important ones into files
        type(QCkits_output), intent(in) :: output
        integer, intent(in) :: wf_type
        integer, intent(in) :: threshold

        !! locals
        character(len=30) :: file_out
        character(len=120) :: buffer
        real(kind=fp) :: ampl
        integer :: n
        integer :: num_states
        logical :: found
        integer :: istate
        integer :: u
        
        select case (wf_type)
            case (cis_wavefunction)
                call output%find("TDDFT/TDA Excitation Energies", found)
            case (rpa_wavefunction)
                call output%find("TDDFT Excitation Energies", found)
            case default
                call terminate(qckits_failure, "Unknown excited state wavefunction")
        end select

        if (.not. found) call terminate(qckits_failure, "Excited states not found")

        !! now read and output
        file_out = "new.out"

        open(newunit=u, file=file_out)
        num_states = 0
        do
            call output%find_next("Excited state", found)
            if (.not. found) exit !! end of record

            read(output%get_unit(),"(A)") buffer
            read(buffer,"(14X,I4)") istate

            if (num_states > istate) exit !! TDA done, and meet with TDDFT

            write(u,"(A)") trim(buffer)
            read(output%get_unit(),"(A)") buffer
            write(u,"(A)") trim(buffer)
            read(output%get_unit(),"(A)") buffer
            write(u,"(A)") trim(buffer)
            read(output%get_unit(),"(A)") buffer
            write(u,"(A)") trim(buffer)
            read(output%get_unit(),"(A)") buffer
            write(u,"(A)") trim(buffer)

            !! cis amplitude
            do
                read(output%get_unit(),"(A)") buffer
                !! check "->" and "<-"
                if (index(buffer, "->") /= 0 .or. index(buffer, "<-") /= 0) then
                    n = index(buffer, '=') + 1
                    read(buffer(n:),"(F8.4)") ampl
                    if (abs(ampl) > 0.001_fp * threshold) write(u,"(A)") trim(buffer)
                else
                    write(u,"('')")
                    exit
                end if
            end do

            num_states = istate
        end do
        close(u)

    end subroutine QCkits_simplify_cis_ampl_print

end module QCkits_excited_state