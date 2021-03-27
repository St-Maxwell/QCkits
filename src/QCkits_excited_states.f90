module QCkits_excited_state
    use QCkits_global, only: fp
    use QCkits_utils, only: locate_label
    use QCkits_error
    use QCkits_file, only: QCkits_output
    implicit none
    private
    public :: cis_wavefunction, rpa_wavefunction
    public :: simplify_cis_ampl_print

    integer, parameter :: cis_wavefunction = 1
    integer, parameter :: rpa_wavefunction = 2

contains
    
    subroutine simplify_cis_ampl_print(output, wf_type, threshold)
        type(QCkits_output), intent(in) :: output
        integer, intent(in) :: wf_type
        integer, intent(in) :: threshold

        !! locals
        logical :: found
        
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

    end subroutine simplify_cis_ampl_print

end module QCkits_excited_state