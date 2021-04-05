module QCkits_excited_state
    use QCkits_global, only: fp, output_unit, input_unit
    use QCkits_utils, only: locate_label
    use QCkits_error
    use QCkits_file, only: qchem_file
    implicit none
    private
    public :: QCkits_simplify_cis_ampl_print
    public :: simplify_cis_ampl_print

    integer, parameter :: simplify_cis_ampl_print = 1


    integer, parameter :: cis_wavefunction = 1
    integer, parameter :: rpa_wavefunction = 2

contains

    subroutine QCkits_simplify_cis_ampl_print()
        !! read orbital pairs contribution in excited state,
        !! and print the most important ones into files

        !! locals
        integer :: wf_type
        integer :: threshold
        character(len=30) :: file_out
        character(len=120) :: buffer
        real(kind=fp) :: ampl
        integer :: n
        integer :: num_states
        logical :: found
        integer :: istate
        integer :: u

        write(output_unit,"('Select wavefunction (1: CIS, 2: RPA): ')", advance="no")
        read(input_unit,*) wf_type
        write(output_unit,"('Threshold (values greater than 0.001 * threshold will be printed): ')", &
                           advance="no")
        read(input_unit,*) threshold

        select case (wf_type)
            case (cis_wavefunction)
                call qchem_file%find("TDDFT/TDA Excitation Energies", found)
            case (rpa_wavefunction)
                call qchem_file%find("TDDFT Excitation Energies", found)
            case default
                call terminate(qckits_failure, "Unknown excited state wavefunction")
        end select

        if (.not. found) call terminate(qckits_failure, "Excited states not found")

        !! now read and output
        file_out = "new.out"

        open(newunit=u, file=file_out)
        num_states = 0
        do
            call qchem_file%find_next("Excited state", found)
            if (.not. found) exit !! end of record

            read(qchem_file%get_unit(),"(A)") buffer
            read(buffer,"(14X,I4)") istate

            if (num_states > istate) exit !! TDA done, and meet with TDDFT

            write(u,"(A)") trim(buffer)
            read(qchem_file%get_unit(),"(A)") buffer
            write(u,"(A)") trim(buffer)
            read(qchem_file%get_unit(),"(A)") buffer
            write(u,"(A)") trim(buffer)
            read(qchem_file%get_unit(),"(A)") buffer
            write(u,"(A)") trim(buffer)
            read(qchem_file%get_unit(),"(A)") buffer
            write(u,"(A)") trim(buffer)

            !! cis amplitude
            do
                read(qchem_file%get_unit(),"(A)") buffer
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