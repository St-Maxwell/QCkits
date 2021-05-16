module QCkits_excited_state
    use QCkits_global, only: fp, output_unit, input_unit
    use QCkits_utils, only: locate_label
    use QCkits_error
    use QCkits_file, only: qchem_file
    implicit none
    private
    public :: QCkits_extract_excitation_energy, &
              QCkits_simplify_cis_ampl_print
    public :: extract_excitation_energy, &
              simplify_cis_ampl_print

    integer, parameter :: extract_excitation_energy = 1
    integer, parameter :: simplify_cis_ampl_print = 2

    integer, parameter :: cis_wavefunction = 1
    integer, parameter :: rpa_wavefunction = 2

    type :: excited_states
        real(kind=fp), dimension(:), allocatable :: e !! excitation energy
        real(kind=fp), dimension(:), allocatable :: S2 !! <S**2>
        real(kind=fp), dimension(:), allocatable :: f !! oscillator strength
    contains
        procedure, pass :: save_file
    end type

contains

    subroutine QCkits_extract_excitation_energy()
        !! extract excitation energy with corresponding multiplicity

        !! locals
        type(excited_states) :: ex
        character(len=100) :: buffer
        integer :: istate, istate_tmp
        real(kind=fp) :: ex_energy_tmp
        real(kind=fp) :: S2_tmp
        real(kind=fp) :: f_tmp
        integer :: wf_type
        logical :: found
        character(len=1) :: select

        !! wait for input
        write(output_unit,"('Select wavefunction (1: CIS, 2: RPA): ')", advance="no")
        read(input_unit,*) wf_type

        call qchem_file%open()

        select case (wf_type)
        case (cis_wavefunction)
            call qchem_file%find("TDDFT/TDA Excitation Energies", found)
        case (rpa_wavefunction)
            call qchem_file%find("TDDFT Excitation Energies", found)
        case default
            call terminate(qckits_failure, "Unknown excited state wavefunction")
        end select

        if (.not. found) call terminate(qckits_failure, "Excited states not found")

        ex%e  = [real(kind=fp) ::]
        ex%S2 = [real(kind=fp) ::]
        ex%f  = [real(kind=fp) ::]

        write(output_unit,"('Excitation Energy (eV)     S2')")
        istate = 0
        do
            call locate_label(found, qchem_file%get_unit(), "Excited state", rewind=.false.)

            if (.not. found) exit

            read(qchem_file%get_unit(),"(A)") buffer
            read(buffer(15:18),*) istate_tmp

            if (istate_tmp < istate) exit

            read(buffer(45:54),*) ex_energy_tmp

            read(qchem_file%get_unit(),"(A)") buffer
            read(qchem_file%get_unit(),"(A)") buffer
            
            !! extract multiplicity
            if (index(buffer, "Multiplicity") /= 0) then
                if (index(buffer, "Singlet") /= 0) then
                    S2_tmp = 0._fp
                else if (index(buffer, "Triplet") /= 0) then
                    S2_tmp = 2._fp
                end if
            else if (index(buffer, "<S**2>") /= 0) then
                read(buffer(17:),*) S2_tmp
            end if

            read(qchem_file%get_unit(),"(A)") buffer
            read(qchem_file%get_unit(),"(A)") buffer
            read(buffer(17:),*) f_tmp

            write(output_unit,"(F14.4,11X,F6.4)") ex_energy_tmp, S2_tmp

            ex%e = [ex%e, ex_energy_tmp]
            ex%S2 = [ex%S2, S2_tmp]
            ex%f = [ex%f, f_tmp]

            istate = istate + 1

        end do
        write(output_unit,"('')")

        call qchem_file%close()

        write(output_unit,"('Save excitation energy to file (y/n): ')",advance="no")
        read(input_unit,*) select
        if (select == 'y') then
            call ex%save_file()
        end if

    end subroutine QCkits_extract_excitation_energy


    subroutine save_file(this)
        class(excited_states), intent(in) :: this

        !! locals
        integer :: i
        integer :: u

        open(newunit=u, file="excited_states.dat", action="write")

        write(u,"('Excitation Energy (eV)   <S**2>   oscillator strength')")
        do i = 1, size(this%e)
            write(u,"(F14.4,F17.4,F18.10)") this%e(i), this%S2(i), this%f(i)
        end do

        close(u)

    end subroutine save_file


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

        !! wait for input
        write(output_unit,"('Select wavefunction (1: CIS, 2: RPA): ')", advance="no")
        read(input_unit,*) wf_type
        write(output_unit,"('Threshold (values greater than 0.001 * threshold will be printed): ')", &
                           advance="no")
        read(input_unit,*) threshold

        call qchem_file%open()

        !! locate and check
        !! find cis_n_roots


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
        write(output_unit,"('Found record, and begin working.')")
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
        call qchem_file%close()

    end subroutine QCkits_simplify_cis_ampl_print

end module QCkits_excited_state
