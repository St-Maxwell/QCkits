module QCkits_properties
    use QCkits_global, only: fp, output_unit, input_unit
    use QCkits_file, only: qchem_file
    use QCkits_utils, only: locate_label
    implicit none
    private
    public :: QCkits_extract_SOC
    public :: extract_SOC

    integer, parameter :: extract_SOC = 1


!===============================================================================
    type :: soc_helper
        integer :: num_states
        !! tp stands for Triplet Plus, Ms = 1
        !! t0 stands for Triplet 0, Ms = 0
        !! tm stands for Triplet Minus, Ms = -1
        complex(kind=fp), dimension(:,:), allocatable :: tp_tp_coupling
        complex(kind=fp), dimension(:,:), allocatable :: tp_t0_coupling
        complex(kind=fp), dimension(:,:), allocatable :: s_tp_coupling
        complex(kind=fp), dimension(:,:), allocatable :: t0_tp_coupling
        complex(kind=fp), dimension(:,:), allocatable :: t0_tm_coupling
        complex(kind=fp), dimension(:,:), allocatable :: s_t0_coupling
        complex(kind=fp), dimension(:,:), allocatable :: tm_t0_coupling
        complex(kind=fp), dimension(:,:), allocatable :: tm_tm_coupling
        complex(kind=fp), dimension(:,:), allocatable :: s_tm_coupling
        !! contracted (root-mean-square) SOC
        real(kind=fp), dimension(:,:), allocatable :: s_t_coupling
        real(kind=fp), dimension(:,:), allocatable :: t_t_coupling
    end type

    interface
        module subroutine init_SOC(soc, n)
            type(soc_helper), intent(inout) :: soc
            integer, intent(in) :: n
        end subroutine init_SOC
        module subroutine read_SOC(soc, unit)
            type(soc_helper), intent(inout) :: soc
            integer, intent(in) :: unit
        end subroutine read_SOC
        module subroutine print_SOC(soc, filename)
            type(soc_helper), intent(in) :: soc
            character(len=*), intent(in) :: filename
        end subroutine print_SOC
    end interface
!===============================================================================

contains

    subroutine QCkits_extract_SOC()
        !! extract SOC matrix elements

        !! locals
        type(soc_helper) :: the_soc
        logical :: found
        character(len=20) :: buffer
        integer :: n

        write(output_unit,"('Reading SOC...')")
        call qchem_file%open()

        call locate_label(found, qchem_file%get_unit(), "cis_n_roots", matchcase=.false.)
        read(qchem_file%get_unit(),*) buffer, n

        call init_SOC(the_soc, n)
        call read_SOC(the_soc, qchem_file%get_unit())

        call qchem_file%close()

        write(output_unit,"('Done. If you need to save them in file (socme.dat), press ENTER')")
        read(input_unit,*)
        call print_SOC(the_soc, "socme.dat")

    end subroutine QCkits_extract_SOC

end module QCkits_properties
