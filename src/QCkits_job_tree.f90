module QCkits_job_tree
    use QCkits_job_item
    !!
    use QCkits_excited_state
    !!
    implicit none
    private
    public :: qckits_main_menu, init_menu, destroy_menu

!===============================================================================
! head
    type(qckits_menu_t), pointer :: qckits_main_menu

!===============================================================================
! the second layer
    type(qckits_menu_t), pointer :: excited_state_menu
    type(qckits_menu_t), pointer :: properties_menu

!===============================================================================
! the third layer


contains

    subroutine init_menu()
        allocate(qckits_main_menu)
        call qckits_main_menu%init("QCkits Main Menu")
        call build_main_menu()
    end subroutine init_menu


    subroutine destroy_menu()
        deallocate(qckits_main_menu)
    end subroutine destroy_menu


!===============================================================================
! define the construction of menu
!===============================================================================
! head
    subroutine build_main_menu()

        call build_excited_menu()

        call qckits_main_menu%add_option(excited_state_menu, 1)

    end subroutine build_main_menu

!===============================================================================
! the second layer
    subroutine build_excited_menu()

        allocate(excited_state_menu)
        call excited_state_menu%init("Excited State Analysis and Utilities")
        call excited_state_menu%add_option(new_job(QCkits_extract_excitation_energy, &
                                           "Extract Excitation Energy"), &
                                           index=extract_excitation_energy)
        call excited_state_menu%add_option(new_job(QCkits_simplify_cis_ampl_print, &
                                           "Simplify CIS (and RPA) Amplitude Print"), &
                                           index=simplify_cis_ampl_print)

    end subroutine build_excited_menu


!===============================================================================
! the third layer







end module QCkits_job_tree