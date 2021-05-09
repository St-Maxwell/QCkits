submodule (QCkits_properties) SOCME
    use QCkits_global, only: fp
    use QCkits_utils, only: locate_label, fill_lower_triangular, print_matrix, remove_char
    implicit none

contains

    module subroutine init_SOC(soc, n)
        type(soc_helper), intent(inout) :: soc
        integer, intent(in) :: n

        soc%num_states = n
        allocate(soc%tp_tp_coupling(n,n), source=cmplx(0,0,fp))
        allocate(soc%tp_t0_coupling(n,n), source=cmplx(0,0,fp))
        allocate(soc%s_tp_coupling(n,n), source=cmplx(0,0,fp))
        allocate(soc%t0_tp_coupling(n,n), source=cmplx(0,0,fp))
        allocate(soc%t0_tm_coupling(n,n), source=cmplx(0,0,fp))
        allocate(soc%s_t0_coupling(n,n), source=cmplx(0,0,fp))
        allocate(soc%tm_t0_coupling(n,n), source=cmplx(0,0,fp))
        allocate(soc%tm_tm_coupling(n,n), source=cmplx(0,0,fp))
        allocate(soc%s_tm_coupling(n,n), source=cmplx(0,0,fp))
        allocate(soc%s_t_coupling(n,n), source=0._fp)
        allocate(soc%t_t_coupling(n,n), source=0._fp)

    end subroutine init_SOC


    module subroutine read_SOC(soc, unit)
        type(soc_helper), intent(inout) :: soc
        integer, intent(in) :: unit

        call read_SOC_tt(soc, unit)
        call read_SOC_st(soc, unit)
        call contract_SOC(soc)

    end subroutine read_SOC


    subroutine read_SOC_tt(soc, unit)
        type(soc_helper), intent(inout) :: soc
        integer, intent(in) :: unit

        !! locals
        logical :: found
        integer :: i

        call locate_label(found, unit, "SPIN-ORBIT COUPLING BETWEEN EXCITED TRIPLET STATES")
        read(unit,*)
        read(unit,*)
        read(unit,*)

        do i = 1, soc%num_states
            call parse_tt_sections(soc, unit, i)
        end do

        ! complete lower triangular part
        call fill_lower_triangular(soc%tp_tp_coupling)
        call fill_lower_triangular(soc%tp_t0_coupling)
        call fill_lower_triangular(soc%t0_tp_coupling)
        call fill_lower_triangular(soc%t0_tm_coupling)
        call fill_lower_triangular(soc%tm_t0_coupling)
        call fill_lower_triangular(soc%tm_tm_coupling)

    end subroutine read_SOC_tt

    !! parse the section for root [state]
    !! contains 6 subsections
    !! order:
    !!   ms =  0, ms =  1
    !!   ms =  0, ms = -1
    !!   ms =  1, ms =  0
    !!   ms =  1, ms =  1
    !!   ms = -1, ms =  0
    !!   ms = -1, ms = -1
    subroutine parse_tt_sections(soc, unit, state)
        type(soc_helper), intent(inout) :: soc
        integer, intent(in) :: unit
        integer, intent(in) :: state

        !! locals
        character(len=120) :: buffer
        integer :: i
        real(kind=fp) :: re, img

        ! subsection 1: ms =  0, ms =  1
        read(unit,"(A)") buffer
        do i = state + 1, soc%num_states
            read(unit,"(A)") buffer
            ! get real part and imaginary part
            call get_real_img(buffer, re, img)
            soc%t0_tp_coupling(state, i) = cmplx(re, img, kind=fp)
        end do
        read(unit,"(A)") buffer

        ! subsection 2: ms =  0, ms = -1
        read(unit,"(A)") buffer
        do i = state + 1, soc%num_states
            read(unit,"(A)") buffer
            ! get real part and imaginary part
            call get_real_img(buffer, re, img)
            soc%t0_tm_coupling(state, i) = cmplx(re, img, kind=fp)
        end do
        read(unit,"(A)") buffer

        ! subsection 3: ms =  1, ms =  0
        read(unit,"(A)") buffer
        do i = state + 1, soc%num_states
            read(unit,"(A)") buffer
            ! get real part and imaginary part
            call get_real_img(buffer, re, img)
            soc%tp_t0_coupling(state, i) = cmplx(re, img, kind=fp)
        end do
        read(unit,"(A)") buffer

        ! subsection 4: ms =  1, ms =  1
        read(unit,"(A)") buffer
        do i = state + 1, soc%num_states
            read(unit,"(A)") buffer
            ! get real part and imaginary part
            call get_real_img(buffer, re, img)
            soc%tp_tp_coupling(state, i) = cmplx(re, img, kind=fp)
        end do
        read(unit,"(A)") buffer

        ! subsection 5: ms = -1, ms =  0
        read(unit,"(A)") buffer
        do i = state + 1, soc%num_states
            read(unit,"(A)") buffer
            ! get real part and imaginary part
            call get_real_img(buffer, re, img)
            soc%tm_t0_coupling(state, i) = cmplx(re, img, kind=fp)
        end do
        read(unit,"(A)") buffer

        ! subsection 6: ms = -1, ms = -1
        read(unit,"(A)") buffer
        do i = state + 1, soc%num_states
            read(unit,"(A)") buffer
            ! get real part and imaginary part
            call get_real_img(buffer, re, img)
            soc%tm_tm_coupling(state, i) = cmplx(re, img, kind=fp)
        end do
        read(unit,"(A)") buffer

        ! skip remaining lines
        read(unit,"(A)") buffer
        do i = state + 1, soc%num_states
            read(unit,"(A)") buffer
        end do
        read(unit,"(A)") buffer

    end subroutine parse_tt_sections


    subroutine read_SOC_st(soc, unit)
        type(soc_helper), intent(inout) :: soc
        integer, intent(in) :: unit

        !! locals
        logical :: found
        integer :: i

        call locate_label(found, unit, "SPIN-ORBIT COUPLING BETWEEN EXCITED SINGLET STATES AND")
        read(unit,*)
        read(unit,*)
        read(unit,*)

        do i = 1, soc%num_states
            call parse_st_sections(soc, unit, i)
        end do

    end subroutine read_SOC_st

    !! parse the section for singlet root [state]
    !! contains 3 subsections
    !! order:
    !!   ms =  0
    !!   ms =  1
    !!   ms = -1
    subroutine parse_st_sections(soc, unit, state)
        type(soc_helper), intent(inout) :: soc
        integer, intent(in) :: unit
        integer, intent(in) :: state

        !! locals
        character(len=120) :: buffer
        integer :: i
        real(kind=fp) :: re, img

        !! subsection 1: ms =  0
        read(unit,"(A)") buffer
        do i = 1, soc%num_states
            read(unit,"(A)") buffer
            !! get real part and imaginary part
            call get_real_img(buffer, re, img)
            soc%s_t0_coupling(state, i) = cmplx(re, img, kind=fp)
        end do
        read(unit,"(A)") buffer

        !! subsection 2: ms =  1
        read(unit,"(A)") buffer
        do i = 1, soc%num_states
            read(unit,"(A)") buffer
            !! get real part and imaginary part
            call get_real_img(buffer, re, img)
            soc%s_tp_coupling(state, i) = cmplx(re, img, kind=fp)
        end do
        read(unit,"(A)") buffer

        !! subsection 3: ms = -1
        read(unit,"(A)") buffer
        do i = 1, soc%num_states
            read(unit,"(A)") buffer
            !! get real part and imaginary part
            call get_real_img(buffer, re, img)
            soc%s_tm_coupling(state, i) = cmplx(re, img, kind=fp)
        end do
        read(unit,"(A)") buffer

        !! skip remaining lines
        read(unit,"(A)") buffer
        do i = 1, soc%num_states
            read(unit,"(A)") buffer
        end do
        read(unit,"(A)") buffer

    end subroutine parse_st_sections


    subroutine get_real_img(string, r_, i_)
        character(len=*), intent(in) :: string
        real(kind=fp), intent(inout) :: r_
        real(kind=fp), intent(inout) :: i_
        character(len=15) :: buf1, buf2, buf3, buf4

        !! Tx(ms=x)        xxxxxxx +        xxxxxxxi  cm-1
        read(string,*) buf1, r_, buf2, buf3, buf4
        !! deal with buf3
        call remove_char(buf3, "i")
        read(buf3,*) i_
        if (buf2(1:1) == '-') i_ = -i_

    end subroutine get_real_img


    subroutine contract_SOC(soc)
        type(soc_helper), intent(inout) :: soc

        !! triplet and singlet
        soc%s_t_coupling = sqmod(soc%s_tp_coupling) &
                         + sqmod(soc%s_t0_coupling) &
                         + sqmod(soc%s_tm_coupling)
        soc%s_t_coupling = sqrt(soc%s_t_coupling)
        !! triplet and triplet
        soc%t_t_coupling = sqmod(soc%tp_tp_coupling) &
                         + sqmod(soc%tp_t0_coupling) &
                         + sqmod(soc%t0_tp_coupling) &
                         + sqmod(soc%t0_tm_coupling) &
                         + sqmod(soc%tm_t0_coupling) &
                         + sqmod(soc%tm_tm_coupling)
        soc%t_t_coupling = sqrt(soc%t_t_coupling)

    end subroutine contract_SOC


    module subroutine print_SOC(soc, filename)
        type(soc_helper), intent(in) :: soc
        character(len=*), intent(in) :: filename

        !! locals
        integer :: u

        open(newunit=u, file=filename)
        call print_matrix(soc%tp_tp_coupling, u, "T(1) and T(1)")
        call print_matrix(soc%tp_t0_coupling, u, "T(1) and T(0)")
        call print_matrix(soc%t0_tp_coupling, u, "T(0) and T(1)")
        call print_matrix(soc%t0_tm_coupling, u, "T(0) and T(-1)")
        call print_matrix(soc%tm_tm_coupling, u, "T(-1) and T(-1)")
        call print_matrix(soc%tm_t0_coupling, u, "T(-1) and T(0)")
        call print_matrix(soc%s_tp_coupling, u, "S and T(1)")
        call print_matrix(soc%s_t0_coupling, u, "S and T(0)")
        call print_matrix(soc%s_tm_coupling, u, "S and T(-1)")
        call print_matrix(soc%s_t_coupling, u, "S and T (S_I, T_J)")
        call print_matrix(soc%t_t_coupling, u, "T and T")
        close(u)

    end subroutine print_SOC


    elemental function sqmod(c)
        complex(kind=fp), intent(in) :: c
        real(kind=fp) :: sqmod

        sqmod = c%re * c%re + c%im * c%im

    end function sqmod

end submodule SOCME
