module QCkits_test_menu
    use QCkits_job_item
    implicit none
    private
    public :: test_menu
    
contains
    
    subroutine test_menu()
        type(qckits_menu_t), pointer :: main
        type(qckits_menu_t), pointer :: sub_menu1
        type(qckits_menu_t), pointer :: sub_menu2

        allocate(sub_menu1)
        call sub_menu1%init("this is sub_menu1")
        call sub_menu1%add_option(new_job(sub1, "this is sub1"), 1)
        call sub_menu1%add_option(new_job(sub2, "this is sub2"), 3)

        allocate(sub_menu2)
        call sub_menu2%init("this is sub_menu2")
        call sub_menu2%add_option(new_job(sub3, "this is sub3"), 1)
        call sub_menu2%add_option(new_job(sub4, "this is sub4"), 2)

        allocate(main)
        call main%init("this is main")
        call main%add_option(sub_menu1, 1)
        call main%add_option(sub_menu2, 2)
        call main%add_option(new_job(sub5, "this is sub5"), 5)

        deallocate(main)

    end subroutine test_menu


    subroutine sub1()
        write(*,*) "sub1"
    end subroutine sub1

    subroutine sub2()
        write(*,*) "sub2"
    end subroutine sub2
    
    subroutine sub3()
        write(*,*) "sub3"
    end subroutine sub3

    subroutine sub4()
        write(*,*) "sub4"
    end subroutine sub4

    subroutine sub5()
        write(*,*) "sub5"
    end subroutine sub5

end module QCkits_test_menu