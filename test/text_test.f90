!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  Test of the add_text routine.

    program text_test

    use pyplot_module
    use iso_fortran_env, only: wp => real64

    implicit none

    type(pyplot) :: plt
    real(wp), dimension(100) :: x, y
    real(wp) :: pi
    integer :: i
    integer :: istat

    pi = acos(-1.0_wp)

    ! Generate some data
    do i = 1, 100
        x(i) = real(i-1, wp) * 2.0_wp * pi / 99.0_wp
        y(i) = sin(x(i))
    end do

    ! Initialize the plot
    call plt%initialize(grid=.true., xlabel='x', ylabel='sin(x)', &
                        title='Text Annotation Demo', &
                        legend=.false., figsize=[10,6])

    ! Add the main plot
    call plt%add_plot(x, y, label='sin(x)', linestyle='b-', linewidth=2)

    ! Add various text annotations with different styles
    call plt%add_text(pi/2.0_wp, 1.0_wp, 'Peak at π/2', &
                      fontsize=12, &
                      color='red', &
                      horizontalalignment='center', &
                      verticalalignment='bottom', &
                      istat=istat)

    call plt%add_text(3.0_wp*pi/2.0_wp, -1.0_wp, 'Minimum at 3π/2', &
                      fontsize=12, &
                      color='blue', &
                      horizontalalignment='center', &
                      verticalalignment='top', &
                      istat=istat)

    call plt%add_text(pi, 0.0_wp, 'Zero crossing', &
                      fontsize=10, &
                      color='green', &
                      rotation=45.0_wp, &
                      horizontalalignment='left', &
                      verticalalignment='bottom', &
                      istat=istat)

    call plt%add_text(0.5_wp, -0.8_wp, 'Bold text', &
                      fontsize=14, &
                      color='purple', &
                      fontweight='bold', &
                      istat=istat)

    call plt%add_text(5.0_wp, 0.5_wp, 'Italic text', &
                      fontsize=11, &
                      color='orange', &
                      fontstyle='italic', &
                      istat=istat)

    call plt%add_text(4.0_wp, -0.5_wp, 'Semi-transparent', &
                      fontsize=10, &
                      color='black', &
                      alpha=0.5_wp, &
                      istat=istat)

    ! Save the figure
    call plt%savefig('test/text_test.png', istat=istat)

    if (istat /= 0) then
        write(*,*) 'Error saving figure'
    else
        write(*,*) 'Figure saved successfully: test/text_test.png'
    end if

    end program text_test
