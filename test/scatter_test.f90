!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  Test of the add_scatter routine.

    program scatter_test

    use pyplot_module
    use iso_fortran_env, only: wp => real64

    implicit none

    type(pyplot) :: plt
    real(wp), dimension(50) :: x1, y1, x2, y2, x3, y3
    real(wp), dimension(3) :: color_red, color_blue, color_green
    integer :: i
    integer :: istat
    real(wp) :: pi

    pi = acos(-1.0_wp)

    ! Define some colors
    color_red   = [1.0_wp, 0.0_wp, 0.0_wp]
    color_blue  = [0.0_wp, 0.0_wp, 1.0_wp]
    color_green = [0.0_wp, 0.7_wp, 0.0_wp]

    ! Generate some data - three different datasets
    do i = 1, 50
        x1(i) = real(i-1, wp) / 10.0_wp
        y1(i) = sin(x1(i)) + 0.1_wp * (real(i, wp) / 50.0_wp - 0.5_wp)

        x2(i) = real(i-1, wp) / 10.0_wp + 0.2_wp
        y2(i) = cos(x2(i)) + 0.15_wp * (real(i, wp) / 50.0_wp - 0.5_wp)

        x3(i) = real(i-1, wp) / 10.0_wp + 0.1_wp
        y3(i) = sin(x3(i) * 2.0_wp) * 0.5_wp + 0.1_wp * (real(i, wp) / 50.0_wp - 0.5_wp)
    end do

    ! Initialize the plot
    call plt%initialize(grid=.true., &
                        xlabel='X axis', &
                        ylabel='Y axis', &
                        title='Scatter Plot Demo', &
                        legend=.true., &
                        figsize=[10,8])

    ! Add first scatter plot with custom size and color
    call plt%add_scatter(x1, y1, &
                         label='Dataset 1', &
                         s=80, &
                         marker='o', &
                         color=color_red, &
                         alpha=0.6_wp, &
                         istat=istat)

    if (istat /= 0) then
        write(*,*) 'Error in first scatter plot'
        stop
    end if

    ! Add second scatter plot with different marker and color
    call plt%add_scatter(x2, y2, &
                         label='Dataset 2', &
                         s=60, &
                         marker='s', &
                         color=color_blue, &
                         alpha=0.7_wp, &
                         edgecolors='black', &
                         linewidths=1, &
                         istat=istat)

    if (istat /= 0) then
        write(*,*) 'Error in second scatter plot'
        stop
    end if

    ! Add third scatter plot with triangular markers
    call plt%add_scatter(x3, y3, &
                         label='Dataset 3', &
                         s=50, &
                         marker='^', &
                         color=color_green, &
                         alpha=0.5_wp, &
                         istat=istat)

    if (istat /= 0) then
        write(*,*) 'Error in third scatter plot'
        stop
    end if

    ! Save the figure
    call plt%savefig('test/scatter_test.png', istat=istat)

    if (istat /= 0) then
        write(*,*) 'Error saving figure'
    else
        write(*,*) 'Figure saved successfully: test/scatter_test.png'
    end if

    end program scatter_test
