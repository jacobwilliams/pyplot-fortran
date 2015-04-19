!*****************************************************************************************
    program test
!*****************************************************************************************
!****u* pyplotfortran/test
!
!  NAME
!    test
!
!  DESCRIPTION
!    Unit test for pyplot_module.
!
!  AUTHOR
!    Jacob Williams : 4/14/2015
!
!  SOURCE

    use,intrinsic :: iso_fortran_env, only: wp => real64
    use pyplot_module

    implicit none

    real(wp),dimension(100) :: x,sx,cx,tx
    type(pyplot) :: plt
    integer :: i

    !generate some data:
    x = [(real(i,wp), i=0,size(x)-1)]/5.0_wp
    sx = sin(x)
    cx = cos(x)
    tx = sx * cx

    !2d line plot:
    call plt%initialize(grid=.true.,xlabel='angle (rad)',&
                        title='plot test',legend=.true.)
    call plt%add_plot(x,sx,label='$\sin (x)$',linestyle='b-o',markersize=5,linewidth=2)
    call plt%add_plot(x,cx,label='$\cos (x)$',linestyle='r-o',markersize=5,linewidth=2)
    call plt%add_plot(x,tx,label='$\sin (x) \cos (x)$',linestyle='g-o',markersize=2,linewidth=1)
    call plt%savefig('plottest.png', pyfile='plottest.py')

    !bar chart:
    tx = 0.1_wp !for bar width
    call plt%initialize(grid=.true.,xlabel='angle (rad)',&
                        title='bar test',legend=.true.,figsize=[20,5],&
                        font_size = 20,&
                        axes_labelsize = 20,&
                        xtick_labelsize = 20,&
                        ytick_labelsize = 20,&
                        legend_fontsize = 20 )
    call plt%add_bar(left=x,height=sx,width=tx,label='$\sin (x)$',color='r')
    call plt%savefig('bartest.png', pyfile='bartest.py')

    end program test
!*****************************************************************************************