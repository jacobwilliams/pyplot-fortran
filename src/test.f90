!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/14/2015
!  license: BSD
!
!  Unit test for [[pyplot_module]].

    program test

    use, intrinsic :: iso_fortran_env, only : wp => real64
    use pyplot_module, only : pyplot

    implicit none

    integer,parameter :: n = 100

    real(wp), dimension(n)   :: x     !! x values
    real(wp), dimension(n)   :: y     !! y values
    real(wp), dimension(n)   :: yerr  !! error values for bar chart
    real(wp), dimension(n)   :: sx    !! sin(x) values
    real(wp), dimension(n)   :: cx    !! cos(x) values
    real(wp), dimension(n)   :: tx    !! sin(x)*cos(x) values
    real(wp), dimension(n,n) :: z     !! z matrix for contour plot
    type(pyplot)             :: plt   !! pytplot handler
    integer                  :: i     !! counter
    integer                  :: j     !! counter
    real(wp)                 :: r2    !! temp variable

    !generate some data:
    x    = [(real(i,wp), i=0,size(x)-1)]/5.0_wp
    sx   = sin(x)
    cx   = cos(x)
    tx   = sx * cx
    yerr = abs(sx*.25_wp)

    !2d line plot:
    call plt%initialize(grid=.true.,xlabel='angle (rad)',figsize=[20,10],&
                        title='plot test',legend=.true.,axis_equal=.true.)
    call plt%add_plot(x,sx,label='$\sin (x)$',linestyle='b-o',markersize=5,linewidth=2)
    call plt%add_plot(x,cx,label='$\cos (x)$',linestyle='r-o',markersize=5,linewidth=2)
    call plt%add_plot(x,tx,label='$\sin (x) \cos (x)$',linestyle='g-o',markersize=2,linewidth=1)
    call plt%savefig('plottest.png', pyfile='plottest.py')

    !bar chart:
    tx = 0.1_wp !for bar width
    call plt%initialize(grid=.true.,xlabel='angle (rad)',&
                        title='bar test',legend=.true.,figsize=[20,10],&
                        font_size = 20,&
                        axes_labelsize = 20,&
                        xtick_labelsize = 20,&
                        ytick_labelsize = 20,&
                        legend_fontsize = 20 )
    call plt%add_bar(left=x,height=sx,width=tx,label='$\sin (x)$',&
                        color='r',yerr=yerr,xlim=[0.0_wp, 20.0_wp],align='center')
    call plt%savefig('bartest.png', pyfile='bartest.py')

    !contour plot:
    x = [(real(i,wp), i=0,n-1)]/100.0_wp
    y = [(real(i,wp), i=0,n-1)]/100.0_wp
    do i=1,n
        do j=1,n
            r2 = x(i)**2 + y(j)**2
            z(i,j) = sin(x(i))*cos(y(j))*sin(r2)/(1.0_wp+log(r2+1.0_wp))
        end do
    end do
    call plt%initialize(grid=.true.,xlabel='x angle (rad)',ylabel='y angle (rad)',figsize=[10,10],&
                        title='Contour plot test', real_fmt='*')
    call plt%add_contour(x, y, z, label='contour', linestyle='-', linewidth=2, filled=.true., cmap='bone')
    call plt%savefig('contour.png',pyfile='contour.py')

    end program test
!*****************************************************************************************
