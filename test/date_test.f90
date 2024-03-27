!*****************************************************************************************
!>
!  Unit test for [[pyplot_module]]. Using the `xaxis_date_fmt` option for x-axis dates.

    program date_test

    use pyplot_module, only : pyplot, wp => pyplot_wp

    implicit none

    integer,parameter :: n = 100

    real(wp), dimension(:),allocatable   :: x     !! x values
    real(wp), dimension(:),allocatable   :: y     !! y values
    real(wp), dimension(:),allocatable   :: sx    !! sin(x) values
    real(wp), dimension(:),allocatable   :: cx    !! cos(x) values
    real(wp), dimension(:),allocatable   :: tx    !! sin(x)*cos(x) values
    type(pyplot) :: plt   !! pytplot handler
    integer      :: i     !! counter
    integer      :: istat !! status code

    character(len=*), parameter :: testdir = "test/"
    character(len=*), parameter :: xaxis_date_fmt = '%m/%d/%y %H:%M:%S'
    character(len=*), parameter :: yaxis_date_fmt = '%H:%M:%S'

    ! size arrays:
    allocate(x(n))
    allocate(sx(n))
    allocate(cx(n))
    allocate(tx(n))

    !generate some data:
    x    = [(real(i,wp), i=0,size(x)-1)]/5.0_wp
    sx   = 10*sin(x)
    cx   = cos(x)
    tx   = sx * cx

    !2d line plot:
    call plt%initialize(grid=.true.,xlabel='Calendar date',figsize=[20,10],&
                        title='date test',legend=.true.,axis_equal=.true.,&
                        tight_layout=.true., &
                        xaxis_date_fmt=xaxis_date_fmt, yaxis_date_fmt=yaxis_date_fmt)
    call plt%add_plot(x,sx,label='$\sin (x)$',linestyle='b-o',markersize=5,linewidth=2,istat=istat)
    call plt%add_plot(x,cx,label='$\cos (x)$',linestyle='r-o',markersize=5,linewidth=2,istat=istat)
    call plt%add_plot(x,tx,label='$\sin (x) \cos (x)$',linestyle='g-o',markersize=2,linewidth=1,istat=istat)
    call plt%savefig(testdir//'datetest.png', pyfile=testdir//'datetest.py',&
                     istat=istat)

    end program date_test
!*****************************************************************************************
