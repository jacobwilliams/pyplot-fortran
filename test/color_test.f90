!*****************************************************************************************
!>
!  Color test

    program color_test

    use pyplot_module, only : pyplot, wp => pyplot_wp

    implicit none
    
    type(pyplot) :: plt   !! pytplot handler
    integer :: istat
    real(wp), parameter :: F(3) = [0.4510d0, 0.3098d0, 0.5882d0] ! Fortran-lang color 
    real(wp), parameter :: Y(3) = [0.9608d0, 0.8157d0, 0.0118d0] ! Yellow
 
    real(wp),dimension(3),parameter :: Ax = [1,2,3]
    real(wp),dimension(3),parameter :: Ay = [1,2,3]
    real(wp),dimension(3),parameter :: Bx = [1,2,3]
    real(wp),dimension(3),parameter :: By = [4,5,6]

    call plt%initialize(figsize=[20,10],title='color test')

    call plt%add_plot(Ax,Ay,label='',linestyle='o',markersize=5,color=F)
    call plt%add_plot(Bx,By,label='',linestyle='o',markersize=5,color=Y)

    call plt%savefig('color_test.png', pyfile='color_test.py',istat=istat)

    end program color_test
!*****************************************************************************************
