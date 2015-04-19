!*****************************************************************************************
    module pyplot_module
!*****************************************************************************************
!****h* pyplotfortran/pyplot_module
!
!  NAME
!    pyplot_module
!
!  DESCRIPTION
!    For making simple x-y plots from Fortran.
!    It works by generating a Python script and executing it.
!
!  SEE ALSO
!    Inspired by these:
!    * http://pypi.python.org/pypi/EasyPlot/1.0.0
!    * http://nbviewer.ipython.org/github/HamsterHuey/easyplot/blob/master/docs/easyplot_docs.ipynb
!
!  AUTHOR
!    Jacob Williams
!
!  HISTORY
!    * Jacob Williams : Created : 4/14/2015
!
!*****************************************************************************************

    !*********************************************************
    !****d* pyplot_module/wp
    !
    !  NAME
    !    wp
    !
    !  DESCRIPTION
    !    Default real kind [8 bytes].
    !
    !  SOURCE
    use,intrinsic :: iso_fortran_env, only: wp => real64
    !*********************************************************

    implicit none

    !*********************************************************
    !****d* pyplot_module/tmp_file
    !
    !  NAME
    !    tmp_file
    !
    !  DESCRIPTION
    !    Default name of the temporary file 
    !    (this can also be user-specified).
    !
    !  SOURCE
    character(len=*),parameter :: tmp_file = 'pyplot_module_temp_1234567890.py'
    !*********************************************************

    !*********************************************************
    !****d* pyplot_module/python_exe
    !
    !  NAME
    !    python_exe
    !
    !  DESCRIPTION
    !    The python executable name.
    !
    !  SOURCE
    character(len=*),parameter :: python_exe ='python'
    !*********************************************************

    character(len=*),parameter :: int_fmt      = '(I10)'      !integer format string
    integer,parameter          :: max_int_len  = 10           !max string length for integers
    character(len=*),parameter :: real_fmt     = '(E30.16)'   !real number format string
    integer,parameter          :: max_real_len = 30           !max string length for reals

    !*********************************************************
    !****c* pyplot_module/pyplot
    !
    !  NAME
    !    pyplot
    !
    !  DESCRIPTION
    !    The main class.
    !
    !  SOURCE

    type,public :: pyplot

        private

        character(len=:),allocatable :: str  !string buffer

        logical :: show_legend = .false.
        logical :: use_numpy   = .true.

    contains

        procedure,public :: initialize
        procedure,public :: add_plot
        procedure,public :: add_bar
        procedure,public :: savefig
        procedure,public :: destroy

        procedure :: execute
        procedure :: add_str

    end type pyplot
    !*********************************************************

    contains
!*****************************************************************************************

!*****************************************************************************************
!****f* pyplot_module/destroy
!
!  NAME  
!    destroy
!
!  DESCRIPTION
!    Destructor
!
!  SOURCE

    subroutine destroy(me)

    implicit none

    class(pyplot),intent(inout) :: me

    if (allocated(me%str)) deallocate(me%str)

    end subroutine destroy
!*****************************************************************************************

!*****************************************************************************************
!****f* pyplot_module/add_str
!
!  NAME  
!    add_str
!
!  DESCRIPTION
!    Add a string to the buffer.
!
!  SOURCE

    subroutine add_str(me,str)

    implicit none

    class(pyplot),intent(inout) :: me
    character(len=*),intent(in) :: str
    
    me%str = me%str//str//new_line(' ')

    end subroutine add_str
!*****************************************************************************************

!*****************************************************************************************
!****f* pyplot_module/initialize
!
!  NAME  
!    initialize
!
!  DESCRIPTION
!    initialize a plot
!
!  SOURCE

    subroutine initialize(me,grid,xlabel,ylabel,title,legend,use_numpy,figsize,&
                          font_size,axes_labelsize,xtick_labelsize,ytick_labelsize,legend_fontsize)

    implicit none

    class(pyplot),intent(inout)              :: me
    logical,intent(in),optional              :: grid
    character(len=*),intent(in),optional     :: xlabel
    character(len=*),intent(in),optional     :: ylabel
    character(len=*),intent(in),optional     :: title
    logical,intent(in),optional              :: legend
    logical,intent(in),optional              :: use_numpy
    integer,dimension(2),intent(in),optional :: figsize
    integer,intent(in),optional              :: font_size
    integer,intent(in),optional              :: axes_labelsize
    integer,intent(in),optional              :: xtick_labelsize
    integer,intent(in),optional              :: ytick_labelsize
    integer,intent(in),optional              :: legend_fontsize

    character(len=max_int_len) :: width_str, height_str,font_size_str,&
                                  axes_labelsize_str,xtick_labelsize_str,&
                                  ytick_labelsize_str,legend_fontsize_str

    character(len=*),parameter :: default_font_size_str = '10'  !the default font size for plots

    call me%destroy()

    if (present(legend)) then
        me%show_legend = legend
    else
        me%show_legend = .false.
    end if
    if (present(use_numpy)) then
        me%use_numpy = use_numpy
    else
        me%use_numpy = .true.
    end if
    if (present(figsize)) then
        call integer_to_string(figsize(1), width_str)
        call integer_to_string(figsize(2), height_str)
    end if
    call optional_int_to_string(font_size,      font_size_str,      default_font_size_str)
    call optional_int_to_string(axes_labelsize, axes_labelsize_str, default_font_size_str)
    call optional_int_to_string(xtick_labelsize,xtick_labelsize_str,default_font_size_str)
    call optional_int_to_string(ytick_labelsize,ytick_labelsize_str,default_font_size_str)
    call optional_int_to_string(legend_fontsize,legend_fontsize_str,default_font_size_str)

    me%str = ''

    call me%add_str('#!/usr/bin/python')
    call me%add_str('')

    call me%add_str('import matplotlib')
    call me%add_str('import matplotlib.pyplot as plt')
    if (me%use_numpy) call me%add_str('import numpy as np')
    call me%add_str('')

    call me%add_str('matplotlib.rcParams["font.family"] = "Serif"')
    call me%add_str('matplotlib.rcParams["font.size"] = '//trim(font_size_str))
    call me%add_str('matplotlib.rcParams["axes.labelsize"] = '//trim(axes_labelsize_str))
    call me%add_str('matplotlib.rcParams["xtick.labelsize"] = '//trim(xtick_labelsize_str))
    call me%add_str('matplotlib.rcParams["ytick.labelsize"] = '//trim(ytick_labelsize_str))
    call me%add_str('matplotlib.rcParams["legend.fontsize"] = '//trim(legend_fontsize_str))

    call me%add_str('')

    if (present(figsize)) then  !if specifying the figure size
        call me%add_str('fig = plt.figure(figsize=('//trim(width_str)//','//trim(height_str)//'))')
    else
        call me%add_str('fig = plt.figure()')
    end if
    call me%add_str('ax = fig.gca()')

    if (present(grid)) then
        if (grid) call me%add_str('ax.grid()')
    end if

    if (present(xlabel)) call me%add_str('ax.set_xlabel("'//trim(xlabel)//'")')
    if (present(ylabel)) call me%add_str('ax.set_ylabel("'//trim(ylabel)//'")')
    if (present(title))  call me%add_str('ax.set_title("' //trim(title) //'")')

    call me%add_str('')

    end subroutine initialize
!*****************************************************************************************

!*****************************************************************************************
!****f* pyplot_module/add_plot
!
!  NAME  
!    add_plot
!
!  DESCRIPTION
!    Add an x,y plot.
!
!  SOURCE

    subroutine add_plot(me,x,y,label,linestyle,markersize,linewidth)

    implicit none

    class(pyplot),intent(inout)      :: me
    real(wp),dimension(:),intent(in) :: x
    real(wp),dimension(:),intent(in) :: y
    character(len=*),intent(in)      :: label
    character(len=*),intent(in)      :: linestyle
    integer,intent(in),optional      :: markersize
    integer,intent(in),optional      :: linewidth
    
    character(len=:),allocatable :: xstr,ystr
    character(len=max_int_len) :: imark,iline

    character(len=*),parameter :: xname = 'x'    !variable names for script
    character(len=*),parameter :: yname = 'y'    !

    if (allocated(me%str)) then

        !convert the arrays to strings:
        call vec_to_string(x,xstr,me%use_numpy)
        call vec_to_string(y,ystr,me%use_numpy)

        !get optional inputs (if not present, set default value):
        call optional_int_to_string(markersize,imark,'3')
        call optional_int_to_string(linewidth, iline,'3')

        !write the arrays:
        call me%add_str(trim(xname)//' = '//xstr)
        call me%add_str(trim(yname)//' = '//ystr)
        call me%add_str('')

        !write the plot statement:
        call me%add_str('ax.plot('//&
                        trim(xname)//','//&
                        trim(yname)//','//&
                        '"'//trim(linestyle)//'",'//&
                        'linewidth='//trim(adjustl(iline))//','//&
                        'markersize='//trim(adjustl(imark))//','//&
                        'label="'//trim(label)//'")')
        call me%add_str('')

    else
        error stop 'Error in add_plot: pyplot class not properly initialized.'
    end if

    end subroutine add_plot
!*****************************************************************************************

!*****************************************************************************************
!****f* pyplot_module/add_bar
!
!  NAME  
!    add_bar
!
!  DESCRIPTION
!    Add a bar plot.
!
!  SOURCE

    subroutine add_bar(me,left,height,label,width,bottom,color)

    implicit none

    class(pyplot),intent(inout)               :: me
    real(wp),dimension(:),intent(in)          :: left
    real(wp),dimension(:),intent(in)          :: height
    character(len=*),intent(in)               :: label
    real(wp),dimension(:),intent(in),optional :: width
    real(wp),dimension(:),intent(in),optional :: bottom
    character(len=*),intent(in),optional      :: color

    character(len=:),allocatable :: xstr,ystr,wstr,bstr,plt_str

    character(len=*),parameter :: xname = 'x'    !variable names for script
    character(len=*),parameter :: yname = 'y'    !
    character(len=*),parameter :: wname = 'w'    !
    character(len=*),parameter :: bname = 'b'    !

    if (allocated(me%str)) then

        !convert the arrays to strings:
                             call vec_to_string(left,xstr,  me%use_numpy)
                             call vec_to_string(height,ystr,me%use_numpy)
        if (present(width))  call vec_to_string(width,wstr, me%use_numpy)
        if (present(bottom)) call vec_to_string(bottom,bstr,me%use_numpy)

        !write the arrays:
                             call me%add_str(trim(xname)//' = '//xstr)
                             call me%add_str(trim(yname)//' = '//ystr)
        if (present(width))  call me%add_str(trim(wname)//' = '//wstr)
        if (present(bottom)) call me%add_str(trim(bname)//' = '//bstr)
        call me%add_str('')

        !create the plot string:
        plt_str = 'ax.bar('//&
                  'left='//trim(xname)//','//&
                  'height='//trim(yname)//','
        if (present(width))  plt_str=plt_str//'width='//trim(wname)//','
        if (present(bottom)) plt_str=plt_str//'bottom='//trim(bstr)//','
        if (present(color))  plt_str=plt_str//'color="'//trim(color)//'",'
        plt_str=plt_str//'label="'//trim(label)//'")'

        !write the plot statement:
        call me%add_str(plt_str)
        call me%add_str('')

    else
        error stop 'Error in add_bar: pyplot class not properly initialized.'
    end if

    end subroutine add_bar
!*****************************************************************************************

!*****************************************************************************************
!****f* pyplot_module/optional_int_to_string
!
!  NAME  
!    optional_int_to_string
!
!  DESCRIPTION
!    Integer to string, specifying the default value if 
!    the optional argument is not present.
!
!  SOURCE

    subroutine optional_int_to_string(int_value,string_value,default_value)

    implicit none

    integer,intent(in),optional  :: int_value
    character(len=*),intent(out) :: string_value
    character(len=*),intent(in)  :: default_value

    if (present(int_value)) then
        call integer_to_string(int_value, string_value)
    else
        string_value = default_value
    end if

    end subroutine optional_int_to_string
!*****************************************************************************************

!*****************************************************************************************
!****f* pyplot_module/integer_to_string
!
!  NAME  
!    integer_to_string
!
!  DESCRIPTION
!    Integer to string conversion.
!
!  SOURCE

    subroutine integer_to_string(i,s)

    implicit none

    integer,intent(in),optional  :: i
    character(len=*),intent(out) :: s

    integer :: istat

    write(s,int_fmt,iostat=istat) i

    if (istat/=0) then
        error stop 'Error converting integer to string'
    else
        s = adjustl(s)
    end if

    end subroutine integer_to_string
!*****************************************************************************************

!*****************************************************************************************
!****f* pyplot_module/vec_to_string
!
!  NAME  
!    vec_to_string
!
!  DESCRIPTION
!    Real vector to string.
!
!  SOURCE

    subroutine vec_to_string(v,str,use_numpy)

    implicit none

    real(wp),dimension(:),intent(in)         :: v
    character(len=:),allocatable,intent(out) :: str
    logical,intent(in)                       :: use_numpy

    integer :: i,istat
    character(len=max_real_len) :: tmp

    str = '['
    do i=1,size(v)
        write(tmp,real_fmt,iostat=istat) v(i)
        if (istat/=0) error stop 'Error in vec_to_string'
        str = str//trim(adjustl(tmp))
        if (i<size(v)) str = str // ','
    end do
    str = str // ']'

    !convert to numpy array if necessary:
    if (use_numpy) str = 'np.array('//str//')'

    end subroutine vec_to_string
!*****************************************************************************************

!*****************************************************************************************
!****f* pyplot_module/execute
!
!  NAME  
!    execute
!
!  DESCRIPTION
!    Write the buffer to a file, and then execute it with Python.
!
!  SOURCE

    subroutine execute(me,pyfile)

    implicit none

    class(pyplot),intent(inout) :: me
    character(len=*),intent(in),optional :: pyfile  !name of the python script to generate

    integer :: istat,iunit
    character(len=:),allocatable :: file

    if (allocated(me%str)) then

        !file name for python script:
        if (present(pyfile)) then
            file = trim(pyfile)    !use the user-specified name
        else
            file = trim(tmp_file)  !use the default
        end if

        !generate the file:
        open(newunit=iunit,file=file,status='REPLACE',iostat=istat)
        if (istat/=0) error stop 'Error creating python script.'
        write(iunit,'(A)') me%str
        close(iunit,iostat=istat)

        !run the file using python:
        call execute_command_line(python_exe//' '//file)

        !cleanup:
        deallocate(file)

    end if

    end subroutine execute
!*****************************************************************************************

!*****************************************************************************************
!****f* pyplot_module/savefig
!
!  NAME  
!    savefig
!
!  DESCRIPTION
!    Save the figure.
!
!  SOURCE

    subroutine savefig(me,figfile,pyfile)

    implicit none

    class(pyplot),intent(inout) :: me
    character(len=*),intent(in) :: figfile  !file name for the figure
    character(len=*),intent(in),optional :: pyfile  !name of the python script to generate

    if (allocated(me%str)) then

        !finish up the string:
        if (me%show_legend) then
            call me%add_str('ax.legend(loc="best")')
            call me%add_str('')
        end if
        call me%add_str('plt.savefig("'//trim(figfile)//'")')

        !run it:
        call me%execute(pyfile)

    else
        error stop 'error in savefig: pyplot class not properly initialized.'
    end if

    end subroutine savefig
!*****************************************************************************************

!*****************************************************************************************
    end module pyplot_module
!*****************************************************************************************