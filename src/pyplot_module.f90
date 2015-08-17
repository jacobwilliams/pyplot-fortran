!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/14/2015
!  license: BSD
!
!  For making simple x-y plots from Fortran.
!  It works by generating a Python script and executing it.
!
!# See also
!   * Inspired by: [EasyPlot](https://pypi.python.org/pypi/EasyPlot)

    module pyplot_module

    use, intrinsic :: iso_fortran_env, only : real64

    implicit none

    private

    integer, parameter, private :: wp = real64 !! Default real kind [8 bytes].

    character(len=*), parameter :: tmp_file = 'pyplot_module_temp_1234567890.py' !! Default name of the temporary file
                                                                                 !! (this can also be user-specified).

    character(len=*), parameter :: python_exe   ='python'    !! The python executable name.
    character(len=*), parameter :: int_fmt      = '(I10)'    !! integer format string
    integer, parameter          :: max_int_len  = 10         !! max string length for integers
    character(len=*), parameter :: real_fmt     = '(E30.16)' !! real number format string
    integer, parameter          :: max_real_len = 30         !! max string length for reals

    type, public :: pyplot
    
        !!  The main pyplot class.
        
        private

        character(len=:), allocatable :: str !! string buffer

        logical :: show_legend = .false.     !! show legend into plot
        logical :: use_numpy   = .true.      !! use numpy python module
        
    contains
    
        ! public methods
        procedure, public :: initialize !! initialize pyplot instance
        procedure, public :: add_plot   !! add a plot to pyplot instance
        procedure, public :: add_bar    !! add a barplot to pyplot instance
        procedure, public :: savefig    !! save plots of pyplot instance
        procedure, public :: destroy    !! destroy pyplot instance
        
        ! private methods
        procedure :: execute !! execute pyplot commands
        procedure :: add_str !! add string to pytplot instance buffer
        
    end type pyplot

    contains
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Destructor.

    subroutine destroy(me)
    
    class(pyplot),intent(inout) :: me !! pyplot handler

    if (allocated(me%str)) deallocate(me%str)
    
    end subroutine destroy
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Add a string to the buffer.

    subroutine add_str(me,str)
    
    class(pyplot),    intent(inout) :: me  !! pyplot handler
    character(len=*), intent(in)    :: str !! str to be added to pyplot handler buffer

    me%str = me%str//str//new_line(' ')
    
    end subroutine add_str
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Initialize a plot

    subroutine initialize(me, grid, xlabel, ylabel, title, legend, use_numpy, figsize, &
                          font_size, axes_labelsize, xtick_labelsize, ytick_labelsize, legend_fontsize)
                          
    class(pyplot),         intent(inout)        :: me                           !! pyplot handler
    logical,               intent(in), optional :: grid                         !! activate grid drawing
    character(len=*),      intent(in), optional :: xlabel                       !! label of x axis
    character(len=*),      intent(in), optional :: ylabel                       !! label of y axis
    character(len=*),      intent(in), optional :: title                        !! plot title
    logical,               intent(in), optional :: legend                       !! plot legend
    logical,               intent(in), optional :: use_numpy                    !! activate usage of numpy python module
    integer, dimension(2), intent(in), optional :: figsize                      !! dimension of the figure
    integer,               intent(in), optional :: font_size                    !! font size
    integer,               intent(in), optional :: axes_labelsize               !! size of axis labels
    integer,               intent(in), optional :: xtick_labelsize              !! size of x axis tick lables
    integer,               intent(in), optional :: ytick_labelsize              !! size of y axis tick lables
    integer,               intent(in), optional :: legend_fontsize              !! size of legend font
    character(len=max_int_len)                  :: width_str                    !! figure width dummy string
    character(len=max_int_len)                  :: height_str                   !! figure height dummy string
    character(len=max_int_len)                  :: font_size_str                !! font size dummy string
    character(len=max_int_len)                  :: axes_labelsize_str           !! size of axis labels dummy string
    character(len=max_int_len)                  :: xtick_labelsize_str          !! sise of x axis tick labels dummy string
    character(len=max_int_len)                  :: ytick_labelsize_str          !! sise of x axis tick labels dummy string
    character(len=max_int_len)                  :: legend_fontsize_str          !! sise of legend font dummy string
    character(len=*), parameter                 :: default_font_size_str = '10' !! the default font size for plots

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
    call optional_int_to_string(font_size, font_size_str, default_font_size_str)
    call optional_int_to_string(axes_labelsize, axes_labelsize_str, default_font_size_str)
    call optional_int_to_string(xtick_labelsize, xtick_labelsize_str, default_font_size_str)
    call optional_int_to_string(ytick_labelsize, ytick_labelsize_str, default_font_size_str)
    call optional_int_to_string(legend_fontsize, legend_fontsize_str, default_font_size_str)

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
!> author: Jacob Williams
!
! Add an x,y plot.

    subroutine add_plot(me, x, y, label, linestyle, markersize, linewidth)
    
    class(pyplot),          intent (inout)        :: me           !! pyplot handler
    real(wp), dimension(:), intent (in)           :: x            !! x values
    real(wp), dimension(:), intent (in)           :: y            !! y values
    character(len=*),       intent (in)           :: label        !! plot label
    character(len=*),       intent (in)           :: linestyle    !! style of the plot line
    integer,                intent (in), optional :: markersize   !! size of the plot markers
    integer,                intent (in), optional :: linewidth    !! width of the plot line
    character(len=:), allocatable                 :: xstr         !! x values strinfied
    character(len=:), allocatable                 :: ystr         !! y values strinfied
    character(len=max_int_len)                    :: imark        !! actual markers size
    character(len=max_int_len)                    :: iline        !! actual line width
    character(len=*), parameter                   :: xname = 'x'  !! x variable name for script
    character(len=*), parameter                   :: yname = 'y'  !! y variable name for script

    if (allocated(me%str)) then

        !convert the arrays to strings:
        call vec_to_string(x, xstr, me%use_numpy)
        call vec_to_string(y, ystr, me%use_numpy)

        !get optional inputs (if not present, set default value):
        call optional_int_to_string(markersize, imark, '3')
        call optional_int_to_string(linewidth, iline, '3')

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
!> author: Jacob Williams
!
! Add a bar plot.
    
    subroutine add_bar(me, left, height, label, width, bottom, color)

    class(pyplot),          intent(inout)        :: me            !! pyplot handler
    real(wp), dimension(:), intent(in)           :: left          !! left bar values
    real(wp), dimension(:), intent(in)           :: height        !! height bar values
    character(len=*),       intent(in)           :: label         !! plot label
    real(wp), dimension(:), intent(in), optional :: width         !! width values
    real(wp), dimension(:), intent(in), optional :: bottom        !! bottom values
    character(len=*),       intent(in), optional :: color         !! plot color
    character(len=:), allocatable                :: xstr          !! x axis values stringified
    character(len=:), allocatable                :: ystr          !! y axis values stringified
    character(len=:), allocatable                :: wstr          !! width values stringified
    character(len=:), allocatable                :: bstr          !! bottom values stringified
    character(len=:), allocatable                :: plt_str       !! plot string
    character(len=*), parameter                  :: xname = 'x'   !! x axis name
    character(len=*), parameter                  :: yname = 'y'   !! y axis name
    character(len=*), parameter                  :: wname = 'w'   !! width name
    character(len=*), parameter                  :: bname = 'b'   !! bottom name

    if (allocated(me%str)) then

        !convert the arrays to strings:
                             call vec_to_string(left, xstr, me%use_numpy)
                             call vec_to_string(height, ystr, me%use_numpy)
        if (present(width))  call vec_to_string(width, wstr, me%use_numpy)
        if (present(bottom)) call vec_to_string(bottom, bstr, me%use_numpy)

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
!> author: Jacob Williams
!
! Integer to string, specifying the default value if
! the optional argument is not present.

    subroutine optional_int_to_string(int_value, string_value, default_value)
    
    integer,          intent(in), optional :: int_value      !! integer value
    character(len=*), intent(out)          :: string_value   !! integer value stringified
    character(len=*), intent(in)           :: default_value  !! default integer value

    if (present(int_value)) then
        call integer_to_string(int_value, string_value)
    else
        string_value = default_value
    end if

    end subroutine optional_int_to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Integer to string conversion.

    subroutine integer_to_string(i, s)
    integer,          intent(in), optional  :: i     !! integer value
    character(len=*), intent(out)           :: s     !! integer value stringified
    integer                                 :: istat !! IO status

    write(s, int_fmt, iostat=istat) i

    if (istat/=0) then
        error stop 'Error converting integer to string'
    else
        s = adjustl(s)
    end if

    end subroutine integer_to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Real vector to string.

    subroutine vec_to_string(v, str, use_numpy)
    
    real(wp), dimension(:),        intent(in)  :: v         !! real values
    character(len=:), allocatable, intent(out) :: str       !! real values stringified
    logical,                       intent(in)  :: use_numpy !! activate numpy python module usage
    integer                                    :: i         !! counter
    integer                                    :: istat     !! IO status
    character(len=max_real_len)                :: tmp       !! dummy string

    str = '['
    do i=1, size(v)
        write(tmp, real_fmt, iostat=istat) v(i)
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
!> author: Jacob Williams
!
!  Write the buffer to a file, and then execute it with Python.

    subroutine execute(me, pyfile)
    
    class(pyplot),    intent(inout)        :: me     !! pytplot handler
    character(len=*), intent(in), optional :: pyfile !! name of the python script to generate
    integer                                :: istat  !! IO status
    integer                                :: iunit  !! IO unit
    character(len=:), allocatable          :: file   !! file name

    if (allocated(me%str)) then

        !file name for python script:
        if (present(pyfile)) then
            file = trim(pyfile)    !use the user-specified name
        else
            file = trim(tmp_file)  !use the default
        end if

        !generate the file:
        open(newunit=iunit, file=file, status='REPLACE', iostat=istat)
        if (istat/=0) error stop 'Error creating python script.'
        write(iunit, '(A)') me%str
        close(iunit, iostat=istat)

        !run the file using python:
        call execute_command_line(python_exe//' '//file)

        !cleanup:
        deallocate(file)

    end if
    
    end subroutine execute
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Save the figure.

    subroutine savefig(me, figfile, pyfile)
    
    class(pyplot),    intent(inout)        :: me      !! pyplot handler
    character(len=*), intent(in)           :: figfile !! file name for the figure
    character(len=*), intent(in), optional :: pyfile  !! name of the Python script to generate

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