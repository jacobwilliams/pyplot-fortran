!*****************************************************************************************
!> author: Jacob Williams
!  date: 6/16/2017
!  license: BSD
!
!  For making simple x-y plots from Fortran.
!  It works by generating a Python script and executing it.
!
!# See also
!   * Inspired by: [EasyPlot](https://pypi.python.org/pypi/EasyPlot)
!
!@note The default real kind (`wp`) can be
!      changed using optional preprocessor flags.
!      This library was built with real kind:
#ifdef REAL32
!      `real(kind=real32)` [4 bytes]
#elif REAL64
!      `real(kind=real64)` [8 bytes]
#elif REAL128
!      `real(kind=real128)` [16 bytes]
#else
!      `real(kind=real64)` [8 bytes]
#endif

    module pyplot_module

    use, intrinsic :: iso_fortran_env 

    implicit none

    private

#ifdef REAL32
    integer,parameter,public :: pyplot_wp = real32   !! real kind used by this module [4 bytes]
#elif REAL64
    integer,parameter,public :: pyplot_wp = real64   !! real kind used by this module [8 bytes]
#elif REAL128
    integer,parameter,public :: pyplot_wp = real128  !! real kind used by this module [16 bytes]
#else
    integer,parameter,public :: pyplot_wp = real64   !! real kind used by this module [8 bytes]
#endif

    integer,parameter :: wp = pyplot_wp  !! local copy of `pyplot_wp` with a shorter name

    character(len=*), parameter :: tmp_file = 'pyplot_module_temp_1234567890.py' !! Default name of the temporary file
                                                                                 !! (this can also be user-specified).

    character(len=*), parameter :: python_exe       ='python'    !! The python executable name.
    character(len=*), parameter :: int_fmt          = '(I10)'    !! integer format string
    integer, parameter          :: max_int_len      = 10         !! max string length for integers
    character(len=*), parameter :: real_fmt_default = '(E30.16)' !! default real number format string
    integer, parameter          :: max_real_len     = 60         !! max string length for reals

    type, public :: pyplot

        !!  The main pyplot class.

        private

        character(len=:), allocatable :: str !! string buffer

        character(len=1) :: raw_str_token = ' ' !! will be 'r' if using raw strings

        logical :: show_legend = .false.     !! show legend into plot
        logical :: use_numpy   = .true.      !! use numpy python module
        logical :: use_oo_api  = .false.     !! use OO interface of matplotlib (incopatible with showfig subroutine)
        logical :: mplot3d     = .false.     !! it is a 3d plot
        logical :: polar       = .false.     !! it is a polar plot
        logical :: axis_equal  = .false.     !! equal scale on each axis
        logical :: axisbelow   = .true.      !! axis below other chart elements
        logical :: tight_layout = .false.    !! tight layout option
        logical :: usetex      = .false.     !! enable LaTeX

        character(len=:),allocatable :: real_fmt  !! real number formatting

    contains

        ! public methods
        procedure, public :: initialize    !! initialize pyplot instance

        procedure, public :: add_plot      !! add a 2d plot to pyplot instance
        procedure, public :: add_errorbar  !! add a 2d error bar plot to pyplot instance
        procedure, public :: add_3d_plot   !! add a 3d plot to pyplot instance
        procedure, public :: add_sphere    !! add a 3d sphere to pyplot instance
        procedure, public :: add_contour   !! add a contour plot to pyplot instance
        procedure, public :: plot_surface  !! add a surface plot to pyplot instance
        procedure, public :: add_bar       !! add a barplot to pyplot instance
        procedure, public :: add_imshow    !! add an image plot (using `imshow`)
        procedure, public :: add_hist      !! add a histogram plot to pyplot instance
        procedure, public :: savefig       !! save plots of pyplot instance
        procedure, public :: showfig       !! show plots of pyplot instance
        procedure, public :: destroy       !! destroy pyplot instance

        ! private methods
        procedure :: execute    !! execute pyplot commands
        procedure :: add_str    !! add string to pytplot instance buffer
        procedure :: finish_ops !! some final ops before saving

    end type pyplot

    contains
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Destructor.

    subroutine destroy(me)

    class(pyplot),intent(inout) :: me !! pyplot handler

    if (allocated(me%str))      deallocate(me%str)
    if (allocated(me%real_fmt)) deallocate(me%real_fmt)

    me%raw_str_token = ' '

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

    subroutine initialize(me, grid, xlabel, ylabel, zlabel, title, legend, use_numpy, figsize, &
                          font_size, axes_labelsize, xtick_labelsize, ytick_labelsize, ztick_labelsize, &
                          legend_fontsize, mplot3d, axis_equal, polar, real_fmt, use_oo_api, axisbelow,&
                          tight_layout, raw_strings, usetex)

    class(pyplot),         intent(inout)        :: me              !! pyplot handler
    logical,               intent(in), optional :: grid            !! activate grid drawing
    character(len=*),      intent(in), optional :: xlabel          !! label of x axis
    character(len=*),      intent(in), optional :: ylabel          !! label of y axis
    character(len=*),      intent(in), optional :: zlabel          !! label of z axis
    character(len=*),      intent(in), optional :: title           !! plot title
    logical,               intent(in), optional :: legend          !! plot legend
    logical,               intent(in), optional :: use_numpy       !! activate usage of numpy python module
    integer, dimension(2), intent(in), optional :: figsize         !! dimension of the figure
    integer,               intent(in), optional :: font_size       !! font size
    integer,               intent(in), optional :: axes_labelsize  !! size of axis labels
    integer,               intent(in), optional :: xtick_labelsize !! size of x axis tick lables
    integer,               intent(in), optional :: ytick_labelsize !! size of y axis tick lables
    integer,               intent(in), optional :: ztick_labelsize !! size of z axis tick lables
    integer,               intent(in), optional :: legend_fontsize !! size of legend font
    logical,               intent(in), optional :: mplot3d         !! set true for 3d plots (cannot use with polar)
    logical,               intent(in), optional :: axis_equal      !! set true for axis = 'equal'
    logical,               intent(in), optional :: polar           !! set true for polar plots (cannot use with mplot3d)
    character(len=*),      intent(in), optional :: real_fmt        !! format string for real numbers (examples: '(E30.16)' [default], '*')
    logical,               intent(in), optional :: use_oo_api      !! avoid matplotlib's GUI by using the OO interface (cannot use with showfig)
    logical,               intent(in), optional :: axisbelow       !! to put the grid lines below the other chart elements [default is true]
    logical,               intent(in), optional :: tight_layout    !! enable tight layout [default is false]
    logical,               intent(in), optional :: raw_strings     !! if True, all strings sent to Python are treated as
                                                                   !! raw strings (e.g., r'str'). Default is False.
    logical,               intent(in), optional :: usetex          !! if True, enable LaTeX. (default if false)

    character(len=max_int_len)  :: width_str             !! figure width dummy string
    character(len=max_int_len)  :: height_str            !! figure height dummy string
    character(len=max_int_len)  :: font_size_str         !! font size dummy string
    character(len=max_int_len)  :: axes_labelsize_str    !! size of axis labels dummy string
    character(len=max_int_len)  :: xtick_labelsize_str   !! size of x axis tick labels dummy string
    character(len=max_int_len)  :: ytick_labelsize_str   !! size of x axis tick labels dummy string
    character(len=max_int_len)  :: ztick_labelsize_str   !! size of z axis tick labels dummy string
    character(len=max_int_len)  :: legend_fontsize_str   !! size of legend font dummy string
    character(len=:),allocatable :: python_fig_func      !! Python's function for creating a new Figure instance

    character(len=*), parameter :: default_font_size_str = '10' !! the default font size for plots

    call me%destroy()

    if (present(raw_strings)) then
        if (raw_strings) me%raw_str_token = 'r'
    end if

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
    if (present(use_oo_api)) then
        me%use_oo_api = use_oo_api
    else
        me%use_oo_api = .false.
    end if
    if (present(figsize)) then
        call integer_to_string(figsize(1), width_str)
        call integer_to_string(figsize(2), height_str)
    end if
    if (present(mplot3d)) then
        me%mplot3d = mplot3d
    else
        me%mplot3d = .false.
    end if
    if (present(polar)) then
        me%polar = polar
    else
        me%polar = .false.
    end if
    if (present(axis_equal)) then
        me%axis_equal = axis_equal
    else
        me%axis_equal = .false.
    end if
    if (present(real_fmt)) then
        me%real_fmt = trim(adjustl(real_fmt))
    else
        me%real_fmt = real_fmt_default
    end if
    if (present(tight_layout)) then
        me%tight_layout = tight_layout
    else
        me%tight_layout = .false.
    end if
    if (present(usetex)) then
        me%usetex = usetex
    else
        me%usetex = .false.
    end if

    call optional_int_to_string(font_size, font_size_str, default_font_size_str)
    call optional_int_to_string(axes_labelsize, axes_labelsize_str, default_font_size_str)
    call optional_int_to_string(xtick_labelsize, xtick_labelsize_str, default_font_size_str)
    call optional_int_to_string(ytick_labelsize, ytick_labelsize_str, default_font_size_str)
    call optional_int_to_string(ztick_labelsize, ztick_labelsize_str, default_font_size_str)
    call optional_int_to_string(legend_fontsize, legend_fontsize_str, default_font_size_str)

    me%str = ''

    call me%add_str('#!/usr/bin/env python')
    call me%add_str('')

    call me%add_str('import matplotlib')
    if (me%use_oo_api) then
        call me%add_str('from matplotlib.figure import Figure')
        call me%add_str('from matplotlib.backends.backend_agg import FigureCanvasAgg as FigureCanvas')
    else
        call me%add_str('import matplotlib.pyplot as plt')
    endif
    if (me%mplot3d) call me%add_str('from mpl_toolkits.mplot3d import Axes3D')
    if (me%use_numpy) call me%add_str('import numpy as np')
    call me%add_str('')

    call me%add_str('matplotlib.rcParams["font.family"] = "Serif"')
    call me%add_str('matplotlib.rcParams["font.size"] = '//trim(font_size_str))
    call me%add_str('matplotlib.rcParams["axes.labelsize"] = '//trim(axes_labelsize_str))
    call me%add_str('matplotlib.rcParams["xtick.labelsize"] = '//trim(xtick_labelsize_str))
    call me%add_str('matplotlib.rcParams["ytick.labelsize"] = '//trim(ytick_labelsize_str))
    call me%add_str('matplotlib.rcParams["legend.fontsize"] = '//trim(legend_fontsize_str))
    if (me%usetex) call me%add_str('matplotlib.rcParams["text.usetex"] = True')

    call me%add_str('')

    if (me%use_oo_api) then
        python_fig_func = 'Figure'
    else
        python_fig_func = 'plt.figure'
    endif
    if (present(figsize)) then  !if specifying the figure size
        call me%add_str('fig = '//python_fig_func//'(figsize=('//trim(width_str)//','//trim(height_str)//'),facecolor="white")')
    else
        call me%add_str('fig = '//python_fig_func//'(facecolor="white")')
    end if

    if (me%mplot3d) then
        call me%add_str('ax = fig.add_subplot(1, 1, 1, projection=''3d'')')
    elseif (me%polar) then
        call me%add_str('ax = fig.add_subplot(1, 1, 1, projection=''polar'')')
    else
        call me%add_str('ax = fig.add_subplot(1, 1, 1)')
    end if

    if (present(grid)) then
        if (grid) call me%add_str('ax.grid()')
    end if

    if (present(axisbelow)) then
        me%axisbelow = axisbelow
    else
        me%axisbelow = .true. ! default
    end if
    if (me%axisbelow) call me%add_str('ax.set_axisbelow(True)')

    if (present(xlabel)) call me%add_str('ax.set_xlabel('//trim(me%raw_str_token)//'"'//trim(xlabel)//'")')
    if (present(ylabel)) call me%add_str('ax.set_ylabel('//trim(me%raw_str_token)//'"'//trim(ylabel)//'")')
    if (present(zlabel)) call me%add_str('ax.set_zlabel('//trim(me%raw_str_token)//'"'//trim(zlabel)//'")')
    if (present(title))  call me%add_str('ax.set_title('//trim(me%raw_str_token)//'"' //trim(title) //'")')

    call me%add_str('')

    end subroutine initialize
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Add an x,y plot.

    subroutine add_plot(me, x, y, label, linestyle, markersize, linewidth, xlim, ylim, xscale, yscale, color, istat)

    class(pyplot),          intent (inout)         :: me           !! pyplot handler
    real(wp), dimension(:), intent (in)            :: x            !! x values
    real(wp), dimension(:), intent (in)            :: y            !! y values
    character(len=*),       intent (in)            :: label        !! plot label
    character(len=*),       intent (in)            :: linestyle    !! style of the plot line
    integer,                intent (in), optional  :: markersize   !! size of the plot markers
    integer,                intent (in), optional  :: linewidth    !! width of the plot line
    real(wp),dimension(2),  intent (in), optional  :: xlim         !! x-axis range
    real(wp),dimension(2),  intent (in), optional  :: ylim         !! y-axis range
    character(len=*),       intent (in), optional  :: xscale       !! example: 'linear' (default), 'log'
    character(len=*),       intent (in), optional  :: yscale       !! example: 'linear' (default), 'log'
    real(wp),dimension(:),  intent (in), optional  :: color        !! RGB color tuple [0-1,0-1,0-1]
    integer,                intent (out), optional :: istat        !! status output (0 means no problems)

    character(len=:), allocatable :: arg_str      !! the arguments to pass to `plot`
    character(len=:), allocatable :: xstr         !! x values stringified
    character(len=:), allocatable :: ystr         !! y values stringified
    character(len=:), allocatable :: xlimstr      !! xlim values stringified
    character(len=:), allocatable :: ylimstr      !! ylim values stringified
    character(len=:), allocatable :: color_str    !! color values stringified
    character(len=max_int_len)    :: imark        !! actual markers size
    character(len=max_int_len)    :: iline        !! actual line width
    character(len=*), parameter   :: xname = 'x'  !! x variable name for script
    character(len=*), parameter   :: yname = 'y'  !! y variable name for script

    if (allocated(me%str)) then

        if (present(istat)) istat = 0

        !axis limits (optional):
        if (present(xlim)) call vec_to_string(xlim, me%real_fmt, xlimstr, me%use_numpy)
        if (present(ylim)) call vec_to_string(ylim, me%real_fmt, ylimstr, me%use_numpy)

        !convert the arrays to strings:
        call vec_to_string(x, me%real_fmt, xstr, me%use_numpy)
        call vec_to_string(y, me%real_fmt, ystr, me%use_numpy)

        !get optional inputs (if not present, set default value):
        call optional_int_to_string(markersize, imark, '3')
        call optional_int_to_string(linewidth, iline, '3')

        !write the arrays:
        call me%add_str(trim(xname)//' = '//xstr)
        call me%add_str(trim(yname)//' = '//ystr)
        call me%add_str('')

        !main arguments for plot:
        arg_str = trim(xname)//','//&
                  trim(yname)//','//&
                  trim(me%raw_str_token)//'"'//trim(linestyle)//'",'//&
                  'linewidth='//trim(adjustl(iline))//','//&
                  'markersize='//trim(adjustl(imark))//','//&
                  'label='//trim(me%raw_str_token)//'"'//trim(label)//'"'

        ! optional arguments:
        if (present(color)) then
            if (size(color)<=3) then
                call vec_to_string(color(1:3), '*', color_str, use_numpy=.false., is_tuple=.true.)
                arg_str = arg_str//',color='//trim(color_str)
            end if
        end if

        !write the plot statement:
        call me%add_str('ax.plot('//arg_str//')')

        !axis limits:
        if (allocated(xlimstr)) call me%add_str('ax.set_xlim('//xlimstr//')')
        if (allocated(ylimstr)) call me%add_str('ax.set_ylim('//ylimstr//')')

        !axis scales:
        if (present(xscale)) call me%add_str('ax.set_xscale('//trim(me%raw_str_token)//'"'//xscale//'")')
        if (present(yscale)) call me%add_str('ax.set_yscale('//trim(me%raw_str_token)//'"'//yscale//'")')

        call me%add_str('')

    else
        if (present(istat)) istat = -1
        write(error_unit,'(A)') 'Error in add_plot: pyplot class not properly initialized.'
    end if

    end subroutine add_plot
!*****************************************************************************************

!*****************************************************************************************
!> author: Jimmy Leta
!
! Add a histogram plot.

    subroutine add_hist(me, x, label, xlim, ylim, xscale, yscale, bins, normed, cumulative, istat)

    class(pyplot),          intent (inout)        :: me           !! pyplot handler
    real(wp), dimension(:), intent (in)           :: x            !! array of data
    character(len=*),       intent (in)           :: label        !! plot label
    real(wp),dimension(2),  intent (in), optional :: xlim         !! x-axis range
    real(wp),dimension(2),  intent (in), optional :: ylim         !! y-axis range
    character(len=*),       intent (in), optional :: xscale       !! example: 'linear' (default), 'log'
    character(len=*),       intent (in), optional :: yscale       !! example: 'linear' (default), 'log'
    integer,                intent (in), optional :: bins         !! number of bins
    logical,                intent (in), optional :: normed       !! boolean flag that determines whether bin counts are normalized [NO LONGER USED]
    logical,                intent (in), optional :: cumulative   !! boolean flag that determines whether histogram represents the cumulative density of dataset
    integer,                intent (out),optional :: istat        !! status output (0 means no problems)

    character(len=*), parameter   :: xname = 'x'      !! x variable name for script
    character(len=:), allocatable :: xstr             !! x values stringified
    character(len=:), allocatable :: xlimstr          !! xlim values stringified
    character(len=:), allocatable :: ylimstr          !! ylim values stringified
    character(len=:), allocatable :: cumulativestr    !!
    character(len=max_int_len)    :: binsstr          !!

    if (allocated(me%str)) then

        if (present(istat)) istat = 0

        !axis limits (optional):
        if (present(xlim)) call vec_to_string(xlim, me%real_fmt, xlimstr, me%use_numpy)
        if (present(ylim)) call vec_to_string(ylim, me%real_fmt, ylimstr, me%use_numpy)

        !convert the arrays to strings:
        call vec_to_string(x, me%real_fmt, xstr, me%use_numpy)

        !write the arrays:
        call me%add_str(trim(xname)//' = '//xstr)
        call me%add_str('')

        !get optional inputs (if not present, set default value):
        call optional_int_to_string(bins, binsstr, '10')
        call optional_logical_to_string(cumulative, cumulativestr, 'False')

        !write the plot statement:
        call me%add_str('ax.hist('//&
                        trim(xname)//','//&
                        'label='//trim(me%raw_str_token)//'"'//trim(label)//'",'//&
                        'bins='//trim(binsstr)//','//&
                        'cumulative='//trim(cumulativestr)//')')

        !axis limits:
        if (allocated(xlimstr)) call me%add_str('ax.set_xlim('//xlimstr//')')
        if (allocated(ylimstr)) call me%add_str('ax.set_ylim('//ylimstr//')')

        !axis scales:
        if (present(xscale)) call me%add_str('ax.set_xscale('//trim(me%raw_str_token)//'"'//xscale//'")')
        if (present(yscale)) call me%add_str('ax.set_yscale('//trim(me%raw_str_token)//'"'//yscale//'")')

        call me%add_str('')

    else
        if (present(istat)) istat = -1
        write(error_unit,'(A)') 'Error in add_plot: pyplot class not properly initialized.'
    end if

    end subroutine add_hist
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Add a contour plot.
!
!@note This requires `use_numpy` to be True.

    subroutine add_contour(me, x, y, z, linestyle, linewidth, levels, color, &
                           filled, cmap, colorbar, istat)

    class(pyplot),           intent (inout)        :: me           !! pyplot handler
    real(wp),dimension(:),   intent (in)           :: x            !! x values
    real(wp),dimension(:),   intent (in)           :: y            !! y values
    real(wp),dimension(:,:), intent (in)           :: z            !! z values (a matrix)
    character(len=*),        intent (in)           :: linestyle    !! style of the plot line
    integer,                 intent (in), optional :: linewidth    !! width of the plot line [only used when `filled=False`]
    real(wp),dimension(:),   intent (in), optional :: levels       !! contour levels to plot
    character(len=*),        intent (in), optional :: color        !! color of the contour line
    logical,                 intent (in), optional :: filled       !! use filled control (default=False)
    character(len=*),        intent (in), optional :: cmap         !! colormap if filled=True (examples: 'jet', 'bone')
    logical,                 intent (in), optional :: colorbar     !! add a colorbar (default=False)
    integer,                 intent (out),optional :: istat        !! status output (0 means no problems)

    character(len=:), allocatable :: xstr          !! x values stringified
    character(len=:), allocatable :: ystr          !! y values stringified
    character(len=:), allocatable :: zstr          !! z values stringified
    character(len=:), allocatable :: levelstr      !! levels vector stringified
    character(len=max_int_len)    :: iline         !! actual line width
    character(len=*), parameter   :: xname = 'x'   !! x variable name for script
    character(len=*), parameter   :: yname = 'y'   !! y variable name for script
    character(len=*), parameter   :: zname = 'z'   !! z variable name for script
    character(len=*), parameter   :: xname_ = 'X'  !! X variable name for contour
    character(len=*), parameter   :: yname_ = 'Y'  !! Y variable name for contour
    character(len=*), parameter   :: zname_ = 'Z'  !! Z variable name for contour
    character(len=:), allocatable :: extras        !! optional stuff
    character(len=:), allocatable :: contourfunc   !! 'contour' or 'contourf'
    logical :: is_filled !! if it is a filled contour plot

    if (allocated(me%str)) then

        if (present(istat)) istat = 0

        !convert the arrays to strings:
        call vec_to_string(x, me%real_fmt, xstr, me%use_numpy)
        call vec_to_string(y, me%real_fmt, ystr, me%use_numpy)
        call matrix_to_string(z, me%real_fmt, zstr, me%use_numpy)
        if (present(levels)) call vec_to_string(levels, me%real_fmt, levelstr, me%use_numpy)

        !get optional inputs (if not present, set default value):
        call optional_int_to_string(linewidth, iline, '3')

        !write the arrays:
        call me%add_str(trim(xname)//' = '//xstr)
        call me%add_str(trim(yname)//' = '//ystr)
        call me%add_str(trim(zname)//' = '//zstr)
        call me%add_str('')

        !convert inputs for contour plotting:
        call me%add_str(xname_//', '//yname_//' = np.meshgrid('//trim(xname)//', '//trim(yname)//')')
        call me%add_str(zname_//' = np.transpose('//zname//')')

        !optional arguments:
        extras = ''
        if (present(levels))     extras = extras//','//'levels='//levelstr
        if (present(color))      extras = extras//','//'colors='//trim(me%raw_str_token)//'"'//color//'"'
        if (present(linewidth))  extras = extras//','//'linewidths='//trim(adjustl(iline))
        if (present(cmap))       extras = extras//','//'cmap='//trim(me%raw_str_token)//'"'//cmap//'"'

        !filled or regular:
        contourfunc = 'contour'  !default
        is_filled = .false.
        if (present(filled)) then
            is_filled = filled
            if (filled) contourfunc = 'contourf'  !filled contour
        end if

        !write the plot statement:
        call me%add_str('CS = ax.'//contourfunc//'('//xname_//','//yname_//','//zname_//','//&
                        'linestyles='//trim(me%raw_str_token)//'"'//trim(adjustl(linestyle))//'"'//&
                        extras//')')

        if (present(colorbar)) then
            if (colorbar) call me%add_str('fig.colorbar(CS)')
        end if

        if (.not. is_filled) call me%add_str('ax.clabel(CS, fontsize=9, inline=1)')
        call me%add_str('')

    else
        if (present(istat)) istat = -1
        write(error_unit,'(A)') 'Error in add_plot: pyplot class not properly initialized.'
    end if

    end subroutine add_contour
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Add a surface plot.
!
!@note This requires `use_numpy` to be True.

    subroutine plot_surface(me, x, y, z, label, linestyle, linewidth, levels, color, &
                            cmap, colorbar, antialiased, istat)

    class(pyplot),           intent (inout)         :: me           !! pyplot handler
    real(wp),dimension(:),   intent (in)            :: x            !! x values
    real(wp),dimension(:),   intent (in)            :: y            !! y values
    real(wp),dimension(:,:), intent (in)            :: z            !! z values (a matrix)
    character(len=*),        intent (in)            :: label        !! plot label
    character(len=*),        intent (in)            :: linestyle    !! style of the plot line
    integer,                 intent (in),  optional :: linewidth    !! width of the plot line
    real(wp),dimension(:),   intent (in),  optional :: levels       !! contour levels to plot
    character(len=*),        intent (in),  optional :: color        !! Color of the surface patches
    character(len=*),        intent (in),  optional :: cmap         !! colormap if filled=True (examples: 'jet', 'bone')
    logical,                 intent (in),  optional :: colorbar     !! add a colorbar (default=False)
    logical,                 intent (in),  optional :: antialiased  !! The surface is made opaque by using antialiased=False
    integer,                 intent (out), optional :: istat        !! status output (0 means no problems)

    character(len=:), allocatable :: xstr           !! x values stringified
    character(len=:), allocatable :: ystr           !! y values stringified
    character(len=:), allocatable :: zstr           !! z values stringified
    character(len=:), allocatable :: levelstr       !! levels vector stringified
    character(len=:), allocatable :: antialiasedstr !! antialiased stringified
    character(len=max_int_len)    :: iline          !! actual line width
    character(len=*), parameter   :: xname = 'x'    !! x variable name for script
    character(len=*), parameter   :: yname = 'y'    !! y variable name for script
    character(len=*), parameter   :: zname = 'z'    !! z variable name for script
    character(len=*), parameter   :: xname_ = 'X'   !! X variable name for contour
    character(len=*), parameter   :: yname_ = 'Y'   !! Y variable name for contour
    character(len=*), parameter   :: zname_ = 'Z'   !! Z variable name for contour
    character(len=:), allocatable :: extras         !! optional stuff

    if (allocated(me%str)) then

        if (present(istat)) istat = 0

        !convert the arrays to strings:
        call vec_to_string(x, me%real_fmt, xstr, me%use_numpy)
        call vec_to_string(y, me%real_fmt, ystr, me%use_numpy)
        call matrix_to_string(z, me%real_fmt, zstr, me%use_numpy)
        if (present(levels)) call vec_to_string(levels, me%real_fmt, levelstr, me%use_numpy)

        !get optional inputs (if not present, set default value):
        call optional_int_to_string(linewidth, iline, '3')
        call optional_logical_to_string(antialiased, antialiasedstr, 'False')

        !write the arrays:
        call me%add_str(trim(xname)//' = '//xstr)
        call me%add_str(trim(yname)//' = '//ystr)
        call me%add_str(trim(zname)//' = '//zstr)
        call me%add_str('')

        !convert inputs for contour plotting:
        call me%add_str(xname_//', '//yname_//' = np.meshgrid('//trim(xname)//', '//trim(yname)//')')
        call me%add_str(zname_//' = np.transpose('//zname//')')

        !optional arguments:
        extras = ''
        if (present(levels))     extras = extras//','//'levels='//levelstr
        if (present(color))      extras = extras//','//'colors='//trim(me%raw_str_token)//'"'//color//'"'
        if (present(linewidth))  extras = extras//','//'linewidths='//trim(adjustl(iline))
        if (present(cmap))       extras = extras//','//'cmap='//trim(me%raw_str_token)//'"'//cmap//'"'

        !write the plot statement:
        call me%add_str('CS = ax.plot_surface'//'('//xname_//','//yname_//','//zname_//','//&
                        'label='//trim(me%raw_str_token)//'"'//trim(label)//'",'//&
                        'antialiased='//trim(antialiasedstr)//','//&
                        'linestyles='//trim(me%raw_str_token)//'"'//trim(adjustl(linestyle))//'"'//&
                        extras//')')

        if (present(colorbar)) then
            if (colorbar) call me%add_str('fig.colorbar(CS)')
        end if

        call me%add_str('')

    else
        if (present(istat)) istat = -1
        write(error_unit,'(A)') 'Error in add_plot: pyplot class not properly initialized.'
    end if

    end subroutine plot_surface
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Add a 3D x,y,z plot.
!
!@note Must initialize the class with ```mplot3d=.true.```

    subroutine add_3d_plot(me, x, y, z, label, linestyle, markersize, linewidth, istat)

    class(pyplot),          intent (inout)         :: me           !! pyplot handler
    real(wp), dimension(:), intent (in)            :: x            !! x values
    real(wp), dimension(:), intent (in)            :: y            !! y values
    real(wp), dimension(:), intent (in)            :: z            !! z values
    character(len=*),       intent (in)            :: label        !! plot label
    character(len=*),       intent (in)            :: linestyle    !! style of the plot line
    integer,                intent (in),  optional :: markersize   !! size of the plot markers
    integer,                intent (in),  optional :: linewidth    !! width of the plot line
    integer,                intent (out), optional :: istat        !! status output (0 means no problems)

    character(len=:), allocatable :: xstr         !! x values stringified
    character(len=:), allocatable :: ystr         !! y values stringified
    character(len=:), allocatable :: zstr         !! z values stringified
    character(len=max_int_len)    :: imark        !! actual markers size
    character(len=max_int_len)    :: iline        !! actual line width
    character(len=*), parameter   :: xname = 'x'  !! x variable name for script
    character(len=*), parameter   :: yname = 'y'  !! y variable name for script
    character(len=*), parameter   :: zname = 'z'  !! z variable name for script

    if (allocated(me%str)) then

        if (present(istat)) istat = 0

        !convert the arrays to strings:
        call vec_to_string(x, me%real_fmt, xstr, me%use_numpy)
        call vec_to_string(y, me%real_fmt, ystr, me%use_numpy)
        call vec_to_string(z, me%real_fmt, zstr, me%use_numpy)

        !get optional inputs (if not present, set default value):
        call optional_int_to_string(markersize, imark, '3')
        call optional_int_to_string(linewidth, iline, '3')

        !write the arrays:
        call me%add_str(trim(xname)//' = '//xstr)
        call me%add_str(trim(yname)//' = '//ystr)
        call me%add_str(trim(zname)//' = '//zstr)
        call me%add_str('')

        !write the plot statement:
        call me%add_str('ax.plot('//&
                        trim(xname)//','//&
                        trim(yname)//','//&
                        trim(zname)//','//&
                        trim(me%raw_str_token)//'"'//trim(linestyle)//'",'//&
                        'linewidth='//trim(adjustl(iline))//','//&
                        'markersize='//trim(adjustl(imark))//','//&
                        'label='//trim(me%raw_str_token)//'"'//trim(label)//'")')
        call me%add_str('')

    else
        if (present(istat)) istat = -1
        write(error_unit,'(A)') 'Error in add_3d_plot: pyplot class not properly initialized.'
    end if

    end subroutine add_3d_plot
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Add a sphere to a 3D x,y,z plot.
!
!@note Must initialize the class with `mplot3d=.true.` and `use_numpy=.true.`.

    subroutine add_sphere(me, r, xc, yc, zc, n_facets, linewidth, antialiased, color, istat)

    implicit none

    class(pyplot),    intent (inout)         :: me            !! pyplot handler
    real(wp),         intent (in)            :: r             !! radius of the sphere
    real(wp),         intent (in)            :: xc            !! x value of sphere center
    real(wp),         intent (in)            :: yc            !! y value of sphere center
    real(wp),         intent (in)            :: zc            !! z value of sphere center
    integer,          intent (in),  optional :: n_facets      !! [default is 100]
    integer,          intent (in),  optional :: linewidth     !! line width
    logical,          intent (in),  optional :: antialiased   !! enabled anti-aliasing
    character(len=*), intent (in),  optional :: color         !! color of the contour line
    integer,          intent (out), optional :: istat         !! status output (0 means no problems)

    character(len=:), allocatable :: rstr         !! `r` value stringified
    character(len=:), allocatable :: xcstr        !! `xc` value stringified
    character(len=:), allocatable :: ycstr        !! `yc` value stringified
    character(len=:), allocatable :: zcstr        !! `zc` value stringified
    character(len=*), parameter   :: xname = 'x'  !! `x` variable name for script
    character(len=*), parameter   :: yname = 'y'  !! `y` variable name for script
    character(len=*), parameter   :: zname = 'z'  !! `z` variable name for script

    character(len=max_int_len)    :: linewidth_str       !! `linewidth` input stringified
    character(len=:), allocatable :: antialiased_str     !! `antialised` input stringified
    character(len=max_int_len)    :: n_facets_str        !! `n_facets` input stringified
    character(len=:), allocatable :: extras              !! optional stuff string

    if (allocated(me%str)) then

        !get optional inputs (if not present, set default value):
        call optional_int_to_string(n_facets, n_facets_str, '100')
        extras = ''
        if (present(linewidth)) then
            call optional_int_to_string(linewidth, linewidth_str, '1')
            extras = extras//','//'linewidth='//linewidth_str
        end if
        if (present(antialiased)) then
            call optional_logical_to_string(antialiased, antialiased_str, 'False')
            extras = extras//','//'antialiased='//antialiased_str
        end if
        if (present(color)) then
            extras = extras//','//'color='//trim(me%raw_str_token)//'"'//trim(color)//'"'
        end if

        if (present(istat)) istat = 0

        !convert the arrays to strings:
        call real_to_string(r , me%real_fmt, rstr)
        call real_to_string(xc, me%real_fmt, xcstr)
        call real_to_string(yc, me%real_fmt, ycstr)
        call real_to_string(zc, me%real_fmt, zcstr)

        ! sphere code:
        call me%add_str('u = np.linspace(0, 2 * np.pi, '//n_facets_str//')')
        call me%add_str('v = np.linspace(0, np.pi, '//n_facets_str//')')
        call me%add_str(xname//' = '//xcstr//' + '//rstr//' * np.outer(np.cos(u), np.sin(v))')
        call me%add_str(yname//' = '//ycstr//' + '//rstr//' * np.outer(np.sin(u), np.sin(v))')
        call me%add_str(zname//' = '//zcstr//' + '//rstr//' * np.outer(np.ones(np.size(u)), np.cos(v))')
        call me%add_str('ax.plot_surface('//xname//', '//yname//', '//zname//extras//')')
        call me%add_str('')

    else
        if (present(istat)) istat = -1
        write(error_unit,'(A)') 'Error in add_sphere: pyplot class not properly initialized.'
    end if

    end subroutine add_sphere
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Add a bar plot.

    subroutine add_bar(me, x, height, label, width, bottom, color, &
                        yerr, align, xlim, ylim, xscale, yscale, istat)

    class(pyplot),          intent(inout)         :: me            !! pyplot handler
    real(wp), dimension(:), intent(in)            :: x             !! x bar values
    real(wp), dimension(:), intent(in)            :: height        !! height bar values
    character(len=*),       intent(in)            :: label         !! plot label
    real(wp), dimension(:), intent(in),  optional :: width         !! width values
    real(wp), dimension(:), intent(in),  optional :: bottom        !! bottom values
    character(len=*),       intent(in),  optional :: color         !! plot color
    real(wp), dimension(:), intent(in),  optional :: yerr          !! yerr values
    character(len=*),       intent(in),  optional :: align         !! default: 'center'
    real(wp),dimension(2),  intent (in), optional :: xlim          !! x-axis range
    real(wp),dimension(2),  intent (in), optional :: ylim          !! y-axis range
    character(len=*),       intent (in), optional :: xscale        !! example: 'linear' (default), 'log'
    character(len=*),       intent (in), optional :: yscale        !! example: 'linear' (default), 'log'
    integer,                intent (out),optional :: istat         !! status output (0 means no problems)

    character(len=:), allocatable :: xstr               !! x axis values stringified
    character(len=:), allocatable :: ystr               !! y axis values stringified
    character(len=:), allocatable :: xlimstr            !! xlim values stringified
    character(len=:), allocatable :: ylimstr            !! ylim values stringified
    character(len=:), allocatable :: wstr               !! width values stringified
    character(len=:), allocatable :: bstr               !! bottom values stringified
    character(len=:), allocatable :: plt_str            !! plot string
    character(len=:), allocatable :: yerr_str           !!  yerr values stringified
    character(len=*), parameter   :: xname = 'x'        !! x axis name
    character(len=*), parameter   :: yname = 'y'        !! y axis name
    character(len=*), parameter   :: wname = 'w'        !! width name
    character(len=*), parameter   :: bname = 'b'        !! bottom name
    character(len=*), parameter   :: yerrname = 'yerr'  !! yerr name

    if (allocated(me%str)) then

        if (present(istat)) istat = 0

        !axis limits (optional):
        if (present(xlim)) call vec_to_string(xlim, me%real_fmt, xlimstr, me%use_numpy)
        if (present(ylim)) call vec_to_string(ylim, me%real_fmt, ylimstr, me%use_numpy)

        !convert the arrays to strings:
                             call vec_to_string(x,      me%real_fmt, xstr,     me%use_numpy)
                             call vec_to_string(height, me%real_fmt, ystr,     me%use_numpy)
        if (present(width))  call vec_to_string(width,  me%real_fmt, wstr,     me%use_numpy)
        if (present(bottom)) call vec_to_string(bottom, me%real_fmt, bstr,     me%use_numpy)
        if (present(yerr))   call vec_to_string(yerr,   me%real_fmt, yerr_str, me%use_numpy)

        !write the arrays:
                             call me%add_str(trim(xname)//' = '//xstr)
                             call me%add_str(trim(yname)//' = '//ystr)
        if (present(width))  call me%add_str(trim(wname)//' = '//wstr)
        if (present(bottom)) call me%add_str(trim(bname)//' = '//bstr)
        if (present(yerr))   call me%add_str(trim(yerrname)//' = '//yerr_str)
        call me%add_str('')

        !create the plot string:
        plt_str = 'ax.bar('//&
                  'x='//trim(xname)//','//&
                  'height='//trim(yname)//','
        if (present(yerr))   plt_str=plt_str//'yerr='//trim(yerrname)//','
        if (present(width))  plt_str=plt_str//'width='//trim(wname)//','
        if (present(bottom)) plt_str=plt_str//'bottom='//trim(bstr)//','
        if (present(color))  plt_str=plt_str//'color='//trim(me%raw_str_token)//'"'//trim(color)//'",'
        if (present(align))  plt_str=plt_str//'align='//trim(me%raw_str_token)//'"'//trim(align)//'",'
        plt_str=plt_str//'label='//trim(me%raw_str_token)//'"'//trim(label)//'")'

        !write the plot statement:
        call me%add_str(plt_str)

        !axis limits:
        if (allocated(xlimstr)) call me%add_str('ax.set_xlim('//xlimstr//')')
        if (allocated(ylimstr)) call me%add_str('ax.set_ylim('//ylimstr//')')

        !axis scales:
        if (present(xscale)) call me%add_str('ax.set_xscale('//trim(me%raw_str_token)//'"'//xscale//'")')
        if (present(yscale)) call me%add_str('ax.set_yscale('//trim(me%raw_str_token)//'"'//yscale//'")')

        call me%add_str('')

    else
        if (present(istat)) istat = -1
        write(error_unit,'(A)') 'Error in add_bar: pyplot class not properly initialized.'
    end if

    end subroutine add_bar
!*****************************************************************************************

!*****************************************************************************************
!>
! Add an image plot using `imshow`.
!
!### Note
!  * Based on code by Ricardo Torres, 4/2/2017.

    subroutine add_imshow(me, x, xlim, ylim, istat)

    class(pyplot),          intent (inout) :: me            !! pyplot handler
    real(wp),dimension(:,:),intent (in)    :: x             !! x values
    real(wp),dimension(2),  intent (in), optional :: xlim   !! x-axis range
    real(wp),dimension(2),  intent (in), optional :: ylim   !! y-axis range
    integer,                intent (out),optional :: istat  !! status output (0 means no problems)

    character(len=:), allocatable :: xstr         !! x values stringified
    character(len=*), parameter   :: xname = 'x'  !! x variable name for script

    !axis limits (optional):
    character(len=:), allocatable :: xlimstr      !! xlim values stringified
    character(len=:), allocatable :: ylimstr      !! ylim values stringified

    if (allocated(me%str)) then

        if (present(istat)) istat = 0

        if (present(xlim)) call vec_to_string(xlim, me%real_fmt, xlimstr, me%use_numpy)
        if (present(ylim)) call vec_to_string(ylim, me%real_fmt, ylimstr, me%use_numpy)

        !convert the arrays to strings:
        call matrix_to_string(x, me%real_fmt, xstr, me%use_numpy)

        !write the arrays:
        call me%add_str(trim(xname)//' = '//xstr)
        call me%add_str('')

        !write the plot statement:
        call me%add_str('ax.imshow('//trim(xname)//')')
        call me%add_str('')

        !axis limits:
        if (allocated(xlimstr)) call me%add_str('ax.set_xlim('//xlimstr//')')
        if (allocated(ylimstr)) call me%add_str('ax.set_ylim('//ylimstr//')')

    else
        if (present(istat)) istat = -1
        write(error_unit,'(A)') 'Error in add_imshow: pyplot class not properly initialized.'
    end if

    end subroutine add_imshow
!*****************************************************************************************

!*****************************************************************************************
!> author: Alexander Sandrock
!
! Add an x,y plot with errorbars.

    subroutine add_errorbar(me, x, y, label, linestyle, xerr, yerr, markersize, linewidth, xlim, ylim, xscale, yscale, color, istat)

    class(pyplot),          intent (inout)        :: me           !! pyplot handler
    real(wp), dimension(:), intent (in)           :: x            !! x values
    real(wp), dimension(:), intent (in)           :: y            !! y values
    character(len=*),       intent (in)           :: label        !! plot label
    character(len=*),       intent (in)           :: linestyle    !! style of the plot line
    real(wp), dimension(:), intent (in), optional :: xerr         !! x errorbar sizes
    real(wp), dimension(:), intent (in), optional :: yerr         !! y errorbar sizes
    integer,                intent (in), optional :: markersize   !! size of the plot markers
    integer,                intent (in), optional :: linewidth    !! width of the plot line
    real(wp),dimension(2),  intent (in), optional :: xlim         !! x-axis range
    real(wp),dimension(2),  intent (in), optional :: ylim         !! y-axis range
    character(len=*),       intent (in), optional :: xscale       !! example: 'linear' (default), 'log'
    character(len=*),       intent (in), optional :: yscale       !! example: 'linear' (default), 'log'
    real(wp),dimension(:),  intent (in), optional :: color        !! RGB color tuple [0-1,0-1,0-1]
    integer,                intent (out),optional :: istat        !! status output (0 means no problems)

    character(len=:), allocatable :: arg_str            !! the arguments to pass to `plot`
    character(len=:), allocatable :: xstr               !! x values stringified
    character(len=:), allocatable :: ystr               !! y values stringified
    character(len=:), allocatable :: xlimstr            !! xlim values stringified
    character(len=:), allocatable :: ylimstr            !! ylim values stringified
    character(len=:), allocatable :: xerrstr            !! xerr values stringified
    character(len=:), allocatable :: yerrstr            !! yerr values stringified
    character(len=:), allocatable :: color_str          !! color values stringified
    character(len=max_int_len)    :: imark              !! actual markers size
    character(len=max_int_len)    :: iline              !! actual line width
    character(len=*), parameter   :: xname = 'x'        !! x variable name for script
    character(len=*), parameter   :: yname = 'y'        !! y variable name for script
    character(len=*), parameter   :: xerrname = 'xerr'  !! xerr variable name for script
    character(len=*), parameter   :: yerrname = 'yerr'  !! yerr variable name for script

    if (allocated(me%str)) then

        if (present(istat)) istat = 0

        !axis limits (optional):
        if (present(xlim)) call vec_to_string(xlim, me%real_fmt, xlimstr, me%use_numpy)
        if (present(ylim)) call vec_to_string(ylim, me%real_fmt, ylimstr, me%use_numpy)
        !errorbar sizes (optional):
        if (present(xerr)) call vec_to_string(xerr, me%real_fmt, xerrstr, me%use_numpy)
        if (present(yerr)) call vec_to_string(yerr, me%real_fmt, yerrstr, me%use_numpy)

        !convert the arrays to strings:
        call vec_to_string(x, me%real_fmt, xstr, me%use_numpy)
        call vec_to_string(y, me%real_fmt, ystr, me%use_numpy)

        !get optional inputs (if not present, set default value):
        call optional_int_to_string(markersize, imark, '3')
        call optional_int_to_string(linewidth, iline, '3')

        !write the arrays:
        call me%add_str(trim(xname)//' = '//xstr)
        call me%add_str(trim(yname)//' = '//ystr)
        call me%add_str('')
        if (present(xerr)) call me%add_str(trim(xerrname)//' = '//xerrstr)
        if (present(yerr)) call me%add_str(trim(yerrname)//' = '//yerrstr)
        if (present(xerr) .or. present(yerr)) call me%add_str('')

        !main arguments for plot:
        arg_str = trim(xname)//','//&
                  trim(yname)//','//&
                  'fmt='//trim(me%raw_str_token)//'"'//trim(linestyle)//'",'//&
                  'linewidth='//trim(adjustl(iline))//','//&
                  'markersize='//trim(adjustl(imark))//','//&
                  'label='//trim(me%raw_str_token)//'"'//trim(label)//'"'

        ! optional arguments:
        if (present(xerr)) then
            arg_str = arg_str//','//'xerr='//trim(xerrname)
        end if
        if (present(yerr)) then
            arg_str = arg_str//','//'yerr='//trim(yerrname)
        end if
        if (present(color)) then
            if (size(color)<=3) then
                call vec_to_string(color(1:3), '*', color_str, use_numpy=.false., is_tuple=.true.)
                arg_str = arg_str//',color='//trim(color_str)
            end if
        end if

        !write the plot statement:
        call me%add_str('ax.errorbar('//arg_str//')')

        !axis limits:
        if (allocated(xlimstr)) call me%add_str('ax.set_xlim('//xlimstr//')')
        if (allocated(ylimstr)) call me%add_str('ax.set_ylim('//ylimstr//')')

        !axis scales:
        if (present(xscale)) call me%add_str('ax.set_xscale('//trim(me%raw_str_token)//'"'//xscale//'")')
        if (present(yscale)) call me%add_str('ax.set_yscale('//trim(me%raw_str_token)//'"'//yscale//'")')

        call me%add_str('')

    else
        if (present(istat)) istat = -1
        write(error_unit,'(A)') 'Error in add_errorbar: pyplot class not properly initialized.'
    end if

    end subroutine add_errorbar
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
! Logical to string, specifying the default value if
! the optional argument is not present.

    subroutine optional_logical_to_string(logical_value, string_value, default_value)

    logical,intent(in),optional              :: logical_value
    character(len=:),allocatable,intent(out) :: string_value   !! integer value stringified
    character(len=*),intent(in)              :: default_value  !! default integer value

    if (present(logical_value)) then
        if (logical_value) then
            string_value = 'True'
        else
            string_value = 'False'
        end if
    else
        string_value = default_value
    end if

    end subroutine optional_logical_to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Integer to string conversion.

    subroutine integer_to_string(i, s)

    integer,          intent(in), optional  :: i     !! integer value
    character(len=*), intent(out)           :: s     !! integer value stringified

    integer :: istat !! IO status

    write(s, int_fmt, iostat=istat) i

    if (istat/=0) then
        write(error_unit,'(A)') 'Error converting integer to string'
        s = '****'
    else
        s = adjustl(s)
    end if

    end subroutine integer_to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Real scalar to string.

    subroutine real_to_string(v, fmt, str)

    real(wp),                      intent(in)  :: v         !! real values
    character(len=*),              intent(in)  :: fmt       !! real format string
    character(len=:), allocatable, intent(out) :: str       !! real values stringified

    integer                     :: istat     !! IO status
    character(len=max_real_len) :: tmp       !! dummy string

    if (fmt=='*') then
        write(tmp, *, iostat=istat) v
    else
        write(tmp, fmt, iostat=istat) v
    end if
    if (istat/=0) then
        write(error_unit,'(A)') 'Error in real_to_string'
        str = '****'
    else
        str = trim(adjustl(tmp))
    end if

    end subroutine real_to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Real vector to string.

    subroutine vec_to_string(v, fmt, str, use_numpy, is_tuple)

    real(wp), dimension(:),        intent(in)  :: v         !! real values
    character(len=*),              intent(in)  :: fmt       !! real format string
    character(len=:), allocatable, intent(out) :: str       !! real values stringified
    logical,                       intent(in)  :: use_numpy !! activate numpy python module usage
    logical,intent(in),optional                :: is_tuple  !! if true [default], use '()', if false use '[]'

    integer                     :: i         !! counter
    integer                     :: istat     !! IO status
    character(len=max_real_len) :: tmp       !! dummy string
    logical                     :: tuple

    if (present(is_tuple)) then
        tuple = is_tuple
    else
        tuple = .false.
    end if

    if (tuple) then
        str = '('
    else
        str = '['
    end if

    do i=1, size(v)
        if (fmt=='*') then
            write(tmp, *, iostat=istat) v(i)
        else
            write(tmp, fmt, iostat=istat) v(i)
        end if
        if (istat/=0) then
            write(error_unit,'(A)') 'Error in vec_to_string'
            str = '****'
            return
        end if
        str = str//trim(adjustl(tmp))
        if (i<size(v)) str = str // ','
    end do

    if (tuple) then
        str = str // ')'
    else
        str = str // ']'
    end if

    !convert to numpy array if necessary:
    if (use_numpy) str = 'np.array('//str//')'

    end subroutine vec_to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Real matrix (rank 2) to string.

    subroutine matrix_to_string(v, fmt, str, use_numpy)

    real(wp), dimension(:,:),      intent(in)  :: v         !! real values
    character(len=*),              intent(in)  :: fmt       !! real format string
    character(len=:), allocatable, intent(out) :: str       !! real values stringified
    logical,                       intent(in)  :: use_numpy !! activate numpy python module usage

    integer                      :: i         !! counter
    character(len=:),allocatable :: tmp       !! dummy string

    str = '['
    do i=1, size(v,1)  !rows
        call vec_to_string(v(i,:), fmt, tmp, use_numpy)  !one row at a time
        str = str//trim(adjustl(tmp))
        if (i<size(v,1)) str = str // ','
    end do
    str = str // ']'

    !convert to numpy array if necessary:
    if (use_numpy) str = 'np.array('//str//')'

    end subroutine matrix_to_string
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 8/16/2015
!
!  Write the buffer to a file, and then execute it with Python.
!
!  If user specifies a Python file name, then the file is kept, otherwise
!  a temporary filename is used, and the file is deleted after it is used.

    subroutine execute(me, pyfile, istat)

    class(pyplot),    intent(inout)         :: me     !! pytplot handler
    character(len=*), intent(in),  optional :: pyfile !! name of the python script to generate
    integer,          intent (out),optional :: istat  !! status output (0 means no problems)

    integer                       :: iunit   !! IO unit
    character(len=:), allocatable :: file    !! file name
    logical                       :: scratch !! if a scratch file is to be used
    integer                       :: iostat  !! open/close status code

    if (allocated(me%str)) then

        if (present(istat)) istat = 0

        scratch = (.not. present(pyfile))

        !file name to use:
        if (scratch) then
            file = trim(tmp_file)  !use the default
        else
            file = trim(pyfile)    !use the user-specified name
        end if

        !open the file:
        open(newunit=iunit, file=file, status='REPLACE', iostat=iostat)
        if (iostat/=0) then
            if (present(istat)) istat = iostat
            write(error_unit,'(A)') 'Error opening file: '//trim(file)
            return
        end if

        !write to the file:
        write(iunit, '(A)') me%str

        !to ensure that the file is there for the next
        !command line call, we have to close it here.
        close(iunit, iostat=iostat)
        if (iostat/=0) then
            if (present(istat)) istat = iostat
            write(error_unit,'(A)') 'Error closing file: '//trim(file)
        else

            !run the file using python:
            if (index(file,' ')>0) then
                ! space in path, probably should enclose in quotes
                call execute_command_line(python_exe//' "'//file//'"')
            else
                call execute_command_line(python_exe//' '//file)
            end if

            if (scratch) then
                !delete the file (have to reopen it because
                !Fortran has no file delete function)
                open(newunit=iunit, file=file, status='OLD', iostat=iostat)
                if (iostat==0) close(iunit, status='DELETE', iostat=iostat)
            end if
            if (iostat/=0) then
                if (present(istat)) istat = iostat
                write(error_unit,'(A)') 'Error closing file.'
            end if

        end if

        !cleanup:
        if (allocated(file)) deallocate(file)

    else
        if (present(istat)) istat = -1
    end if

    end subroutine execute
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Some final things to add before saving or showing the figure.

    subroutine finish_ops(me)

    class(pyplot),intent(inout) :: me  !! pyplot handler

    if (me%show_legend) then
        call me%add_str('ax.legend(loc="best")')
        call me%add_str('')
    end if
    if (me%axis_equal) then
        if (me%mplot3d) then
            call me%add_str('ax.set_aspect("auto")')
            call me%add_str('')

            call me%add_str('def set_axes_equal(ax):')
            call me%add_str('    x_limits = ax.get_xlim3d()')
            call me%add_str('    y_limits = ax.get_ylim3d()')
            call me%add_str('    z_limits = ax.get_zlim3d()')
            call me%add_str('    x_range = abs(x_limits[1] - x_limits[0])')
            call me%add_str('    x_middle = np.mean(x_limits)')
            call me%add_str('    y_range = abs(y_limits[1] - y_limits[0])')
            call me%add_str('    y_middle = np.mean(y_limits)')
            call me%add_str('    z_range = abs(z_limits[1] - z_limits[0])')
            call me%add_str('    z_middle = np.mean(z_limits)')
            call me%add_str('    plot_radius = 0.5*max([x_range, y_range, z_range])')
            call me%add_str('    ax.set_xlim3d([x_middle - plot_radius, x_middle + plot_radius])')
            call me%add_str('    ax.set_ylim3d([y_middle - plot_radius, y_middle + plot_radius])')
            call me%add_str('    ax.set_zlim3d([z_middle - plot_radius, z_middle + plot_radius])')
            call me%add_str('set_axes_equal(ax)')

        else
            call me%add_str('ax.axis("equal")')
        end if
        call me%add_str('')
    end if
    if (me%tight_layout) then
        call me%add_str('fig.tight_layout()')
        call me%add_str('')
    end if

    end subroutine finish_ops
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
! Save the figure.
!
!### History
!  * modified: Johannes Rieke 6/16/2017
!  * modified: Jacob Williams 6/16/2017

    subroutine savefig(me, figfile, pyfile, dpi, transparent, facecolor, edgecolor, orientation, istat)

    class(pyplot),    intent(inout)          :: me          !! pyplot handler
    character(len=*), intent(in)             :: figfile     !! file name for the figure
    character(len=*), intent(in), optional   :: pyfile      !! name of the Python script to generate
    character(len=*), intent(in), optional   :: dpi         !! resolution of the figure for png
                                                            !! [note this is a string]
    logical, intent(in), optional            :: transparent !! transparent background (T/F)
    character(len=*), intent(in), optional   :: facecolor   !! the colors of the figure rectangle
    character(len=*), intent(in), optional   :: edgecolor   !! the colors of the figure rectangle
    character(len=*), intent(in), optional   :: orientation !! 'landscape' or 'portrait'
    integer,          intent (out), optional :: istat       !! status output (0 means no problems)

    character(len=:),allocatable :: tmp  !! for building the `savefig` arguments.

    if (allocated(me%str)) then

        if (present(istat)) istat = 0

        !finish up the string:
        call me%finish_ops()

        !build the savefig arguments:
        tmp = trim(me%raw_str_token)//'"'//trim(figfile)//'"'
        if (present(dpi)) tmp = tmp//', dpi='//trim(dpi)
        if (present(transparent)) then
            if (transparent) then
                tmp = tmp//', transparent=True'
            else
                tmp = tmp//', transparent=False'
            end if
        end if
        if (present(facecolor)) tmp = tmp//', facecolor="'//trim(facecolor)//'"'
        if (present(edgecolor)) tmp = tmp//', edgecolor="'//trim(edgecolor)//'"'
        if (present(orientation)) tmp = tmp//', orientation="'//trim(orientation)//'"'
        if (me%use_oo_api) then
            call me%add_str('canvas = FigureCanvas(fig)')
            call me%add_str('canvas.print_figure('//tmp//')')
        else
            call me%add_str('plt.savefig('//tmp//')')
        endif
        deallocate(tmp)

        !run it:
        call me%execute(pyfile, istat=istat)

    else
        if (present(istat)) istat = -1
        write(error_unit,'(A)') 'error in savefig: pyplot class not properly initialized.'
    end if

    end subroutine savefig
!*****************************************************************************************

!*****************************************************************************************
!> author: Johannes Rieke
!  date: 6/16/2017
!
! Shows the figure.

    subroutine showfig(me, pyfile, istat)

    class(pyplot),    intent(inout)          :: me      !! pyplot handler
    character(len=*), intent(in), optional   :: pyfile  !! name of the Python script to generate
    integer,          intent (out), optional :: istat   !! status output (0 means no problems)

    if (.not. allocated(me%str)) then

        if (present(istat)) istat = -1
        write(error_unit,'(A)') 'error in showfig: pyplot class not properly initialized.'

    else if (me%use_oo_api) then

        if (present(istat)) istat = -2
        write(error_unit,'(A)') 'error in showfig: not compatible with "use_oo_api" option'

    else

        if (present(istat)) istat = 0

        !finish up the string:
        call me%finish_ops()

        !show figure:
        call me%add_str('plt.show()')

        !run it:
        call me%execute(pyfile, istat=istat)

    end if

    end subroutine showfig
!*****************************************************************************************

!*****************************************************************************************
    end module pyplot_module
!*****************************************************************************************
