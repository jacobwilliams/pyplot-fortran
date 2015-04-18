Pyplot-Fortran
=============================

A simple module for generating plots from Fortran using Python's matplotlib.pyplot.

Overview
---------------

Currently, this module can be used to generate simple x-y plots from Fortran.  Eventually, it may be expanded to provide additional features and other types of plots.

The way it works is simply to generate a Python script with the plotting code, which
is then executed from the command line using the Fortran ```execute_command_line``` function.

The module requires a modern Fortran compiler (it uses various Fortran 2003/2008 features such as deferred-length strings). It should work fine with the latest gfortran and ifort compilers.

Example
---------------

The following example generates a plot of the sine function:

```Fortran
    program test

    use,intrinsic :: iso_fortran_env, only: wp => real64
    use pyplot_module

    implicit none

    real(wp),dimension(100) :: x,sx
    type(pyplot) :: plt
    integer :: i

    !generate some data:
    x = [(real(i,wp), i=0,size(x)-1)]/5.0_wp
    sx = sin(x)

    !plot it:
    call plt%initialize(grid=.true.,xlabel='angle (rad)',&
                        title='Plot of $\sin(x)$',legend=.true.)
    call plt%add_plot(x,sx,label='$\sin(x)$',linestyle='b-o',markersize=5,linewidth=2)
    call plt%savefig('sinx.png', pyfile='sinx.py')

    end program test

```

See also
---------------
 * [Mathplotlib](http://matplotlib.org)
