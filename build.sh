#!/bin/bash

#
#  Simple build script for pyplot-fortran.
#

PROJECTNAME='pyplot-fortran'    # project name for robodoc
MODCODE='pyplot_module.f90'     # module file name
FORDMD='pyplot-fortran.md'      # FORD settings file
LIBOUT='libpyplot.a'            # name of library
DOCDIR='./doc/'                 # build directory for documentation
SRCDIR='./src/'                 # library source directory
BINDIR='./bin/'                 # build directory for unit tests
LIBDIR='./lib/'                 # build directory for library

#compiler flags:

FCOMPILER='gnu' #Set default compiler to gfortran
FCOMPILERFLAGS='-c -O2 -fbacktrace -Wall -Wextra -Wno-maybe-uninitialized -Wno-unused-function -pedantic -std=f2008'

#build the library and example using FoBiS:

if hash FoBiS.py 2>/dev/null; then
    FoBiS.py build -compiler ${FCOMPILER} -cflags "${FCOMPILERFLAGS}" -dbld ${LIBDIR} -s ${SRCDIR} -dmod ./ -dobj ./ -t ${MODCODE} -o ${LIBOUT} -mklib static -colors
    FoBiS.py build -compiler ${FCOMPILER} -cflags "${FCOMPILERFLAGS}" -dbld ${BINDIR} -s ${SRCDIR} -dmod ./ -dobj ./ -colors
else
    echo "FoBiS.py not found! Cannot build library. Install using: sudo pip install FoBiS.py"
fi

# build the documentation using FORD:

#if hash ford 2>/dev/null; then
#    echo "Building documentation..."
#    ford ${FORDMD}
#else
#    echo "FORD not found! Cannot build documentation. To install, run: sudo pip install ford"
#fi