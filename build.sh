#!/bin/bash

#
#  Simple build script for pyplot-fortran.
#

PROJECTNAME='pyplot-fortran'    # project name for robodoc
MODCODE='pyplot_module.f90'     # module file name
LIBOUT='libpyplot.a'            # name of library
DOCDIR='./doc/'                 # build directory for documentation
SRCDIR='./src/'                 # library source directory
BINDIR='./bin/'                 # build directory for unit tests
LIBDIR='./lib/'                 # build directory for library

#compiler flags:

FCOMPILER='gnu' #Set default compiler to gfortran
FCOMPILERFLAGS='-c -O2 -fbacktrace -Wall -Wextra -Wno-maybe-uninitialized -Wno-unused-function -pedantic -std=f2008'

#build the library and example:

FoBiS.py build -compiler ${FCOMPILER} -cflags "${FCOMPILERFLAGS}" -dbld ${LIBDIR} -s ${SRCDIR} -dmod ./ -dobj ./ -t ${MODCODE} -o ${LIBOUT} -mklib static -colors
FoBiS.py build -compiler ${FCOMPILER} -cflags "${FCOMPILERFLAGS}" -dbld ${BINDIR} -s ${SRCDIR} -dmod ./ -dobj ./ -colors

# build the documentation:

if hash robodoc 2>/dev/null; then
	echo "Building documentation..."
	robodoc --rc ./robodoc.rc --src ${SRCDIR} --doc ${DOCDIR} --documenttitle ${PROJECTNAME}
else
	echo "ROBODoc not found! Cannot build documentation. ROBODoc can be obtained from: http://www.xs4all.nl/~rfsber/Robo/"
fi
