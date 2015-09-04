project: pyplot-fortran
project_dir: src
output_dir: doc
project_github: https://github.com/jacobwilliams/pyplot-fortran
summary: For generating plots from Fortran using Python's matplotlib.pyplot
author: Jacob Williams
github: https://github.com/jacobwilliams
predocmark_alt: >
predocmark: <
docmark_alt:
docmark: !
display: public
         protected
         private
source: true
graph: true

# Brief description

Currently, this module can be used to generate simple plots from Fortran.  Eventually, it may be expanded to provide additional features and other types of plots.

The way it works is simply to generate a Python script with the plotting code, which
is then executed from the command line using the Fortran ```execute_command_line``` function.