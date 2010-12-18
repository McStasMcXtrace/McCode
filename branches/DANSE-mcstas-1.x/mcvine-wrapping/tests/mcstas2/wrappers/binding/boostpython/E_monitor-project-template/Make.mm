# -*- Makefile -*-
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                               Michael A.G. Aivazis
#                        California Institute of Technology
#                        (C) 1998-2004  All Rights Reserved
#
# <LicenseText>
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#

PROJECT = 
MODULE = E_monitorbp
PACKAGE = E_monitorbpmodule

include std-pythonmodule.def
include local.def


PROJ_CXX_SRCLIB = -lboost_python  -L$(BOOSTPYTHON_LIBDIR) -ljournal -lmcni -lmcstas2


PROJ_SRCS = \
	E_monitor.cc \
	wrap.cc \


EXPORT_PYTHON_MODULES = \
	E_monitor.py \


export:: export-python-modules 

include doxygen/default.def
docs: export-doxygen-docs



# version
# $Id$

# End of file
