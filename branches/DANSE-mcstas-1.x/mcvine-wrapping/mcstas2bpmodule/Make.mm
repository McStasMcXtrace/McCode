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

PROJECT = mcstas2
MODULE = mcstas2
PACKAGE = mcstas2bpmodule

include std-pythonmodule.def
include local.def


PROJ_CXX_SRCLIB = -lboost_python  -L$(BOOSTPYTHON_LIBDIR) -ljournal -lmcni -lmcstas2


PROJ_SRCS = \
	wrap_mcstas2_Component.cc \
	wrap_mcni_integration_Component.cc \
	wrap_random_numbers.cc \


include doxygen/default.def
docs: export-doxygen-docs



# version
# $Id$

# End of file
