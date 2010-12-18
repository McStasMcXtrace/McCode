# -*- Makefile -*-
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                               Michael A.G. Aivazis
#                        California Institute of Technology
#                        (C) 1998-2005  All Rights Reserved
#
# <LicenseText>
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#

PROJECT = projectname
PACKAGE = projectnamemodule
MODULE = XXX
#MODULE = projectname

include std-pythonmodule.def
include local.def

PROJ_CXX_SRCLIB = -lprojectname

PROJ_SRCS = \
    bindings.cc \
    exceptions.cc \
    misc.cc


# version
# $Id$

# End of file
