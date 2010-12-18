# -*- Makefile -*-
#

include local.def

PROJECT = mcstas2/mcni_integration

# directory structure

BUILD_DIRS = \

OTHER_DIRS = \


RECURSE_DIRS = $(BUILD_DIRS) $(OTHER_DIRS)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# build the library

all: export release-headers
	BLD_ACTION="all" $(MM) recurse


distclean::
	BLD_ACTION="distclean" $(MM) recurse

clean::
	BLD_ACTION="clean" $(MM) recurse

tidy::
	BLD_ACTION="tidy" $(MM) recurse


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


EXPORT_HEADERS = \
	Component.h \


export:: export-headers


# version
# $Id$

#
# End of file
