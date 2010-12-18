
PROJECT = mcstas2

# directory structure

BUILD_DIRS = \
	lib \
	mcstas2bpmodule \
	mcstas2 \


OTHER_DIRS = \
	share \
	tests \


RECURSE_DIRS = $(BUILD_DIRS) $(OTHER_DIRS)

#--------------------------------------------------------------------------
#

all:
	BLD_ACTION="all" $(MM) recurse

distclean::
	BLD_ACTION="distclean" $(MM) recurse

clean::
	BLD_ACTION="clean" $(MM) recurse

tidy::
	BLD_ACTION="tidy" $(MM) recurse

export::
	BLD_ACTION="export" $(MM) recurse

# version
# $Id$

