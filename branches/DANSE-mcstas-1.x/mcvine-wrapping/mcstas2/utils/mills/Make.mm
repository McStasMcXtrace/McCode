PROJECT = mcstas2
PACKAGE = utils/mills


RECURSE_DIRS = \
    cxx \

#--------------------------------------------------------------------------
#

all: export
	BLD_ACTION="all" $(MM) recurse

tidy::
	BLD_ACTION="tidy" $(MM) recurse


#--------------------------------------------------------------------------
#
# export

EXPORT_PYTHON_MODULES = \
    __init__.py \

export:: export-package-python-modules

# version
# $Id$
