PROJECT = mcstas2/wrappers
PACKAGE = component2cppClass


RECURSE_DIRS = \

#--------------------------------------------------------------------------
#

all: export
	BLD_ACTION="all" $(MM) recurse

#--------------------------------------------------------------------------
#
# export

EXPORT_PYTHON_MODULES = \
	__init__.py \
	component2cppClass.py \

export:: export-package-python-modules

# version
# $Id$
