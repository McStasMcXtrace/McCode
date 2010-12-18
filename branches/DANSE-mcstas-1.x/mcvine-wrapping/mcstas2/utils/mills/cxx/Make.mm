PROJECT = mcstas2
PACKAGE = utils/mills/cxx


RECURSE_DIRS = \

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
    factory.py \
    installationInfo.py \
    Argument.py \
    CCMill.py \
    Class.py \
    CxxClassMillBase.py \
    CxxClassRenderer.py \
    HHMill.py \
    Member.py \
    Method.py \
    __init__.py \

export:: export-package-python-modules

# version
# $Id$
