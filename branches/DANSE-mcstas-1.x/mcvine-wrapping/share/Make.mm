# -*- Makefile -*-
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                               Michael A.G. Aivazis
#                        California Institute of Technology
#                        (C) 1998-2004  All Rights Reserved
#
# <LicenseText>
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#

PROJECT = mcstas2

# directory structure

#--------------------------------------------------------------------------
all: export
#

#CP_RF = cp -rf
CP_RF = rsync -a

EXPORT_DATADIRS = \
    McStas-Components \


EXPORT_SHAREDIR=$(EXPORT_ROOT)/share
SHARE_DEST =  $(EXPORT_SHAREDIR)/$(PROJECT)

export:: export-package-data

export-package-data:: $(EXPORT_DATADIRS)
	mkdir -p $(SHARE_DEST); \
	for x in $(EXPORT_DATADIRS); do { \
            if [ -d $$x ]; then { \
	        $(CP_RF) $$x/ $(SHARE_DEST)/$$x/; \
            } fi; \
        } done


# version
# $Id$

# End of file
