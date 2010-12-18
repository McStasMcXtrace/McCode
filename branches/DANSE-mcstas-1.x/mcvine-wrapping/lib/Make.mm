
include local.def

PROJECT = mcstas2
PACKAGE = libmcstas2

PROJ_SAR = $(BLD_LIBDIR)/$(PACKAGE).$(EXT_SAR)
PROJ_DLL = $(BLD_BINDIR)/$(PACKAGE).$(EXT_SO)
PROJ_TMPDIR = $(BLD_TMPDIR)/$(PROJECT)/$(PACKAGE)
PROJ_CLEAN += $(PROJ_SAR) $(PROJ_DLL)

PROJ_SRCS = \
	Component.cc \
	Gravity.cc \
	assert.cc \
	detector_outputs.cc \
	display.cc \
	exception.cc \
	geometry.cc \
	misc.cc \
	misc_macros.cc \
	propagators.cc \
	random_numbers.cc \

# directory structure

BUILD_DIRS = \
	mcni_integration \
	boostpython_binding \
	share \

OTHER_DIRS = \


RECURSE_DIRS = $(BUILD_DIRS) $(OTHER_DIRS)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# build the library

all: $(PROJ_SAR) export release-headers
	BLD_ACTION="all" $(MM) recurse


distclean::
	BLD_ACTION="distclean" $(MM) recurse

clean::
	BLD_ACTION="clean" $(MM) recurse

tidy::
	BLD_ACTION="tidy" $(MM) recurse


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ifeq (Win32, ${findstring Win32, $(PLATFORM_ID)})

# build the shared object
$(PROJ_SAR): product_dirs $(PROJ_OBJS)
	$(CXX) $(LCXXFLAGS) -o $(PROJ_DLL) \
    -Wl,--out-implib=$(PROJ_SAR) $(PROJ_OBJS)

# export
export:: export-headers export-libraries export-binaries
	BLD_ACTION="export" $(MM) recurse

else

# build the shared object
$(PROJ_SAR): product_dirs $(PROJ_OBJS)
	$(CXX) $(LCXXFLAGS) -o $(PROJ_SAR) $(PROJ_OBJS)

# export
export:: export-headers export-libraries
	BLD_ACTION="export" $(MM) recurse

endif

EXPORT_HEADERS = \
	Component.h \
	Gravity.h \
	abs.h \
	assert.h \
	cross_ref_macros.h \
	detector_output_macros.h \
	detector_outputs.h \
	display.h \
	exception.h \
	geometry.h \
	mcstas2.h \
	misc.h \
	misc_macros.h \
	phys_constants.h \
	propagator_macros.h \
	propagators.h \
	random_numbers.h \
	tracing_macros.h \

EXPORT_LIBS = $(PROJ_SAR)
EXPORT_BINS = $(PROJ_DLL)


# version
# $Id$

#
# End of file
