#
#
# $Id: common.mak,v 1.3.2.3 2001/12/09 03:54:18 idiscovery Exp $
#
# TOOLS       = location of BC++ 32-bit development tools.
#               (DEFAULT: C:\BC45)
# TIX_DEBUG   = Compile Tix with debug information.
#               (DEFAULT: undefined -- debug is not enabled.)
# TCL_VER     = version of Tcl to compile with. Should be either 8.0
#               8.1, 8.2 or 8.3.
#               (DEFAULT: Compile with Tcl 8.3)
# 		You may also need to change TCLPATCH for the patchlevel
# ITCL_VER    = version of ITcl to compile with. Should be either 3.0 3.1
#               3.2 or nothing - nothing implies no ITcl.
#               (DEFAULT: Compile without ITcl)
# INSTALLDIR = where the install- targets should copy the binaries and
#	    support files
#
#----------------------------------------------------------------------
TCL_VER          = 8.3
ITCL_VER          = 

INSTALLDIR      = J:\IDI\install-windows-x86

!IFNDEF TOOLS
TOOLS = C:\BC45
!ENDIF

!IFNDEF TIX_DEBUG
NODEBUG = 1
!ENDIF

TIXMAJOR=8
TIXMINOR=1

!IF "$(TCL_VER)" == "8.0"
TCLMAJOR=8
TCLMINOR=2
TCLPATCH=3
TMPDIR          = tk$(TCL_VER)
!ENDIF

!IF "$(TCL_VER)" == "8.1"
TCLMAJOR=8
TCLMINOR=1
TCLPATCH=3
TMPDIR          = tk$(TCL_VER)
!ENDIF

!IF "$(TCL_VER)" == "8.2"
TCLMAJOR=8
TCLMINOR=2
TCLPATCH=3
TMPDIR          = tk$(TCL_VER)
!ENDIF

!IF "$(TCL_VER)" == "8.3"
TCLMAJOR=8
TCLMINOR=3
TCLPATCH=3
TMPDIR          = tk$(TCL_VER)
!ENDIF

DOTVERSION=$(TIXMAJOR).$(TIXMINOR)

TCLDIR  = ..\..\tcl$(TCLMAJOR).$(TCLMINOR).$(TCLPATCH)
TKDIR   = ..\..\tk$(TCLMAJOR).$(TCLMINOR).$(TCLPATCH)
TCLLIB  = tcl$(TCLMAJOR)$(TCLMINOR).lib
TCLDLL  = tcl$(TCLMAJOR)$(TCLMINOR).dll
TKLIB   = tk$(TCLMAJOR)$(TCLMINOR).lib
TKDLL   = tk$(TCLMAJOR)$(TCLMINOR).dll
TIXLIB  = $(TMPDIR)\tix$(TIXMAJOR)$(TIXMINOR)$(TCLMAJOR)$(TCLMINOR).lib
TIXDLL  = $(TMPDIR)\tix$(TIXMAJOR)$(TIXMINOR)$(TCLMAJOR)$(TCLMINOR).dll
TIXWISH = $(TMPDIR)\tix$(TIXMAJOR)$(TIXMINOR)$(TCLMAJOR)$(TCLMINOR).exe

CONSOLE_OBJ = tkConsole80.obj

LIB_INSTALL_DIR	= $(INSTALLDIR)\lib
BIN_INSTALL_DIR	= $(INSTALLDIR)\bin
SCRIPT_INSTALL_DIR	= $(INSTALLDIR)\lib\tix$(DOTVERSION)
INCLUDE_INSTALL_DIR	= $(INSTALLDIR)\include

IEXT=1
!IF "$(ITCL_VER)" != ""
TIXLIB  = $(TMPDIR)\tix$(TIXMAJOR)$(TIXMINOR)$(TCLMAJOR)$(TCLMINOR)$(IEXT).lib
TIXDLL  = $(TMPDIR)\tix$(TIXMAJOR)$(TIXMINOR)$(TCLMAJOR)$(TCLMINOR)$(IEXT).dll
TIXWISH = $(TMPDIR)\tix$(TIXMAJOR)$(TIXMINOR)$(TCLMAJOR)$(TCLMINOR)$(IEXT).exe
!ENDIF

!IF "$(ITCL_VER)" == "3.0"

TMPDIR          = itcl$(ITCL_VER)
ITCL_DIR 	= ..\..\itcl3.0.1
# Define this for ITcl2 or Itcl3
ITCL_DEFINES  	= -DITCL_2
ITCL_CFLAGS	 = -I$(ITCL_DIR)\itcl\generic \
		   -I$(ITCL_DIR)\itk\generic
ITCL_LIBS = $(ITCL_DIR)\itk\win\Release\itk30.lib $(ITCL_DIR)\itcl\win\Release\itcl30.lib
!ENDIF

!IF "$(ITCL_VER)" == "3.1"
TMPDIR          = itcl$(ITCL_VER)
ITCL_DIR 	= ..\..\itcl3.1
# Define this for ITcl2 or Itcl3
ITCL_DEFINES  	= -DITCL_2
ITCL_CFLAGS	 = -I$(ITCL_DIR)\itcl\generic \
		   -I$(ITCL_DIR)\itk\generic
ITCL_LIBS = $(ITCL_DIR)\itk\win\Release\itk31.lib $(ITCL_DIR)\itcl\win\Release\itcl31.lib
!ENDIF

!IF "$(ITCL_VER)" == "3.2"
TMPDIR          = itcl$(ITCL_VER)
ITCL_DIR 	= ..\..\itcl3.2
# Define this for ITcl2 or Itcl3
ITCL_DEFINES  	= -DITCL_2
ITCL_CFLAGS	 = -I$(ITCL_DIR)\itcl\generic \
		   -I$(ITCL_DIR)\itk\generic
# Warnimg - these libs may be 31 due to an error in the itcl-3.2 release
ITCL_LIBS = $(ITCL_DIR)\itk\win\Release\itk32.lib $(ITCL_DIR)\itcl\win\Release\itcl32.lib
!ENDIF

!IFNDEF TCLDIR
!ERROR "Unsupported Tcl version $(TCL_VER)"
!ENDIF


WISHOBJS = \
	$(TMPDIR)\tixWinMain.obj

TIXOBJS = \
	$(TMPDIR)\$(CONSOLE_OBJ)  \
	$(TMPDIR)\tixClass.obj    \
	$(TMPDIR)\tixCmds.obj     \
	$(TMPDIR)\tixCompat.obj   \
	$(TMPDIR)\tixDiImg.obj    \
	$(TMPDIR)\tixDiITxt.obj   \
	$(TMPDIR)\tixDiStyle.obj  \
	$(TMPDIR)\tixDItem.obj    \
	$(TMPDIR)\tixDiText.obj   \
	$(TMPDIR)\tixDiWin.obj    \
	$(TMPDIR)\tixError.obj    \
	$(TMPDIR)\tixForm.obj     \
	$(TMPDIR)\tixFormMisc.obj \
	$(TMPDIR)\tixGeometry.obj \
	$(TMPDIR)\tixHLCol.obj    \
	$(TMPDIR)\tixHLHdr.obj    \
	$(TMPDIR)\tixHLInd.obj    \
	$(TMPDIR)\tixImgCmp.obj   \
	$(TMPDIR)\tixHList.obj    \
	$(TMPDIR)\tixList.obj     \
	$(TMPDIR)\tixMethod.obj   \
	$(TMPDIR)\tixOption.obj   \
	$(TMPDIR)\tixSmpLs.obj    \
	$(TMPDIR)\tixWidget.obj   \
	$(TMPDIR)\tixInit.obj     \
	$(TMPDIR)\tixItcl.obj     \
	$(TMPDIR)\tixUtils.obj    \
	$(TMPDIR)\tixImgXpm.obj   \
	$(TMPDIR)\tixNBFrame.obj  \
	$(TMPDIR)\tixTList.obj    \
	$(TMPDIR)\tixGrid.obj     \
	$(TMPDIR)\tixGrData.obj   \
	$(TMPDIR)\tixGrRC.obj     \
	$(TMPDIR)\tixGrFmt.obj    \
	$(TMPDIR)\tixGrSel.obj    \
	$(TMPDIR)\tixGrUtl.obj    \
	$(TMPDIR)\tixScroll.obj   \
	$(TMPDIR)\tixWCmpt.obj    \
	$(TMPDIR)\tixWinDraw.obj  \
	$(TMPDIR)\tixWinXpm.obj   \
	$(TMPDIR)\tixWinWm.obj

RMDIR		= $(TCLDIR)\win\rmd.bat
MKDIR		= $(TCLDIR)\win\mkd.bat
RM		= del

install:    install-binaries install-libraries

install-binaries: $(TCLSH)
	$(MKDIR) "$(BIN_INSTALL_DIR)"
	$(MKDIR) "$(LIB_INSTALL_DIR)"
	@echo installing $(TIXDLL)
	@copy "$(TIXDLL)" "$(BIN_INSTALL_DIR)"
	@copy "$(TIXLIB)" "$(LIB_INSTALL_DIR)"
	@echo installing "$(TIXWISH)"
	@copy "$(TIXWISH)" "$(BIN_INSTALL_DIR)"

install-libraries:
	-@$(MKDIR) "$(LIB_INSTALL_DIR)"
	@echo installing include files
	-@$(MKDIR) "$(INCLUDE_INSTALL_DIR)"
	copy "$(ROOT)\generic\tix.h"             "$(INCLUDE_INSTALL_DIR)"
	@echo installing library files
	-@$(MKDIR) "$(SCRIPT_INSTALL_DIR)"
	-@$(MKDIR) "$(SCRIPT_INSTALL_DIR)\pref"
	-@$(MKDIR) "$(SCRIPT_INSTALL_DIR)\bitmaps"
	xcopy "$(ROOT)\library"            "$(SCRIPT_INSTALL_DIR)"
	xcopy "$(ROOT)\library\pref"       "$(SCRIPT_INSTALL_DIR)\pref"
	xcopy "$(ROOT)\library\bitmaps"    "$(SCRIPT_INSTALL_DIR)\bitmaps"
	copy "$(TMPDIR)\pkgIndex.tcl"     "$(SCRIPT_INSTALL_DIR)"
	
