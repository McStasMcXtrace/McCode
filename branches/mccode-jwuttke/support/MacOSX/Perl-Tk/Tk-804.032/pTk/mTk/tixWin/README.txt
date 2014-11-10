$Id: README.txt,v 1.1 2000/11/03 02:32:35 idiscovery Exp $

NOTES:
=====

1. Versions of Tcl/Tk prior to 8.0 are no longer supported.
2. The makefile.bc is for Borland 4.5/5.x,
   The makefile.vc4 is for MSVC 4.x,
   The makefile.vc is for MSVC 5.x and 6.x
3. Edit the file "common.mak" set set the common settings:
   TCL_VER     = version of Tcl to compile with. Should be either 8.0 8.1
   8.2 or 8.3. You may also have to set the Tk/Tcl patchlevel TCLPATCH.
   ITCL_VER    = version of ITcl to compile with. Should be either 3.0 3.1
               3.2 or nothing - nothing implies no ITcl (default).
   INSTALLDIR = where the install- targets should copy the binaries and
	    support files.

See the file ..\docs\WinInst.txt.
