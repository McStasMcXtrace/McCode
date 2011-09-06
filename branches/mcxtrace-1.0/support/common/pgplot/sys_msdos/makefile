# Makefile PGPLOT5.10 for use with Microsoft Power Station(FL32)
#  C. T. Dum, 12/93;7/94;3/95;5/95;4/96.
#  derived from AFT 91-Jun-27
#  This generates the PGPLOT binary files (libraries and demos) in the
#  current default directory (which need not be the source directory).
#
#       Directory containing source code
SRC=C:
SRCDIR=$(SRC)\pgl\src
#       Directory containing demo programs
DEMODIR=$(SRC)\pgl\examples
#       Directory containing drivers
DRVDIR=$(SRC)\pgl\drivers
#       Directory containing font stuff
FONTDIR=$(SRC)\pgl\fonts
#       Directory containing system-dependent code
SYSDIR=$(SRC)\pgl\sys_msdo
#       Directory to put the libraries
LIBDIR=.
#       Fortran compiler
FCOMPL=FL32
FFLAGC=/Op /nologo  /G4
#       C compiler
CCOMPL=
CFLAGC=
# Libraries required for linking
LIBS = PGPLOT.LIB 

#       Rules for compiling Fortran
.SUFFIXES: .F

{$(SRCDIR)\}.F{}.OBJ:
	$(FCOMPL) /c $(FFLAGC) /Tf$<

all: PGPLOT.LIB PGPACK.EXE PROG
#-----------------------------------------------------------------------
# Routine lists:
#   PG_ROUTINES: basic PGPLOT routines (Fortran-77)
#   PG_NON_STANDARD: non-Fortran-77 aliases for basic routines
#   GR_ROUTINES: support routines, not called directly by applications
#       (Fortran-77)
#   SYSTEM_ROUTINES: potentially non-portable routines, usually
#       operating-system dependent
#-----------------------------------------------------------------------
PGPLOT.LIB:: $(SRCDIR)\pgplot.inc $(SRCDIR)\grpckg1.inc
	     REM
PGPLOT.LIB:: pgask.obj pgbbuf.obj pgbeg.obj pgbin.obj pgbox.obj pgbox1.obj
	link32 -lib /out:pgplot.lib $?
PGPLOT.LIB::pgcn01.obj pgcnsc.obj pgconb.obj pgcons.obj pgcont.obj pgconx.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: pgcp.obj pgcurs.obj pgdraw.obj pgebuf.obj pgend.obj pgenv.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB::pgerrb.obj pgerrx.obj pgerry.obj pgetxt.obj pgfunt.obj pgfunx.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB::pgfuny.obj pggray.obj pghi2d.obj pghis1.obj pghist.obj pgiden.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: pglab.obj pglcur.obj pgldev.obj pglen.obj pgline.obj pgmove.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: pgmtxt.obj pgncur.obj pgnpl.obj pgnumb.obj pgolin.obj pgpage.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: pgpap.obj pgpixl.obj pgpnts.obj pgpoly.obj pgpt.obj pgptxt.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: pgqcf.obj pgqch.obj pgqci.obj pgqcol.obj pgqcr.obj pgqfs.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: pgqinf.obj pgqls.obj  pgqlw.obj  pgqpos.obj pgqvp.obj  pgqwin.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: pgrect.obj pgrnd.obj  pgrnge.obj pgscf.obj  pgsch.obj  pgsci.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: pgscr.obj  pgsfs.obj  pgshls.obj pgsls.obj  pgslw.obj  pgsvp.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: pgswin.obj pgtbox.obj pgtext.obj  pgqtxt.obj pgqtbg.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: pgupdt.obj pgvsiz.obj pgvstd.obj pgvw.obj  pgwnad.obj  pgstbg.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: pgadvanc.obj pgbegin.obj  pgcurse.obj  pglabel.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: pgmtext.obj   pgncurse.obj pgpaper.obj  pgpoint.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: pgptext.obj   pgvport.obj  pgvsize.obj  pgvstand.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: pgwindow.obj  pgqcs.obj  pgsubp.obj  pgcl.obj pghtch.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: pgarro.obj pgsah.obj pgsave.obj pgscrn.obj pgqvsz.obj pgqhs.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: pgcirc.obj pgvect.obj pgqah.obj pgwedg.obj pgpanl.obj pgshs.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: pgscir.obj pgqcir.obj pgeras.obj pgconl.obj pgband.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: pgimag.obj pgctab.obj pgsitf.obj pgqitf.obj pgnoto.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: pgclos.obj pginit.obj pgopen.obj pgqid.obj pgslct.obj
	link32 -lib pgplot.lib $?

PGPLOT.LIB:: grarea.obj grbpic.obj grchsz.obj grclip.obj grctoi.obj grmsg.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: grclos.obj grclpl.obj grcurs.obj grdot0.obj grdtyp.obj grwarn.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: gresc.obj gretxt.obj grfa.obj grskpb.obj grepic.obj grpxps.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: grgray.obj  grqci.obj grqdev.obj grqdt.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: grqfnt.obj grqcol.obj grqls.obj  grqlw.obj grqpos.obj grqtyp.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: grldev.obj grlen.obj grlin0.obj grlin1.obj grlin2.obj grlin3.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: grlina.obj grmcur.obj grmker.obj grmova.obj gropen.obj grquit.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: grpage.obj grpars.obj grrect.obj grscr.obj grslct.obj grqcap.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: grsetc.obj grsci.obj grsfnt.obj grsls.obj grslw.obj  grqtxt.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: grsets.obj grsize.obj grsyds.obj grsymk.obj grsyxd.obj grprom.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: grterm.obj grtext.obj grtoup.obj grtrn0.obj grtxy0.obj grtrim.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: grvct0.obj grxhls.obj grxrgb.obj  grpixl.obj grgfil.obj grinit.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: grpxpo.obj grpxpx.obj grpxre.obj grrec0.obj grfao.obj gritoc.obj
	link32 -lib pgplot.lib $?
PGPLOT.LIB:: grpocl.obj grqcr.obj grimg0.obj grimg1.obj grimg2.obj grimg3.obj
	link32 -lib pgplot.lib $?
# DOS
PGPLOT.LIB:: grsy00.obj grexec.obj grdos.obj msdriv.obj grms1c.obj grms2m.obj
	link32 -lib pgplot.lib $?
grsy00.obj : $(SYSDIR)\grsy00.f
	$(FCOMPL) /c $(FFLAGC) /Tf$(SYSDIR)\grsy00.f
grexec.obj : $(SYSDIR)\grexec.f
	$(FCOMPL) /c $(FFLAGC) /Tf$(SYSDIR)\grexec.f
grdos.obj : $(SYSDIR)\grdos.f
	$(FCOMPL) /c $(FFLAGC) /Tf$(SYSDIR)\grdos.f
msdriv.obj : $(SYSDIR)\msdriv.f
	$(FCOMPL) /c $(FFLAGC) /Tf$(SYSDIR)\msdriv.f
grms1c.obj : $(SYSDIR)\grms1c.f
	$(FCOMPL) /c  /Tf$(SYSDIR)\grms1c.f
grms2m.obj : $(SYSDIR)\grms2m.f
        $(FCOMPL) /c  /Tf$(SYSDIR)\grms2m.f
PGPLOT.LIB::
	link32 -lib pgplot.lib mouse.obj

#-----------------------------------------------------------------------
# Device drivers
# Compile drivers that compile under Microsoft Fortran.
# ctd : need to modify rules for finding /drivers or simply copy into /src
PGPLOT.LIB:: nudriv.obj psdriv.obj lxdriv.obj
#hjdriv.obj
	link32 -lib pgplot.lib $?
nudriv.obj : $(DRVDIR)\nudriv.f
	$(FCOMPL) /c $(FFLAGC) /Tf$(DRVDIR)\nudriv.f
psdriv.obj : $(DRVDIR)\psdriv.f
	$(FCOMPL) /c $(FFLAGC) /Tf$(DRVDIR)\psdriv.f
#hjdriv.obj : $(DRVDIR)\hjdriv.f
#        $(FCOMPL) /c $(FFLAGC) /Tf$(DRVDIR)\hjdriv.f
#gidriv.obj : $(DRVDIR)\gidriv.f
#        $(FCOMPL) /c $(FFLAGC) /Tf$(DRVDIR)\gidriv.f
#ttdriv.obj : $(DRVDIR)\ttdriv.f  grcter groter grpter grwter missing
#        $(FCOMPL) /c $(FFLAGC) /Tf$(DRVDIR)\ttdriv.f
lxdriv.obj : $(DRVDIR)\lxdriv.f
	$(FCOMPL) /c $(FFLAGC) /Tf$(DRVDIR)\lxdriv.f
#ljdriv.obj : $(DRVDIR)\ljdriv.f
#        $(FCOMPL) /c $(FFLAGC) /Tf$(DRVDIR)\ljdriv.f
#-----------------------------------------------------------------------
# Target "lib" is used to built the PGPLOT subroutine library.
# libpgplot.a is the primary PGPLOT library.
#-----------------------------------------------------------------------
lib : PGPLOT.LIB

#-----------------------------------------------------------------------
# libpgobs.a contains obsolete routines used by some programs
#-----------------------------------------------------------------------
#OBSOLETE_ROUTINES=\
#    grchar.obj grchr0.obj grdat2.obj grgtc0.obj grmark.obj grinqli.obj\
#    grinqpen.obj\
#    grsetli.obj grsetpen.obj grlinr.obj grmovr.obj grtran.obj grvect.obj\
#    pgsetc.obj pgsize.obj grinqfon.obj grsetfon.obj
#
#PGOBS.LIB : $(OBSOLETE_ROUTINES)
#        link32 -lib PGOBS.LIB $?
#
#-----------------------------------------------------------------------
# Target "prog" is used to make the demo programs. They can also be made
# individually.
#-----------------------------------------------------------------------
# List of demo programs
DEMOS = pgdemo1.exe pgdemo2.exe pgdemo3.exe pgdemo4.exe pgdemo5.exe \
      pgdemo6.exe pgdemo7.exe pgdemo8.exe pgdemo9.exe pgdemo10.exe \
      pgdemo11.exe pgdemo12.exe pgdemo13.exe pgdemo14.exe

prog: $(DEMOS)

pgdemo1.exe: $(DEMODIR)\pgdemo1.f
	$(FCOMPL) $(FFLAGC) /Tf$(DEMODIR)\pgdemo1.f $(LIBS)
pgdemo2.exe: $(DEMODIR)\pgdemo2.f
	$(FCOMPL) $(FFLAGC) /Tf$(DEMODIR)\pgdemo2.F $(LIBS)
pgdemo3.exe: $(DEMODIR)\pgdemo3.f
	$(FCOMPL) $(FFLAGC) /Tf$(DEMODIR)\pgdemo3.F $(LIBS)
pgdemo4.exe: $(DEMODIR)\pgdemo4.f
	$(FCOMPL) $(FFLAGC) /Tf$(DEMODIR)\pgdemo4.F $(LIBS)
pgdemo5.exe: $(DEMODIR)\pgdemo5.f
	$(FCOMPL) $(FFLAGC) /Tf$(DEMODIR)\pgdemo5.F $(LIBS)
pgdemo6.exe: $(DEMODIR)\pgdemo6.f
	$(FCOMPL) $(FFLAGC) /Tf$(DEMODIR)\pgdemo6.F $(LIBS)
pgdemo7.exe: $(DEMODIR)\pgdemo7.f
	$(FCOMPL) $(FFLAGC) /Tf$(DEMODIR)\pgdemo7.F $(LIBS)
pgdemo8.exe: $(DEMODIR)\pgdemo8.f
	$(FCOMPL) $(FFLAGC) /Tf$(DEMODIR)\pgdemo8.F $(LIBS)
pgdemo9.exe: $(DEMODIR)\pgdemo9.f
	$(FCOMPL) $(FFLAGC) /Tf$(DEMODIR)\pgdemo9.F $(LIBS)
pgdemo10.exe: $(DEMODIR)\pgdemo10.f
	$(FCOMPL) $(FFLAGC) /Tf$(DEMODIR)\pgdemo10.F $(LIBS)
pgdemo11.exe: $(DEMODIR)\pgdemo11.f
	$(FCOMPL) $(FFLAGC) /Tf$(DEMODIR)\pgdemo11.F $(LIBS)
pgdemo12.exe: $(DEMODIR)\pgdemo12.f
	$(FCOMPL) $(FFLAGC) /Tf$(DEMODIR)\pgdemo12.F $(LIBS)
pgdemo13.exe: $(DEMODIR)\pgdemo13.f
        $(FCOMPL) $(FFLAGC) /Tf$(DEMODIR)\pgdemo13.F $(LIBS)
pgdemo14.exe: $(DEMODIR)\pgdemo14.f
        $(FCOMPL) $(FFLAGC) /Tf$(DEMODIR)\pgdemo14.F $(LIBS)
#-----------------------------------------------------------------------
# Target "grfont.dat" is the binary font file.
# This is created from grfont.txt with the "pgpack" program.
#    (a) compile the `pgpack' program; then
#    (b) run `pgpack' to convert the ASCII version of the font file
#    (grfont.txt) into the binary version (grfont.dat). When executed,
#    `pgpack' should report:
#       Characters defined:   996
#       Array cells used:   26732
#-----------------------------------------------------------------------
pgpack.exe: $(FONTDIR)\pgpack.f
	$(FCOMPL) $(FFLAGC) /Tf$(FONTDIR)\pgpack.f
#
#grfont.dat: $(FONTDIR)\grfont.txt pgpack.exe
#        DEL grfont.dat
#        pgpack < $(FONTDIR)\grfont.txt
#  problem with dos extender: run outside nmake,note program name:pack
#-----------------------------------------------------------------------
# Target "install" is required for Figaro.
#-----------------------------------------------------------------------
install:

#-----------------------------------------------------------------------
# Target "clean" is used to remove all the intermediate files.
#-----------------------------------------------------------------------
clean :
	DEL *.OBJ
	DEL *.FOR
	DEL PGPLOT.BAK
	DEL PGPACK.EXE
	DEL PGPLOT.INC
	DEL GRPCKG1.INC

SPOTLESS: CLEAN
	DEL *.EXE
	DEL GRFONT.DAT
	DEL PGPLOT.LIB

# Include file dependencies:
# The following PG routines reference `pgplot.inc'

grgray.o  grgrgr.o pgask.o  pgbbuf.o  pgbeg.o  pgbin.o  pgbox.o \
pgcont.o pgcp.o pgcurs.o pgebuf.o pgend.o pgerrb.o pgerrx.o pgerry.o pgscir.o\
pggray.o pghi2d.o pghist.o pgiden.o pglcur.o pglen.o pgmtxt.o pgncur.o pgsitf.o\
pgolin.o pgpage.o pgpap.o pgpixl.o pgpoly.o pgpt.o pgptxt.o pgqah.o \
pgqch.o pgqcs.o pgqfs.o pgqinf.o pgqvsz.o pgqtbg.o pgqvp.o pgqwin.o \
pgrect.o pgsah.o pgsch.o pgsfs.o pgstbg.o pgsubp.o pgsvp.o pgswin.o \
pgband.o pgcl.o pgconl.o pgimag.o pgnoto.o  pgcirc.o pgqitf.o pgqtxt.o \
pgupdt.o pgvsiz.o pgvstd.o pgvw.o pgwedg.o pgwnad.o: $(SRC)/pgplot.inc

# The following GR routines reference `grpckg.inc'

grarea.o grbpic.o grchr0.o grchsz.o grclos.o grclpl.o grcurs.o grepic.o\
grdot0.o grdtyp.o gresc.o  gretxt.o grfa.o   grgray.o grgrgr.o grqcr.o \
grldev.o grlen.o  grlin0.o grlin1.o grlin2.o grlin3.o grqcap.o grqtxt.o \
grlina.o grlinr.o grmker.o grmova.o grmovr.o gropen.o grpage.o \
grpixl.o grpxpo.o grpxpx.o grimg0.o grimg1.o grimg2.o grimg3.o \
grqci.o  grqcol.o grqdev.o grqdt.o  grqfnt.o grqls.o grpxps.o  \
grqlw.o  grqpos.o grqtyp.o grrec0.o grrect.o \
grsci.o  grscr.o  grsetc.o grsetli.o grsets.o grsfnt.o grsfs.o grsize.o \
grslct.o grsls.o  grslw.o  grterm.o grtext.o grtrn0.o grtxy0.o \
grvct0.o: $(SRC)/grpckg1.inc

griv00.o : $(SRCDIR)/gadef.h $(SRCDIR)/gmdef.h $(SRCDIR)/gphdef.h
grtv00.o : $(SRCDIR)/imdef.h
