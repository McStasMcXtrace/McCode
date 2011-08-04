$! Define logical names for PGPLOT. It is assumed that the PGPLOT
$! directory is the current default directory.
$!----------------------------------------------------------------------
$ PGPLOT   = F$ENVIRONMENT("DEFAULT")
$ DEF      = "DEFINE/NOLOG"
$!
$ DEF PGPLOT_DIR      'PGPLOT'
$                        ! directory containing PGPLOT library.
$ DEF PGPLOT_TYPE     "PS"
$			 ! default device type, used if "/type" is
$			 ! omitted (example).
$ DEF PGPLOT_DEV      "/XSERV"
$                        ! default device spec, used if blank string
$			 ! specified in PGBEG (example).
$! DEF PGPLOT_FONT     PGPLOT_DIR:GRFONT.DAT
$			 ! location of binary font file, used at
$			 ! run-time.
$! DEF PGPLOT_RGB      PGPLOT_DIR:RGB.TXT
$			 ! location of color definition file.
$ DEF GRPSHR          PGPLOT_DIR:GRPSHR.EXE
$			 ! location of PGPLOT shareable image, used
$			 ! at run-time.
$ DEF LNK$LIBRARY     PGPLOT_DIR:GRPSHR.OLB
$			 ! location of symbol-table library, used
$			 ! when linking PGPLOT programs.
$!DEF PGPLOT_BACKGROUND "slate grey"
$			 ! default background color (example).
$!DEF PGPLOT_FOREGROUND "yellow"
$			 ! default foreground color (example).
