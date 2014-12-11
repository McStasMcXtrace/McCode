$! PGPLOT Installation (OpenVMS)
$! Usage: P1 = name of top-level directory of PGPLOT distribution,
$!        e.g., USR:[LOCAL.PGPLOT]
$!        P2 = option to compile (blank for default, PGDISP, CPG, or PGMDEMO)
$!----------------------------------------------------------------------
$ ECHO = "WRITE SYS$OUTPUT"
$!
$! Find operating system version and current directory
$!
$ NODE  = F$GETSY("NODENAME")
$ CPU   = F$GETSYI("HW_NAME")
$ SW    = F$GETSYI("NODE_SWTYPE")+F$GETSYI("NODE_SWVERS")
$ PGBIN = F$ENVIRONMENT("DEFAULT")
$ PGSRC = "[-.PGPLOT]"
$ IF P1 .NES. "" THEN PGSRC = P1
$ PGSRC = F$PARSE(PGSRC,,,"DEVICE") + -
          F$PARSE(PGSRC,,,"DIRECTORY")
$ PGVMS = PGSRC - "]" + ".SYS_VMS]"
$
$ ECHO "------------------------------------------------------------------"
$ ECHO "Installing PGPLOT ", P2, " on ", NODE, " at ", F$TIME()
$ ECHO "    Machine type: ", CPU
$ ECHO "    Software:     ", SW
$ ECHO "PGPLOT library, demos, and  run time files will be installed"
$ ECHO "in the current directory: ", PGBIN
$ ECHO "from the distribution in directory: ",PGSRC
$ ECHO "------------------------------------------------------------------"
$
$ IF P2 .EQS. ""
$ THEN
$    IF F$SEARCH("RGB.TXT") .EQS. ""
$    THEN
$       ECHO "Copying color definition file RGB.TXT"
$       COPY/LOG 'PGSRC'RGB.TXT []
$    ENDIF
$
$    IF F$SEARCH("DRIVERS.LIST") .EQS. "" 
$    THEN
$       ECHO "Copying list of available drivers DRIVERS.LIST"
$       COPY 'PGSRC'DRIVERS.LIST []
$       ECHO "Please edit DRIVERS.LIST to select drivers, and then rerun this procedure"
$       EXIT
$    ENDIF
$
$    @'PGVMS'COMPILE     'PGSRC'
$    @'PGVMS'NEWEXEC
$    @'PGVMS'BUILD
$    @'PGVMS'LOGICAL
$    @'PGVMS'MAKE_FONT   'PGSRC'
$    @'PGVMS'MAKE_DEMOS  'PGSRC'
$ ELSE IF P2 .EQS. "CPG"
$ THEN
$    @'PGVMS'MAKE_CPG    'PGSRC'
$ ELSE IF P2 .EQS. "PGMDEMO"
$ THEN
$    @'PGVMS'MAKE_PGMDEMO 'PGSRC'
$ ELSE IF P2 .EQS. "PGDISP"
$ THEN
$    @'PGVMS'MAKE_PGDISP 'PGSRC'
$ ELSE ECHO "Option P2 = ",P2," is not recognized"
$ ENDIF
$ ENDIF
$ ENDIF
$ ENDIF
$
$ ECHO "--------------------------------------------------------------"
$ ECHO "PGPLOT ", P2, " installation completed at ", F$TIME()
$ ECHO "--------------------------------------------------------------"
$ EXIT
