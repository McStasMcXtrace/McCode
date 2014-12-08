C*GRLGTR -- translate logical name (VMS)
C+
      SUBROUTINE GRLGTR (NAME)
      CHARACTER*(*) NAME
C
C This is used in the parsing of device specifications in the
C VMS implementation of PGPLOT. In other implementations, it may
C be replaced by a null routine.
C
C Argument:
C  NAME (input/output): initially contains the name to be
C       inspected.  If an equivalence is found it will be replaced
C       with the new name. If not, the old name will be left there.
C--
C (1-Feb-1983)
C 10-Apr-1996: remove uppercasing; logical names may be case-sensitive.
C 13-Sep-1996: new version using $TRNLNM; not recursive.
C-----------------------------------------------------------------------
      INCLUDE '($LNMDEF)'
      INCLUDE '($SYSSRVNAM)'
      STRUCTURE /LIST/
         INTEGER*2 BUF_LEN/255/
         INTEGER*2 ITEM_CODE/LNM$_STRING/
         INTEGER*4 TRANS_LOG
         INTEGER*4 TRANS_LEN
         INTEGER*4 END_ENTRY/0/
      END STRUCTURE
      
      CHARACTER*255 EQV_BUFFER
      INTEGER*2 W_NAMELEN
      INTEGER L, GRTRIM, ISTAT
      RECORD /LIST/ ITEM_LIST

      ITEM_LIST.TRANS_LOG = %LOC(EQV_BUFFER)
      ITEM_LIST.TRANS_LEN = %LOC(W_NAMELEN)
      
      L = GRTRIM(NAME)
      ISTAT = SYS$TRNLNM(LNM$M_CASE_BLIND, 
     :     'LNM$FILE_DEV',
     :     NAME(1:L),
     :     ,
     :     ITEM_LIST)
      IF (ISTAT) NAME = EQV_BUFFER(:W_NAMELEN)
      RETURN
      END
