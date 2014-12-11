
C*GRGCOM -- read with prompt from user's terminal (VMS)
C+
      INTEGER FUNCTION GRGCOM(STRING, PROMPT, L)
      CHARACTER*(*) STRING, PROMPT
      INTEGER L
C
C Issue prompt and read a line from the user's terminal; in VMS,
C this is equivalent to LIB$GET_COMMAND.
C
C Arguments:
C  STRING : (output) receives the string read from the terminal.
C  PROMPT : (input) prompt string.
C  L      : (output) length of STRING.
C
C Returns:
C  GRGCOM : 1 if successful, 0 if an error occurs (e.g., end of file).
C--
C  9-Feb-1988
C 12-May-1998: workaround for VMS bug, provided by Magnus Zolliker; the
C      extra CR//LF is sometimes required if the terminal is used as
C      a graphics device of type /TEK or similar.
C-----------------------------------------------------------------------
      INTEGER CR, LF
      PARAMETER (CR=13, LF=10)
      INTEGER LIB$GET_COMMAND
C
      GRGCOM = LIB$GET_COMMAND(STRING, CHAR(CR)//CHAR(LF)//PROMPT, L)
      IF (GRGCOM.NE.1) GRGCOM = 0
      END
