#include <fortran.h>

/*GRFILEIO -- Fast low-level UNIX I/O routines
 * +
 *
 * GRFILEIO is a set of functions that makes fast, low-level Unix I/O routines
 * available to a Fortran program, accounting for some of the differences in the
 * C/Fortran interfaces of different machines.
 *
 * Specifically, some linkers expect a C-routine which is called from a 
 * Fortran program to have a name ending in an underscore.  So, for example,
 * if the Fortran program calls "grofil()", the C-function's name must
 * be "grofil_()".  Other vendor's (like HP) look for a C-routine with
 * the actual name as called from Fortran (without the underscore).  Therefore,
 * the following routines link the underscored function name to the same
 * name without an underscore so that both implementations of the C/Fortran
 * interface will ultimately call the same low-level routines.
 *
 * Secondly, when character strings are passed from Fortran to C, the
 * string's length is implicitly passed, unbeknownst to the Fortran caller.
 * Some C/Fortran interfaces put the string length as the next argument after
 * the string itself; others place the string length at the end of the
 * argument list.  Again, to support both implementations, when a string is
 * passed to the following routines it is the last argument in the Fortran
 * call, so that the string and its length are the last two arguments in the
 * corresponding C-function.
 *
 *-------
 * 2-Dec-92 - fastio.c: John L. Lillibridge, NOAA/NOS/OES Geosciences Lab
 * 11-Nov-93 - Addition of seekf and warning by Remko Scharroo, DUT/SSR&T
 * 17-May-94 - Nice manual
 * 13-Oct-94 - Bits not required by PGPLOT stripped out; routine names
 *            changed [TJP].
 * 09-Nov-94 - Tidied and ported to Cray [mcs] (untested).
 * 10-Nov-94 - Added GRFCH() routine to write FORTRAN CHARACTER sub-strings.
 *-------
 */

#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>

/*
 **&GROFIL -- Open file for writing with GRFILEIO
 *+
 *     FUNCTION GROFIL (FNAME)
 *     INTEGER GROFIL
 *     CHARACTER*(*) FNAME
 *
 * Opens file FNAME for writing.
 * GROFIL returns the file descriptor for use in subsequent calls to
 * grwfil or grcfil. If GROFIL is negative, an error occurred while
 * opening the file.
 *
 **
 * Usage:
 *
 *     FD = GROFIL ('output_file')
 *     CALL GRWFIL (FD, 4, STRING)
 *
 * Arguments:
 *  FNAME  (input) : File name of the input or output file
 *  GROFIL (output) : Contains the file descriptor on return. If GROFIL < 0
 *                   an error occurred while opening the file.
 *-
 */
int GROFIL(_fcd fname)
{
  char *name = _fcdtocp(fname); /* C pointer to FORTRAN string */
  int   slen = _fctlen(fname);  /* Length of the FORTRAN string */
  char *buff=0;                 /* Dynamically allocated copy of name[] */
  int fd = -1;                  /* File descriptor to be returned */
/*
 * Determine how long the FORTRAN string is by searching for the last
 * non-blank character in the string.
 */
  while(slen>0 && name[slen-1]==' ')
    slen--;
/*
 * Dynamically allocate a buffer to copy the FORTRAN string into.
 */
  buff = (char *) malloc((slen+1) * sizeof(char));
  if(buff) {
/*
 * Make a C string copy of the FORTRAN string.
 */
    strncpy(buff, name, slen);
    buff[slen] = '\0';
/*
 * Open the file and return its descriptor.
 */
    fd = open(buff, O_WRONLY | O_CREAT | O_TRUNC, 0666);
    free(buff);
  } else {
    fprintf(stderr, "gropfil: Insufficient memory\n");
  };
  return fd;
}

/*
 **&GRCFIL -- Close file from GRFILEIO access
 *+
 *     FUNCTION GRCFIL (FD)
 *     INTEGER GRCFIL (FD)
 *
 * Closes the file with descriptor FD from GRFILEIO access. GRCFIL returns
 * 0 when properly closed. Otherwise, use PERRORF to report the error.
 * 
 * Usage:
 *      IOS = GRCFIL (FD)
 * or:
 *      CALL GRCFIL (FD)
 *
 * In the last case the return code is ignored.
 *
 * Arguments:
 *  FD      (input) : File descriptor returned by GROFIL.
 *  GRCFIL (output) : Error code or 0 on proper closing.
 *-
 */
int GRCFIL(int *fd)
{
  return close(*fd);
}

/*
 **&GRWFIL -- GRFILEIO write routine
 *+
 *     FUNCTION GRWFIL (FD, NBYTE, BUFFER)
 *     INTEGER FD, NBYTE, GRWFIL
 *     BYTE    BUFFER(NBYTE)
 *
 * Writes NBYTE bytes into the file associated by descriptor FD (which is
 * returned by the GROFIL call. The array BUFFER contains the data that has
 * to be written, but can (of course) also be associated with any other
 * string, scalar, or n-dimensional array.
 * The function returns the number of bytes actually written in GRWFIL. If
 * GRWFIL < 0, a write error occurred.
 *
 * Arguments:
 *  FD      (input) : File descriptor returned by GROFIL
 *  NBYTE   (input) : Number of bytes to be written
 *  BUFFER  (input) : Buffer containing the bytes that have to be written
 *  GRWFIL (output) : Number of bytes written, or (if negative) error code.
 *-
 */
int GRWFIL(int *fd, int *nbytes, char *buf)
{
  return write(*fd, (void *) buf, *nbytes);
}

/*
 **&GRWFCH -- GRFILEIO write FORTRAN character sub-STRING routine
 *+
 *     FUNCTION GRWFCH (FD, NBYTE, BUFFER)
 *     INTEGER FD, NBYTE, GRWFCH
 *     BYTE    BUFFER(NBYTE)
 *
 * Writes NBYTE bytes into the file associated by descriptor FD (which is
 * returned by the GROFIL call. The array BUFFER contains the data that has
 * to be written, but can (of course) also be associated with any other
 * string, scalar, or n-dimensional array.
 * The function returns the number of bytes actually written in GRWFCH. If
 * GRWFCH < 0, a write error occurred.
 *
 * Arguments:
 *  FD      (input) : File descriptor returned by GROFIL
 *  NBYTE   (input) : Number of bytes to be written
 *  BUFFER  (input) : Buffer containing the bytes that have to be written
 *  GRWFCH (output) : Number of bytes written, or (if negative) error code.
 *-
 */
int GRWFCH(int *fd, int *nbytes, _fcd buf)
{
  return write(*fd, (void *) _fcdtocp(buf), *nbytes);
}
