#include <fortran.h>

/*
 **GRUSER -- get user name (Cray)
 *+
 *     SUBROUTINE GRUSER(STRING, L)
 *     CHARACTER*(*) STRING
 *     INTEGER L
 *
 * Return the name of the user running the program.
 *
 * Arguments:
 *  STRING : receives user name, truncated or extended with
 *           blanks as necessary.
 *  L      : receives the number of characters in VALUE, excluding
 *           trailing blanks.
 *--
 * 09-Nov-1994 - [mcs] Fortran callable C version for CRAY.
 *-----------------------------------------------------------------------
 */

char *getlogin();

void GRUSER(_fcd fortran_string, int *length)
{
  int i;
/*
 * Extract the return string pointer and length from the fortran string.
 */
  char *string = _fcdtocp(fortran_string);
  int maxlen = _fcdlen(fortran_string);
/*
 * Get the login name of the PGPLOT user.
 */
  char *user = getlogin();
/*
 * If the user name is not available substitute an empty string.
 */
  if(!user)
    user = "";
/*
 * Copy the user name to the output string.
 */
  for(i=0; i<maxlen && user[i]; i++)
    string[i] = user[i];
/*
 * Return the un-padded length of the user name string.
 */
  *length = i;
/*
 * Pad to the end of the output string with spaces.
 */
  for( ; i<maxlen; i++)
    string[i] = ' ';
  return;
}
