/*
 **GRUSER -- get user name.
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
 * 13-Nov-1994 [mcs] Absoft FORTRAN callable version for NeXT.
 *-----------------------------------------------------------------------
 */

char *getlogin();

void GRUSER(string, length, maxlen, w_length)
     char *string;
     int *length;
     int maxlen;
     int w_length;
{
  int i;
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
