#include <fortran.h>
#include <stdlib.h>
#include <string.h>

/*
 **GRGENV -- get value of PGPLOT environment parameter (Cray)
 *+
 *    SUBROUTINE GRGENV(NAME, VALUE, L)
 *    CHARACTER*(*) NAME, VALUE
 *    INTEGER L
 *
 * Return the value of a PGPLOT environment parameter. In Sun/Convex-UNIX,
 * environment parameters are UNIX environment variables; e.g. parameter
 * ENVOPT is environment variable PGPLOT_ENVOPT. Translation is not
 * recursive and is case-sensitive.
 *
 * Arguments:
 *  NAME   : (input) the name of the parameter to evaluate.
 *  VALUE  : receives the value of the parameter, truncated or extended
 *           with blanks as necessary. If the parameter is undefined,
 *           a blank string is returned.
 *  L      : receives the number of characters in VALUE, excluding
 *           trailing blanks. If the parameter is undefined, zero is
 *           returned.
 *--
 * 09-Nov-1994 - [mcs] Fortran callable C version for CRAY.
 *-----------------------------------------------------------------------
 */
void GRGENV(_fcd fortran_name, _fcd fortran_value, int *length)
{
  static char *prefix = "PGPLOT_";  /* Environment variable name prefix */
  char test[33];     /* PGPLOT_* Concatenation buffer */
  int name_len;      /* Un-padded length of 'name' string */
  int prefix_len;    /* The length of prefix[] */
  char *env=0;       /* Environment variable value */
  int i;
/*
 * Extract the string pointer and dimension of each FORTRAN string argument.
 */
  char *name = _fcdtocp(fortran_name);
  int name_dim = _fcdlen(fortran_name);
  char *value = _fcdtocp(fortran_value);
  int value_dim = _fcdlen(fortran_value);
/*
 * Determine the length of 'name' by searching for the last
 * non-space character.
 */
  name_len = name_dim;
  while(name_len > 0 && name[name_len-1] == ' ')
    name_len--;
/*
 * Determine the length of the prefix.
 */
  prefix_len = strlen(prefix);
/*
 * Prefix 'name' with PGPLOT_ if there is room in test[].
 */
  if(prefix_len + name_len + 1 <= sizeof(test)/sizeof(char)) {
    strcpy(test, prefix);
    strncpy(&test[prefix_len], name, name_len);
    test[prefix_len+name_len] = '\0';
/*
 * Get the value of the environment variable now named in test[].
 */
    env = getenv(test);
  };
/*
 * Substitute an empty string if no value was obtained, or the value
 * obtained is too long to fit in the output string.
 */
  if(env==0 || strlen(env) > value_dim)
    env = "";
/*
 * Copy the environment variable value into the output string.
 */
  strncpy(value, env, value_dim);
/*
 * Return the unpadded length of the string.
 */
  {
    int env_len = strlen(env);
    *length = (env_len <= value_dim) ? env_len : value_dim;
  };
/*
 * Pad the fortran string with spaces.
 */
  for(i = *length; i<value_dim; i++)
    value[i] = ' ';
  return;
}
