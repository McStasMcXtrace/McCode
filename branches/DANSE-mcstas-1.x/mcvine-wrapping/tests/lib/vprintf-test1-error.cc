/*
 This test is for the "vprintf" function.
 Compile this file with
   $ g++ -O <filename>
 and run
   $ ./a.out
 For some compilers, the following error shows up:
    *** invalid %N$ use detected ***
    Aborted

 Without -O flag, it works fine.

 Compilers that show error:
   * gcc version 4.4.1 (Ubuntu 4.4.1-4ubuntu9)

 It seems that this error occurs when the "fmt" string
 does not have all the args.
*/

#include <cstdio>
#include <cstdarg>

static int pprintf(char *fmt, char *fmt_args, ...)
{ /* wrapper to standard fprintf */
  va_list ap;
  int tmp;

  va_start(ap, fmt_args);
  tmp=vprintf(fmt, ap);
  va_end(ap);
  return(tmp);
}

int main()
{
  char fmt[] = "%2$s\n";
  char fmt1[] = "%s%s";
  char str1[] = "abcd";
  char str2[]  = "efg";

  pprintf (fmt, fmt1, str1, str2);
}
