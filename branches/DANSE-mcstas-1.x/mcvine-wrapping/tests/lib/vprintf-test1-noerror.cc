/*
 This test is for the "vprintf" function.
 This file should work fine. Compare it to vprintf-test1-error.cc.
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
  char fmt[] = "%2$s%1$s%2$s\n";
  char fmt1[] = "%s%s";
  char str1[] = "abcd";
  char str2[]  = "efg";

  pprintf (fmt, fmt1, str1, str2);
}
