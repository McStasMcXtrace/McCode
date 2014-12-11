#include "tkPort.h"
char *
strdup(s)
const char *s;
{
 char *p = (char *) malloc(strlen(s)+1);
 if (p)
  strcpy(p,s);
 return p;
}

