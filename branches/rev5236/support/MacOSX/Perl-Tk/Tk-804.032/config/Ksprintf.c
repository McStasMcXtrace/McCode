#include <stdio.h>
#undef _ANSI_ARGS_
#if defined(USE_PROTOTYPE) || ((defined(__STDC__) || defined(SABER)) && !defined(NO_PROTOTYPE)) || defined(__cplusplus)
#   define _USING_PROTOTYPES_ 1
#   define _ANSI_ARGS_(x)	x
#else
#   define _ANSI_ARGS_(x)	()
#endif

#ifdef SPRINTF_RETURN_CHAR
extern char *sprintf _ANSI_ARGS_((char *,const char *, ...));
#else
extern int sprintf _ANSI_ARGS_((char *,const char *, ...));
#endif

int main()
{
 char buf[16];
 sprintf(buf,"%d",0);
 return 0;
}
