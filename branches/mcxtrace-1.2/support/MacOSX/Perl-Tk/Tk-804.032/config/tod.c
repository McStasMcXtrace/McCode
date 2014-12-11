#include <sys/time.h>
#include <stdlib.h>

#undef _ANSI_ARGS_
#if defined(USE_PROTOTYPE) || ((defined(__STDC__) || defined(SABER)) && !defined(NO_PROTOTYPE)) || defined(__cplusplus)
#   define _USING_PROTOTYPES_ 1
#   define _ANSI_ARGS_(x)	x
#else
#   define _ANSI_ARGS_(x)	()
#endif

#ifdef TIMEOFDAY_NO_TZ
#define Tk_timeofday(x) gettimeofday(x)
 extern int gettimeofday _ANSI_ARGS_((struct timeval *tp));
#else
#ifdef TIMEOFDAY_TZ
#define Tk_timeofday(x) gettimeofday(x, (struct timezone *) 0)
extern int gettimeofday _ANSI_ARGS_((struct timeval *tp ,struct timezone *tzp));
#else
#define Tk_timeofday(x) gettimeofday(x, (void *) 0)
#ifdef TIMEOFDAY_DOTS
extern int gettimeofday _ANSI_ARGS_((struct timeval *tp, ...));
#else
extern int gettimeofday _ANSI_ARGS_((struct timeval *tp, void *));
#endif
#endif
#endif

int main()
{
 struct timeval t;
 Tk_timeofday(&t);
 return 0;
}
