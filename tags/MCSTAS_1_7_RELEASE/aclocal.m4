AC_DEFUN([CF_ANSI_CC_CHECK],
[
AC_MSG_CHECKING(for ${CC-cc} option to accept ANSI C)
AC_CACHE_VAL(cf_cv_ansi_cc,[
cf_cv_ansi_cc=no
cf_save_CFLAGS="$CFLAGS"
# Don't try gcc -ansi; that turns off useful extensions and
# breaks some systems' header files.
# AIX			-qlanglvl=ansi
# Ultrix and OSF/1	-std1
# HP-UX			-Aa -D_HPUX_SOURCE
# SVR4			-Xc
# UnixWare 1.2		(cannot use -Xc, since ANSI/POSIX clashes)
for cf_arg in "-DCC_HAS_PROTOS" "" -qlanglvl=ansi -std1 "-Aa -D_HPUX_SOURCE" -Xc
do
	CFLAGS="$cf_save_CFLAGS $cf_arg"
	AC_TRY_COMPILE(
[
#ifndef CC_HAS_PROTOS
#if !defined(__STDC__) || __STDC__ != 1
choke me
#endif
#endif
],[
	int test (int i, double x);
	struct s1 {int (*f) (int a);};
	struct s2 {int (*f) (double a);};],
	[cf_cv_ansi_cc="$cf_arg"; break])
done
CFLAGS="$cf_save_CFLAGS"
])
AC_MSG_RESULT($cf_cv_ansi_cc)

if test "$cf_cv_ansi_cc" != "no"; then
if test ".$cf_cv_ansi_cc" != ".-DCC_HAS_PROTOS"; then
	CFLAGS="$CFLAGS $cf_cv_ansi_cc"
else
	AC_DEFINE(CC_HAS_PROTOS)
fi
fi
])dnl


AC_DEFUN([MC_ANSI_MATH_PROTO],
[
AC_MSG_CHECKING(for proper ANSI C math prototypes)
AC_CACHE_VAL(mc_cv_ansi_math_proto,[
# OSF/1 (Digital/Compaq Unix) cc needs -std1 for full ANSI C math prototypes
AC_TRY_COMPILE(
[
#include <math.h>
],[
{ double a = sqrt(&a); }],
[mc_save_CFLAGS="$CFLAGS"
CFLAGS="$cf_save_CFLAGS -std1"
AC_TRY_COMPILE(
[
#include <math.h>
],[
{ double a = sqrt(&a); }],
[mc_cv_ansi_math_proto=no],[mc_cv_ansi_math_proto="-std1"])
CFLAGS="$cf_save_CFLAGS"],[mc_cv_ansi_math_proto=yes])
])
AC_MSG_RESULT($mc_cv_ansi_math_proto)

if test "$mc_cv_ansi_math_proto" != "yes"; then
if test "$mc_cv_ansi_math_proto" != "no"; then
	CFLAGS="$CFLAGS $mc_cv_ansi_math_proto"
else
	AC_MSG_WARN(C compiler seems not to do ANSI math prototypes)
fi
fi
])dnl
