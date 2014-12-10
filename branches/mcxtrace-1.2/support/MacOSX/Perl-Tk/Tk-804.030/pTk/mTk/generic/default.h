/*
 * default.h --
 *
 *	This file defines the defaults for all options for all of
 *	the Tk widgets.
 *
 * Copyright (c) 1991-1994 The Regents of the University of California.
 * Copyright (c) 1994 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * RCS: @(#) $Id: default.h,v 1.4 2002/08/31 06:12:19 das Exp $
 */

#ifndef _DEFAULT
#define _DEFAULT

#if defined(__WIN32__) || defined(_WIN32) || \
    (defined(__CYGWIN__) && !defined(WINARCH_X)) || defined(__MINGW32__)
#   include "tkWinDefault.h"
#else
#   if defined(MAC_OSX_TK)
#	include "tkMacOSXDefault.h"
#   elif defined(MAC_TCL)
#	include "tkMacDefault.h"
#   else
#      ifdef __PM__
#         include "tkOS2Default.h"
#      else
#         include "tkUnixDefault.h"
#      endif
#   endif
#endif

#endif /* _DEFAULT */
