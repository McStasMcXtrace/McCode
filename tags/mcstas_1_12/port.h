/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Kernel: port.h
*
* %Identification
* Written by: K.N.
* Date: Nov 11, 1998
* Origin: Risoe
* Release: McStas 1.6
* Version: $Revision: 1.18 $
*
* Header file for portability related stuff.
*
* $Id: port.h,v 1.18 2007-03-09 08:48:58 pkwi Exp $
*
*******************************************************************************/

/* Machintosh specific compiler defines. */
#ifdef __dest_os
#if (__dest_os == __mac_os)
#define MAC
#endif
#endif

/* File system details. */
#ifndef MC_PATHSEP_C
#ifdef WIN32
#define MC_PATHSEP_S "\\"
#define MC_PATHSEP_C '\\'
#define CURRENT_DIR_S "."
#else  /* !WIN32 */
#ifdef MAC
#define MC_PATHSEP_S ":"
#define MC_PATHSEP_C ':'
#define CURRENT_DIR_S ""	/* Apparently no Mac equivalent for this. */
#else  /* !WIN32 && !MAC */
#define MC_PATHSEP_S "/"
#define MC_PATHSEP_C '/'
#define CURRENT_DIR_S "."
#endif /* !MAC */
#endif /* !WIN32 */
#endif /* MC_PATHSEP_C */

#ifndef MCSTAS
#ifdef WIN32
#define MCSTAS "C:\\mcstas\\lib"
#else  /* !WIN32 */
#ifdef MAC
#define MCSTAS ":mcstas:lib" /* ToDo: What to put here? */
#else  /* !MAC */
#define MCSTAS "/usr/local/lib/mcstas"
#endif /* !MAC */
#endif /* !WIN32 */
#endif /* MCSTAS */

#ifdef WIN32
#define PACKAGE_BUGREPORT "www.mcstas.org"
#ifndef HAVE_STRCASECMP
#define HAVE_STRCASECMP
#endif
#endif
#ifndef HAVE_STRCASECMP
int strcasecmp(char *, char *);
#endif

#ifndef HAVE_FDOPEN
#include <stdio.h>
FILE *fdopen(int descr, const char *mode);
#endif /* HAVE_FDOPEN */
