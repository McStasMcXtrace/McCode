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
* Version: 1.1
*
* Header file for portability related stuff.
*
* $Id: port.h,v 1.9 2003-01-21 08:51:11 pkwi Exp $
*
*******************************************************************************/

/* Machintosh specific compiler defines. */
#ifdef __dest_os
#if (__dest_os == __mac_os)
#define MAC
#endif
#endif

/* File system details. */
#ifdef WIN32
#define PATHSEP_S "\\"
#define PATHSEP_C '\\'
#define CURRENT_DIR_S "."
#else  /* !WIN32 */
#ifdef MAC
#define PATHSEP_S ":"
#define PATHSEP_C ':'
#define CURRENT_DIR_S ""	/* Apparently no Mac equivalent for this. */
#else  /* !WIN32 && !MAC */
#define PATHSEP_S "/"
#define PATHSEP_C '/'
#define CURRENT_DIR_S "."
#endif /* !MAC */
#endif /* !WIN32 */

#ifndef MC_SYS_DIR
#ifdef WIN32
#define MC_SYS_DIR "C:\\mcstas\\lib"
#else  /* !WIN32 */
#ifdef MAC
#define MC_SYS_DIR ":mcstas:lib" /* ToDo: What to put here? */
#else  /* !MAC */
#define MC_SYS_DIR "/usr/local/lib/mcstas"
#endif /* !MAC */
#endif /* !WIN32 */
#endif /* MC_SYS_DIR */

#ifndef HAVE_STRCASECMP
int strcasecmp(char *, char *);
#endif

#ifndef HAVE_FDOPEN
#include <stdio.h>
FILE *fdopen(int descr, const char *mode);
#endif /* HAVE_FDOPEN */
