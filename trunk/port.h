/*******************************************************************************
* Header file for portability related stuff.
*
*	Project: Monte Carlo Simulation of Triple Axis Spectrometers
*	File name: port.h
*
*	Author: K.N.			Nov 11, 1998
*
* Copyright (C) Risoe National Laboratory, 1998, All rights reserved
*******************************************************************************/

/* File system details. */
#ifdef WIN32
#define PATHSEP_S "\\"
#define PATHSEP_C '\\'
#define CURRENT_DIR_S "."
#else
#define PATHSEP_S "/"
#define PATHSEP_C '/'
#define CURRENT_DIR_S "."
#endif

#ifndef MC_SYS_DIR
#ifdef WIN32
#define MC_SYS_DIR "C:\\mcstas\\lib"
#else
#define MC_SYS_DIR "/usr/local/lib/mcstas"
#endif
#endif

#ifndef HAVE_STRCASECMP
int strcasecmp(char *, char *);
#endif

#ifndef HAVE_FDOPEN
#include <stdio.h>
FILE *fdopen(int descr, const char *mode);
#endif /* HAVE_FDOPEN */
