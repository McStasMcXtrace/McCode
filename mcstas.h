/*******************************************************************************
* Main header file containing declarations of external functions and
* variables. This file is included by all modules.
*
* 	Project: Monte Carlo Simulation of Tripple Axis Spectrometers
* 	File name: mcstas.h
*
* 	Author: K.N.			Jul  1, 1997
*
* 	$Id: mcstas.h,v 1.1 1997-07-01 08:17:52 kn Exp $
*
* 	$Log: not supported by cvs2svn $
*
* Copyright (C) Risoe National Laboratory, 1991-1997, All rights reserved
*******************************************************************************/


#include <stdlib.h>


/* Functions defined in memory.c */

void *mem(size_t);		/* Allocate memory. */
void memfree(void *);		/* Free memory. */


/* Functions and variables defined in debug.c */

#ifdef DEBUG

void debug_printf(char *, ...);
void debugn_printf(int, char *, ...);

/*******************************************************************************
* Debugging information. When the preprosessor flag DEBUG is defined,
* debugging messages are printed to stderr. This uses the 'debug' macro. A
* statement of the form debug((format, ...)) (note the double parenthesis)
* does nothing when debugging is disabled, and outputs debugging information
* printf-style when debigging is enabled. The macro 'debugn' takes an
* additional argument LEVEL; a compile-time option can be used to select
* output only up to a certain level.
*******************************************************************************/

#define debug(msg) debug_printf msg
#define debugn(msg) debugn_printf msg

/* 'Standard' debugging levels. */
#define DEBUG_ALWAYS  0		/* Always shown (if debugging enabled). */
#define DEBUG_HIGH   10
#define DEBUG_MEDIUM 20
#define DEBUG_LOW    30		/* Only shown at high debugging level. */

/*******************************************************************************
* Macro used to change the current debugging level. Useful to enable
* high-volume debugging output in a specific part of the program.
*******************************************************************************/
extern int debug_current_level;
#define debug_level(n) (debug_current_level = n)

#else  /* !defined(DEBUG) */

#define debug(msg)
#define debugn(msg)
#define DEBUG_ALWAYS
#define DEBUG_HIGH
#define DEBUG_MEDIUM
#define DEBUG_LOW
#define debug_level(n)

#endif /* !defined(DEBUG) */

