/* These are the function prototypes for arrays.c */


/* Functions defined in this module, see header comments on each one
   for more details:                                                  */

#ifndef _INC_ARRAYS_
 
void* pack1D(SV* arg, char packtype);         /* Pack perl 1D array        */
void* pack2D(SV* arg, char packtype);         /* Pack perl 1-2D array      */
void* packND(SV* arg, char packtype);         /* Pack perl array N-D array */
void  unpack1D(SV* arg, void * var,           /* Unpack 1D array           */
               char packtype, int n);
 
AV*   coerce1D ( SV* arg, int n );     /* Coerce/create array to specified size */
 
void* get_mortalspace( int n, char packtype ); /* Utility to just get workspace */


/* Sort out macro name changes in 5.004_53 (PATCHLEVEL < 5)
   Note that recent Perl versions don't define PATCHLEVEL by default */

#ifndef PATCHLEVEL
#include <patchlevel.h>
#endif

#if defined(PATCHLEVEL) && (PATCHLEVEL < 5)
#define PL_na     na
#endif
/* Prevent the prototypes being defined twice */

#define _INC_ARRAYS_

#endif
