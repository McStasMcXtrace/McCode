/* This file contains the initialized global variables for the sv driver */

/* Created: 30-Apr-1991 */
/* Sam Southard, Jr. */

char sv_dev_name[] = "SUN" ;
int sv_COLOR = 1 ;			/* current pen color */
int sv_APPEND = 0 ;			/* to erase or not */
#define NCOLOR	32

/*  The pre-defined dark grey color is too dark */

unsigned char sv_R_table[NCOLOR] =
	{  1,255,255,  0,  0,  0,255,255, 255,127,  0,  0,127,255, 85,170,
	   0,  0,  0,  0,  0,  0,  0,  0,   0,  0,  0,  0,  0,  0,  0,255 } ;
unsigned char sv_G_table[NCOLOR] =
	{  1,255,  0,255,  0,255,  0,255, 127,255,255,127,  0,  0, 85,170,
	   0,  0,  0,  0,  0,  0,  0,  0,   0,  0,  0,  0,  0,  0,  0,255 } ;
unsigned char sv_B_table[NCOLOR] =
	{  1,255,  0,  0,255,255,255,  0,   0,  0,127,255,255,127, 85,170,
	   0,  0,  0,  0,  0,  0,  0,  0,   0,  0,  0,  0,  0,  0,  0,255 } ;

int sv_NPOLY = 0, sv_POLY = 0 ;
