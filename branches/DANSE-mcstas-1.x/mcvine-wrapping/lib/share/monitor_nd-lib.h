/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright (C) 1997-2010, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/monitor_nd-lib.h
*
* %Identification
* Written by: EF
* Date: Aug 28, 2002
* Origin: ILL
* Release: McStas 1.12b
* Version: $Revision: 1.17 $
*
* This file is to be imported by the monitor_nd related components
* It handles some shared functions.
*
* Usage: within SHARE
* %include "monitor_nd-lib"
*
* $Id$
*
* $Log: monitor_nd-lib.h,v $
* Revision 1.17  2008-04-01 09:15:04  farhi
* Monitor_nD now accepts up to 3 user variables, e.g. for coordinates
* to be stored into "list".
*
* Revision 1.16  2006/07/21 14:09:07  farhi
* Fix wrong structure member
*
* Revision 1.15  2006/07/21 09:03:23  farhi
* Added in options'per steradian' flux estimate, and possibility to glue the
* monitor to the shape of the 'previous' component (unactivate propagation), so
* that we can mnonitor what's going on at the output surface of the previous comp.
*
* Revision 1.14  2005/08/24 13:14:40  lieutenant
* new option 'exclusive'
*
* Revision 1.13  2005/07/25 14:55:08  farhi
* DOC update:
* checked all parameter [unit] + text to be OK
* set all versions to CVS Revision
*
* Revision 1.12  2005/07/04 08:20:16  farhi
* added support for radial distributions vxy kxy and xy=radius
*
* Revision 1.11  2005/02/22 16:11:03  farhi
* Now saving absolute position of monitors as "position" field in header
* Useful for plotting e.g. flux vs distance
*
* Revision 1.10  2005/01/18 10:35:56  farhi
* Intall new MACROs for easy User Variable usage in Monitor_nD
* MONND_DECLARE(comp)
* MONND_USER_TITLE(comp, num, title)
* MONND_USER_VALUE(comp, num, value)
* comp is the name of a Monitor_nD component; num is 1 or 2 for UserVariable
*
* Revision 1.9  2004/11/30 16:11:37  farhi
* defined some macros for an easier User variable handling. Should be updated in the header and Comp doc
*
* Revision 1.8  2003/02/11 12:28:46  farhi
* Variouxs bug fixes after tests in the lib directory
* mcstas_r  : disable output with --no-out.. flag. Fix 1D McStas output
* read_table:corrected MC_SYS_DIR -> MCSTAS define
* monitor_nd-lib: fix Log(signal) log(coord)
* HOPG.trm: reduce 4000 points -> 400 which is enough and faster to resample
* Progress_bar: precent -> percent parameter
* CS: ----------------------------------------------------------------------
*
* Revision 1.1 2002/08/28 11:39:00 ef
* Initial revision extracted from lib/monitors/Monitor_nD.comp
*******************************************************************************/

#ifndef MONITOR_ND_LIB_H

#define MONITOR_ND_LIB_H "1.1.1"
#define MONnD_COORD_NMAX  30  /* max number of variables to record */


#include "mcstas2/geometry.h"

  typedef struct MonitornD_Defines
  {
    char COORD_NONE  ;
    char COORD_X     ;
    char COORD_Y     ;
    char COORD_Z     ;
    char COORD_VX    ;
    char COORD_VY    ;
    char COORD_VZ    ;
    char COORD_T     ;
    char COORD_P     ;
    char COORD_SX    ;
    char COORD_SY    ;
    char COORD_SZ    ;
    char COORD_KX    ;
    char COORD_KY    ;
    char COORD_KZ    ;
    char COORD_K     ;
    char COORD_V     ;
    char COORD_ENERGY;
    char COORD_LAMBDA;
    char COORD_RADIUS;
    char COORD_KXY   ;
    char COORD_VXY   ;
    char COORD_HDIV  ;
    char COORD_VDIV  ;
    char COORD_ANGLE ;
    char COORD_NCOUNT;
    char COORD_THETA ;
    char COORD_PHI   ;
    char COORD_USER1 ;
    char COORD_USER2 ;
    char COORD_USER3 ;

    /* token modifiers */
    char COORD_VAR   ; /* next token should be a variable or normal option */
    char COORD_MIN   ; /* next token is a min value */
    char COORD_MAX   ; /* next token is a max value */
    char COORD_DIM   ; /* next token is a bin value */
    char COORD_FIL   ; /* next token is a filename */
    char COORD_EVNT  ; /* next token is a buffer size value */
    char COORD_3HE   ; /* next token is a 3He pressure value */
    char COORD_INTERM; /* next token is an intermediate save value (percent) */
    char COORD_LOG   ; /* next variable will be in log scale */
    char COORD_ABS   ; /* next variable will be in abs scale */
    char COORD_SIGNAL; /* next variable will be the signal var */
    int  COORD_AUTO  ; /* set auto limits */

    char TOKEN_DEL[32]; /* token separators */

    char SHAPE_SQUARE; /* shape of the monitor */
    char SHAPE_DISK  ;
    char SHAPE_SPHERE;
    char SHAPE_CYLIND;
    char SHAPE_BANANA; /* cylinder without top/bottom, on restricted angular area */
    char SHAPE_BOX   ;
    char SHAPE_PREVIOUS;

  } MonitornD_Defines_type;

  typedef struct MonitornD_Variables
  {
    double area, steradian;
    double Sphere_Radius     ;
    double Cylinder_Height   ;
    char   Flag_With_Borders ;   /* 2 means xy borders too */
    char   Flag_List         ;   /* 1 store 1 buffer, 2 is list all, 3 list all+append */
    char   Flag_Multiple     ;   /* 1 when n1D, 0 for 2D */
    char   Flag_Verbose      ;
    int    Flag_Shape        ;
    char   Flag_Auto_Limits  ;   /* get limits from first Buffer */
    char   Flag_Absorb       ;   /* monitor is also a slit */
    char   Flag_Exclusive    ;   /* absorb neutrons out of monitor limits */
    char   Flag_per_cm2      ;   /* flux is per cm2 */
    char   Flag_per_st       ;   /* flux is per steradian */
    char   Flag_log          ;   /* log10 of the flux */
    char   Flag_parallel     ;   /* set neutron state back after detection (parallel components) */
    char   Flag_Binary_List  ;
    char   Flag_capture      ;   /* lambda monitor with lambda/lambda(2200m/s = 1.7985 Angs) weightening */
    int    Flag_signal       ;   /* 0:monitor p, else monitor a mean value */

    long   Coord_Number      ;   /* total number of variables to monitor, plus intensity (0) */
    long   Buffer_Block      ;   /* Buffer size for list or auto limits */
    long   Neutron_Counter   ;   /* event counter, simulation total counts is mcget_ncount() */
    long   Buffer_Counter    ;   /* index in Buffer size (for realloc) */
    long   Buffer_Size       ;
    int    Coord_Type[MONnD_COORD_NMAX];    /* type of variable */
    char   Coord_Label[MONnD_COORD_NMAX][30];       /* label of variable */
    char   Coord_Var[MONnD_COORD_NMAX][30]; /* short id of variable */
    long   Coord_Bin[MONnD_COORD_NMAX];             /* bins of variable array */
    double Coord_Min[MONnD_COORD_NMAX];
    double Coord_Max[MONnD_COORD_NMAX];
    char   Monitor_Label[MONnD_COORD_NMAX*30];      /* Label for monitor */
    char   Mon_File[128];    /* output file name */

    double cx,cy,cz;
    double cvx, cvy, cvz;
    double csx, csy, csz;
    double cs1, cs2, ct, cp;
    double He3_pressure;
    char   Flag_UsePreMonitor    ;   /* use a previously stored neutron parameter set */
    char   UserName1[128];
    char   UserName2[128];
    char   UserName3[128];
    double UserVariable1;
    double UserVariable2;
    double UserVariable3;
    double Intermediate;
    double IntermediateCnts;
    char   option[1024];

    double Nsum;
    double psum, p2sum;
    double **Mon2D_N;
    double **Mon2D_p;
    double **Mon2D_p2;
    double *Mon2D_Buffer;

    double mxmin,mxmax,mymin,mymax,mzmin,mzmax;
    double mean_dx, mean_dy, min_x, min_y, max_x, max_y, mean_p;

    char   compcurname[128];
    Coords compcurpos;

  } MonitornD_Variables_type;

/* monitor_nd-lib function prototypes */
/* ========================================================================= */

void Monitor_nD_Init(MonitornD_Defines_type *, MonitornD_Variables_type *, MCNUM, MCNUM, MCNUM, MCNUM, MCNUM, MCNUM, MCNUM, MCNUM, MCNUM);
double Monitor_nD_Trace(MonitornD_Defines_type *, MonitornD_Variables_type *);
void Monitor_nD_Save(MonitornD_Defines_type *, MonitornD_Variables_type *);
void Monitor_nD_Finally(MonitornD_Defines_type *, MonitornD_Variables_type *);
void Monitor_nD_McDisplay(MonitornD_Defines_type *,
 MonitornD_Variables_type *);

#define MONND_DECLARE(monname) \
  struct MonitornD_Variables *mcmonnd ## monname;
#define MONND_USER_TITLE(monname, num, title) \
  { mcmonnd ## monname = &(MC_GETPAR(monname, Vars)); \
    strcpy(mcmonnd ## monname->UserName ## num, title); }
#define MONND_USER_VALUE(monname, num, value) \
  { mcmonnd ## monname = &(MC_GETPAR(monname, Vars)); \
    mcmonnd ## monname->UserVariable ## num = (value); }

#endif

/* end of monitor_nd-lib.h */
