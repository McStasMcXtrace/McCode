/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/monitor_nd-lib.h
*
* %Identification
* Written by: EF
* Date: Aug 28, 2002
* Origin: ILL
* Release: McStas 1.6
* Version: 1.1
*
* This file is to be imported by the monitor_nd related components
* It handles some shared functions.
*
* Usage: within SHARE
* %include "monitor_nd-lib"
*
* $Id: monitor_nd-lib.h,v 1.6 2003-01-21 08:51:12 pkwi Exp $
*
*	$Log: not supported by cvs2svn $
* Revision 1.1 2002/08/28 11:39:00 ef
*	Initial revision extracted from lib/monitors/Monitor_nD.comp
*******************************************************************************/

#ifndef MONITOR_ND_LIB_H

#define MONITOR_ND_LIB_H "1.1.0"
#define MONnD_COORD_NMAX  30  /* max number of variables to record */

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
    char COORD_HDIV  ; 
    char COORD_VDIV  ; 
    char COORD_ANGLE ; 
    char COORD_NCOUNT; 
    char COORD_THETA ; 
    char COORD_PHI   ; 
    char COORD_USER1 ; 
    char COORD_USER2 ;

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

    char TOKEN_DEL[32]; /* token separators */

    char SHAPE_SQUARE; /* shape of the monitor */
    char SHAPE_DISK  ; 
    char SHAPE_SPHERE; 
    char SHAPE_CYLIND; 
    char SHAPE_BOX   ; 
    
  } MonitornD_Defines_type;
  
  typedef struct MonitornD_Variables
  {
    double area;
    double Sphere_Radius     ;
    double Cylinder_Height   ;
    char   Flag_With_Borders ;   /* 2 means xy borders too */
    char   Flag_List         ;   /* 1 store 1 buffer, 2 is list all */
    char   Flag_Multiple     ;   /* 1 when n1D, 0 for 2D */
    char   Flag_Verbose      ;
    int    Flag_Shape        ;
    char   Flag_Auto_Limits  ;   /* get limits from first Buffer */
    char   Flag_Absorb       ;   /* monitor is also a slit */
    char   Flag_per_cm2      ;   /* flux is per cm2 */
    char   Flag_log          ;   /* log10 of the flux */
    char   Flag_parallel     ;   /* set neutron state back after detection (parallel components) */
    char   Flag_Binary_List  ;
    char   Flag_capture      ;   /* lambda monitor with lambda/lambda(2200m/s = 1.7985 Angs) weightening */
    char   Flag_signal       ;   /* 0:monitor p, else monitor a mean value */
    
    long   Coord_Number      ;   /* total number of variables to monitor, plus intensity (0) */
    long   Buffer_Block      ;   /* Buffer size for list or auto limits */
    long   Neutron_Counter   ;   /* event counter, simulation total counts is mcget_ncount() */
    long   Buffer_Counter    ;   /* index in Buffer size (for realloc) */
    long   Buffer_Size       ;
    char   Coord_Type[MONnD_COORD_NMAX];    /* type of variable */
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
    double UserVariable1;
    double UserVariable2;
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
    
    char   compcurname[128];

  } MonitornD_Variables_type;
  
/* monitor_nd-lib function prototypes */
/* ========================================================================= */
  
void Monitor_nD_Init(MonitornD_Defines_type *, MonitornD_Variables_type *, MCNUM, MCNUM, MCNUM, MCNUM, MCNUM, MCNUM, MCNUM, MCNUM, MCNUM);
double Monitor_nD_Trace(MonitornD_Defines_type *, MonitornD_Variables_type *);
void Monitor_nD_Save(MonitornD_Defines_type *, MonitornD_Variables_type *);
void Monitor_nD_Finally(MonitornD_Defines_type *, MonitornD_Variables_type *);
void Monitor_nD_McDisplay(MonitornD_Defines_type *,
 MonitornD_Variables_type *);

#endif

/* end of monitor_nd-lib.h */
