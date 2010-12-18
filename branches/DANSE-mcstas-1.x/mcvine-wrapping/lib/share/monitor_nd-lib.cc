/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright (C) 1997-2010, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/monitor_nd-lib.c
*
* %Identification
* Written by: EF
* Date: Aug 28, 2002
* Origin: ILL
* Release: McStas 1.12b
* Version: $Revision: 1.41 $
*
* This file is to be imported by the monitor_nd related components
* It handles some shared functions. Embedded within instrument in runtime mode.
* Variable names have prefix 'mc_mn_' for 'McStas Monitor' to avoid conflicts
*
* Usage: within SHARE
* %include "monitor_nd-lib"
*
* $Id$
*
* $Log: monitor_nd-lib.c,v $
* Revision 1.41  2008-07-01 20:28:13  farhi
* Fixed zthick -> zdepth
*
* Revision 1.40  2008/07/01 19:50:23  farhi
* Common naming convention for components:
*   xwidth, yheight, zdepth, radius, thickness
*   sigma_abs, sigma_inc
*
* Revision 1.39  2008/04/06 14:02:41  pkwi
* Intel C compiler version 10.1 series fail with Monitor_nD (All events absorbed)....
* The inserted empty printf seems to solve this?!
*
* I will try to file a support request at Intel on this problem...
*
* Revision 1.38  2008/04/01 09:15:04  farhi
* Monitor_nD now accepts up to 3 user variables, e.g. for coordinates
* to be stored into "list".
*
* Revision 1.37  2007/04/02 12:13:13  farhi
* Fixed length of username1|2 to avoid buffer overflow.
*
* Revision 1.36  2006/11/16 10:14:21  farhi
* Correct mcdisplay section with restricted banana/sphere (skipped last monitored variable)
*
* Revision 1.35  2006/07/21 09:03:23  farhi
* Added in options'per steradian' flux estimate, and possibility to glue the
* monitor to the shape of the 'previous' component (unactivate propagation), so
* that we can mnonitor what's going on at the output surface of the previous comp.
*
* Revision 1.34  2006/03/15 16:01:43  farhi
* 'keyword ignored' warning only in verbose mode
*
* Revision 1.33  2005/12/12 13:42:11  farhi
* Corrected bug on multiple limits specifications (K. Lieutenant)
*
* Revision 1.32  2005/09/19 15:13:53  farhi
* using 'y' variable also sets limits to detector dimensions, to enable 'banana' view without troubles.
*
* Revision 1.31  2005/09/16 08:43:19  farhi
* Removed floor+0.5 in Monitor_nD
* Take care of ploting with bin centers in mcplot stuff (inline+matlab+scilab+octave...)
*
* Revision 1.30  2005/08/24 13:14:00  lieutenant
* new option 'exclusive'
*
* Revision 1.29  2005/07/25 14:55:08  farhi
* DOC update:
* checked all parameter [unit] + text to be OK
* set all versions to CVS Revision
*
* Revision 1.28  2005/07/18 14:38:00  farhi
* Added 0.5 top all floor's so that bin are centered (at last)
*
* Revision 1.27  2005/07/06 08:16:28  farhi
* Misprint for Kxy/Vxy. Better Variable name as well.
*
* Revision 1.26  2005/07/04 08:19:50  farhi
* added support for kxy and vxy radial distributions
*
* Revision 1.25  2005/04/11 11:40:44  farhi
* Added missing n-dims argument to printf for capture flux warning
*
* Revision 1.24  2005/03/14 10:48:54  farhi
* Added warning when setting capture flux (meaningful with integral flux) for
* more than 1 bin.
*
* Revision 1.23  2005/02/25 15:26:02  farhi
* Removed usage of round function
* made Guide_honeycomb work with gravitation
*
* Revision 1.22  2005/02/22 16:11:03  farhi
* Now saving absolute position of monitors as "position" field in header
* Useful for plotting e.g. flux vs distance
*
* Revision 1.21  2005/02/21 16:05:13  farhi
* Misprint correction
*
* Revision 1.20  2005/02/21 12:38:03  farhi
* Removed warning in Monitor_nD for global scope keywords in options
*
* Revision 1.19  2005/02/17 16:06:32  farhi
* Added 'per bin' to labels if more than 1 bin, and a message for unknow keywords found in options parameter. Requested by R. Cubitt.
*
* Revision 1.18  2004/11/16 13:36:35  farhi
* Paging update
*
* Revision 1.17  2004/09/01 13:54:18  farhi
* Corrected bug when using list=EVNTS large values written as float, read as int.
* E.g. 1e6 gave 1 as number of events to save !
*
* Revision 1.16  2004/06/30 12:13:47  farhi
* For lists (and Res_monitor), uses 'list' 'no header' and 'no footer' options
* in mcformat.Name so that catenated file does contain only one instance of
* footer and header.
*
* Revision 1.15  2004/02/26 12:55:41  farhi
* Handles 0d monitor outputs for bins=0, and limits are restrictive (i.e. neutron must be within all limits to be stored in monitor)
*
* Revision 1.14  2004/02/04 18:01:12  farhi
* Use hdiv=theta and vdiv=phi for banana.
*
* Revision 1.13  2003/08/26 12:33:27  farhi
* Corrected computation of angle PHI (was projected on vertical plane)
*
* Revision 1.12  2003/04/15 16:01:28  farhi
* incoming/outgoing syntax mismatch correction
*
* Revision 1.11  2003/04/15 15:45:56  farhi
* outgoing time is default (vs. incoming)
*
* Revision 1.10  2003/04/09 15:49:25  farhi
* corrected bug when no signal and auto limits requested
*
* Revision 1.9  2003/02/18 09:11:36  farhi
* Corrected binary format for lists
*
* Revision 1.1 2002/08/28 11:39:00 ef
* Initial revision extracted from lib/monitors/Monitor_nD.comp
*******************************************************************************/

#ifndef MONITOR_ND_LIB_H
#include "monitor_nd-lib.h"
#endif

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <math.h>
#include <stdarg.h>

#include "mcstas2/phys_constants.h"
#include "mcstas2/misc_macros.h"
#include "mcstas2/detector_outputs.h"
#include "mcstas2/display.h"

/* ========================================================================= */
/* ADD: E.Farhi, Aug 6th, 2001: Monitor_nD section */
/* this routine is used to parse options */
/* ========================================================================= */

void Monitor_nD_Init(MonitornD_Defines_type *mc_mn_DEFS,
  MonitornD_Variables_type *mc_mn_Vars,
  MCNUM mc_mn_xwidth,
  MCNUM mc_mn_yheight,
  MCNUM mc_mn_zdepth,
  MCNUM mc_mn_xmin,
  MCNUM mc_mn_xmax,
  MCNUM mc_mn_ymin,
  MCNUM mc_mn_ymax,
  MCNUM mc_mn_zmin,
  MCNUM mc_mn_zmax)
  {
    long mc_mn_carg = 1;
    char *mc_mn_option_copy, *mc_mn_token;
    char mc_mn_Flag_New_token = 1;
    char mc_mn_Flag_End       = 1;
    char mc_mn_Flag_All       = 0;
    char mc_mn_Flag_No        = 0;
    char mc_mn_Flag_abs       = 0;
    int  mc_mn_Flag_auto      = 0;  /* -1: all, 1: the current variable */
    int  mc_mn_Set_Vars_Coord_Type;
    char mc_mn_Set_Vars_Coord_Label[64];
    char mc_mn_Set_Vars_Coord_Var[64];
    char mc_mn_Short_Label[MONnD_COORD_NMAX][64];
    int  mc_mn_Set_Coord_Mode;
    long mc_mn_i=0, mc_mn_j=0;
    double mc_mn_lmin, mc_mn_lmax, mc_mn_XY;
    long mc_mn_t;


    mc_mn_t = (long)time(0);

    mc_mn_DEFS->COORD_NONE   =0;
    mc_mn_DEFS->COORD_X      =1;
    mc_mn_DEFS->COORD_Y      =2;
    mc_mn_DEFS->COORD_Z      =3;
    mc_mn_DEFS->COORD_VX     =4;
    mc_mn_DEFS->COORD_VY     =5;
    mc_mn_DEFS->COORD_VZ     =6;
    mc_mn_DEFS->COORD_T      =7;
    mc_mn_DEFS->COORD_P      =8;
    mc_mn_DEFS->COORD_SX     =9;
    mc_mn_DEFS->COORD_SY     =10;
    mc_mn_DEFS->COORD_SZ     =11;
    mc_mn_DEFS->COORD_KX     =12;
    mc_mn_DEFS->COORD_KY     =13;
    mc_mn_DEFS->COORD_KZ     =14;
    mc_mn_DEFS->COORD_K      =15;
    mc_mn_DEFS->COORD_V      =16;
    mc_mn_DEFS->COORD_ENERGY =17;
    mc_mn_DEFS->COORD_LAMBDA =18;
    mc_mn_DEFS->COORD_RADIUS =19;
    mc_mn_DEFS->COORD_HDIV   =20;
    mc_mn_DEFS->COORD_VDIV   =21;
    mc_mn_DEFS->COORD_ANGLE  =22;
    mc_mn_DEFS->COORD_NCOUNT =23;
    mc_mn_DEFS->COORD_THETA  =24;
    mc_mn_DEFS->COORD_PHI    =25;
    mc_mn_DEFS->COORD_USER1  =26;
    mc_mn_DEFS->COORD_USER2  =27;
    mc_mn_DEFS->COORD_USER3  =28;
    mc_mn_DEFS->COORD_KXY    =29;
    mc_mn_DEFS->COORD_VXY    =30;

/* mc_mn_token modifiers */
    mc_mn_DEFS->COORD_VAR    =0;    /* next mc_mn_token should be a variable or normal option */
    mc_mn_DEFS->COORD_MIN    =1;    /* next mc_mn_token is a min value */
    mc_mn_DEFS->COORD_MAX    =2;    /* next mc_mn_token is a max value */
    mc_mn_DEFS->COORD_DIM    =3;    /* next mc_mn_token is a bin value */
    mc_mn_DEFS->COORD_FIL    =4;    /* next mc_mn_token is a filename */
    mc_mn_DEFS->COORD_EVNT   =5;    /* next mc_mn_token is a buffer size value */
    mc_mn_DEFS->COORD_3HE    =6;    /* next mc_mn_token is a 3He pressure value */
    mc_mn_DEFS->COORD_INTERM =7;    /* next mc_mn_token is an intermediate save value (%) */
    mc_mn_DEFS->COORD_LOG    =32;   /* next variable will be in log scale */
    mc_mn_DEFS->COORD_ABS    =64;   /* next variable will be in abs scale */
    mc_mn_DEFS->COORD_SIGNAL =128;  /* next variable will be the signal var */
    mc_mn_DEFS->COORD_AUTO   =256;  /* set auto limits */

    strcpy(mc_mn_DEFS->TOKEN_DEL, " =,;[](){}:");  /* mc_mn_token separators */

    mc_mn_DEFS->SHAPE_SQUARE =0;    /* shape of the monitor */
    mc_mn_DEFS->SHAPE_DISK   =1;
    mc_mn_DEFS->SHAPE_SPHERE =2;
    mc_mn_DEFS->SHAPE_CYLIND =3;
    mc_mn_DEFS->SHAPE_BANANA =4;
    mc_mn_DEFS->SHAPE_BOX    =5;
    mc_mn_DEFS->SHAPE_PREVIOUS=6;

    mc_mn_Vars->Sphere_Radius     = 0;
    mc_mn_Vars->Cylinder_Height   = 0;
    mc_mn_Vars->Flag_With_Borders = 0;   /* 2 means xy borders too */
    mc_mn_Vars->Flag_List         = 0;   /* 1=store 1 buffer, 2=list all, 3=re-use buffer */
    mc_mn_Vars->Flag_Multiple     = 0;   /* 1 when n1D, 0 for 2D */
    mc_mn_Vars->Flag_Verbose      = 0;
    mc_mn_Vars->Flag_Shape        = mc_mn_DEFS->SHAPE_SQUARE;
    mc_mn_Vars->Flag_Auto_Limits  = 0;   /* get limits from first Buffer */
    mc_mn_Vars->Flag_Absorb       = 0;   /* monitor is also a slit */
    mc_mn_Vars->Flag_Exclusive    = 0;   /* absorb neutrons out of monitor limits */
    mc_mn_Vars->Flag_per_cm2      = 0;   /* flux is per cm2 */
    mc_mn_Vars->Flag_per_st       = 0;   /* flux is per steradian (in Auto mode only) */
    mc_mn_Vars->Flag_log          = 0;   /* log10 of the flux */
    mc_mn_Vars->Flag_parallel     = 0;   /* set neutron state back after detection (parallel components) */
    mc_mn_Vars->Flag_Binary_List  = 0;   /* save list as a binary file (smaller) */
    mc_mn_Vars->Coord_Number      = 0;   /* total number of variables to monitor, plus intensity (0) */
    mc_mn_Vars->Buffer_Block      = 10000;     /* Buffer size for list or auto limits */
    mc_mn_Vars->Neutron_Counter   = 0;   /* event counter, simulation total counts is mcget_ncount() */
    mc_mn_Vars->Buffer_Counter    = 0;   /* mc_mn_index in Buffer size (for realloc) */
    mc_mn_Vars->Buffer_Size       = 0;
    mc_mn_Vars->UserVariable1     = 0;
    mc_mn_Vars->UserVariable2     = 0;
    mc_mn_Vars->He3_pressure      = 0;
    mc_mn_Vars->IntermediateCnts  = 0;
    mc_mn_Vars->Flag_capture      = 0;
    mc_mn_Vars->Flag_signal       = mc_mn_DEFS->COORD_P;
    mc_mn_Vars->mean_dx=mc_mn_Vars->mean_dy=0;
    mc_mn_Vars->min_x = mc_mn_Vars->max_x  =0;
    mc_mn_Vars->min_y = mc_mn_Vars->max_y  =0;
    mc_mn_Vars->steradian=0;

    mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_NONE;
    mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_VAR;

    /* handle size parameters */
    /* normal use is with xwidth, yheight, zdepth */
    /* if xmin,xmax,ymin,ymax,zmin,zmax are non 0, use them */
    if (fabs(mc_mn_xmin-mc_mn_xmax) == 0)
      { mc_mn_Vars->mxmin = -fabs(mc_mn_xwidth)/2; mc_mn_Vars->mxmax = fabs(mc_mn_xwidth)/2; }
    else
      { if (mc_mn_xmin < mc_mn_xmax) {mc_mn_Vars->mxmin = mc_mn_xmin; mc_mn_Vars->mxmax = mc_mn_xmax;}
        else {mc_mn_Vars->mxmin = mc_mn_xmax; mc_mn_Vars->mxmax = mc_mn_xmin;}
      }
    if (fabs(mc_mn_ymin-mc_mn_ymax) == 0)
      { mc_mn_Vars->mymin = -fabs(mc_mn_yheight)/2; mc_mn_Vars->mymax = fabs(mc_mn_yheight)/2; }
    else
      { if (mc_mn_ymin < mc_mn_ymax) {mc_mn_Vars->mymin = mc_mn_ymin; mc_mn_Vars->mymax = mc_mn_ymax;}
        else {mc_mn_Vars->mymin = mc_mn_ymax; mc_mn_Vars->mymax = mc_mn_ymin;}
      }
    if (fabs(mc_mn_zmin-mc_mn_zmax) == 0)
      { mc_mn_Vars->mzmin = -fabs(mc_mn_zdepth)/2; mc_mn_Vars->mzmax = fabs(mc_mn_zdepth)/2; }
    else
      { if (mc_mn_zmin < mc_mn_zmax) {mc_mn_Vars->mzmin = mc_mn_zmin; mc_mn_Vars->mzmax = mc_mn_zmax; }
        else {mc_mn_Vars->mzmin = mc_mn_zmax; mc_mn_Vars->mzmax = mc_mn_zmin; }
      }

    if (fabs(mc_mn_Vars->mzmax-mc_mn_Vars->mzmin) == 0)
      mc_mn_Vars->Flag_Shape        = mc_mn_DEFS->SHAPE_SQUARE;
    else
      mc_mn_Vars->Flag_Shape        = mc_mn_DEFS->SHAPE_BOX;

    /* parse option string */

    mc_mn_option_copy = (char*)malloc(strlen(mc_mn_Vars->option)+1);
    if (mc_mn_option_copy == NULL)
    {
      fprintf(stderr,"Monitor_nD: %s cannot allocate mc_mn_option_copy (%li). Fatal.\n", mc_mn_Vars->compcurname, strlen(mc_mn_Vars->option));
      exit(-1);
    }

    if (strlen(mc_mn_Vars->option))
    {
      mc_mn_Flag_End = 0;
      strcpy(mc_mn_option_copy, mc_mn_Vars->option);
    }

    if (strstr(mc_mn_Vars->option, "cm2") || strstr(mc_mn_Vars->option, "cm^2")) mc_mn_Vars->Flag_per_cm2 = 1;
    if (strstr(mc_mn_Vars->option, "steradian")) mc_mn_Vars->Flag_per_st = 1;

    if (strstr(mc_mn_Vars->option, "binary") || strstr(mc_mn_Vars->option, "float"))
      mc_mn_Vars->Flag_Binary_List  = 1;
    if (strstr(mc_mn_Vars->option, "double"))
      mc_mn_Vars->Flag_Binary_List  = 2;

    strcpy(mc_mn_Vars->Coord_Label[0],"Intensity");
    strncpy(mc_mn_Vars->Coord_Var[0],"p",30);
    mc_mn_Vars->Coord_Type[0] = mc_mn_DEFS->COORD_P;
    mc_mn_Vars->Coord_Bin[0] = 1;
    mc_mn_Vars->Coord_Min[0] = 0;
    mc_mn_Vars->Coord_Max[0] = FLT_MAX;

    /* default file name is comp name+dateID */
    sprintf(mc_mn_Vars->Mon_File, "%s_%li", mc_mn_Vars->compcurname, mc_mn_t);

    mc_mn_carg = 1;
    while((mc_mn_Flag_End == 0) && (mc_mn_carg < 128))
    {

      if (mc_mn_Flag_New_token) /* to get the previous mc_mn_token sometimes */
      {
        if (mc_mn_carg == 1) mc_mn_token=(char *)strtok(mc_mn_option_copy,mc_mn_DEFS->TOKEN_DEL);
        else mc_mn_token=(char *)strtok(NULL,mc_mn_DEFS->TOKEN_DEL);
        if (mc_mn_token == NULL) mc_mn_Flag_End=1;
      }
      mc_mn_Flag_New_token = 1;
      if ((mc_mn_token != NULL) && (strlen(mc_mn_token) != 0))
      {
        char mc_mn_iskeyword=0;
        int  mc_mn_old_Mode;
        /* first handle option values from preceeding keyword mc_mn_token detected */
        mc_mn_old_Mode = mc_mn_Set_Coord_Mode;
        if (mc_mn_Set_Coord_Mode == mc_mn_DEFS->COORD_MAX)
        {
          if (!mc_mn_Flag_All)
            mc_mn_Vars->Coord_Max[mc_mn_Vars->Coord_Number] = atof(mc_mn_token);
          else
            for (mc_mn_i = 0; mc_mn_i <= mc_mn_Vars->Coord_Number; mc_mn_Vars->Coord_Max[mc_mn_i++] = atof(mc_mn_token));
          mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_VAR; mc_mn_Flag_All = 0;
        }
        if (mc_mn_Set_Coord_Mode == mc_mn_DEFS->COORD_MIN)
        {
          if (!mc_mn_Flag_All)
            mc_mn_Vars->Coord_Min[mc_mn_Vars->Coord_Number] = atof(mc_mn_token);
          else
            for (mc_mn_i = 0; mc_mn_i <= mc_mn_Vars->Coord_Number; mc_mn_Vars->Coord_Min[mc_mn_i++] = atof(mc_mn_token));
          mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_MAX;
        }
        if (mc_mn_Set_Coord_Mode == mc_mn_DEFS->COORD_DIM)
        {
          if (!mc_mn_Flag_All)
            mc_mn_Vars->Coord_Bin[mc_mn_Vars->Coord_Number] = atoi(mc_mn_token);
          else
            for (mc_mn_i = 0; mc_mn_i <= mc_mn_Vars->Coord_Number; mc_mn_Vars->Coord_Bin[mc_mn_i++] = atoi(mc_mn_token));
          mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_VAR; mc_mn_Flag_All = 0;
        }
        if (mc_mn_Set_Coord_Mode == mc_mn_DEFS->COORD_FIL)
        {
          if (!mc_mn_Flag_No) strncpy(mc_mn_Vars->Mon_File,mc_mn_token,128);
          else { strcpy(mc_mn_Vars->Mon_File,""); mc_mn_Vars->Coord_Number = 0; mc_mn_Flag_End = 1;}
          mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_VAR;
        }
        if (mc_mn_Set_Coord_Mode == mc_mn_DEFS->COORD_EVNT)
        {
          if (!strcmp(mc_mn_token, "all") || mc_mn_Flag_All) mc_mn_Vars->Flag_List = 2;
          else { mc_mn_i = (long)ceil(atof(mc_mn_token)); if (mc_mn_i) mc_mn_Vars->Buffer_Block = mc_mn_i;
            mc_mn_Vars->Flag_List = 1; }
          mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_VAR; mc_mn_Flag_All = 0;
        }
        if (mc_mn_Set_Coord_Mode == mc_mn_DEFS->COORD_3HE)
        {
            mc_mn_Vars->He3_pressure = atof(mc_mn_token);
            mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_VAR; mc_mn_Flag_All = 0;
        }
        if (mc_mn_Set_Coord_Mode == mc_mn_DEFS->COORD_INTERM)
        {
            mc_mn_Vars->Intermediate = atof(mc_mn_token);
            mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_VAR; mc_mn_Flag_All = 0;
        }

        /* now look for general option keywords */
        if (!strcmp(mc_mn_token, "borders"))  {mc_mn_Vars->Flag_With_Borders = 1; mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "verbose"))  {mc_mn_Vars->Flag_Verbose      = 1; mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "log"))      {mc_mn_Vars->Flag_log          = 1; mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "abs"))      {mc_mn_Flag_abs                = 1; mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "multiple")) {mc_mn_Vars->Flag_Multiple     = 1; mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "exclusive")){mc_mn_Vars->Flag_Exclusive    = 1; mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "list")) {
          mc_mn_Vars->Flag_List = 1; mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_EVNT;  }
        if (!strcmp(mc_mn_token, "limits") || !strcmp(mc_mn_token, "min"))
          mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_MIN;
        if (!strcmp(mc_mn_token, "slit") || !strcmp(mc_mn_token, "absorb")) {
          mc_mn_Vars->Flag_Absorb = 1;  mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "max"))  mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_MAX;
        if (!strcmp(mc_mn_token, "bins")) mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_DIM;
        if (!strcmp(mc_mn_token, "file")) {
          mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_FIL;
          if (mc_mn_Flag_No) { strcpy(mc_mn_Vars->Mon_File,""); mc_mn_Vars->Coord_Number = 0; mc_mn_Flag_End = 1; }
        }
        if (!strcmp(mc_mn_token, "unactivate")) {
          mc_mn_Flag_End = 1; mc_mn_Vars->Coord_Number = 0; mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "all"))    { mc_mn_Flag_All = 1;  mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "sphere")) { mc_mn_Vars->Flag_Shape = mc_mn_DEFS->SHAPE_SPHERE; mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "cylinder")) { mc_mn_Vars->Flag_Shape = mc_mn_DEFS->SHAPE_CYLIND; mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "banana")) { mc_mn_Vars->Flag_Shape = mc_mn_DEFS->SHAPE_BANANA; mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "square")) { mc_mn_Vars->Flag_Shape = mc_mn_DEFS->SHAPE_SQUARE; mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "disk"))   { mc_mn_Vars->Flag_Shape = mc_mn_DEFS->SHAPE_DISK; mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "box"))     { mc_mn_Vars->Flag_Shape = mc_mn_DEFS->SHAPE_BOX; mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "previous")) { mc_mn_Vars->Flag_Shape = mc_mn_DEFS->SHAPE_PREVIOUS; mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "parallel")){ mc_mn_Vars->Flag_parallel = 1; mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "capture")) { mc_mn_Vars->Flag_capture = 1; mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "auto") && (mc_mn_Flag_auto != -1)) {
          mc_mn_Vars->Flag_Auto_Limits = 1;
          if (mc_mn_Flag_All) mc_mn_Flag_auto = -1;
          else mc_mn_Flag_auto = 1;
          mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "premonitor")) {
          mc_mn_Vars->Flag_UsePreMonitor = 1; mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "3He_pressure")) {
          mc_mn_Vars->He3_pressure = 3; mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "intermediate")) {
          mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_INTERM;
          mc_mn_Vars->Intermediate = 5; mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "no") || !strcmp(mc_mn_token, "not")) { mc_mn_Flag_No = 1;  mc_mn_iskeyword=1; }
        if (!strcmp(mc_mn_token, "signal")) mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_SIGNAL;

        /* Mode has changed: this was a keyword or value */
        if (mc_mn_Set_Coord_Mode != mc_mn_old_Mode) mc_mn_iskeyword=1;

        /* now look for variable names to monitor */
        mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_NONE; mc_mn_lmin = 0; mc_mn_lmax = 0;

        if (!strcmp(mc_mn_token, "x"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_X; strcpy(mc_mn_Set_Vars_Coord_Label,"x [m]"); strcpy(mc_mn_Set_Vars_Coord_Var,"x");
          mc_mn_lmin = mc_mn_Vars->mxmin; mc_mn_lmax = mc_mn_Vars->mxmax;
          mc_mn_Vars->Coord_Min[mc_mn_Vars->Coord_Number+1] = mc_mn_Vars->mxmin;
          mc_mn_Vars->Coord_Max[mc_mn_Vars->Coord_Number+1] = mc_mn_Vars->mxmax;}
        if (!strcmp(mc_mn_token, "y"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_Y; strcpy(mc_mn_Set_Vars_Coord_Label,"y [m]"); strcpy(mc_mn_Set_Vars_Coord_Var,"y");
          mc_mn_lmin = mc_mn_Vars->mymin; mc_mn_lmax = mc_mn_Vars->mymax;
          mc_mn_Vars->Coord_Min[mc_mn_Vars->Coord_Number+1] = mc_mn_Vars->mymin;
          mc_mn_Vars->Coord_Max[mc_mn_Vars->Coord_Number+1] = mc_mn_Vars->mymax;}
        if (!strcmp(mc_mn_token, "z"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_Z; strcpy(mc_mn_Set_Vars_Coord_Label,"z [m]"); strcpy(mc_mn_Set_Vars_Coord_Var,"z"); mc_mn_lmin = mc_mn_Vars->mzmin; mc_mn_lmax = mc_mn_Vars->mzmax; }
        if (!strcmp(mc_mn_token, "k") || !strcmp(mc_mn_token, "wavevector"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_K; strcpy(mc_mn_Set_Vars_Coord_Label,"|k| [Angs-1]"); strcpy(mc_mn_Set_Vars_Coord_Var,"k"); mc_mn_lmin = 0; mc_mn_lmax = 10; }
        if (!strcmp(mc_mn_token, "v"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_V; strcpy(mc_mn_Set_Vars_Coord_Label,"Velocity [m/s]"); strcpy(mc_mn_Set_Vars_Coord_Var,"v"); mc_mn_lmin = 0; mc_mn_lmax = 10000; }
        if (!strcmp(mc_mn_token, "t") || !strcmp(mc_mn_token, "time"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_T; strcpy(mc_mn_Set_Vars_Coord_Label,"TOF [s]"); strcpy(mc_mn_Set_Vars_Coord_Var,"t"); mc_mn_lmin = 0; mc_mn_lmax = .1; }
        if ((!strcmp(mc_mn_token, "p") || !strcmp(mc_mn_token, "intensity") || !strcmp(mc_mn_token, "flux")))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_P;
            strcpy(mc_mn_Set_Vars_Coord_Label,"Intensity");
            strncat(mc_mn_Set_Vars_Coord_Label, " [n/s", 30);
            if (mc_mn_Vars->Flag_per_cm2) strncat(mc_mn_Set_Vars_Coord_Label, "/cm2", 30);
            if (mc_mn_Vars->Flag_per_st) {
              if (mc_mn_Vars->Flag_Auto_Limits)
                strncat(mc_mn_Set_Vars_Coord_Label, "/st", 30);
            }
            if (mc_mn_XY > 1 && mc_mn_Vars->Coord_Number)
              strncat(mc_mn_Set_Vars_Coord_Label, "/bin", 30);
            strncat(mc_mn_Set_Vars_Coord_Label, "]", 30);
            strcpy(mc_mn_Set_Vars_Coord_Var,"I");
            mc_mn_lmin = 0; mc_mn_lmax = FLT_MAX;
          }

        if (!strcmp(mc_mn_token, "vx"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_VX; strcpy(mc_mn_Set_Vars_Coord_Label,"vx [m/s]"); strcpy(mc_mn_Set_Vars_Coord_Var,"vx"); mc_mn_lmin = -1000; mc_mn_lmax = 1000; }
        if (!strcmp(mc_mn_token, "vy"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_VY; strcpy(mc_mn_Set_Vars_Coord_Label,"vy [m/s]"); strcpy(mc_mn_Set_Vars_Coord_Var,"vy"); mc_mn_lmin = -1000; mc_mn_lmax = 1000; }
        if (!strcmp(mc_mn_token, "vz"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_VZ; strcpy(mc_mn_Set_Vars_Coord_Label,"vz [m/s]"); strcpy(mc_mn_Set_Vars_Coord_Var,"vz"); mc_mn_lmin = -10000; mc_mn_lmax = 10000; }
        if (!strcmp(mc_mn_token, "kx"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_KX; strcpy(mc_mn_Set_Vars_Coord_Label,"kx [Angs-1]"); strcpy(mc_mn_Set_Vars_Coord_Var,"kx"); mc_mn_lmin = -1; mc_mn_lmax = 1; }
        if (!strcmp(mc_mn_token, "ky"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_KY; strcpy(mc_mn_Set_Vars_Coord_Label,"ky [Angs-1]"); strcpy(mc_mn_Set_Vars_Coord_Var,"ky"); mc_mn_lmin = -1; mc_mn_lmax = 1; }
        if (!strcmp(mc_mn_token, "kz"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_KZ; strcpy(mc_mn_Set_Vars_Coord_Label,"kz [Angs-1]"); strcpy(mc_mn_Set_Vars_Coord_Var,"kz"); mc_mn_lmin = -10; mc_mn_lmax = 10; }
        if (!strcmp(mc_mn_token, "sx"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_SX; strcpy(mc_mn_Set_Vars_Coord_Label,"sx [1]"); strcpy(mc_mn_Set_Vars_Coord_Var,"sx"); mc_mn_lmin = -1; mc_mn_lmax = 1; }
        if (!strcmp(mc_mn_token, "sy"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_SY; strcpy(mc_mn_Set_Vars_Coord_Label,"sy [1]"); strcpy(mc_mn_Set_Vars_Coord_Var,"sy"); mc_mn_lmin = -1; mc_mn_lmax = 1; }
        if (!strcmp(mc_mn_token, "sz"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_SZ; strcpy(mc_mn_Set_Vars_Coord_Label,"sz [1]"); strcpy(mc_mn_Set_Vars_Coord_Var,"sz"); mc_mn_lmin = -1; mc_mn_lmax = 1; }

        if (!strcmp(mc_mn_token, "energy") || !strcmp(mc_mn_token, "omega"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_ENERGY; strcpy(mc_mn_Set_Vars_Coord_Label,"Energy [meV]"); strcpy(mc_mn_Set_Vars_Coord_Var,"E"); mc_mn_lmin = 0; mc_mn_lmax = 100; }
        if (!strcmp(mc_mn_token, "lambda") || !strcmp(mc_mn_token, "wavelength"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_LAMBDA; strcpy(mc_mn_Set_Vars_Coord_Label,"Wavelength [Angs]"); strcpy(mc_mn_Set_Vars_Coord_Var,"L"); mc_mn_lmin = 0; mc_mn_lmax = 100; }
        if (!strcmp(mc_mn_token, "radius") || !strcmp(mc_mn_token, "xy"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_RADIUS; strcpy(mc_mn_Set_Vars_Coord_Label,"Radius [m]"); strcpy(mc_mn_Set_Vars_Coord_Var,"R"); mc_mn_lmin = 0; mc_mn_lmax = mc_mn_xmax; }
        if (!strcmp(mc_mn_token, "vxy"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_VXY; strcpy(mc_mn_Set_Vars_Coord_Label,"Radial Velocity [m]"); strcpy(mc_mn_Set_Vars_Coord_Var,"Vxy"); mc_mn_lmin = 0; mc_mn_lmax = 2000; }
        if (!strcmp(mc_mn_token, "kxy"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_KXY; strcpy(mc_mn_Set_Vars_Coord_Label,"Radial Wavevector [Angs-1]"); strcpy(mc_mn_Set_Vars_Coord_Var,"Kxy"); mc_mn_lmin = 0; mc_mn_lmax = 2; }
        if (!strcmp(mc_mn_token, "angle"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_ANGLE; strcpy(mc_mn_Set_Vars_Coord_Label,"Angle [deg]"); strcpy(mc_mn_Set_Vars_Coord_Var,"A"); mc_mn_lmin = -5; mc_mn_lmax = 5; }
        if (!strcmp(mc_mn_token, "hdiv")|| !strcmp(mc_mn_token, "divergence") || !strcmp(mc_mn_token, "xdiv") || !strcmp(mc_mn_token, "dx"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_HDIV; strcpy(mc_mn_Set_Vars_Coord_Label,"Hor. Divergence [deg]"); strcpy(mc_mn_Set_Vars_Coord_Var,"HD"); mc_mn_lmin = -5; mc_mn_lmax = 5; }
        if (!strcmp(mc_mn_token, "vdiv") || !strcmp(mc_mn_token, "ydiv") || !strcmp(mc_mn_token, "dy"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_VDIV; strcpy(mc_mn_Set_Vars_Coord_Label,"Vert. Divergence [deg]"); strcpy(mc_mn_Set_Vars_Coord_Var,"VD"); mc_mn_lmin = -5; mc_mn_lmax = 5; }
        if (!strcmp(mc_mn_token, "theta") || !strcmp(mc_mn_token, "longitude"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_THETA; strcpy(mc_mn_Set_Vars_Coord_Label,"Longitude [deg]"); strcpy(mc_mn_Set_Vars_Coord_Var,"th"); mc_mn_lmin = -180; mc_mn_lmax = 180; }
        if (!strcmp(mc_mn_token, "phi") || !strcmp(mc_mn_token, "lattitude"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_PHI; strcpy(mc_mn_Set_Vars_Coord_Label,"Lattitude [deg]"); strcpy(mc_mn_Set_Vars_Coord_Var,"ph"); mc_mn_lmin = -180; mc_mn_lmax = 180; }
        if (!strcmp(mc_mn_token, "ncounts"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_NCOUNT; strcpy(mc_mn_Set_Vars_Coord_Label,"Neutrons [1]"); strcpy(mc_mn_Set_Vars_Coord_Var,"N"); mc_mn_lmin = 0; mc_mn_lmax = 1e10; }
        if (!strcmp(mc_mn_token, "user") || !strcmp(mc_mn_token, "user1"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_USER1; strncpy(mc_mn_Set_Vars_Coord_Label,mc_mn_Vars->UserName1,30); strcpy(mc_mn_Set_Vars_Coord_Var,"U1"); mc_mn_lmin = -1e10; mc_mn_lmax = 1e10; }
        if (!strcmp(mc_mn_token, "user2"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_USER2; strncpy(mc_mn_Set_Vars_Coord_Label,mc_mn_Vars->UserName2,30); strcpy(mc_mn_Set_Vars_Coord_Var,"U2"); mc_mn_lmin = -1e10; mc_mn_lmax = 1e10; }
        if (!strcmp(mc_mn_token, "user3"))
          { mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_USER3; strncpy(mc_mn_Set_Vars_Coord_Label,mc_mn_Vars->UserName3,30); strcpy(mc_mn_Set_Vars_Coord_Var,"U3"); mc_mn_lmin = -1e10; mc_mn_lmax = 1e10; }

        /* now stores variable keywords detected, if any */
        if (mc_mn_Set_Vars_Coord_Type != mc_mn_DEFS->COORD_NONE)
        {
          int mc_mn_Coord_Number = mc_mn_Vars->Coord_Number;
          if (mc_mn_Vars->Flag_log) { mc_mn_Set_Vars_Coord_Type |= mc_mn_DEFS->COORD_LOG; mc_mn_Vars->Flag_log = 0; }
          if (mc_mn_Flag_abs) { mc_mn_Set_Vars_Coord_Type |= mc_mn_DEFS->COORD_ABS; mc_mn_Flag_abs = 0; }
          if (mc_mn_Flag_auto != 0) { mc_mn_Set_Vars_Coord_Type |= mc_mn_DEFS->COORD_AUTO; mc_mn_Flag_auto = 0; }
          if (mc_mn_Set_Coord_Mode == mc_mn_DEFS->COORD_SIGNAL)
          {
            mc_mn_Coord_Number = 0;
            mc_mn_Vars->Flag_signal = mc_mn_Set_Vars_Coord_Type;
          }
          else
          {
            if (mc_mn_Coord_Number < MONnD_COORD_NMAX)
            { mc_mn_Coord_Number++;
              mc_mn_Vars->Coord_Number = mc_mn_Coord_Number; }
            else if (mc_mn_Vars->Flag_Verbose) printf("Monitor_nD: %s reached max number of variables (%i).\n", mc_mn_Vars->compcurname, MONnD_COORD_NMAX);
          }
          mc_mn_Vars->Coord_Type[mc_mn_Coord_Number] = mc_mn_Set_Vars_Coord_Type;
          strncpy(mc_mn_Vars->Coord_Label[mc_mn_Coord_Number], mc_mn_Set_Vars_Coord_Label,30);
          strncpy(mc_mn_Vars->Coord_Var[mc_mn_Coord_Number], mc_mn_Set_Vars_Coord_Var,30);
          if (mc_mn_lmin > mc_mn_lmax) { mc_mn_XY = mc_mn_lmin; mc_mn_lmin=mc_mn_lmax; mc_mn_lmax = mc_mn_XY; }
          mc_mn_Vars->Coord_Min[mc_mn_Coord_Number] = mc_mn_lmin;
          mc_mn_Vars->Coord_Max[mc_mn_Coord_Number] = mc_mn_lmax;
          if (mc_mn_Set_Coord_Mode != mc_mn_DEFS->COORD_SIGNAL) mc_mn_Vars->Coord_Bin[mc_mn_Coord_Number] = 20;
          mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_VAR;
          mc_mn_Flag_All = 0;
          mc_mn_Flag_No  = 0;
        } else {
          /* no variable name could be read from options */
          if (!mc_mn_iskeyword) {
            if (strcmp(mc_mn_token, "cm2") && strcmp(mc_mn_token, "incoming")
             && strcmp(mc_mn_token, "outgoing") && strcmp(mc_mn_token, "cm2")
             && strcmp(mc_mn_token, "cm^2") && strcmp(mc_mn_token, "float")
             && strcmp(mc_mn_token, "double") && strcmp(mc_mn_token, "binary")
             && strcmp(mc_mn_token, "steradian") && mc_mn_Vars->Flag_Verbose)
              printf("Monitor_nD: %s: unknown '%s' keyword in 'options'. Ignoring.\n", mc_mn_Vars->compcurname, mc_mn_token);
          }
        }
      mc_mn_carg++;
      } /* end if mc_mn_token */
    } /* end while mc_mn_carg */
    free(mc_mn_option_copy);
    if (mc_mn_carg == 128) printf("Monitor_nD: %s reached max number of mc_mn_tokens (%i). Skipping.\n", mc_mn_Vars->compcurname, 128);

    if ((mc_mn_Vars->Flag_Shape == mc_mn_DEFS->SHAPE_BOX) && (fabs(mc_mn_Vars->mzmax - mc_mn_Vars->mzmin) == 0)) mc_mn_Vars->Flag_Shape = mc_mn_DEFS->SHAPE_SQUARE;

    if (mc_mn_Vars->Flag_log == 1) mc_mn_Vars->Coord_Type[0] |= mc_mn_DEFS->COORD_LOG;
    if (mc_mn_Vars->Coord_Number == 0)
    { mc_mn_Vars->Flag_Auto_Limits=0; mc_mn_Vars->Flag_Multiple=0; mc_mn_Vars->Flag_List=0; }

    /* now setting Monitor Name from variable mc_mn_labels */
    strcpy(mc_mn_Vars->Monitor_Label,"");
    mc_mn_XY = 1; /* will contain total bin number */
    for (mc_mn_i = 0; mc_mn_i <= mc_mn_Vars->Coord_Number; mc_mn_i++)
    {
      if (mc_mn_Flag_auto != 0) mc_mn_Vars->Coord_Type[mc_mn_i] |= mc_mn_DEFS->COORD_AUTO;
      mc_mn_Set_Vars_Coord_Type = (mc_mn_Vars->Coord_Type[mc_mn_i] & 31);
      if ((mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_THETA)
       || (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_PHI)
       || (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_X)
       || (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_Y)
       || (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_Z)
       || (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_RADIUS))
       strcpy(mc_mn_Short_Label[mc_mn_i],"Position");
      else
      if ((mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_VX)
       || (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_VY)
       || (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_VZ)
       || (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_V)
       || (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_VXY))
       strcpy(mc_mn_Short_Label[mc_mn_i],"Velocity");
      else
      if ((mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_KX)
       || (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_KY)
       || (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_KZ)
       || (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_KXY)
       || (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_K))
       strcpy(mc_mn_Short_Label[mc_mn_i],"Wavevector");
      else
      if ((mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_SX)
       || (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_SY)
       || (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_SZ))
       strcpy(mc_mn_Short_Label[mc_mn_i],"Spin");
      else
      if ((mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_HDIV)
       || (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_VDIV)
       || (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_ANGLE))
       strcpy(mc_mn_Short_Label[mc_mn_i],"Divergence");
      else
      if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_ENERGY)
       strcpy(mc_mn_Short_Label[mc_mn_i],"Energy");
      else
      if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_LAMBDA)
       strcpy(mc_mn_Short_Label[mc_mn_i],"Wavelength");
      else
      if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_NCOUNT)
       strcpy(mc_mn_Short_Label[mc_mn_i],"Neutron counts");
      else
      if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_T)
          strcpy(mc_mn_Short_Label[mc_mn_i],"Time Of Flight");
      else
      if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_P)
          strcpy(mc_mn_Short_Label[mc_mn_i],"Intensity");
      else
      if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_USER1)
          strncpy(mc_mn_Short_Label[mc_mn_i],mc_mn_Vars->UserName1,30);
      else
      if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_USER2)
          strncpy(mc_mn_Short_Label[mc_mn_i],mc_mn_Vars->UserName2,30);
      else
      if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_USER3)
          strncpy(mc_mn_Short_Label[mc_mn_i],mc_mn_Vars->UserName3,30);
      else
          strcpy(mc_mn_Short_Label[mc_mn_i],"Unknown");

      if (mc_mn_Vars->Coord_Type[mc_mn_i] & mc_mn_DEFS->COORD_ABS)
      { strcat(mc_mn_Vars->Coord_Label[mc_mn_i]," (abs)"); }

      if (mc_mn_Vars->Coord_Type[mc_mn_i] & mc_mn_DEFS->COORD_LOG)
      { strcat(mc_mn_Vars->Coord_Label[mc_mn_i]," (log)"); }

      strcat(mc_mn_Vars->Monitor_Label, " ");
      strcat(mc_mn_Vars->Monitor_Label, mc_mn_Short_Label[mc_mn_i]);
      mc_mn_XY *= mc_mn_Vars->Coord_Bin[mc_mn_i];
    } /* end for mc_mn_Short_Label */

    if ((mc_mn_Vars->Coord_Type[0] & 31) == mc_mn_DEFS->COORD_P) {
      strncat(mc_mn_Vars->Coord_Label[0], " [n/s", 30);
      if (mc_mn_Vars->Flag_per_cm2) strncat(mc_mn_Vars->Coord_Label[0], "/cm2", 30);
      if (mc_mn_Vars->Flag_per_st) {
        if (mc_mn_Vars->Flag_Auto_Limits)
          strncat(mc_mn_Vars->Coord_Label[0], "/st", 30);
        else
          printf("Monitor_nD: %s: Flux per steradian requires Auto limits mode\n"
                 "WARNING     use options='.. auto ...'\n", mc_mn_Vars->compcurname);
      }
      if (mc_mn_XY > 1 && mc_mn_Vars->Coord_Number)
        strncat(mc_mn_Vars->Coord_Label[0], "/bin", 30);
      strncat(mc_mn_Vars->Coord_Label[0], "]", 30);
    }

    /* update label 'signal per bin' if more than 1 bin */
    if (mc_mn_XY > 1 && mc_mn_Vars->Coord_Number) {
      if (mc_mn_Vars->Flag_capture)
        printf("Monitor_nD: %s: Using capture flux weightening on %ld bins.\n"
               "            Use binned data with caution, and prefer monitor integral value (I,Ierr).\n", mc_mn_Vars->compcurname, (long)mc_mn_XY);
    }

    strcat(mc_mn_Vars->Monitor_Label, " Monitor");
    if (mc_mn_Vars->Flag_Shape == mc_mn_DEFS->SHAPE_SQUARE) strcat(mc_mn_Vars->Monitor_Label, " (Square)");
    if (mc_mn_Vars->Flag_Shape == mc_mn_DEFS->SHAPE_DISK)   strcat(mc_mn_Vars->Monitor_Label, " (Disk)");
    if (mc_mn_Vars->Flag_Shape == mc_mn_DEFS->SHAPE_SPHERE) strcat(mc_mn_Vars->Monitor_Label, " (Sphere)");
    if (mc_mn_Vars->Flag_Shape == mc_mn_DEFS->SHAPE_CYLIND) strcat(mc_mn_Vars->Monitor_Label, " (Cylinder)");
    if (mc_mn_Vars->Flag_Shape == mc_mn_DEFS->SHAPE_BANANA) strcat(mc_mn_Vars->Monitor_Label, " (Banana)");
    if (mc_mn_Vars->Flag_Shape == mc_mn_DEFS->SHAPE_BOX)    strcat(mc_mn_Vars->Monitor_Label, " (Box)");
    if (mc_mn_Vars->Flag_Shape == mc_mn_DEFS->SHAPE_PREVIOUS) strcat(mc_mn_Vars->Monitor_Label, " (on PREVIOUS)");
    if ((mc_mn_Vars->Flag_Shape == mc_mn_DEFS->SHAPE_CYLIND) || (mc_mn_Vars->Flag_Shape == mc_mn_DEFS->SHAPE_BANANA) || (mc_mn_Vars->Flag_Shape == mc_mn_DEFS->SHAPE_SPHERE) || (mc_mn_Vars->Flag_Shape == mc_mn_DEFS->SHAPE_BOX))
    {
      if (strstr(mc_mn_Vars->option, "incoming"))
      {
        mc_mn_Vars->Flag_Shape = abs(mc_mn_Vars->Flag_Shape);
        strcat(mc_mn_Vars->Monitor_Label, " [in]");
      }
      else /* if strstr(mc_mn_Vars->option, "outgoing")) */
      {
        mc_mn_Vars->Flag_Shape = -abs(mc_mn_Vars->Flag_Shape);
        strcat(mc_mn_Vars->Monitor_Label, " [out]");
      }
    }
    if (mc_mn_Vars->Flag_UsePreMonitor == 1)
    {
        strcat(mc_mn_Vars->Monitor_Label, " at ");
        strncat(mc_mn_Vars->Monitor_Label, mc_mn_Vars->UserName1,30);
    }
    if (mc_mn_Vars->Flag_log == 1) strcat(mc_mn_Vars->Monitor_Label, " [log] ");

    /* mc_mn_Vars->Coord_Number  0   : intensity or signal
     * mc_mn_Vars->Coord_Number  1:n : detector variables */

    /* now allocate memory to store variables in TRACE */
    if ((mc_mn_Vars->Coord_Number != 2) && !mc_mn_Vars->Flag_Multiple && !mc_mn_Vars->Flag_List)
    { mc_mn_Vars->Flag_Multiple = 1; mc_mn_Vars->Flag_List = 0; } /* default is n1D */

   /* list and auto limits case : mc_mn_Vars->Flag_List or mc_mn_Vars->Flag_Auto_Limits
    * -> Buffer to flush and suppress after mc_mn_Vars->Flag_Auto_Limits
    */
    if ((mc_mn_Vars->Flag_Auto_Limits || mc_mn_Vars->Flag_List) && mc_mn_Vars->Coord_Number)
    { /* Dim : (mc_mn_Vars->Coord_Number+1)*mc_mn_Vars->Buffer_Block matrix (for p, dp) */
      mc_mn_Vars->Mon2D_Buffer = (double *)malloc((mc_mn_Vars->Coord_Number+1)*mc_mn_Vars->Buffer_Block*sizeof(double));
      if (mc_mn_Vars->Mon2D_Buffer == NULL)
      { printf("Monitor_nD: %s cannot allocate mc_mn_Vars->Mon2D_Buffer (%li). No list and auto limits.\n", mc_mn_Vars->compcurname, mc_mn_Vars->Buffer_Block*(mc_mn_Vars->Coord_Number+1)*sizeof(double)); mc_mn_Vars->Flag_List = 0; mc_mn_Vars->Flag_Auto_Limits = 0; }
      else
      {
        for (mc_mn_i=0; mc_mn_i < (mc_mn_Vars->Coord_Number+1)*mc_mn_Vars->Buffer_Block; mc_mn_Vars->Mon2D_Buffer[mc_mn_i++] = (double)0);
      }
      mc_mn_Vars->Buffer_Size = mc_mn_Vars->Buffer_Block;
    }

    /* 1D and n1D case : mc_mn_Vars->Flag_Multiple */
    if (mc_mn_Vars->Flag_Multiple && mc_mn_Vars->Coord_Number)
    { /* Dim : mc_mn_Vars->Coord_Number*mc_mn_Vars->Coord_Bin[mc_mn_i] vectors */
      mc_mn_Vars->Mon2D_N  = (double **)malloc((mc_mn_Vars->Coord_Number)*sizeof(double *));
      mc_mn_Vars->Mon2D_p  = (double **)malloc((mc_mn_Vars->Coord_Number)*sizeof(double *));
      mc_mn_Vars->Mon2D_p2 = (double **)malloc((mc_mn_Vars->Coord_Number)*sizeof(double *));
      if ((mc_mn_Vars->Mon2D_N == NULL) || (mc_mn_Vars->Mon2D_p == NULL) || (mc_mn_Vars->Mon2D_p2 == NULL))
      { fprintf(stderr,"Monitor_nD: %s n1D cannot allocate mc_mn_Vars->Mon2D_N/p/2p (%li). Fatal.\n", mc_mn_Vars->compcurname, (mc_mn_Vars->Coord_Number)*sizeof(double *)); exit(-1); }
      for (mc_mn_i= 1; mc_mn_i <= mc_mn_Vars->Coord_Number; mc_mn_i++)
      {
        mc_mn_Vars->Mon2D_N[mc_mn_i-1]  = (double *)malloc(mc_mn_Vars->Coord_Bin[mc_mn_i]*sizeof(double));
        mc_mn_Vars->Mon2D_p[mc_mn_i-1]  = (double *)malloc(mc_mn_Vars->Coord_Bin[mc_mn_i]*sizeof(double));
        mc_mn_Vars->Mon2D_p2[mc_mn_i-1] = (double *)malloc(mc_mn_Vars->Coord_Bin[mc_mn_i]*sizeof(double));
        if ((mc_mn_Vars->Mon2D_N == NULL) || (mc_mn_Vars->Mon2D_p == NULL) || (mc_mn_Vars->Mon2D_p2 == NULL))
        { fprintf(stderr,"Monitor_nD: %s n1D cannot allocate %s mc_mn_Vars->Mon2D_N/p/2p[%li] (%li). Fatal.\n", mc_mn_Vars->compcurname, mc_mn_Vars->Coord_Var[mc_mn_i], mc_mn_i, (mc_mn_Vars->Coord_Bin[mc_mn_i])*sizeof(double *)); exit(-1); }
        else
        {
          for (mc_mn_j=0; mc_mn_j < mc_mn_Vars->Coord_Bin[mc_mn_i]; mc_mn_j++ )
          { mc_mn_Vars->Mon2D_N[mc_mn_i-1][mc_mn_j] = (double)0; mc_mn_Vars->Mon2D_p[mc_mn_i-1][mc_mn_j] = (double)0; mc_mn_Vars->Mon2D_p2[mc_mn_i-1][mc_mn_j] = (double)0; }
        }
      }
    }
    else /* 2D case : mc_mn_Vars->Coord_Number==2 and !mc_mn_Vars->Flag_Multiple and !mc_mn_Vars->Flag_List */
    if ((mc_mn_Vars->Coord_Number == 2) && !mc_mn_Vars->Flag_Multiple)
    { /* Dim : mc_mn_Vars->Coord_Bin[1]*mc_mn_Vars->Coord_Bin[2] matrix */
      mc_mn_Vars->Mon2D_N  = (double **)malloc((mc_mn_Vars->Coord_Bin[1])*sizeof(double *));
      mc_mn_Vars->Mon2D_p  = (double **)malloc((mc_mn_Vars->Coord_Bin[1])*sizeof(double *));
      mc_mn_Vars->Mon2D_p2 = (double **)malloc((mc_mn_Vars->Coord_Bin[1])*sizeof(double *));
      if ((mc_mn_Vars->Mon2D_N == NULL) || (mc_mn_Vars->Mon2D_p == NULL) || (mc_mn_Vars->Mon2D_p2 == NULL))
      { fprintf(stderr,"Monitor_nD: %s 2D cannot allocate %s mc_mn_Vars->Mon2D_N/p/2p (%li). Fatal.\n", mc_mn_Vars->compcurname, mc_mn_Vars->Coord_Var[1], (mc_mn_Vars->Coord_Bin[1])*sizeof(double *)); exit(-1); }
      for (mc_mn_i= 0; mc_mn_i < mc_mn_Vars->Coord_Bin[1]; mc_mn_i++)
      {
        mc_mn_Vars->Mon2D_N[mc_mn_i]  = (double *)malloc(mc_mn_Vars->Coord_Bin[2]*sizeof(double));
        mc_mn_Vars->Mon2D_p[mc_mn_i]  = (double *)malloc(mc_mn_Vars->Coord_Bin[2]*sizeof(double));
        mc_mn_Vars->Mon2D_p2[mc_mn_i] = (double *)malloc(mc_mn_Vars->Coord_Bin[2]*sizeof(double));
        if ((mc_mn_Vars->Mon2D_N == NULL) || (mc_mn_Vars->Mon2D_p == NULL) || (mc_mn_Vars->Mon2D_p2 == NULL))
        { fprintf(stderr,"Monitor_nD: %s 2D cannot allocate %s mc_mn_Vars->Mon2D_N/p/2p[%li] (%li). Fatal.\n", mc_mn_Vars->compcurname, mc_mn_Vars->Coord_Var[1], mc_mn_i, (mc_mn_Vars->Coord_Bin[2])*sizeof(double *)); exit(-1); }
        else
        {
          for (mc_mn_j=0; mc_mn_j < mc_mn_Vars->Coord_Bin[2]; mc_mn_j++ )
          { mc_mn_Vars->Mon2D_N[mc_mn_i][mc_mn_j] = (double)0; mc_mn_Vars->Mon2D_p[mc_mn_i][mc_mn_j] = (double)0; mc_mn_Vars->Mon2D_p2[mc_mn_i][mc_mn_j] = (double)0; }
        }
      }
    }
      /* no Mon2D allocated for
       * (mc_mn_Vars->Coord_Number != 2) && !mc_mn_Vars->Flag_Multiple && mc_mn_Vars->Flag_List */

    mc_mn_Vars->psum  = 0;
    mc_mn_Vars->p2sum = 0;
    mc_mn_Vars->Nsum  = 0;

    mc_mn_Vars->area  = fabs(mc_mn_Vars->mxmax - mc_mn_Vars->mxmin)*fabs(mc_mn_Vars->mymax - mc_mn_Vars->mymin)*1E4; /* in cm**2 for square and box shapes */
    mc_mn_Vars->Sphere_Radius = fabs(mc_mn_Vars->mxmax - mc_mn_Vars->mxmin)/2;
    if ((abs(mc_mn_Vars->Flag_Shape) == mc_mn_DEFS->SHAPE_DISK) || (abs(mc_mn_Vars->Flag_Shape) == mc_mn_DEFS->SHAPE_SPHERE))
    {
      mc_mn_Vars->area = PI*mc_mn_Vars->Sphere_Radius*mc_mn_Vars->Sphere_Radius; /* disk shapes */
    }
    if (mc_mn_Vars->area == 0) mc_mn_Vars->Coord_Number = 0;
    if (mc_mn_Vars->Coord_Number == 0 && mc_mn_Vars->Flag_Verbose)
      printf("Monitor_nD: %s is unactivated (0D)\n", mc_mn_Vars->compcurname);
    mc_mn_Vars->Cylinder_Height = fabs(mc_mn_Vars->mymax - mc_mn_Vars->mymin);

    if (mc_mn_Vars->Intermediate < 0) mc_mn_Vars->Intermediate = 0;
    if (mc_mn_Vars->Intermediate > 1) mc_mn_Vars->Intermediate /= 100;
    mc_mn_Vars->IntermediateCnts = mc_mn_Vars->Intermediate*mcget_ncount();

    if (mc_mn_Vars->Flag_Verbose)
    {
      printf("Monitor_nD: %s is a %s.\n", mc_mn_Vars->compcurname, mc_mn_Vars->Monitor_Label);
      printf("Monitor_nD: version %s with options=%s\n", MONITOR_ND_LIB_H, mc_mn_Vars->option);
    }
  } /* end Monitor_nD_Init */

/* ========================================================================= */
/* ADD: E.Farhi, Aug 6th, 2001: Monitor_nD section */
/* this routine is used to monitor one propagating neutron */
/* ========================================================================= */

double Monitor_nD_Trace(MonitornD_Defines_type *mc_mn_DEFS, MonitornD_Variables_type *mc_mn_Vars)
{

  double  mc_mn_XY=0;
  long    mc_mn_i,mc_mn_j;
  double  mc_mn_pp;
  double  mc_mn_Coord[MONnD_COORD_NMAX];
  long    mc_mn_Coord_Index[MONnD_COORD_NMAX];
  char    mc_mn_While_End   =0;
  long    mc_mn_While_Buffer=0;
  char    mc_mn_Set_Vars_Coord_Type = mc_mn_DEFS->COORD_NONE;

  /* mc_mn_Vars->Flag_Auto_Limits */
  if ((mc_mn_Vars->Buffer_Counter >= mc_mn_Vars->Buffer_Block) && (mc_mn_Vars->Flag_Auto_Limits == 1) && (mc_mn_Vars->Coord_Number > 0))
  {
    /* auto limits case : get limits in Buffer for each variable */
          /* Dim : (mc_mn_Vars->Coord_Number+1)*mc_mn_Vars->Buffer_Block matrix (for p, dp) */
    if (mc_mn_Vars->Flag_Verbose) printf("Monitor_nD: %s getting %li Auto Limits from List (%li).\n", mc_mn_Vars->compcurname, mc_mn_Vars->Coord_Number, mc_mn_Vars->Buffer_Counter);
    for (mc_mn_i = 1; mc_mn_i <= mc_mn_Vars->Coord_Number; mc_mn_i++)
    {
      if (mc_mn_Vars->Coord_Type[mc_mn_i] & mc_mn_DEFS->COORD_AUTO)
      {
        mc_mn_Vars->Coord_Min[mc_mn_i] = FLT_MAX;
        mc_mn_Vars->Coord_Max[mc_mn_i] = -FLT_MAX;
        for (mc_mn_j = 0; mc_mn_j < mc_mn_Vars->Buffer_Block; mc_mn_j++)
        {
          mc_mn_XY = mc_mn_Vars->Mon2D_Buffer[mc_mn_i+mc_mn_j*(mc_mn_Vars->Coord_Number+1)];  /* scanning variables in Buffer */
          if (mc_mn_XY < mc_mn_Vars->Coord_Min[mc_mn_i]) mc_mn_Vars->Coord_Min[mc_mn_i] = mc_mn_XY;
          if (mc_mn_XY > mc_mn_Vars->Coord_Max[mc_mn_i]) mc_mn_Vars->Coord_Max[mc_mn_i] = mc_mn_XY;
        }
      }
    }
    mc_mn_Vars->Flag_Auto_Limits = 2;  /* pass to 2nd auto limits step */
  }

  /* manage realloc for list all if Buffer size exceeded */
  if ((mc_mn_Vars->Buffer_Counter >= mc_mn_Vars->Buffer_Block) && (mc_mn_Vars->Flag_List >= 2))
  {
    if (mc_mn_Vars->Buffer_Size >= 20000 || mc_mn_Vars->Flag_List == 3)
    { /* save current (possibly append) and re-use Buffer */
      Monitor_nD_Save(mc_mn_DEFS, mc_mn_Vars);
      mc_mn_Vars->Flag_List = 3;
      mc_mn_Vars->Buffer_Block = mc_mn_Vars->Buffer_Size;
      mc_mn_Vars->Buffer_Counter  = 0;
      mc_mn_Vars->Neutron_Counter = 0;
    }
    else
    {
      mc_mn_Vars->Mon2D_Buffer  = (double *)realloc(mc_mn_Vars->Mon2D_Buffer, (mc_mn_Vars->Coord_Number+1)*(mc_mn_Vars->Neutron_Counter+mc_mn_Vars->Buffer_Block)*sizeof(double));
      if (mc_mn_Vars->Mon2D_Buffer == NULL)
            { printf("Monitor_nD: %s cannot reallocate mc_mn_Vars->Mon2D_Buffer[%li] (%li). Skipping.\n", mc_mn_Vars->compcurname, mc_mn_i, (mc_mn_Vars->Neutron_Counter+mc_mn_Vars->Buffer_Block)*sizeof(double)); mc_mn_Vars->Flag_List = 1; }
      else { mc_mn_Vars->Buffer_Counter = 0; mc_mn_Vars->Buffer_Size = mc_mn_Vars->Neutron_Counter+mc_mn_Vars->Buffer_Block; }
    }
  }

  while (!mc_mn_While_End)
  { /* we generate mc_mn_Coord[] and Coord_mc_mn_index[] from Buffer (auto limits) or passing neutron */
    if ((mc_mn_Vars->Flag_Auto_Limits == 2) && (mc_mn_Vars->Coord_Number > 0))
    {
      if (mc_mn_While_Buffer < mc_mn_Vars->Buffer_Block)
      {
        /* first while loops (mc_mn_While_Buffer) */
        /* auto limits case : scan Buffer within limits and store in Mon2D */
        mc_mn_pp = mc_mn_Vars->Mon2D_Buffer[mc_mn_While_Buffer*(mc_mn_Vars->Coord_Number+1)];
	/* For some reason the Intel c compiler version 10.1 gives 0 counts with Monitor_nD!
	   An ugly patch seems to be the following printf */
	printf("");
        mc_mn_Coord[0] = mc_mn_pp;

        for (mc_mn_i = 1; mc_mn_i <= mc_mn_Vars->Coord_Number; mc_mn_i++)
        {
          /* scanning variables in Buffer */
          mc_mn_XY = (mc_mn_Vars->Coord_Max[mc_mn_i]-mc_mn_Vars->Coord_Min[mc_mn_i]);

          mc_mn_Coord[mc_mn_i] = mc_mn_Vars->Mon2D_Buffer[mc_mn_i+mc_mn_While_Buffer*(mc_mn_Vars->Coord_Number+1)];
          if (mc_mn_XY > 0) mc_mn_Coord_Index[mc_mn_i] = floor((mc_mn_Coord[mc_mn_i]-mc_mn_Vars->Coord_Min[mc_mn_i])*mc_mn_Vars->Coord_Bin[mc_mn_i]/mc_mn_XY);
          else mc_mn_Coord_Index[mc_mn_i] = 0;
          if (mc_mn_Vars->Flag_With_Borders)
          {
            if (mc_mn_Coord_Index[mc_mn_i] < 0) mc_mn_Coord_Index[mc_mn_i] = 0;
            if (mc_mn_Coord_Index[mc_mn_i] >= mc_mn_Vars->Coord_Bin[mc_mn_i]) mc_mn_Coord_Index[mc_mn_i] = mc_mn_Vars->Coord_Bin[mc_mn_i] - 1;
          }
        } /* end for */
        mc_mn_While_Buffer++;
      } /* end if in Buffer */
      else /* (mc_mn_While_Buffer >= mc_mn_Vars->Buffer_Block) && (mc_mn_Vars->Flag_Auto_Limits == 2) */
      {
        mc_mn_Vars->Flag_Auto_Limits = 0;
        if (!mc_mn_Vars->Flag_List) /* free Buffer not needed (no list to output) */
        { /* Dim : (mc_mn_Vars->Coord_Number+1)*mc_mn_Vars->Buffer_Block matrix (for p, dp) */
          free(mc_mn_Vars->Mon2D_Buffer); mc_mn_Vars->Mon2D_Buffer = NULL;
        }
      }
    }
    else /* mc_mn_Vars->Flag_Auto_Limits == 0 or 1 */
    {
      for (mc_mn_i = 0; mc_mn_i <= mc_mn_Vars->Coord_Number; mc_mn_i++)
      { /* handle current neutron : last while */
        if (mc_mn_Vars->Flag_Auto_Limits==1) {
          double v;
          v=sqrt(mc_mn_Vars->cvx*mc_mn_Vars->cvx
                +mc_mn_Vars->cvy*mc_mn_Vars->cvy
                +mc_mn_Vars->cvz*mc_mn_Vars->cvz);
          if (mc_mn_Vars->min_x > mc_mn_Vars->cx) mc_mn_Vars->min_x = mc_mn_Vars->cx;
          if (mc_mn_Vars->max_x < mc_mn_Vars->cx) mc_mn_Vars->max_x = mc_mn_Vars->cx;
          if (mc_mn_Vars->min_y > mc_mn_Vars->cy) mc_mn_Vars->min_y = mc_mn_Vars->cy;
          if (mc_mn_Vars->max_y < mc_mn_Vars->cy) mc_mn_Vars->max_y = mc_mn_Vars->cy;
          mc_mn_Vars->mean_p  += mc_mn_Vars->cp;
          if (v) {
            mc_mn_Vars->mean_dx += mc_mn_Vars->cp*fabs(mc_mn_Vars->cvx/v);
            mc_mn_Vars->mean_dy += mc_mn_Vars->cp*fabs(mc_mn_Vars->cvy/v);
          }
          mc_mn_Vars->area =(mc_mn_Vars->max_x-mc_mn_Vars->min_x)
                           *(mc_mn_Vars->max_y-mc_mn_Vars->min_y)*1E4; /* cm2 */
          if (mc_mn_Vars->Flag_per_st)
          mc_mn_Vars->steradian = 2*fabs(2*atan(mc_mn_Vars->mean_dx/mc_mn_Vars->mean_p)
                                    *sin(2*atan(mc_mn_Vars->mean_dy/mc_mn_Vars->mean_p)/2));
        }

        mc_mn_XY = 0;
        mc_mn_Set_Vars_Coord_Type = (mc_mn_Vars->Coord_Type[mc_mn_i] & 31);
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_X) mc_mn_XY = mc_mn_Vars->cx;
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_Y) mc_mn_XY = mc_mn_Vars->cy;
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_Z) mc_mn_XY = mc_mn_Vars->cz;
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_VX) mc_mn_XY = mc_mn_Vars->cvx;
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_VY) mc_mn_XY = mc_mn_Vars->cvy;
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_VZ) mc_mn_XY = mc_mn_Vars->cvz;
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_KX) mc_mn_XY = V2K*mc_mn_Vars->cvx;
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_KY) mc_mn_XY = V2K*mc_mn_Vars->cvy;
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_KZ) mc_mn_XY = V2K*mc_mn_Vars->cvz;
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_SX) mc_mn_XY = mc_mn_Vars->csx;
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_SY) mc_mn_XY = mc_mn_Vars->csy;
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_SZ) mc_mn_XY = mc_mn_Vars->csz;
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_T) mc_mn_XY = mc_mn_Vars->ct;
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_P) mc_mn_XY = mc_mn_Vars->cp;
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_HDIV) mc_mn_XY = RAD2DEG*atan2(mc_mn_Vars->cvx,mc_mn_Vars->cvz);
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_VDIV) mc_mn_XY = RAD2DEG*atan2(mc_mn_Vars->cvy,mc_mn_Vars->cvz);
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_V) mc_mn_XY = sqrt(mc_mn_Vars->cvx*mc_mn_Vars->cvx+mc_mn_Vars->cvy*mc_mn_Vars->cvy+mc_mn_Vars->cvz*mc_mn_Vars->cvz);
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_RADIUS) mc_mn_XY = sqrt(mc_mn_Vars->cx*mc_mn_Vars->cx+mc_mn_Vars->cy*mc_mn_Vars->cy);
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_VXY) mc_mn_XY = sqrt(mc_mn_Vars->cvx*mc_mn_Vars->cvx+mc_mn_Vars->cvy*mc_mn_Vars->cvy);
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_K) { mc_mn_XY = sqrt(mc_mn_Vars->cvx*mc_mn_Vars->cvx+mc_mn_Vars->cvy*mc_mn_Vars->cvy+mc_mn_Vars->cvz*mc_mn_Vars->cvz);  mc_mn_XY *= V2K; }
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_KXY) { mc_mn_XY = sqrt(mc_mn_Vars->cvx*mc_mn_Vars->cvx+mc_mn_Vars->cvy*mc_mn_Vars->cvy);  mc_mn_XY *= V2K; }
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_ENERGY) { mc_mn_XY = mc_mn_Vars->cvx*mc_mn_Vars->cvx+mc_mn_Vars->cvy*mc_mn_Vars->cvy+mc_mn_Vars->cvz*mc_mn_Vars->cvz;  mc_mn_XY *= VS2E; }
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_LAMBDA) { mc_mn_XY = sqrt(mc_mn_Vars->cvx*mc_mn_Vars->cvx+mc_mn_Vars->cvy*mc_mn_Vars->cvy+mc_mn_Vars->cvz*mc_mn_Vars->cvz);  mc_mn_XY *= V2K; if (mc_mn_XY != 0) mc_mn_XY = 2*PI/mc_mn_XY; }
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_NCOUNT) mc_mn_XY = mc_mn_Coord[mc_mn_i]+1;
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_ANGLE)
        {  mc_mn_XY = sqrt(mc_mn_Vars->cvx*mc_mn_Vars->cvx+mc_mn_Vars->cvy*mc_mn_Vars->cvy);
           if (mc_mn_Vars->cvz != 0)
           {
             mc_mn_XY= RAD2DEG*atan2(mc_mn_XY,mc_mn_Vars->cvz);
           } else mc_mn_XY = 0;
        }
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_THETA)  { if (mc_mn_Vars->cz != 0) mc_mn_XY = RAD2DEG*atan2(mc_mn_Vars->cx,mc_mn_Vars->cz); }
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_PHI) { if (mc_mn_Vars->cz != 0) mc_mn_XY = RAD2DEG*asin(mc_mn_Vars->cy/mc_mn_Vars->cz); }
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_USER1) mc_mn_XY = mc_mn_Vars->UserVariable1;
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_USER2) mc_mn_XY = mc_mn_Vars->UserVariable2;
        else
        if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_USER3) mc_mn_XY = mc_mn_Vars->UserVariable3;
        else
        mc_mn_XY = 0;

        if (mc_mn_Vars->Coord_Type[mc_mn_i] & mc_mn_DEFS->COORD_ABS) mc_mn_XY=fabs(mc_mn_XY);

        if (mc_mn_i && (mc_mn_Vars->Coord_Type[mc_mn_i] & mc_mn_DEFS->COORD_LOG)) /* not for the flux */
        {  if (mc_mn_XY > 0) mc_mn_XY = log(mc_mn_XY)/log(10);
           else mc_mn_XY = -100; }

        mc_mn_Coord[mc_mn_i] = mc_mn_XY;
        if (mc_mn_i == 0) { mc_mn_pp = mc_mn_XY; mc_mn_Coord_Index[mc_mn_i] = 0; }
        else if (!mc_mn_Vars->Flag_Auto_Limits)
        {
          mc_mn_XY = (mc_mn_Vars->Coord_Max[mc_mn_i]-mc_mn_Vars->Coord_Min[mc_mn_i]);
          if (mc_mn_XY > 0) mc_mn_Coord_Index[mc_mn_i] = floor((mc_mn_Coord[mc_mn_i]-mc_mn_Vars->Coord_Min[mc_mn_i])*mc_mn_Vars->Coord_Bin[mc_mn_i]/mc_mn_XY);
          else mc_mn_Coord_Index[mc_mn_i] = 0;
          if (mc_mn_Vars->Flag_With_Borders)
          {
            if (mc_mn_Coord_Index[mc_mn_i] < 0) mc_mn_Coord_Index[mc_mn_i] = 0;
            if (mc_mn_Coord_Index[mc_mn_i] >= mc_mn_Vars->Coord_Bin[mc_mn_i]) mc_mn_Coord_Index[mc_mn_i] = mc_mn_Vars->Coord_Bin[mc_mn_i] - 1;
          }
        } /* else Auto_Limits will get Index later from Buffer */
      } /* end for mc_mn_i */
      mc_mn_While_End = 1;
    } /* end else if mc_mn_Vars->Flag_Auto_Limits == 2 */

    if (mc_mn_Vars->Flag_Auto_Limits != 2) /* not when reading auto limits Buffer */
    { /* now store Coord into Buffer (no mc_mn_index needed) if necessary */
      if ((mc_mn_Vars->Buffer_Counter < mc_mn_Vars->Buffer_Block) && ((mc_mn_Vars->Flag_List) || (mc_mn_Vars->Flag_Auto_Limits == 1)))
      {
        for (mc_mn_i = 0; mc_mn_i <= mc_mn_Vars->Coord_Number; mc_mn_i++)
        {
          mc_mn_Vars->Mon2D_Buffer[mc_mn_i + mc_mn_Vars->Neutron_Counter*(mc_mn_Vars->Coord_Number+1)] = mc_mn_Coord[mc_mn_i];
        }
        mc_mn_Vars->Buffer_Counter++;
        if (mc_mn_Vars->Flag_Verbose && (mc_mn_Vars->Buffer_Counter >= mc_mn_Vars->Buffer_Block) && (mc_mn_Vars->Flag_List == 1)) printf("Monitor_nD: %s %li neutrons stored in List.\n", mc_mn_Vars->compcurname, mc_mn_Vars->Buffer_Counter);
      }
      mc_mn_Vars->Neutron_Counter++;
    } /* end (mc_mn_Vars->Flag_Auto_Limits != 2) */

    /* store n1d/2d section for Buffer or current neutron in while */
    if (mc_mn_Vars->Flag_Auto_Limits != 1) /* not when storing auto limits Buffer */
    {

      if (mc_mn_Vars->Flag_per_cm2 && mc_mn_Vars->area      != 0)
        mc_mn_pp /= mc_mn_Vars->area;
      if (mc_mn_Vars->Flag_per_st  && mc_mn_Vars->steradian != 0)
        mc_mn_pp /= mc_mn_Vars->steradian;

    /* 1D and n1D case : mc_mn_Vars->Flag_Multiple */
      if (mc_mn_Vars->Flag_Multiple)
      { /* Dim : mc_mn_Vars->Coord_Number*mc_mn_Vars->Coord_Bin[mc_mn_i] vectors (intensity is not included) */
        /* check limits: monitors define a phase space to record */
        char within_limits=1;
        for (mc_mn_i= 1; mc_mn_i <= mc_mn_Vars->Coord_Number; mc_mn_i++)
        {
          mc_mn_j = mc_mn_Coord_Index[mc_mn_i];
          if (mc_mn_j < 0 || mc_mn_j >= mc_mn_Vars->Coord_Bin[mc_mn_i])
            within_limits=0;
        }
        if (within_limits)
        { for (mc_mn_i= 1; mc_mn_i <= mc_mn_Vars->Coord_Number; mc_mn_i++)
          {
            mc_mn_j = mc_mn_Coord_Index[mc_mn_i];
            if (mc_mn_j >= 0 && mc_mn_j < mc_mn_Vars->Coord_Bin[mc_mn_i])
            {
              mc_mn_Vars->Mon2D_N[mc_mn_i-1][mc_mn_j]++;
              mc_mn_Vars->Mon2D_p[mc_mn_i-1][mc_mn_j] += mc_mn_pp;
              mc_mn_Vars->Mon2D_p2[mc_mn_i-1][mc_mn_j] += mc_mn_pp*mc_mn_pp;
            }
          }
        }
        else if (mc_mn_Vars->Flag_Exclusive)
        { mc_mn_pp = 0.0;
        }
      }
      else /* 2D case : mc_mn_Vars->Coord_Number==2 and !mc_mn_Vars->Flag_Multiple and !mc_mn_Vars->Flag_List */
      if ((mc_mn_Vars->Coord_Number == 2) && !mc_mn_Vars->Flag_Multiple)
      { /* Dim : mc_mn_Vars->Coord_Bin[1]*mc_mn_Vars->Coord_Bin[2] matrix */
        mc_mn_i = mc_mn_Coord_Index[1];
        mc_mn_j = mc_mn_Coord_Index[2];
        if (mc_mn_i >= 0 && mc_mn_i < mc_mn_Vars->Coord_Bin[1] && mc_mn_j >= 0 && mc_mn_j < mc_mn_Vars->Coord_Bin[2])
        {
          mc_mn_Vars->Mon2D_N[mc_mn_i][mc_mn_j]++;
          mc_mn_Vars->Mon2D_p[mc_mn_i][mc_mn_j] += mc_mn_pp;
          mc_mn_Vars->Mon2D_p2[mc_mn_i][mc_mn_j] += mc_mn_pp*mc_mn_pp;
        }
        else if (mc_mn_Vars->Flag_Exclusive)
        { mc_mn_pp = 0.0;
        }
      }
    } /* end (mc_mn_Vars->Flag_Auto_Limits != 1) */
  } /* end while */
  return mc_mn_pp;
} /* end Monitor_nD_Trace */

/* ========================================================================= */
/* ADD: E.Farhi, Aug 6th, 2001: Monitor_nD section */
/* this routine is used to save data files */
/* ========================================================================= */

void Monitor_nD_Save(MonitornD_Defines_type *mc_mn_DEFS, MonitornD_Variables_type *mc_mn_Vars)
  {
    char   *mc_mn_fname;
    long    mc_mn_i,mc_mn_j;
    double *mc_mn_p0m = NULL;
    double *mc_mn_p1m = NULL;
    double *mc_mn_p2m = NULL;
    char    mc_mn_Coord_X_Label[1024];
    double  mc_mn_min1d, mc_mn_max1d;
    double  mc_mn_min2d, mc_mn_max2d;
    long    mc_mn_bin1d, mc_mn_bin2d;
    char    mc_mn_While_End = 0;
    long    mc_mn_While_Buffer = 0;
    double  mc_mn_XY, mc_mn_pp;
    double  mc_mn_Coord[MONnD_COORD_NMAX];
    long    mc_mn_Coord_Index[MONnD_COORD_NMAX];
    char    mc_mn_label[1024];
    double  mc_mn_ratio;

    mc_mn_ratio = 100*mcget_run_num()/mcget_ncount();
    if (mc_mn_Vars->Flag_per_cm2 && mc_mn_Vars->area && mc_mn_Vars->Flag_Verbose)
      printf("Monitor_nD: %s: detector area is %g [cm2]\n",
        mc_mn_Vars->compcurname, mc_mn_Vars->area);
    if (mc_mn_Vars->Flag_per_st && mc_mn_Vars->steradian && mc_mn_Vars->Flag_Verbose)
      printf("Monitor_nD: %s: beam solid angle is %g [st] (%g x %g [deg2])\n",
        mc_mn_Vars->compcurname, mc_mn_Vars->steradian,
        atan(mc_mn_Vars->mean_dx/mc_mn_Vars->mean_p)*RAD2DEG,
        atan(mc_mn_Vars->mean_dy/mc_mn_Vars->mean_p)*RAD2DEG);

    if (mc_mn_ratio < 99)
    {
      if (mc_mn_Vars->Flag_Verbose) printf("Monitor_nD: %s save intermediate results (%.2f %%).\n", mc_mn_Vars->compcurname, mc_mn_ratio);
    }
    /* check Buffer flush when end of simulation reached */
    if ((mc_mn_Vars->Buffer_Counter <= mc_mn_Vars->Buffer_Block) && mc_mn_Vars->Flag_Auto_Limits && mc_mn_Vars->Mon2D_Buffer && mc_mn_Vars->Buffer_Counter)
    {
      /* Get Auto Limits */
      if (mc_mn_Vars->Flag_Verbose) printf("Monitor_nD: %s getting %li Auto Limits from List (%li).\n", mc_mn_Vars->compcurname, mc_mn_Vars->Coord_Number, mc_mn_Vars->Buffer_Counter);
      for (mc_mn_i = 1; mc_mn_i <= mc_mn_Vars->Coord_Number; mc_mn_i++)
      {
        if (mc_mn_Vars->Coord_Type[mc_mn_i] & mc_mn_DEFS->COORD_AUTO)
        {
          mc_mn_Vars->Coord_Min[mc_mn_i] = FLT_MAX;
          mc_mn_Vars->Coord_Max[mc_mn_i] = -FLT_MAX;

          for (mc_mn_j = 0; mc_mn_j < mc_mn_Vars->Buffer_Counter; mc_mn_j++)
          {
            mc_mn_XY = mc_mn_Vars->Mon2D_Buffer[mc_mn_i+mc_mn_j*(mc_mn_Vars->Coord_Number+1)];  /* scanning variables in Buffer */
            if (mc_mn_XY < mc_mn_Vars->Coord_Min[mc_mn_i]) mc_mn_Vars->Coord_Min[mc_mn_i] = mc_mn_XY;
            if (mc_mn_XY > mc_mn_Vars->Coord_Max[mc_mn_i]) mc_mn_Vars->Coord_Max[mc_mn_i] = mc_mn_XY;

          }
        }
      }
      mc_mn_Vars->Flag_Auto_Limits = 2;  /* pass to 2nd auto limits step */
      mc_mn_Vars->Buffer_Block = mc_mn_Vars->Buffer_Counter;

      while (!mc_mn_While_End)
      { /* we generate mc_mn_Coord[] and Coord_mc_mn_index[] from Buffer (auto limits) or passing neutron */
        if (mc_mn_While_Buffer < mc_mn_Vars->Buffer_Block)
        {
          /* first while loops (mc_mn_While_Buffer) */
          mc_mn_Coord[0] = mc_mn_Vars->Mon2D_Buffer[mc_mn_While_Buffer*(mc_mn_Vars->Coord_Number+1)];

          /* auto limits case : scan Buffer within limits and store in Mon2D */
          for (mc_mn_i = 1; mc_mn_i <= mc_mn_Vars->Coord_Number; mc_mn_i++)
          {
            /* scanning variables in Buffer */
            mc_mn_XY = (mc_mn_Vars->Coord_Max[mc_mn_i]-mc_mn_Vars->Coord_Min[mc_mn_i]);
            mc_mn_Coord[mc_mn_i] = mc_mn_Vars->Mon2D_Buffer[mc_mn_i+mc_mn_While_Buffer*(mc_mn_Vars->Coord_Number+1)];
            if (mc_mn_XY > 0) mc_mn_Coord_Index[mc_mn_i] = floor((mc_mn_Coord[mc_mn_i]-mc_mn_Vars->Coord_Min[mc_mn_i])*mc_mn_Vars->Coord_Bin[mc_mn_i]/mc_mn_XY);
            else mc_mn_Coord_Index[mc_mn_i] = 0;
            if (mc_mn_Vars->Flag_With_Borders)
            {
              if (mc_mn_Coord_Index[mc_mn_i] < 0) mc_mn_Coord_Index[mc_mn_i] = 0;
              if (mc_mn_Coord_Index[mc_mn_i] >= mc_mn_Vars->Coord_Bin[mc_mn_i]) mc_mn_Coord_Index[mc_mn_i] = mc_mn_Vars->Coord_Bin[mc_mn_i] - 1;
            }
          } /* end for */
          mc_mn_While_Buffer++;
        } /* end if in Buffer */
        else /* (mc_mn_While_Buffer >= mc_mn_Vars->Buffer_Block) && (mc_mn_Vars->Flag_Auto_Limits == 2) */
        {
          mc_mn_Vars->Flag_Auto_Limits = 0;
          mc_mn_While_End = 1;
        }

        /* store n1d/2d section from Buffer */

        mc_mn_pp = mc_mn_Coord[0];
        /* 1D and n1D case : mc_mn_Vars->Flag_Multiple */
        if (mc_mn_Vars->Flag_Multiple)
        { /* Dim : mc_mn_Vars->Coord_Number*mc_mn_Vars->Coord_Bin[mc_mn_i] vectors (intensity is not included) */
          for (mc_mn_i= 0; mc_mn_i < mc_mn_Vars->Coord_Number; mc_mn_i++)
          {
            mc_mn_j = mc_mn_Coord_Index[mc_mn_i+1];
            if (mc_mn_j >= 0 && mc_mn_j < mc_mn_Vars->Coord_Bin[mc_mn_i+1])
            {
              mc_mn_Vars->Mon2D_N[mc_mn_i][mc_mn_j]++;
              mc_mn_Vars->Mon2D_p[mc_mn_i][mc_mn_j] += mc_mn_pp;
              mc_mn_Vars->Mon2D_p2[mc_mn_i][mc_mn_j] += mc_mn_pp*mc_mn_pp;
            }
          }
        }
        else /* 2D case : mc_mn_Vars->Coord_Number==2 and !mc_mn_Vars->Flag_Multiple and !mc_mn_Vars->Flag_List */
        if ((mc_mn_Vars->Coord_Number == 2) && !mc_mn_Vars->Flag_Multiple)
        { /* Dim : mc_mn_Vars->Coord_Bin[1]*mc_mn_Vars->Coord_Bin[2] matrix */
          mc_mn_i = mc_mn_Coord_Index[1];
          mc_mn_j = mc_mn_Coord_Index[2];
          if (mc_mn_i >= 0 && mc_mn_i < mc_mn_Vars->Coord_Bin[1] && mc_mn_j >= 0 && mc_mn_j < mc_mn_Vars->Coord_Bin[2])
          {
            mc_mn_Vars->Mon2D_N[mc_mn_i][mc_mn_j]++;
            mc_mn_Vars->Mon2D_p[mc_mn_i][mc_mn_j] += mc_mn_pp;
            mc_mn_Vars->Mon2D_p2[mc_mn_i][mc_mn_j] += mc_mn_pp*mc_mn_pp;
          }
        } /* end store 2D/1D */
      } /* end while */
    } /* end Force Get Limits */

    /* write output files (sent to file as p[i*n + j] vectors) */
    if (mc_mn_Vars->Coord_Number == 0)
    {
      double mc_mn_Nsum;
      double mc_mn_psum, mc_mn_p2sum;
      mc_mn_Nsum = mc_mn_Vars->Nsum;
      mc_mn_psum = mc_mn_Vars->psum;
      mc_mn_p2sum= mc_mn_Vars->p2sum;
      if (mc_mn_Vars->Flag_signal != mc_mn_DEFS->COORD_P && mc_mn_Nsum > 0)
      { mc_mn_psum /=mc_mn_Nsum; mc_mn_p2sum /= mc_mn_Nsum*mc_mn_Nsum; }
      /* DETECTOR_OUT_0D(mc_mn_Vars->Monitor_Label, mc_mn_Vars->Nsum, mc_mn_Vars->psum, mc_mn_Vars->p2sum); */
      // mcdetector_out_0D(mc_mn_Vars->Monitor_Label, mc_mn_Nsum, mc_mn_psum, mc_mn_p2sum, mc_mn_Vars->compcurname, mc_mn_Vars->compcurpos);
      mcdetector_out_0D(mc_mn_Vars->Monitor_Label, mc_mn_Nsum, mc_mn_psum, mc_mn_p2sum, mc_mn_Vars->compcurname);
    }
    else
    if (strlen(mc_mn_Vars->Mon_File) > 0)
    {
      mc_mn_fname = (char*)malloc(strlen(mc_mn_Vars->Mon_File)+10*mc_mn_Vars->Coord_Number);
      if (mc_mn_Vars->Flag_List && mc_mn_Vars->Mon2D_Buffer) /* List: DETECTOR_OUT_2D */
      {
        int  ascii_only_orig;
        char formatName[64];
        char *formatName_orig;

        if (mc_mn_Vars->Flag_List >= 2) mc_mn_Vars->Buffer_Size = mc_mn_Vars->Neutron_Counter;
        if (mc_mn_Vars->Buffer_Size >= mc_mn_Vars->Neutron_Counter)
          mc_mn_Vars->Buffer_Size = mc_mn_Vars->Neutron_Counter;
        strcpy(mc_mn_fname,mc_mn_Vars->Mon_File);
        if (strchr(mc_mn_Vars->Mon_File,'.') == NULL) strcat(mc_mn_fname, "_list");

        mc_mn_min1d = 1; mc_mn_max1d = mc_mn_Vars->Coord_Number+1;
        mc_mn_min2d = 0; mc_mn_max2d = mc_mn_Vars->Buffer_Size;
        mc_mn_bin1d = mc_mn_Vars->Coord_Number+1; mc_mn_bin2d = mc_mn_Vars->Buffer_Size;
        strcpy(mc_mn_Coord_X_Label,"");
        for (mc_mn_i= 0; mc_mn_i <= mc_mn_Vars->Coord_Number; mc_mn_i++)
        {
          if (mc_mn_min2d < mc_mn_Vars->Coord_Min[mc_mn_i]) mc_mn_min2d = mc_mn_Vars->Coord_Min[mc_mn_i];
          if (mc_mn_max2d < mc_mn_Vars->Coord_Max[mc_mn_i]) mc_mn_max2d = mc_mn_Vars->Coord_Max[mc_mn_i];
          strcat(mc_mn_Coord_X_Label, mc_mn_Vars->Coord_Var[mc_mn_i]);
          strcat(mc_mn_Coord_X_Label, " ");
          if (strchr(mc_mn_Vars->Mon_File,'.') == NULL)
          { strcat(mc_mn_fname, "."); strcat(mc_mn_fname, mc_mn_Vars->Coord_Var[mc_mn_i]); }
        }
        if (mc_mn_Vars->Flag_Verbose) printf("Monitor_nD: %s write monitor file %s List (%lix%li).\n", mc_mn_Vars->compcurname, mc_mn_fname,mc_mn_bin2d,mc_mn_bin1d);

        /* handle the type of list output */
        ascii_only_orig = mcascii_only;
        formatName_orig = mcformat.Name;  /* copy the pointer position */
        strcpy(formatName, mcformat.Name);
        if (mc_mn_Vars->Flag_List >= 1)
        { /* Flag_List mode:
               1=store 1 buffer
               2=list all, triggers 3 when 1st buffer reallocated
               3=re-used buffer (file already opened)
             Format modifiers for Flag_List
               1= normal monitor file (no modifier, export in one go)
               2= write data+header, and footer if buffer not full (mc_mn_Vars->Buffer_Counter < mc_mn_Vars->Buffer_Block)
               3= write data, and footer if buffer not full (final)
           */
          strcat(formatName, " list ");
          if (mc_mn_Vars->Flag_List == 3) strcat(formatName, " no header ");
          if (mc_mn_Vars->Flag_List >= 2 && mc_mn_Vars->Buffer_Counter >= mc_mn_Vars->Buffer_Block)
            strcat(formatName, " no footer ");

          if (mc_mn_Vars->Flag_Binary_List) mcascii_only = 1;
          if (mc_mn_Vars->Flag_Binary_List == 1)
            strcat(formatName, " binary float ");
          else if (mc_mn_Vars->Flag_Binary_List == 2)
            strcat(formatName, " binary double ");
        }
        if (mc_mn_min2d == mc_mn_max2d) mc_mn_max2d = mc_mn_min2d+1e-6;
        if (mc_mn_min1d == mc_mn_max1d) mc_mn_max1d = mc_mn_min1d+1e-6;
        strcpy(mc_mn_label, mc_mn_Vars->Monitor_Label);
        if (!mc_mn_Vars->Flag_Binary_List)
        { mc_mn_bin2d=-mc_mn_bin2d; }
        mcformat.Name = formatName;
        mcdetector_out_2D(
              mc_mn_label,
              "List of neutron events",
              mc_mn_Coord_X_Label,
              mc_mn_min2d, mc_mn_max2d,
              mc_mn_min1d, mc_mn_max1d,
              mc_mn_bin2d,
              mc_mn_bin1d,
            NULL,mc_mn_Vars->Mon2D_Buffer,NULL,
	      mc_mn_fname, mc_mn_Vars->compcurname);
	//, mc_mn_Vars->compcurpos);

        /* reset the original type of output */
        mcascii_only = ascii_only_orig;
        mcformat.Name= formatName_orig;
      }
      if (mc_mn_Vars->Flag_Multiple) /* n1D: DETECTOR_OUT_1D */
      {
        for (mc_mn_i= 0; mc_mn_i < mc_mn_Vars->Coord_Number; mc_mn_i++)
        {

          strcpy(mc_mn_fname,mc_mn_Vars->Mon_File);
          if (strchr(mc_mn_Vars->Mon_File,'.') == NULL)
          { strcat(mc_mn_fname, "."); strcat(mc_mn_fname, mc_mn_Vars->Coord_Var[mc_mn_i+1]); }
          sprintf(mc_mn_Coord_X_Label, "%s monitor", mc_mn_Vars->Coord_Label[mc_mn_i+1]);
          strcpy(mc_mn_label, mc_mn_Coord_X_Label);
          if (mc_mn_Vars->Coord_Bin[mc_mn_i+1] > 0) { /* 1D monitor */
            if (mc_mn_Vars->Flag_Verbose) printf("Monitor_nD: %s write monitor file %s 1D (%li).\n", mc_mn_Vars->compcurname, mc_mn_fname, mc_mn_Vars->Coord_Bin[mc_mn_i+1]);
            mc_mn_min1d = mc_mn_Vars->Coord_Min[mc_mn_i+1];
            mc_mn_max1d = mc_mn_Vars->Coord_Max[mc_mn_i+1];
            if (mc_mn_min1d == mc_mn_max1d) mc_mn_max1d = mc_mn_min1d+1e-6;
            mc_mn_p1m = (double *)malloc(mc_mn_Vars->Coord_Bin[mc_mn_i+1]*sizeof(double));
            mc_mn_p2m = (double *)malloc(mc_mn_Vars->Coord_Bin[mc_mn_i+1]*sizeof(double));
            if (mc_mn_p2m == NULL) /* use Raw Buffer line output */
            {
              if (mc_mn_Vars->Flag_Verbose) printf("Monitor_nD: %s cannot allocate memory for output. Using raw data.\n", mc_mn_Vars->compcurname);
              if (mc_mn_p1m != NULL) free(mc_mn_p1m);
              mcdetector_out_1D(
              mc_mn_label,
              mc_mn_Vars->Coord_Label[mc_mn_i+1],
              mc_mn_Vars->Coord_Label[0],
              mc_mn_Vars->Coord_Var[mc_mn_i+1],
              mc_mn_min1d, mc_mn_max1d,
              mc_mn_Vars->Coord_Bin[mc_mn_i+1],
              mc_mn_Vars->Mon2D_N[mc_mn_i],mc_mn_Vars->Mon2D_p[mc_mn_i],mc_mn_Vars->Mon2D_p2[mc_mn_i],
              mc_mn_fname, mc_mn_Vars->compcurname);//, mc_mn_Vars->compcurpos);
            } /* if (mc_mn_p2m == NULL) */
            else
            {
              if (mc_mn_Vars->Flag_log != 0)
              {
                mc_mn_XY = FLT_MAX;
                for (mc_mn_j=0; mc_mn_j < mc_mn_Vars->Coord_Bin[mc_mn_i+1]; mc_mn_j++) /* search min of signal */
                  if ((mc_mn_XY > mc_mn_Vars->Mon2D_p[mc_mn_i][mc_mn_j]) && (mc_mn_Vars->Mon2D_p[mc_mn_i][mc_mn_j] > 0)) mc_mn_XY = mc_mn_Vars->Mon2D_p[mc_mn_i][mc_mn_j];
                if (mc_mn_XY <= 0) mc_mn_XY = -log(FLT_MAX)/log(10); else mc_mn_XY = log(mc_mn_XY)/log(10)-1;
              } /* if */

              for (mc_mn_j=0; mc_mn_j < mc_mn_Vars->Coord_Bin[mc_mn_i+1]; mc_mn_j++)
              {
                mc_mn_p1m[mc_mn_j] = mc_mn_Vars->Mon2D_p[mc_mn_i][mc_mn_j];
                mc_mn_p2m[mc_mn_j] = mc_mn_Vars->Mon2D_p2[mc_mn_i][mc_mn_j];
                if (mc_mn_Vars->Flag_signal != mc_mn_DEFS->COORD_P && mc_mn_Vars->Mon2D_N[mc_mn_i][mc_mn_j] > 0)
                { /* normalize mean signal to the number of events */
                  mc_mn_p1m[mc_mn_j] /= mc_mn_Vars->Mon2D_N[mc_mn_i][mc_mn_j];
                  mc_mn_p2m[mc_mn_j] /= mc_mn_Vars->Mon2D_N[mc_mn_i][mc_mn_j]*mc_mn_Vars->Mon2D_N[mc_mn_i][mc_mn_j];
                }
                if (mc_mn_Vars->Flag_log != 0)
                {
                  if ((mc_mn_p1m[mc_mn_j] > 0) && (mc_mn_p2m[mc_mn_j] > 0))
                  {
                    mc_mn_p2m[mc_mn_j] /= mc_mn_p1m[mc_mn_j]*mc_mn_p1m[mc_mn_j];
                    mc_mn_p1m[mc_mn_j] = log(mc_mn_p1m[mc_mn_j])/log(10);
                  }
                  else
                  {
                    mc_mn_p1m[mc_mn_j] = mc_mn_XY;
                    mc_mn_p2m[mc_mn_j] = 0;
                  }
                }
              } /* for */
              mcdetector_out_1D(
                mc_mn_label,
                mc_mn_Vars->Coord_Label[mc_mn_i+1],
                mc_mn_Vars->Coord_Label[0],
                mc_mn_Vars->Coord_Var[mc_mn_i+1],
                mc_mn_min1d, mc_mn_max1d,
                mc_mn_Vars->Coord_Bin[mc_mn_i+1],
                mc_mn_Vars->Mon2D_N[mc_mn_i],mc_mn_p1m,mc_mn_p2m,
                mc_mn_fname, mc_mn_Vars->compcurname);//, mc_mn_Vars->compcurpos);

            } /* else */
            if (mc_mn_p1m != NULL) free(mc_mn_p1m); mc_mn_p1m=NULL;
            if (mc_mn_p2m != NULL) free(mc_mn_p2m); mc_mn_p2m=NULL;
          } else { /* 0d monitor */
            mcdetector_out_0D(mc_mn_label, mc_mn_Vars->Mon2D_p[mc_mn_i][0], mc_mn_Vars->Mon2D_p2[mc_mn_i][0], mc_mn_Vars->Mon2D_N[mc_mn_i][0], mc_mn_Vars->compcurname);//, mc_mn_Vars->compcurpos);
          }


        } /* for */
      } /* if 1D */
      else
      if (mc_mn_Vars->Coord_Number == 2)  /* 2D: DETECTOR_OUT_2D */
      {
        strcpy(mc_mn_fname,mc_mn_Vars->Mon_File);

        mc_mn_p0m = (double *)malloc(mc_mn_Vars->Coord_Bin[1]*mc_mn_Vars->Coord_Bin[2]*sizeof(double));
        mc_mn_p1m = (double *)malloc(mc_mn_Vars->Coord_Bin[1]*mc_mn_Vars->Coord_Bin[2]*sizeof(double));
        mc_mn_p2m = (double *)malloc(mc_mn_Vars->Coord_Bin[1]*mc_mn_Vars->Coord_Bin[2]*sizeof(double));
        if (mc_mn_p2m == NULL)
        {
          if (mc_mn_Vars->Flag_Verbose) printf("Monitor_nD: %s cannot allocate memory for 2D array (%li). Skipping.\n", mc_mn_Vars->compcurname, 3*mc_mn_Vars->Coord_Bin[1]*mc_mn_Vars->Coord_Bin[2]*sizeof(double));
          if (mc_mn_p0m != NULL) free(mc_mn_p0m);
          if (mc_mn_p1m != NULL) free(mc_mn_p1m);
        }
        else
        {
          if (mc_mn_Vars->Flag_log != 0)
          {
            mc_mn_XY = FLT_MAX;
            for (mc_mn_i= 0; mc_mn_i < mc_mn_Vars->Coord_Bin[1]; mc_mn_i++)
              for (mc_mn_j= 0; mc_mn_j < mc_mn_Vars->Coord_Bin[2]; mc_mn_j++) /* search min of signal */
                if ((mc_mn_XY > mc_mn_Vars->Mon2D_p[mc_mn_i][mc_mn_j]) && (mc_mn_Vars->Mon2D_p[mc_mn_i][mc_mn_j]>0)) mc_mn_XY = mc_mn_Vars->Mon2D_p[mc_mn_i][mc_mn_j];
            if (mc_mn_XY <= 0) mc_mn_XY = -log(FLT_MAX)/log(10); else mc_mn_XY = log(mc_mn_XY)/log(10)-1;
          }
          for (mc_mn_i= 0; mc_mn_i < mc_mn_Vars->Coord_Bin[1]; mc_mn_i++)
          {
            for (mc_mn_j= 0; mc_mn_j < mc_mn_Vars->Coord_Bin[2]; mc_mn_j++)
            {
              long mc_mn_index;
              mc_mn_index = mc_mn_j + mc_mn_i*mc_mn_Vars->Coord_Bin[2];
              mc_mn_p0m[mc_mn_index] = mc_mn_Vars->Mon2D_N[mc_mn_i][mc_mn_j];
              mc_mn_p1m[mc_mn_index] = mc_mn_Vars->Mon2D_p[mc_mn_i][mc_mn_j];
              mc_mn_p2m[mc_mn_index] = mc_mn_Vars->Mon2D_p2[mc_mn_i][mc_mn_j];
              if (mc_mn_Vars->Flag_signal != mc_mn_DEFS->COORD_P && mc_mn_p0m[mc_mn_index] > 0)
              {
                  mc_mn_p1m[mc_mn_index] /= mc_mn_p0m[mc_mn_index];
                  mc_mn_p2m[mc_mn_index] /= mc_mn_p0m[mc_mn_index]*mc_mn_p0m[mc_mn_index];
              }

              if (mc_mn_Vars->Flag_log != 0)
              {
                if ((mc_mn_p1m[mc_mn_index] > 0) && (mc_mn_p2m[mc_mn_index] > 0))
                {
                  mc_mn_p2m[mc_mn_index] /= (mc_mn_p1m[mc_mn_index]*mc_mn_p1m[mc_mn_index]);
                  mc_mn_p1m[mc_mn_index] = log(mc_mn_p1m[mc_mn_index])/log(10);

                }
                else
                {
                  mc_mn_p1m[mc_mn_index] = mc_mn_XY;
                  mc_mn_p2m[mc_mn_index] = 0;
                }
              }
            }
          }
          if (strchr(mc_mn_Vars->Mon_File,'.') == NULL)
          { strcat(mc_mn_fname, "."); strcat(mc_mn_fname, mc_mn_Vars->Coord_Var[1]);
              strcat(mc_mn_fname, "_"); strcat(mc_mn_fname, mc_mn_Vars->Coord_Var[2]); }
          if (mc_mn_Vars->Flag_Verbose) printf("Monitor_nD: %s write monitor file %s 2D (%lix%li).\n", mc_mn_Vars->compcurname, mc_mn_fname, mc_mn_Vars->Coord_Bin[1], mc_mn_Vars->Coord_Bin[2]);

          mc_mn_min1d = mc_mn_Vars->Coord_Min[1];
          mc_mn_max1d = mc_mn_Vars->Coord_Max[1];
          if (mc_mn_min1d == mc_mn_max1d) mc_mn_max1d = mc_mn_min1d+1e-6;
          mc_mn_min2d = mc_mn_Vars->Coord_Min[2];
          mc_mn_max2d = mc_mn_Vars->Coord_Max[2];
          if (mc_mn_min2d == mc_mn_max2d) mc_mn_max2d = mc_mn_min2d+1e-6;
          strcpy(mc_mn_label, mc_mn_Vars->Monitor_Label);
          if (mc_mn_Vars->Coord_Bin[1]*mc_mn_Vars->Coord_Bin[2] > 1
           && mc_mn_Vars->Flag_signal == mc_mn_DEFS->COORD_P)
            strcat(mc_mn_label, " per bin");

          mcdetector_out_2D(
            mc_mn_label,
            mc_mn_Vars->Coord_Label[1],
            mc_mn_Vars->Coord_Label[2],
            mc_mn_min1d, mc_mn_max1d,
            mc_mn_min2d, mc_mn_max2d,
            mc_mn_Vars->Coord_Bin[1],
            mc_mn_Vars->Coord_Bin[2],
            mc_mn_p0m,mc_mn_p1m,mc_mn_p2m,
            mc_mn_fname, mc_mn_Vars->compcurname);//, mc_mn_Vars->compcurpos);

          if (mc_mn_p0m != NULL) free(mc_mn_p0m);
          if (mc_mn_p1m != NULL) free(mc_mn_p1m);
          if (mc_mn_p2m != NULL) free(mc_mn_p2m);
        }
      }
      free(mc_mn_fname);
    }
  } /* end Monitor_nD_Save */

/* ========================================================================= */
/* ADD: E.Farhi, Aug 6th, 2001: Monitor_nD section */
/* this routine is used to free memory */
/* ========================================================================= */

void Monitor_nD_Finally(MonitornD_Defines_type *mc_mn_DEFS,
  MonitornD_Variables_type *mc_mn_Vars)
  {
    int mc_mn_i;

    /* Now Free memory Mon2D.. */
    if ((mc_mn_Vars->Flag_Auto_Limits || mc_mn_Vars->Flag_List) && mc_mn_Vars->Coord_Number)
    { /* Dim : (mc_mn_Vars->Coord_Number+1)*mc_mn_Vars->Buffer_Block matrix (for p, dp) */
      if (mc_mn_Vars->Mon2D_Buffer != NULL) free(mc_mn_Vars->Mon2D_Buffer);
    }

    /* 1D and n1D case : mc_mn_Vars->Flag_Multiple */
    if (mc_mn_Vars->Flag_Multiple && mc_mn_Vars->Coord_Number)
    { /* Dim : mc_mn_Vars->Coord_Number*mc_mn_Vars->Coord_Bin[mc_mn_i] vectors */
      for (mc_mn_i= 0; mc_mn_i < mc_mn_Vars->Coord_Number; mc_mn_i++)
      {
        free(mc_mn_Vars->Mon2D_N[mc_mn_i]);
        free(mc_mn_Vars->Mon2D_p[mc_mn_i]);
        free(mc_mn_Vars->Mon2D_p2[mc_mn_i]);
      }
      free(mc_mn_Vars->Mon2D_N);
      free(mc_mn_Vars->Mon2D_p);
      free(mc_mn_Vars->Mon2D_p2);
    }


    /* 2D case : mc_mn_Vars->Coord_Number==2 and !mc_mn_Vars->Flag_Multiple and !mc_mn_Vars->Flag_List */
    if ((mc_mn_Vars->Coord_Number == 2) && !mc_mn_Vars->Flag_Multiple)
    { /* Dim : mc_mn_Vars->Coord_Bin[1]*mc_mn_Vars->Coord_Bin[2] matrix */
      for (mc_mn_i= 0; mc_mn_i < mc_mn_Vars->Coord_Bin[1]; mc_mn_i++)
      {
        free(mc_mn_Vars->Mon2D_N[mc_mn_i]);
        free(mc_mn_Vars->Mon2D_p[mc_mn_i]);
        free(mc_mn_Vars->Mon2D_p2[mc_mn_i]);
      }
      free(mc_mn_Vars->Mon2D_N);
      free(mc_mn_Vars->Mon2D_p);
      free(mc_mn_Vars->Mon2D_p2);
    }
  } /* end Monitor_nD_Finally */

/* ========================================================================= */
/* ADD: E.Farhi, Aug 6th, 2001: Monitor_nD section */
/* this routine is used to display component */
/* ========================================================================= */

void Monitor_nD_McDisplay(MonitornD_Defines_type *mc_mn_DEFS,
  MonitornD_Variables_type *mc_mn_Vars)
  {
    double mc_mn_radius, mc_mn_h;
    double mc_mn_xmin;
    double mc_mn_xmax;
    double mc_mn_ymin;
    double mc_mn_ymax;
    double mc_mn_zmin;
    double mc_mn_zmax;
    int    mc_mn_i;
    double mc_mn_hdiv_min=-180, mc_mn_hdiv_max=180, mc_mn_vdiv_min=-180, mc_mn_vdiv_max=180;
    char   mc_mn_restricted = 0;

    mc_mn_radius = mc_mn_Vars->Sphere_Radius;
    mc_mn_h = mc_mn_Vars->Cylinder_Height;
    mc_mn_xmin = mc_mn_Vars->mxmin;
    mc_mn_xmax = mc_mn_Vars->mxmax;
    mc_mn_ymin = mc_mn_Vars->mymin;
    mc_mn_ymax = mc_mn_Vars->mymax;
    mc_mn_zmin = mc_mn_Vars->mzmin;
    mc_mn_zmax = mc_mn_Vars->mzmax;

    /* determine if there are angular limits set at start (no auto) in coord_types
     * cylinder/banana: look for hdiv
     * sphere: look for angle, radius (->atan2(val,mc_mn_radius)), hdiv, vdiv
     * this activates a 'restricted' flag, to draw a region as blades on cylinder/sphere
     */
    for (mc_mn_i= 0; mc_mn_i <= mc_mn_Vars->Coord_Number; mc_mn_i++)
    {
      int mc_mn_Set_Vars_Coord_Type;
      mc_mn_Set_Vars_Coord_Type = (mc_mn_Vars->Coord_Type[mc_mn_i] & 31);
      if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_HDIV || mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_THETA)
      { mc_mn_hdiv_min = mc_mn_Vars->Coord_Min[mc_mn_i]; mc_mn_hdiv_max = mc_mn_Vars->Coord_Max[mc_mn_i]; mc_mn_restricted = 1; }
      else if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_VDIV || mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_PHI)
      { mc_mn_vdiv_min = mc_mn_Vars->Coord_Min[mc_mn_i]; mc_mn_vdiv_max = mc_mn_Vars->Coord_Max[mc_mn_i];mc_mn_restricted = 1;  }
      else if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_ANGLE)
      { mc_mn_hdiv_min = mc_mn_vdiv_min = mc_mn_Vars->Coord_Min[mc_mn_i];
        mc_mn_hdiv_max = mc_mn_vdiv_max = mc_mn_Vars->Coord_Max[mc_mn_i];
        mc_mn_restricted = 1; }
      else if (mc_mn_Set_Vars_Coord_Type == mc_mn_DEFS->COORD_RADIUS)
      { double angle;
        angle = RAD2DEG*atan2(mc_mn_Vars->Coord_Max[mc_mn_i], mc_mn_radius);
        mc_mn_hdiv_min = mc_mn_vdiv_min = angle;
        mc_mn_hdiv_max = mc_mn_vdiv_max = angle;
        mc_mn_restricted = 1; }
    }

    if ((!mc_mn_restricted && (abs(mc_mn_Vars->Flag_Shape) == mc_mn_DEFS->SHAPE_SPHERE))
    || abs(mc_mn_Vars->Flag_Shape) == mc_mn_DEFS->SHAPE_PREVIOUS)
    {
      mcdis_magnify("");
      mcdis_circle("xy",0,0,0,mc_mn_radius);
      mcdis_circle("xz",0,0,0,mc_mn_radius);
      mcdis_circle("yz",0,0,0,mc_mn_radius);
    }
    else if (mc_mn_restricted && ((abs(mc_mn_Vars->Flag_Shape) == mc_mn_DEFS->SHAPE_CYLIND) || (abs(mc_mn_Vars->Flag_Shape) == mc_mn_DEFS->SHAPE_BANANA) || (abs(mc_mn_Vars->Flag_Shape) == mc_mn_DEFS->SHAPE_SPHERE)))
    {
      int NH=24, NV=24;
      int ih, iv;
      double width, height;
      int issphere;
      issphere = (abs(mc_mn_Vars->Flag_Shape) == mc_mn_DEFS->SHAPE_SPHERE);
      width = (mc_mn_hdiv_max-mc_mn_hdiv_min)/NH;
      height= (mc_mn_vdiv_max-mc_mn_vdiv_min)/NV;
      mcdis_magnify("xyz");
      for(ih = 0; ih < NH; ih++)
        for(iv = 0; iv < NV; iv++)
        {
          double theta0, phi0, theta1, phi1;
          double x0,y0,z0,x1,y1,z1,x2,y2,z2,x3,y3,z3;
          double ymin, ymax;
          phi0 = (mc_mn_hdiv_min+ width*ih)*DEG2RAD; /* in xz plane */
          phi1 = (mc_mn_hdiv_min+ width*(ih+1))*DEG2RAD;
          if (issphere)
          {
            theta0= (90-mc_mn_vdiv_min+height*iv)*DEG2RAD;
            theta1= (90-mc_mn_vdiv_min+height*(iv+1))*DEG2RAD;
          } else
          {
            theta0= theta1 = PI/2;
            ymin  = mc_mn_ymin+(mc_mn_ymax-mc_mn_ymin)*(iv/NV);
            ymax  = mc_mn_ymin+(mc_mn_ymax-mc_mn_ymin)*((iv+1)/NV);
          }
          z0 = mc_mn_radius*sin(theta0)*cos(phi0);
          x0 = mc_mn_radius*sin(theta0)*sin(phi0);
          if (issphere) y0 = mc_mn_radius*cos(theta0); else y0 = ymin;
          z1 = mc_mn_radius*sin(theta1)*cos(phi0);
          x1 = mc_mn_radius*sin(theta1)*sin(phi0);
          if (issphere) y1 = mc_mn_radius*cos(theta1); else y1 = ymax;
          z2 = mc_mn_radius*sin(theta1)*cos(phi1);
          x2 = mc_mn_radius*sin(theta1)*sin(phi1);
          y2 = y1;
          z3 = mc_mn_radius*sin(theta0)*cos(phi1);
          x3 = mc_mn_radius*sin(theta0)*sin(phi1);
          y3 = y0;
          mcdis_multiline(5,
            x0,y0,z0,
            x1,y1,z1,
            x2,y2,z2,
            x3,y3,z3,
            x0,y0,z0);
        }
    }
    else
    if (abs(mc_mn_Vars->Flag_Shape) == mc_mn_DEFS->SHAPE_DISK)
    {
      mcdis_magnify("");
      mcdis_circle("xy",0,0,0,mc_mn_radius);
    }
    else
    if (abs(mc_mn_Vars->Flag_Shape) == mc_mn_DEFS->SHAPE_SQUARE)
    {
      mcdis_magnify("xy");
      mcdis_multiline(5, (double)mc_mn_xmin, (double)mc_mn_ymin, 0.0,
             (double)mc_mn_xmax, (double)mc_mn_ymin, 0.0,
             (double)mc_mn_xmax, (double)mc_mn_ymax, 0.0,
             (double)mc_mn_xmin, (double)mc_mn_ymax, 0.0,
             (double)mc_mn_xmin, (double)mc_mn_ymin, 0.0);
    }
    else
    if (!mc_mn_restricted && ((abs(mc_mn_Vars->Flag_Shape) == mc_mn_DEFS->SHAPE_CYLIND) || (abs(mc_mn_Vars->Flag_Shape) == mc_mn_DEFS->SHAPE_BANANA)))
    {
      mcdis_magnify("xyz");
      mcdis_circle("xz", 0,  mc_mn_h/2.0, 0, mc_mn_radius);
      mcdis_circle("xz", 0, -mc_mn_h/2.0, 0, mc_mn_radius);
      mcdis_line(-mc_mn_radius, -mc_mn_h/2.0, 0, -mc_mn_radius, +mc_mn_h/2.0, 0);
      mcdis_line(+mc_mn_radius, -mc_mn_h/2.0, 0, +mc_mn_radius, +mc_mn_h/2.0, 0);
      mcdis_line(0, -mc_mn_h/2.0, -mc_mn_radius, 0, +mc_mn_h/2.0, -mc_mn_radius);
      mcdis_line(0, -mc_mn_h/2.0, +mc_mn_radius, 0, +mc_mn_h/2.0, +mc_mn_radius);
    }
    else
    if (abs(mc_mn_Vars->Flag_Shape) == mc_mn_DEFS->SHAPE_BOX)
    {
      mcdis_magnify("xyz");
      mcdis_multiline(5, mc_mn_xmin, mc_mn_ymin, mc_mn_zmin,
                   mc_mn_xmax, mc_mn_ymin, mc_mn_zmin,
                   mc_mn_xmax, mc_mn_ymax, mc_mn_zmin,
                   mc_mn_xmin, mc_mn_ymax, mc_mn_zmin,
                   mc_mn_xmin, mc_mn_ymin, mc_mn_zmin);
      mcdis_multiline(5, mc_mn_xmin, mc_mn_ymin, mc_mn_zmax,
                   mc_mn_xmax, mc_mn_ymin, mc_mn_zmax,
                   mc_mn_xmax, mc_mn_ymax, mc_mn_zmax,
                   mc_mn_xmin, mc_mn_ymax, mc_mn_zmax,
                   mc_mn_xmin, mc_mn_ymin, mc_mn_zmax);
      mcdis_line(mc_mn_xmin, mc_mn_ymin, mc_mn_zmin, mc_mn_xmin, mc_mn_ymin, mc_mn_zmax);
      mcdis_line(mc_mn_xmax, mc_mn_ymin, mc_mn_zmin, mc_mn_xmax, mc_mn_ymin, mc_mn_zmax);
      mcdis_line(mc_mn_xmin, mc_mn_ymax, mc_mn_zmin, mc_mn_xmin, mc_mn_ymax, mc_mn_zmax);
      mcdis_line(mc_mn_xmax, mc_mn_ymax, mc_mn_zmin, mc_mn_xmax, mc_mn_ymax, mc_mn_zmax);
    }
  } /* end Monitor_nD_McDisplay */

/* end of monitor_nd-lib.c */
