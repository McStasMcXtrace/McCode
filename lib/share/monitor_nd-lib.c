/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/monitor_nd-lib.c
*
* %Identification
* Written by: EF
* Date: Aug 28, 2002
* Origin: ILL
* Release: McStas 1.6
* Version: 1.1
*
* This file is to be imported by the monitor_nd related components
* It handles some shared functions. Embedded within instrument in runtime mode.
* Variable names have prefix 'mc_mn_' for 'McStas Monitor' to avoid conflicts
*
* Usage: within SHARE
* %include "monitor_nd-lib"
*
* $Id: monitor_nd-lib.c,v 1.3 2003-01-21 08:38:42 pkwi Exp $
*
*	$Log: not supported by cvs2svn $
* Revision 1.1 2002/08/28 11:39:00 ef
*	Initial revision extracted from lib/monitors/Monitor_nD.comp
*******************************************************************************/
  
#ifndef MONITOR_ND_LIB_H
#error McStas : please import this library with %include "monitor_nd-lib"
#endif  
  
/* ========================================================================= */
/* ADD: E.Farhi, Aug 6th, 2001: Monitor_nD section */
/* this routine is used to parse options */ 
/* ========================================================================= */
  
void Monitor_nD_Init(mc_mn_DEFS, mc_mn_Vars, mc_mn_xwidth, mc_mn_yheight, mc_mn_zthick, mc_mn_xmin, mc_mn_xmax, mc_mn_ymin, mc_mn_ymax, mc_mn_zmin, mc_mn_zmax)
  MonitornD_Defines_type *mc_mn_DEFS;
  MonitornD_Variables_type *mc_mn_Vars;
  MCNUM mc_mn_xwidth, mc_mn_yheight, mc_mn_zthick;
  MCNUM mc_mn_xmin, mc_mn_xmax, mc_mn_ymin, mc_mn_ymax, mc_mn_zmin, mc_mn_zmax;
  {
    long mc_mn_carg = 1;
    char *mc_mn_option_copy, *mc_mn_token;
    char mc_mn_Flag_New_mc_mn_token = 1;
    char mc_mn_Flag_End       = 1;
    char mc_mn_Flag_All       = 0;
    char mc_mn_Flag_No        = 0;
    char mc_mn_Flag_abs       = 0;
    char mc_mn_Set_mc_mn_Vars_Coord_Type;
    char mc_mn_Set_mc_mn_Vars_Coord_Label[30];
    char mc_mn_Set_mc_mn_Vars_Coord_Var[30];
    char mc_mn_Short_Label[MONnD_COORD_NMAX][30];
    char mc_mn_Set_Coord_Mode;
    long mc_mn_i=0, mc_mn_j=0;
    double mc_mn_lmin, mc_mn_lmax, mc_mn_XY;
    long mc_mn_t;

    
    mc_mn_t = (long)time(NULL);

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

    strcpy(mc_mn_DEFS->TOKEN_DEL, " =,;[](){}:");  /* mc_mn_token separators */

    mc_mn_DEFS->SHAPE_SQUARE =0;    /* shape of the monitor */
    mc_mn_DEFS->SHAPE_DISK   =1;
    mc_mn_DEFS->SHAPE_SPHERE =2;
    mc_mn_DEFS->SHAPE_CYLIND =3;
    mc_mn_DEFS->SHAPE_BOX    =4;

    mc_mn_Vars->Sphere_Radius     = 0;
    mc_mn_Vars->Cylinder_Height   = 0;
    mc_mn_Vars->Flag_With_Borders = 0;   /* 2 means xy borders too */
    mc_mn_Vars->Flag_List         = 0;   /* 1 store 1 buffer, 2 is list all */
    mc_mn_Vars->Flag_Multiple     = 0;   /* 1 when n1D, 0 for 2D */
    mc_mn_Vars->Flag_Verbose      = 0;
    mc_mn_Vars->Flag_Shape        = mc_mn_DEFS->SHAPE_SQUARE;
    mc_mn_Vars->Flag_Auto_Limits  = 0;   /* get limits from first Buffer */
    mc_mn_Vars->Flag_Absorb       = 0;   /* monitor is also a slit */
    mc_mn_Vars->Flag_per_cm2      = 0;   /* flux is per cm2 */
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
    
    mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_NONE;
    mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_VAR;
    
    /* handle size parameters */
    /* normal use is with xwidth, yheight, zthick */
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
      { mc_mn_Vars->mzmin = -fabs(mc_mn_zthick)/2; mc_mn_Vars->mzmax = fabs(mc_mn_zthick)/2; }
    else
      { if (mc_mn_zmin < mc_mn_zmax) {mc_mn_Vars->mzmin = mc_mn_zmin; mc_mn_Vars->mzmax = mc_mn_zmax; }
        else {mc_mn_Vars->mzmin = mc_mn_zmax; mc_mn_Vars->mzmax = mc_mn_zmin; }
      }
    
    if (fabs(mc_mn_Vars->mzmax-mc_mn_Vars->mzmin) == 0)
      mc_mn_Vars->Flag_Shape        = mc_mn_DEFS->SHAPE_SQUARE;
    else
      mc_mn_Vars->Flag_Shape        = mc_mn_DEFS->SHAPE_BOX;
     
    /* parse option string */ 
    
    mc_mn_option_copy = (char*)malloc(strlen(mc_mn_Vars->option));
    if (mc_mn_option_copy == NULL)
    {
      fprintf(stderr,"Monitor_nD: %s cannot allocate mc_mn_option_copy (%i). Fatal.\n", mc_mn_Vars->compcurname, strlen(mc_mn_Vars->option));
      exit(-1);
    }

    if (strlen(mc_mn_Vars->option))
    {
      mc_mn_Flag_End = 0;
      strcpy(mc_mn_option_copy, mc_mn_Vars->option);
    }
    
    if (strstr(mc_mn_Vars->option, "cm2") || strstr(mc_mn_Vars->option, "cm^2")) mc_mn_Vars->Flag_per_cm2 = 1;
    
    if (strstr(mc_mn_Vars->option, "binary"))
      mc_mn_Vars->Flag_Binary_List  = 1;
  
    if (mc_mn_Vars->Flag_per_cm2) strcpy(mc_mn_Vars->Coord_Label[0],"Intensity [n/cm^2/s]");
    else strcpy(mc_mn_Vars->Coord_Label[0],"Intensity [n/s]");
    strcpy(mc_mn_Vars->Coord_Var[0],"p");
    mc_mn_Vars->Coord_Type[0] = mc_mn_DEFS->COORD_P;
    mc_mn_Vars->Coord_Bin[0] = 1;
    mc_mn_Vars->Coord_Min[0] = 0;
    mc_mn_Vars->Coord_Max[0] = FLT_MAX;
    
    /* default file name is comp name+dateID */
    sprintf(mc_mn_Vars->Mon_File, "%s_%li", mc_mn_Vars->compcurname, mc_mn_t); 
  
    mc_mn_carg = 1;
    while((mc_mn_Flag_End == 0) && (mc_mn_carg < 128))
    {
      if (mc_mn_Flag_New_mc_mn_token) /* to get the previous mc_mn_token sometimes */
      {
        if (mc_mn_carg == 1) mc_mn_token=(char *)strtok(mc_mn_option_copy,mc_mn_DEFS->TOKEN_DEL);
        else mc_mn_token=(char *)strtok(NULL,mc_mn_DEFS->TOKEN_DEL);
        if (mc_mn_token == NULL) mc_mn_Flag_End=1;
      }
      mc_mn_Flag_New_mc_mn_token = 1;
      if ((mc_mn_token != NULL) && (strlen(mc_mn_token) != 0))
      {
      /* first handle option values from preceeding keyword mc_mn_token detected */
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
          if (!mc_mn_Flag_No) strcpy(mc_mn_Vars->Mon_File,mc_mn_token); 
          else { strcpy(mc_mn_Vars->Mon_File,""); mc_mn_Vars->Coord_Number = 0; mc_mn_Flag_End = 1;}
          mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_VAR;
        }
        if (mc_mn_Set_Coord_Mode == mc_mn_DEFS->COORD_EVNT)
        { 
          if (!strcmp(mc_mn_token, "all") || mc_mn_Flag_All) mc_mn_Vars->Flag_List = 2;
          else { mc_mn_i = atoi(mc_mn_token); if (mc_mn_i) mc_mn_Vars->Buffer_Block = mc_mn_i; 
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
        if (!strcmp(mc_mn_token, "borders")) 
        { if (mc_mn_Flag_No) { mc_mn_Vars->Flag_With_Borders = 0; mc_mn_Flag_No = 0; }
          else mc_mn_Vars->Flag_With_Borders = 1; }
        if (!strcmp(mc_mn_token, "verbose")) 
        { if (mc_mn_Flag_No) { mc_mn_Vars->Flag_Verbose = 0; mc_mn_Flag_No = 0; }
          else mc_mn_Vars->Flag_Verbose      = 1; }
        if (!strcmp(mc_mn_token, "log")) 
        { if (mc_mn_Flag_No) { mc_mn_Vars->Flag_log = 0; mc_mn_Flag_No = 0; }
          else mc_mn_Vars->Flag_log      = 1; }
        if (!strcmp(mc_mn_token, "abs")) 
        { mc_mn_Flag_abs      = 1; }
        if (!strcmp(mc_mn_token, "multiple")) 
        { if (mc_mn_Flag_No) { mc_mn_Vars->Flag_Multiple = 0; mc_mn_Flag_No = 0; }
          else mc_mn_Vars->Flag_Multiple = 1; }
        if (!strcmp(mc_mn_token, "list")) 
        { if (mc_mn_Flag_No) { mc_mn_Vars->Flag_List = 0; mc_mn_Flag_No = 0; }
          else mc_mn_Vars->Flag_List = 1; 
          mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_EVNT; }

        if (!strcmp(mc_mn_token, "limits") || !strcmp(mc_mn_token, "min")) mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_MIN;
        if (!strcmp(mc_mn_token, "slit") || !strcmp(mc_mn_token, "absorb")) 
        { if (mc_mn_Flag_No) { mc_mn_Vars->Flag_Absorb = 0; mc_mn_Flag_No = 0; }
          else mc_mn_Vars->Flag_Absorb = 1; }
        if (!strcmp(mc_mn_token, "max")) mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_MAX;
        if (!strcmp(mc_mn_token, "bins")) mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_DIM;
        if (!strcmp(mc_mn_token, "file")) 
        { mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_FIL;
          if (mc_mn_Flag_No) { strcpy(mc_mn_Vars->Mon_File,""); mc_mn_Vars->Coord_Number = 0; mc_mn_Flag_End = 1;}}
        if (!strcmp(mc_mn_token, "unactivate")) { mc_mn_Flag_End = 1; mc_mn_Vars->Coord_Number = 0; }
        if (!strcmp(mc_mn_token, "all")) 
        { if (mc_mn_Flag_No) { mc_mn_Flag_All = 0; mc_mn_Flag_No = 0; }
          else mc_mn_Flag_All = 1; }
        if (!strcmp(mc_mn_token, "sphere")) 
        { if (mc_mn_Flag_No) { mc_mn_Vars->Flag_Shape = mc_mn_DEFS->SHAPE_SQUARE; mc_mn_Flag_No = 0; }
          else mc_mn_Vars->Flag_Shape = mc_mn_DEFS->SHAPE_SPHERE; }
        if (!strcmp(mc_mn_token, "cylinder")) 
        { if (mc_mn_Flag_No) { mc_mn_Vars->Flag_Shape = mc_mn_DEFS->SHAPE_SQUARE; mc_mn_Flag_No = 0; }
          else mc_mn_Vars->Flag_Shape = mc_mn_DEFS->SHAPE_CYLIND; }
        if (!strcmp(mc_mn_token, "square")) mc_mn_Vars->Flag_Shape = mc_mn_DEFS->SHAPE_SQUARE; 
        if (!strcmp(mc_mn_token, "disk")) mc_mn_Vars->Flag_Shape = mc_mn_DEFS->SHAPE_DISK; 
        if (!strcmp(mc_mn_token, "box")) mc_mn_Vars->Flag_Shape = mc_mn_DEFS->SHAPE_BOX; 
        if (!strcmp(mc_mn_token, "parallel")) mc_mn_Vars->Flag_parallel = 1; 
        if (!strcmp(mc_mn_token, "capture")) mc_mn_Vars->Flag_capture = 1; 
        if (!strcmp(mc_mn_token, "auto")) 
        { if (mc_mn_Flag_No) { mc_mn_Vars->Flag_Auto_Limits = 0; mc_mn_Flag_No = 0; }
          else mc_mn_Vars->Flag_Auto_Limits = 1; }
        if (!strcmp(mc_mn_token, "premonitor"))
        { if (mc_mn_Flag_No) { mc_mn_Vars->Flag_UsePreMonitor = 0; mc_mn_Flag_No = 0; }
          else mc_mn_Vars->Flag_UsePreMonitor = 1; } 
        if (!strcmp(mc_mn_token, "3He_pressure"))
        { if (!mc_mn_Flag_No)  mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_3HE; 
          mc_mn_Vars->He3_pressure = 3; }
        if (!strcmp(mc_mn_token, "intermediate"))
        { if (!mc_mn_Flag_No)  mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_INTERM; 
          mc_mn_Vars->Intermediate = 5; }
        if (!strcmp(mc_mn_token, "no") || !strcmp(mc_mn_token, "not")) mc_mn_Flag_No = 1;
        if (!strcmp(mc_mn_token, "signal")) mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_SIGNAL;
  
        /* now look for variable names to monitor */
        mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_NONE; mc_mn_lmin = 0; mc_mn_lmax = 0;

        if (!strcmp(mc_mn_token, "x")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_X; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"x [m]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"x"); mc_mn_lmin = mc_mn_Vars->mxmin; mc_mn_lmax = mc_mn_Vars->mxmax; }
        if (!strcmp(mc_mn_token, "y")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_Y; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"y [m]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"y"); mc_mn_lmin = mc_mn_Vars->mymin; mc_mn_lmax = mc_mn_Vars->mymax; }
        if (!strcmp(mc_mn_token, "z")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_Z; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"z [m]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"z"); mc_mn_lmin = mc_mn_Vars->mzmin; mc_mn_lmax = mc_mn_Vars->mzmax; }
        if (!strcmp(mc_mn_token, "k") || !strcmp(mc_mn_token, "wavevector")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_K; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"|k| [Angs-1]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"k"); mc_mn_lmin = 0; mc_mn_lmax = 10; }
        if (!strcmp(mc_mn_token, "v"))
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_V; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"Velocity [m/s]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"v"); mc_mn_lmin = 0; mc_mn_lmax = 10000; }
        if (!strcmp(mc_mn_token, "t") || !strcmp(mc_mn_token, "time")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_T; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"TOF [s]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"t"); mc_mn_lmin = 0; mc_mn_lmax = .1; }
        if ((!strcmp(mc_mn_token, "p") || !strcmp(mc_mn_token, "intensity") || !strcmp(mc_mn_token, "flux")))
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_P;  
            if (mc_mn_Vars->Flag_per_cm2) strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"Intensity [n/cm^2/s]");
            else strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"Intensity [n/s]"); 
            strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"I"); 
            mc_mn_lmin = 0; mc_mn_lmax = FLT_MAX; }

        if (!strcmp(mc_mn_token, "vx")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_VX; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"vx [m/s]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"vx"); mc_mn_lmin = -1000; mc_mn_lmax = 1000; }
        if (!strcmp(mc_mn_token, "vy")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_VY; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"vy [m/s]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"vy"); mc_mn_lmin = -1000; mc_mn_lmax = 1000; }
        if (!strcmp(mc_mn_token, "vz")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_VZ; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"vz [m/s]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"vz"); mc_mn_lmin = -10000; mc_mn_lmax = 10000; }
        if (!strcmp(mc_mn_token, "kx")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_KX; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"kx [Angs-1]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"kx"); mc_mn_lmin = -1; mc_mn_lmax = 1; }
        if (!strcmp(mc_mn_token, "ky")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_KY; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"ky [Angs-1]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"ky"); mc_mn_lmin = -1; mc_mn_lmax = 1; }
        if (!strcmp(mc_mn_token, "kz")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_KZ; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"kz [Angs-1]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"kz"); mc_mn_lmin = -10; mc_mn_lmax = 10; }
        if (!strcmp(mc_mn_token, "sx")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_SX; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"sx [1]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"sx"); mc_mn_lmin = -1; mc_mn_lmax = 1; }
        if (!strcmp(mc_mn_token, "sy")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_SY; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"sy [1]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"sy"); mc_mn_lmin = -1; mc_mn_lmax = 1; }
        if (!strcmp(mc_mn_token, "sz")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_SZ; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"sz [1]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"sz"); mc_mn_lmin = -1; mc_mn_lmax = 1; }

        if (!strcmp(mc_mn_token, "energy") || !strcmp(mc_mn_token, "omega")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_ENERGY; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"Energy [meV]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"E"); mc_mn_lmin = 0; mc_mn_lmax = 100; }
        if (!strcmp(mc_mn_token, "lambda") || !strcmp(mc_mn_token, "wavelength")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_LAMBDA; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"Wavelength [Angs]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"L"); mc_mn_lmin = 0; mc_mn_lmax = 100; }
        if (!strcmp(mc_mn_token, "radius")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_RADIUS; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"Radius [m]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"R"); mc_mn_lmin = 0; mc_mn_lmax = mc_mn_xmax; }
        if (!strcmp(mc_mn_token, "angle")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_ANGLE; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"Angle [deg]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"A"); mc_mn_lmin = -5; mc_mn_lmax = 5; }
        if (!strcmp(mc_mn_token, "hdiv")|| !strcmp(mc_mn_token, "divergence") || !strcmp(mc_mn_token, "xdiv") || !strcmp(mc_mn_token, "dx")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_HDIV; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"Hor. Divergence [deg]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"HD"); mc_mn_lmin = -5; mc_mn_lmax = 5; }
        if (!strcmp(mc_mn_token, "vdiv") || !strcmp(mc_mn_token, "ydiv") || !strcmp(mc_mn_token, "dy")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_VDIV; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"Vert. Divergence [deg]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"VD"); mc_mn_lmin = -5; mc_mn_lmax = 5; }
        if (!strcmp(mc_mn_token, "theta") || !strcmp(mc_mn_token, "longitude")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_THETA; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"Longitude [deg]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"th"); mc_mn_lmin = -180; mc_mn_lmax = 180; }
        if (!strcmp(mc_mn_token, "phi") || !strcmp(mc_mn_token, "lattitude")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_PHI; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"Lattitude [deg]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"ph"); mc_mn_lmin = -180; mc_mn_lmax = 180; }
        if (!strcmp(mc_mn_token, "ncounts")) 
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_NCOUNT; strcpy(mc_mn_Set_mc_mn_Vars_Coord_Label,"Neutrons [1]"); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"N"); mc_mn_lmin = 0; mc_mn_lmax = 1e10; }
        if (!strcmp(mc_mn_token, "user") || !strcmp(mc_mn_token, "user1"))
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_USER1; strncpy(mc_mn_Set_mc_mn_Vars_Coord_Label,mc_mn_Vars->UserName1,32); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"U1"); mc_mn_lmin = -1e10; mc_mn_lmax = 1e10; }
        if (!strcmp(mc_mn_token, "user2"))
          { mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_USER2; strncpy(mc_mn_Set_mc_mn_Vars_Coord_Label,mc_mn_Vars->UserName2,32); strcpy(mc_mn_Set_mc_mn_Vars_Coord_Var,"U2"); mc_mn_lmin = -1e10; mc_mn_lmax = 1e10; }
        if ((mc_mn_Set_mc_mn_Vars_Coord_Type != mc_mn_DEFS->COORD_NONE) && mc_mn_Vars->Flag_log) { mc_mn_Set_mc_mn_Vars_Coord_Type |= mc_mn_DEFS->COORD_LOG; mc_mn_Vars->Flag_log = 0; }
        if ((mc_mn_Set_mc_mn_Vars_Coord_Type != mc_mn_DEFS->COORD_NONE) && mc_mn_Flag_abs) { mc_mn_Set_mc_mn_Vars_Coord_Type |= mc_mn_DEFS->COORD_ABS; mc_mn_Flag_abs = 0; }

        /* now stores variable keywords detected, if any */ 
        if (mc_mn_Set_mc_mn_Vars_Coord_Type != mc_mn_DEFS->COORD_NONE)
        {
          int mc_mn_Coord_Number = mc_mn_Vars->Coord_Number;
          if (mc_mn_Set_Coord_Mode == mc_mn_DEFS->COORD_SIGNAL)
          {
            mc_mn_Coord_Number = 0;
            mc_mn_Vars->Flag_signal = mc_mn_Set_mc_mn_Vars_Coord_Type;
          } 
          else 
          {
            if (mc_mn_Coord_Number < MONnD_COORD_NMAX) 
            { mc_mn_Coord_Number++;
              mc_mn_Vars->Coord_Number = mc_mn_Coord_Number; }
            else if (mc_mn_Vars->Flag_Verbose) printf("Monitor_nD: %s reached max number of variables (%i).\n", mc_mn_Vars->compcurname, MONnD_COORD_NMAX);
          }
          mc_mn_Vars->Coord_Type[mc_mn_Coord_Number] = mc_mn_Set_mc_mn_Vars_Coord_Type;
          strcpy(mc_mn_Vars->Coord_Label[mc_mn_Coord_Number], mc_mn_Set_mc_mn_Vars_Coord_Label); 
          strcpy(mc_mn_Vars->Coord_Var[mc_mn_Coord_Number], mc_mn_Set_mc_mn_Vars_Coord_Var);
          if (mc_mn_lmin > mc_mn_lmax) { mc_mn_XY = mc_mn_lmin; mc_mn_lmin=mc_mn_lmax; mc_mn_lmax = mc_mn_XY; }
          mc_mn_Vars->Coord_Min[mc_mn_Coord_Number] = mc_mn_lmin;
          mc_mn_Vars->Coord_Max[mc_mn_Coord_Number] = mc_mn_lmax;
          if (mc_mn_Set_Coord_Mode != mc_mn_DEFS->COORD_SIGNAL) mc_mn_Vars->Coord_Bin[mc_mn_Coord_Number] = 20;
          mc_mn_Set_Coord_Mode = mc_mn_DEFS->COORD_VAR;
          mc_mn_Flag_All = 0;
          mc_mn_Flag_No  = 0;
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
    for (mc_mn_i = 0; mc_mn_i <= mc_mn_Vars->Coord_Number; mc_mn_i++)
    {
      mc_mn_Set_mc_mn_Vars_Coord_Type = (mc_mn_Vars->Coord_Type[mc_mn_i] & 31);
      if ((mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_THETA)
       || (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_PHI)
       || (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_X)
       || (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_Y)
       || (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_Z)
       || (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_RADIUS))
       strcpy(mc_mn_Short_Label[mc_mn_i],"Position"); 
      else
      if ((mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_VX)
       || (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_VY)
       || (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_VZ)
       || (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_V))
       strcpy(mc_mn_Short_Label[mc_mn_i],"Velocity"); 
      else
      if ((mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_KX)
       || (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_KY)
       || (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_KZ)
       || (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_K))
       strcpy(mc_mn_Short_Label[mc_mn_i],"Wavevector"); 
      else
      if ((mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_SX)
       || (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_SY)
       || (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_SZ))
       strcpy(mc_mn_Short_Label[mc_mn_i],"Spin");
      else
      if ((mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_HDIV)
       || (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_VDIV)
       || (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_ANGLE))
       strcpy(mc_mn_Short_Label[mc_mn_i],"Divergence");
      else
      if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_ENERGY)
       strcpy(mc_mn_Short_Label[mc_mn_i],"Energy"); 
      else
      if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_LAMBDA)
       strcpy(mc_mn_Short_Label[mc_mn_i],"Wavelength"); 
      else
      if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_NCOUNT)
       strcpy(mc_mn_Short_Label[mc_mn_i],"Neutron counts");
      else
      if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_T)
          strcpy(mc_mn_Short_Label[mc_mn_i],"Time Of Flight");
      else
      if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_P)
          strcpy(mc_mn_Short_Label[mc_mn_i],"Intensity");
      else
      if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_USER1)
          strncpy(mc_mn_Short_Label[mc_mn_i],mc_mn_Vars->UserName1,32);
      else
      if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_USER2)
          strncpy(mc_mn_Short_Label[mc_mn_i],mc_mn_Vars->UserName2,32);
      else
          strcpy(mc_mn_Short_Label[mc_mn_i],"Unknown"); 
          
      if (mc_mn_Vars->Coord_Type[mc_mn_i] & mc_mn_DEFS->COORD_ABS)
      { strcat(mc_mn_Vars->Coord_Label[mc_mn_i]," (abs)"); }
          
      if (mc_mn_Vars->Coord_Type[mc_mn_i] & mc_mn_DEFS->COORD_LOG)
      { strcat(mc_mn_Vars->Coord_Label[mc_mn_i]," (log)"); }
          
      strcat(mc_mn_Vars->Monitor_Label, " ");
      strcat(mc_mn_Vars->Monitor_Label, mc_mn_Short_Label[mc_mn_i]);
    } /* end for mc_mn_Short_Label */
    strcat(mc_mn_Vars->Monitor_Label, " Monitor");
    if (mc_mn_Vars->Flag_Shape == mc_mn_DEFS->SHAPE_SQUARE) strcat(mc_mn_Vars->Monitor_Label, " (Square)");
    if (mc_mn_Vars->Flag_Shape == mc_mn_DEFS->SHAPE_DISK)   strcat(mc_mn_Vars->Monitor_Label, " (Disk)");
    if (mc_mn_Vars->Flag_Shape == mc_mn_DEFS->SHAPE_SPHERE) strcat(mc_mn_Vars->Monitor_Label, " (Sphere)");
    if (mc_mn_Vars->Flag_Shape == mc_mn_DEFS->SHAPE_CYLIND) strcat(mc_mn_Vars->Monitor_Label, " (Cylinder)");
    if (mc_mn_Vars->Flag_Shape == mc_mn_DEFS->SHAPE_BOX)    strcat(mc_mn_Vars->Monitor_Label, " (Box)");
    if (((mc_mn_Vars->Flag_Shape == mc_mn_DEFS->SHAPE_CYLIND) || (mc_mn_Vars->Flag_Shape == mc_mn_DEFS->SHAPE_SPHERE) || (mc_mn_Vars->Flag_Shape == mc_mn_DEFS->SHAPE_BOX))
        && strstr(mc_mn_Vars->option, "outgoing"))
    {
      mc_mn_Vars->Flag_Shape *= -1;
      strcat(mc_mn_Vars->Monitor_Label, " [out]");
    }
    if (mc_mn_Vars->Flag_UsePreMonitor == 1)
    {
        strcat(mc_mn_Vars->Monitor_Label, " at ");
        strncat(mc_mn_Vars->Monitor_Label, mc_mn_Vars->UserName1,32);
    }
    if (mc_mn_Vars->Flag_log == 1) strcat(mc_mn_Vars->Monitor_Label, " [log] ");
    
    /* mc_mn_Vars->Coord_Number  0   : intensity or signal
     * mc_mn_Vars->Coord_Number  1:n : detector variables */
    
    /* now allocate memory to store variables in TRACE */
    if ((mc_mn_Vars->Coord_Number != 2) && !mc_mn_Vars->Flag_Multiple && !mc_mn_Vars->Flag_List) 
    { mc_mn_Vars->Flag_Multiple = 1; mc_mn_Vars->Flag_List = 0; } /* default is n1D */
    
   /* list and auto limits case : mc_mn_Vars->Flag_List or mc_mn_Vars->Flag_Auto_Limits 
    * -> Buffer to flush and sumc_mn_ppress after mc_mn_Vars->Flag_Auto_Limits
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
    if (mc_mn_Vars->Coord_Number == 0 && mc_mn_Vars->Flag_Verbose)  printf("Monitor_nD: %s is unactivated (0D)\n", mc_mn_Vars->compcurname);
    mc_mn_Vars->Cylinder_Height = fabs(mc_mn_Vars->mymax - mc_mn_Vars->mymin);
    
    if (mc_mn_Vars->Intermediate < 0) mc_mn_Vars->Intermediate = 0;
    if (mc_mn_Vars->Intermediate > 1) mc_mn_Vars->Intermediate /= 100;
    mc_mn_Vars->IntermediateCnts = mc_mn_Vars->Intermediate*mcget_ncount();
  } /* end Monitor_nD_Init */
  
/* ========================================================================= */
/* ADD: E.Farhi, Aug 6th, 2001: Monitor_nD section */
/* this routine is used to monitor one propagating neutron */ 
/* ========================================================================= */
 
  
double Monitor_nD_Trace(mc_mn_DEFS, mc_mn_Vars)
  MonitornD_Defines_type *mc_mn_DEFS;
  MonitornD_Variables_type *mc_mn_Vars;
{

  double  mc_mn_XY=0;
  long    mc_mn_i,mc_mn_j;
  double  mc_mn_pp;
  double  mc_mn_Coord[MONnD_COORD_NMAX];
  long    mc_mn_Coord_Index[MONnD_COORD_NMAX];
  char    mc_mn_While_End   =0;
  long    mc_mn_While_Buffer=0;
  char    mc_mn_Set_mc_mn_Vars_Coord_Type = mc_mn_DEFS->COORD_NONE;
  
  /* mc_mn_Vars->Flag_Auto_Limits */
  if ((mc_mn_Vars->Buffer_Counter >= mc_mn_Vars->Buffer_Block) && (mc_mn_Vars->Flag_Auto_Limits == 1) && (mc_mn_Vars->Coord_Number > 0))
  {
    /* auto limits case : get limits in Buffer for each variable */ 
          /* Dim : (mc_mn_Vars->Coord_Number+1)*mc_mn_Vars->Buffer_Block matrix (for p, dp) */ 
    if (mc_mn_Vars->Flag_Verbose) printf("Monitor_nD: %s getting %li Auto Limits from List (%li).\n", mc_mn_Vars->compcurname, mc_mn_Vars->Coord_Number, mc_mn_Vars->Buffer_Counter);
    for (mc_mn_i = 1; mc_mn_i <= mc_mn_Vars->Coord_Number; mc_mn_i++)
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
    mc_mn_Vars->Flag_Auto_Limits = 2;  /* pass to 2nd auto limits step */
  }

  /* manage realloc for list all if Buffer size exceeded */
  if ((mc_mn_Vars->Buffer_Counter >= mc_mn_Vars->Buffer_Block) && (mc_mn_Vars->Flag_List == 2))
  {
    mc_mn_Vars->Mon2D_Buffer  = (double *)realloc(mc_mn_Vars->Mon2D_Buffer, (mc_mn_Vars->Coord_Number+1)*(mc_mn_Vars->Neutron_Counter+mc_mn_Vars->Buffer_Block)*sizeof(double));
    if (mc_mn_Vars->Mon2D_Buffer == NULL)
          { printf("Monitor_nD: %s cannot reallocate mc_mn_Vars->Mon2D_Buffer[%li] (%li). Skipping.\n", mc_mn_Vars->compcurname, mc_mn_i, (mc_mn_Vars->Neutron_Counter+mc_mn_Vars->Buffer_Block)*sizeof(double)); mc_mn_Vars->Flag_List = 1; }
    else { mc_mn_Vars->Buffer_Counter = 0; mc_mn_Vars->Buffer_Size = mc_mn_Vars->Neutron_Counter+mc_mn_Vars->Buffer_Block; }
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
          free(mc_mn_Vars->Mon2D_Buffer);
        }
      }
    }
    else /* mc_mn_Vars->Flag_Auto_Limits == 0 or 1 */
    {
      for (mc_mn_i = 0; mc_mn_i <= mc_mn_Vars->Coord_Number; mc_mn_i++)
      { /* handle current neutron : last while */

        mc_mn_XY = 0;
        mc_mn_Set_mc_mn_Vars_Coord_Type = (mc_mn_Vars->Coord_Type[mc_mn_i] & 31);
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_X) mc_mn_XY = mc_mn_Vars->cx; 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_Y) mc_mn_XY = mc_mn_Vars->cy; 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_Z) mc_mn_XY = mc_mn_Vars->cz; 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_VX) mc_mn_XY = mc_mn_Vars->cvx; 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_VY) mc_mn_XY = mc_mn_Vars->cvy; 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_VZ) mc_mn_XY = mc_mn_Vars->cvz; 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_KX) mc_mn_XY = V2K*mc_mn_Vars->cvx; 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_KY) mc_mn_XY = V2K*mc_mn_Vars->cvy; 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_KZ) mc_mn_XY = V2K*mc_mn_Vars->cvz; 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_SX) mc_mn_XY = mc_mn_Vars->csx; 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_SY) mc_mn_XY = mc_mn_Vars->csy; 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_SZ) mc_mn_XY = mc_mn_Vars->csz; 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_T) mc_mn_XY = mc_mn_Vars->ct; 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_P) mc_mn_XY = mc_mn_Vars->cp; 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_HDIV) mc_mn_XY = RAD2DEG*atan2(mc_mn_Vars->cvx,mc_mn_Vars->cvz); 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_VDIV) mc_mn_XY = RAD2DEG*atan2(mc_mn_Vars->cvy,mc_mn_Vars->cvz); 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_V) mc_mn_XY = sqrt(mc_mn_Vars->cvx*mc_mn_Vars->cvx+mc_mn_Vars->cvy*mc_mn_Vars->cvy+mc_mn_Vars->cvz*mc_mn_Vars->cvz); 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_RADIUS) mc_mn_XY = sqrt(mc_mn_Vars->cx*mc_mn_Vars->cx+mc_mn_Vars->cy*mc_mn_Vars->cy); 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_K) { mc_mn_XY = sqrt(mc_mn_Vars->cvx*mc_mn_Vars->cvx+mc_mn_Vars->cvy*mc_mn_Vars->cvy+mc_mn_Vars->cvz*mc_mn_Vars->cvz);  mc_mn_XY *= V2K; }
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_ENERGY) { mc_mn_XY = mc_mn_Vars->cvx*mc_mn_Vars->cvx+mc_mn_Vars->cvy*mc_mn_Vars->cvy+mc_mn_Vars->cvz*mc_mn_Vars->cvz;  mc_mn_XY *= VS2E; }
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_LAMBDA) { mc_mn_XY = sqrt(mc_mn_Vars->cvx*mc_mn_Vars->cvx+mc_mn_Vars->cvy*mc_mn_Vars->cvy+mc_mn_Vars->cvz*mc_mn_Vars->cvz);  mc_mn_XY *= V2K; if (mc_mn_XY != 0) mc_mn_XY = 2*PI/mc_mn_XY; }
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_NCOUNT) mc_mn_XY = mc_mn_Coord[mc_mn_i]+1; 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_ANGLE) 
        {  mc_mn_XY = sqrt(mc_mn_Vars->cvx*mc_mn_Vars->cvx+mc_mn_Vars->cvy*mc_mn_Vars->cvy+mc_mn_Vars->cvz*mc_mn_Vars->cvz);
           if (mc_mn_Vars->cvz != 0) 
           {
             mc_mn_XY= RAD2DEG*atan2(mc_mn_XY,mc_mn_Vars->cvz);
           } else mc_mn_XY = 0;
        }
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_THETA)  { if (mc_mn_Vars->cz != 0) mc_mn_XY = RAD2DEG*atan2(mc_mn_Vars->cx,mc_mn_Vars->cz); } 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_PHI) { if (mc_mn_Vars->cz != 0) mc_mn_XY = RAD2DEG*atan2(mc_mn_Vars->cy,mc_mn_Vars->cz); } 
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_USER1) mc_mn_XY = mc_mn_Vars->UserVariable1;
        else
        if (mc_mn_Set_mc_mn_Vars_Coord_Type == mc_mn_DEFS->COORD_USER2) mc_mn_XY = mc_mn_Vars->UserVariable2;
        else
        mc_mn_XY = 0;

        if (mc_mn_Vars->Coord_Type[mc_mn_i] & mc_mn_DEFS->COORD_ABS) mc_mn_XY=fabs(mc_mn_XY);

        if (mc_mn_Vars->Coord_Type[mc_mn_i] & mc_mn_DEFS->COORD_LOG)
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
    /* 1D and n1D case : mc_mn_Vars->Flag_Multiple */
      if (mc_mn_Vars->Flag_Multiple)
      { /* Dim : mc_mn_Vars->Coord_Number*mc_mn_Vars->Coord_Bin[mc_mn_i] vectors (intensity is not included) */ 
        for (mc_mn_i= 1; mc_mn_i <= mc_mn_Vars->Coord_Number; mc_mn_i++)
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
    
    if (mc_mn_ratio < 99)
    {
      if (mc_mn_Vars->Flag_Verbose) printf("Monitor_nD: %s save intermediate results (%.2f %%).\n", mc_mn_Vars->compcurname, mc_mn_ratio);
    }

    /* check Buffer flush when end of simulation reached */
    if ((mc_mn_Vars->Buffer_Counter < mc_mn_Vars->Buffer_Block) && mc_mn_Vars->Flag_Auto_Limits && mc_mn_Vars->Mon2D_Buffer)
    {
      /* Get Auto Limits */
      if (mc_mn_Vars->Flag_Verbose) printf("Monitor_nD: %s getting %li Auto Limits from List (%li).\n", mc_mn_Vars->compcurname, mc_mn_Vars->Coord_Number, mc_mn_Vars->Buffer_Counter);
      for (mc_mn_i = 1; mc_mn_i <= mc_mn_Vars->Coord_Number; mc_mn_i++)
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

    if (mc_mn_Vars->Flag_Verbose) 
    {
      printf("Monitor_nD: %s is a %s.\n", mc_mn_Vars->compcurname, mc_mn_Vars->Monitor_Label);
      printf("Monitor_nD: version %s with options=%s\n", MONITOR_ND_LIB_H, mc_mn_Vars->option);
    }

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
      mcdetector_out_0D(mc_mn_Vars->Monitor_Label, mc_mn_Nsum, mc_mn_psum, mc_mn_p2sum, mc_mn_Vars->compcurname);
    }
    else
    if (strlen(mc_mn_Vars->Mon_File) > 0) 
    { 
      mc_mn_fname = (char*)malloc(strlen(mc_mn_Vars->Mon_File)+10*mc_mn_Vars->Coord_Number);
      if (mc_mn_Vars->Flag_List && mc_mn_Vars->Mon2D_Buffer) /* List: DETECTOR_OUT_2D */
      {
        if (mc_mn_Vars->Flag_List == 2) mc_mn_Vars->Buffer_Size = mc_mn_Vars->Neutron_Counter;
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
        if (!mc_mn_Vars->Flag_Binary_List)
        { /* ascii list */
          mc_mn_p1m = (double *)malloc((mc_mn_Vars->Coord_Number+1)*mc_mn_Vars->Buffer_Size*sizeof(double)); 
          if (mc_mn_min2d == mc_mn_max2d) mc_mn_max2d = mc_mn_min2d+1e-6;
          if (mc_mn_min1d == mc_mn_max1d) mc_mn_max1d = mc_mn_min1d+1e-6;
          strcpy(mc_mn_label, mc_mn_Vars->Monitor_Label);
          if (mc_mn_p1m == NULL) /* use Raw Buffer line output */
          {
            if (mc_mn_Vars->Flag_Verbose) printf("Monitor_nD: %s cannot allocate memory for List transpose. Skipping.\n", mc_mn_Vars->compcurname);
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
          } 
          else /* transpose for column output */
          {
            for (mc_mn_i=0; mc_mn_i < mc_mn_Vars->Coord_Number+1; mc_mn_i++) 
              for (mc_mn_j=0; mc_mn_j < mc_mn_Vars->Buffer_Size; mc_mn_j++)
              {
                mc_mn_p1m[mc_mn_i*mc_mn_Vars->Buffer_Size+mc_mn_j] = mc_mn_Vars->Mon2D_Buffer[mc_mn_j*(mc_mn_Vars->Coord_Number+1) + mc_mn_i];
              }

            mcdetector_out_2D(
                mc_mn_label,
                mc_mn_Coord_X_Label,
                "List of neutron events",
                mc_mn_min1d, mc_mn_max1d, 
                mc_mn_min2d, mc_mn_max2d, 
                mc_mn_bin1d,
                mc_mn_bin2d,
                NULL,mc_mn_p1m,NULL,
                mc_mn_fname, mc_mn_Vars->compcurname); 
            free(mc_mn_p1m);
          }
        }
        else
        { /* Binary list (for source) */
          FILE *mc_mn_fnum;
          float *mc_mn_pfm;
          
          mc_mn_pfm = (float *)malloc(mc_mn_bin1d*mc_mn_bin2d*sizeof(float)); 
          if (mc_mn_pfm != NULL)
          {
            for (mc_mn_i=0; mc_mn_i < mc_mn_bin1d*mc_mn_bin2d; mc_mn_i++) 
              mc_mn_pfm[mc_mn_i] = (float) mc_mn_Vars->Mon2D_Buffer[mc_mn_i];

            mc_mn_fnum = fopen(mc_mn_fname, "wb");
            if (!mc_mn_fnum)
              fprintf(stderr, "Monitor_nD: can not open binary file '%s'.\n",
              mc_mn_fname);
            else
            {
              long mc_mn_count;

              mc_mn_count = fwrite(mc_mn_pfm, mc_mn_bin1d*sizeof(float),
                        mc_mn_bin2d, mc_mn_fnum);
              if(mc_mn_count != mc_mn_bin2d)
              {
                fprintf(stderr, "Monitor_nD: error writing binary file '%s' (%li instead of %li).\n",
                  mc_mn_fname,mc_mn_count, mc_mn_bin2d);
              }
              fclose(mc_mn_fnum);
              if (mc_mn_Vars->Flag_Verbose) printf("Monitor_nD: %s write monitor float binary file %s List (%lix%li).\n", mc_mn_Vars->compcurname, mc_mn_fname,mc_mn_bin2d,mc_mn_bin1d);
            }
            free(mc_mn_pfm);
          }
        }
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
            mc_mn_fname, mc_mn_Vars->compcurname); 
          }
          else
          {
            if (mc_mn_Vars->Flag_log != 0)
            {
              mc_mn_XY = FLT_MAX;
              for (mc_mn_j=0; mc_mn_j < mc_mn_Vars->Coord_Bin[mc_mn_i+1]; mc_mn_j++) /* search min of signal */
                if ((mc_mn_XY > mc_mn_Vars->Mon2D_p[mc_mn_i][mc_mn_j]) && (mc_mn_Vars->Mon2D_p[mc_mn_i][mc_mn_j] > 0)) mc_mn_XY = mc_mn_Vars->Mon2D_p[mc_mn_i][mc_mn_j];
              if (mc_mn_XY <= 0) mc_mn_XY = -log(FLT_MAX)/log(10); else mc_mn_XY = log(mc_mn_XY)/log(10)-1;
            }
            
            for (mc_mn_j=0; mc_mn_j < mc_mn_Vars->Coord_Bin[mc_mn_i+1]; mc_mn_j++)
            {
              mc_mn_p1m[mc_mn_j] = mc_mn_Vars->Mon2D_p[mc_mn_i][mc_mn_j];
              mc_mn_p2m[mc_mn_j] = mc_mn_Vars->Mon2D_p2[mc_mn_i][mc_mn_j];
              if (mc_mn_Vars->Flag_signal != mc_mn_DEFS->COORD_P && mc_mn_Vars->Mon2D_N[mc_mn_i][mc_mn_j] > 0)
              {
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
            }
            mcdetector_out_1D(
              mc_mn_label,
              mc_mn_Vars->Coord_Label[mc_mn_i+1],
              mc_mn_Vars->Coord_Label[0],
              mc_mn_Vars->Coord_Var[mc_mn_i+1],
              mc_mn_min1d, mc_mn_max1d, 
              mc_mn_Vars->Coord_Bin[mc_mn_i+1],
              mc_mn_Vars->Mon2D_N[mc_mn_i],mc_mn_p1m,mc_mn_p2m,
              mc_mn_fname, mc_mn_Vars->compcurname); 
            
          }
          if (mc_mn_p1m != NULL) free(mc_mn_p1m);
          if (mc_mn_p2m != NULL) free(mc_mn_p2m);
            
        }
      }
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

          mcdetector_out_2D(
            mc_mn_label,
            mc_mn_Vars->Coord_Label[1],
            mc_mn_Vars->Coord_Label[2],
            mc_mn_min1d, mc_mn_max1d,  
            mc_mn_min2d, mc_mn_max2d,  
            mc_mn_Vars->Coord_Bin[1],
            mc_mn_Vars->Coord_Bin[2],
            mc_mn_p0m,mc_mn_p1m,mc_mn_p2m,
            mc_mn_fname, mc_mn_Vars->compcurname);

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

    mc_mn_radius = mc_mn_Vars->Sphere_Radius;
    mc_mn_h = mc_mn_Vars->Cylinder_Height;
    mc_mn_xmin = mc_mn_Vars->mxmin;
    mc_mn_xmax = mc_mn_Vars->mxmax;
    mc_mn_ymin = mc_mn_Vars->mymin;
    mc_mn_ymax = mc_mn_Vars->mymax;
    mc_mn_zmin = mc_mn_Vars->mzmin;
    mc_mn_zmax = mc_mn_Vars->mzmax;

    if (abs(mc_mn_Vars->Flag_Shape) == mc_mn_DEFS->SHAPE_SPHERE)
    {
      mcdis_magnify("");
      mcdis_circle("xy",0,0,0,mc_mn_radius);
      mcdis_circle("xz",0,0,0,mc_mn_radius);
      mcdis_circle("yz",0,0,0,mc_mn_radius);
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
    if (abs(mc_mn_Vars->Flag_Shape) == mc_mn_DEFS->SHAPE_CYLIND)
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
