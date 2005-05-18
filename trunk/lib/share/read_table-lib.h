/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/read_table-lib.h
*
* %Identification
* Written by: EF
* Date: Aug 28, 2002
* Origin: ILL
* Release: McStas 1.6
* Version: 1.1
*
* This file is to be imported by components that may read data from table files
* It handles some shared functions.
*
* This library may be used directly as an external library. It has no dependency
*
* Usage: within SHARE
* %include "read_table-lib"
*
*
* $Id: read_table-lib.h,v 1.11 2005-05-18 09:34:57 pkwi Exp $
*
*	$Log: not supported by cvs2svn $
*	Revision 1.9  2004/09/10 15:12:02  farhi
*	Make these libs easier to externalize (lower dependencies) and add comment about how to make these independent for external linkage.
*	
*	Revision 1.8  2003/02/11 12:28:46  farhi
*	Variouxs bug fixes after tests in the lib directory
*	mcstas_r  : disable output with --no-out.. flag. Fix 1D McStas output
*	read_table:corrected MC_SYS_DIR -> MCSTAS define
*	monitor_nd-lib: fix Log(signal) log(coord)
*	HOPG.trm: reduce 4000 points -> 400 which is enough and faster to resample
*	Progress_bar: precent -> percent parameter
*	CS: ----------------------------------------------------------------------
*	
* Revision 1.1 2002/08/29 11:39:00 ef
*	Initial revision extracted from lib/optics/Monochromators...
*******************************************************************************/

#ifndef READ_TABLE_LIB_H
#define READ_TABLE_LIB_H "1.1.0"

#ifndef MC_PATHSEP_C
#ifdef WIN32
#define MC_PATHSEP_C '\\'
#define MC_PATHSEP_S "\\"
#else  /* !WIN32 */
#ifdef MAC
#define MC_PATHSEP_C ':'
#define MC_PATHSEP_S ":"
#else  /* !MAC */
#define MC_PATHSEP_C '/'
#define MC_PATHSEP_S "/"
#endif /* !MAC */
#endif /* !WIN32 */
#endif /* !MC_PATHSEP_C */

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
#endif /* !MCSTAS */

#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>

  typedef struct struct_table
  {
    char    filename[128];
    char   *header;
    double *data; /* vector { x[0], y[0], ... x[n-1], y[n-1]... } */
    double  min_x;
    double  max_x;
    double  step_x;
    long    rows;
    long    columns;
    long    block_number;
  } t_Table;
  
/* read_table-lib function prototypes */
/* ========================================================================= */
void Table_Init(t_Table *Table);
void Table_Free(t_Table *Table);
long Table_Read(t_Table *Table,       char *File, long block_number);
long Table_Read_Offset(t_Table *mc_rt_Table, char *mc_rt_File, long mc_rt_block_number, long *offset, long max_lines);
long Table_Read_Offset_Binary(t_Table *mc_rt_Table, char *mc_rt_File, char *mc_rt_type, long *mc_rt_offset, long mc_rt_rows, long mc_rt_columns);
long Table_Read_Handle(t_Table *Table, FILE *fid, long block_number, long max_lines);
long Table_Rebin(t_Table *Table);
double Table_Index(t_Table Table, long i, long j);
double Table_Value(t_Table Table, double X, long j);  
void Table_Info(t_Table Table);
static void Table_Stat(t_Table *mc_rt_Table);

#endif  

/* end of read_table-lib.h */
