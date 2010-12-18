/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright (C) 1997-2007, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/read_table-lib.h
*
* %Identification
* Written by: EF
* Date: Aug 28, 2002
* Origin: ILL
* Release: McStas 1.11
* Version: $Revision: 1.21 $
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
* $Id$
*
* $Log: read_table-lib.h,v $
* Revision 1.21  2006/11/27 15:29:39  farhi
* Small improvement in interpolation methods of read-table lib.
*
* Revision 1.20  2006/03/15 16:04:14  farhi
* interpolation decl.
*
* Revision 1.19  2005/10/14 11:38:28  farhi
* Corrected missing #define
*
* Revision 1.18  2005/10/12 14:04:29  farhi
* Added function to parse header, Table_ParseHeader(header, "symbol1", ... , NULL)
* Useful for complex sample components, as well as mcformat/mcconvert stuff.
*
* Revision 1.17  2005/09/30 14:53:04  farhi
* REdiced length of title line in subplot's
*
* Revision 1.16  2005/07/25 14:55:08  farhi
* DOC update:
* checked all parameter [unit] + text to be OK
* set all versions to CVS Revision
*
* Revision 1.15  2005/07/20 13:08:43  farhi
* Changed Table_Init calling sequence (overrides Table_Alloc)
*
* Revision 1.14  2005/07/12 14:46:34  farhi
* Added Table_Alloc to create a user empty Table
* and Table_SetElement
*
* Revision 1.13  2005/07/05 14:25:59  farhi
* Added filesize in t_Table struct
*
* Revision 1.12  2005/07/05 12:06:40  farhi
* added new functions for table Array handling
* to be used in Isotropic_sqw and mcformat
*
* Revision 1.10  2005/01/20 14:16:43  farhi
* New functions to read separately all numerical bmocks in a text data file
* Will be used for Data conversion from PGPLOT/McStas (mcformat tool)
*
* Revision 1.9  2004/09/10 15:12:02  farhi
* Make these libs easier to externalize (lower dependencies) and add comment about how to make these independent for external linkage.
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
* Revision 1.1 2002/08/29 11:39:00 ef
* Initial revision extracted from lib/optics/Monochromators...
*******************************************************************************/

#ifndef READ_TABLE_LIB_H
#define READ_TABLE_LIB_H "$Revision: 1.21 $"

#define READ_TABLE_STEPTOL  0.02 /* tolerancy for constant step approx */

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

// ----------- added by Jiao Lin --------
#include <string.h>
#include <math.h>
#include "mcstas2/misc_macros.h"
// ----------- added by Jiao Lin --------


  typedef struct struct_table
  {
    char    filename[256];
    long    filesize;
    char   *header;  /* text header, e.g. comments */
    double *data;    /* vector { x[0], y[0], ... x[n-1], y[n-1]... } */
    double  min_x;   /* min value of first column */
    double  max_x;   /* max value of first column */
    double  step_x;  /* minimal step value of first column */
    long    rows;    /* number of rows in matrix block */
    long    columns; /* number of columns in matrix block */

    long    begin;   /* start fseek index of block */
    long    end;     /* stop  fseek index of block */
    long    block_number;  /* block index. 0 is catenation of all */
    long    array_length;  /* number of elements in the t_Table array */
    char    monotonic;     /* true when 1st column/vector data is monotonic */
    char    constantstep;  /* true when 1st column/vector data has constant step */
    char    method[32];    /* interpolation method: nearest, linear */
  } t_Table;

/* read_table-lib function prototypes */
/* ========================================================================= */

/* 'public' functions */
long     Table_Read              (t_Table *Table, char *File, long block_number);
long     Table_Read_Offset       (t_Table *Table, char *File, long block_number,
                                  long *offset, long max_lines);
long     Table_Read_Offset_Binary(t_Table *Table, char *File, char *Type,
                                  long *Offset, long Rows, long Columns);
long     Table_Rebin(t_Table *Table);
long     Table_Info (t_Table Table);
double   Table_Index(t_Table Table,   long i, long j);
double   Table_Value(t_Table Table, double X, long j);
t_Table *Table_Read_Array(char *File, long *blocks);
void     Table_Free_Array(t_Table *Table);
long     Table_Info_Array(t_Table *Table);
int      Table_SetElement(t_Table *Table, long i, long j, double value);
long     Table_Init(t_Table *Table, long rows, long columns);

char **Table_ParseHeader(char *header, ...);

/* private functions */

void Table_Free(t_Table *Table);
long Table_Read_Handle(t_Table *Table, FILE *fid, long block_number, long max_lines);
static void Table_Stat(t_Table *Table);
double Table_Interp1d(double x, double x1, double y1, double x2, double y2);
double Table_Interp1d_nearest(double x, double x1, double y1, double x2, double y2);
double Table_Interp2d(double x, double y, double x1, double y1, double x2, double y2,
  double z11, double z12, double z21, double z22);

#endif

/* end of read_table-lib.h */
