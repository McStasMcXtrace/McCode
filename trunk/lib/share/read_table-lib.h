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
* Usage: within SHARE
* %include "read_table-lib"
*
*
* $Id: read_table-lib.h,v 1.7 2003-01-21 08:55:33 pkwi Exp $
*
*	$Log: not supported by cvs2svn $
* Revision 1.1 2002/08/29 11:39:00 ef
*	Initial revision extracted from lib/optics/Monochromators...
*******************************************************************************/

#ifndef READ_TABLE_LIB_H
#define READ_TABLE_LIB_H "1.1.0"

#include <sys/stat.h>

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
