/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright (C) 1997-2007, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/read_table-lib.c
*
* %Identification
* Written by: EF
* Date: Aug 28, 2002
* Origin: ILL
* Release: McStas 1.11
* Version: $Revision: 1.37 $
*
* This file is to be imported by components that may read data from table files
* It handles some shared functions. Embedded within instrument in runtime mode.
* Variable names have prefix 'mc_rt_' for 'McStas Read Table' to avoid conflicts
*
* Usage: within SHARE
* %include "read_table-lib"
*
* $Id$
*
* $Log: read_table-lib.c,v $
* Revision 1.37  2006/12/01 16:17:10  farhi
* cosmetics
*
* Revision 1.36  2006/11/29 14:07:09  farhi
* cosmetics
*
* Revision 1.35  2006/11/27 15:29:39  farhi
* Small improvement in interpolation methods of read-table lib.
*
* Revision 1.34  2006/10/03 22:16:45  farhi
* Added realistic PSD_Detector component with Gas tables
* Added test for Isotropic_Sqw
* Data files can be in local path, MCSTAS/data and contrib sub-dirs
*
* Revision 1.33  2006/07/21 09:05:05  farhi
* fixed bug in 2D interpolation routine
*
* Revision 1.32  2006/03/15 16:03:27  farhi
* interpolation functions now in the table handling by default.
* corrected nelements in array length return value of Table_Read_Array
*
* Revision 1.31  2006/02/14 15:29:40  farhi
* Fixed error when importing block number > 1
*
* Revision 1.30  2006/01/30 12:55:10  farhi
* incomplete table warning displays file name
*
* Revision 1.29  2005/10/14 11:38:28  farhi
* Corrected missing #define
*
* Revision 1.28  2005/10/12 14:04:29  farhi
* Added function to parse header, Table_ParseHeader(header, "symbol1", ... , NULL)
* Useful for complex sample components, as well as mcformat/mcconvert stuff.
*
* Revision 1.27  2005/10/05 08:50:53  farhi
* Extended buffer for Table line read (fgetl) to 64 ko instead of 4 ko.
* It failed with large matrices (e.g. more than 100 columns)
*
* Revision 1.26  2005/09/16 14:19:03  farhi
* Correct bug #56: SEGV when opening a non existent file with read-table. Was reported on usage of PowderN
*
* Revision 1.25  2005/08/31 14:50:29  farhi
* Fixed bugs when handling vectors with swapped (i,j) indexes
*
* Revision 1.24  2005/07/25 14:55:08  farhi
* DOC update:
* checked all parameter [unit] + text to be OK
* set all versions to CVS Revision
*
* Revision 1.23  2005/07/20 13:08:43  farhi
* Changed Table_Init calling sequence (overrides Table_Alloc)
*
* Revision 1.22  2005/07/12 14:46:26  farhi
* Added Table_Alloc to create a user empty Table
* and Table_SetElement
*
* Revision 1.21  2005/07/08 13:15:43  farhi
* Mismatch in argument swap
*
* Revision 1.20  2005/07/07 14:16:57  farhi
* Min and Max are computed for both row and column vectors
*
* Revision 1.19  2005/07/06 09:50:45  farhi
* Added check for non existing data file in Table_*_Array functions
*
* Revision 1.18  2005/07/06 08:44:28  farhi
* Display headers in Table_Info
* Also works with Arrays
*
* Revision 1.17  2005/07/05 14:30:27  farhi
* misprint
*
* Revision 1.16  2005/07/05 14:25:42  farhi
* added file size in t_Table structure
*
* Revision 1.15  2005/07/05 12:06:40  farhi
* added new functions for table Array handling
* to be used in Isotropic_sqw and mcformat
*
* Revision 1.13  2005/01/20 14:16:43  farhi
* New functions to read separately all numerical bmocks in a text data file
* Will be used for Data conversion from PGPLOT/McStas (mcformat tool)
*
* Revision 1.12  2004/09/10 15:12:02  farhi
* Make these libs easier to externalize (lower dependencies) and add comment about how to make these independent for external linkage.
*
* Revision 1.11  2004/09/09 13:48:02  farhi
* Code clean-up
*
* Revision 1.10  2004/09/03 13:46:50  farhi
* Correct misprint in comment
*
* Revision 1.9  2003/05/20 15:12:33  farhi
* malloc size for read table binary now needs less memory
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
* Revision 1.8  2003/02/06 14:14:41  farhi
* Corrected MC_SYS_DIR into MCSTAS definition of default lib location
*
* Revision 1.2 2002/12/19 12:48:07 ef
* Added binary import. Fixed Rebin. Added Stat.
*
* Revision 1.1 2002/08/29 11:39:00 ef
* Initial revision extracted from lib/optics/Monochromators...
*******************************************************************************/

#ifndef READ_TABLE_LIB_H
#include "read_table-lib.h"

// ----------- added by Jiao Lin --------
#include "stdarg.h"
// ----------- added by Jiao Lin --------
#endif

/*******************************************************************************
* long Read_Table(t_Table *Table, char *name, int block_number)
*   ACTION: read a single Table from a text file
*   input   Table: pointer to a t_Table structure
*           name:  file name from which table should be extracted
*           block_number: if the file does contain more than one
*                 data block, then indicates which one to get (from index 1)
*                 a 0 value means append/catenate all
*   return  initialized single Table t_Table structure containing data, header, ...
*           number of read elements (-1: error, 0:header only)
* The routine stores any line starting with '#', '%' and ';' into the header
* File is opened, read and closed
* Other lines are interpreted as numerical data, and stored.
* Data block should be a rectangular matrix or vector.
* Data block may be rebined with Table_Rebin (also sort in ascending order)
*******************************************************************************/
  long Table_Read(t_Table *mc_rt_Table, char *mc_rt_File, long mc_rt_block_number)
  { /* reads all or a single data block from 'file' and returns a Table structure  */
    return(Table_Read_Offset(mc_rt_Table, mc_rt_File, mc_rt_block_number, NULL, 0));
  } /* end Table_Read */

/*******************************************************************************
* long Table_Read_Offset(t_Table *Table, char *name, int block_number, long *offset
*                        long max_lines)
*   ACTION: read a single Table from a text file, starting at offset
*     Same as Table_Read(..) except:
*   input   offset:    pointer to an offset (*offset should be 0 at start)
*           max_lines: max number of data rows to read from file (0 means all)
*   return  initialized single Table t_Table structure containing data, header, ...
*           number of read elements (-1: error, 0:header only)
*           updated *offset position (where end of reading occured)
*******************************************************************************/
  long Table_Read_Offset(t_Table *mc_rt_Table, char *mc_rt_File,
                         long mc_rt_block_number, long *mc_rt_offset,
                         long mc_rt_max_lines)
  { /* reads all/a data block in 'file' and returns a Table structure  */
    FILE *mc_rt_hfile;
    long  mc_rt_nelements;
    long  mc_rt_begin;
    long  mc_rt_filesize=0;
    struct stat mc_rt_stfile;

    if (!mc_rt_Table) return(-1);
    Table_Init(mc_rt_Table, 0, 0);
    if (!mc_rt_File)  return(-1);
    if (strlen(mc_rt_File) == 0) return (-1);
    mc_rt_hfile = fopen(mc_rt_File, "r");
    if(!mc_rt_hfile)
    {
      char mc_rt_path[256];
      char mc_rt_dir[256];

      if (!mc_rt_hfile)
      {
        strcpy(mc_rt_dir, getenv("MCSTAS") ? getenv("MCSTAS") : MCSTAS);
        sprintf(mc_rt_path, "%s%c%s%c%s", mc_rt_dir, MC_PATHSEP_C, "data", MC_PATHSEP_C, mc_rt_File);
        mc_rt_hfile = fopen(mc_rt_path, "r");
      }
      if (!mc_rt_hfile)
      {
        strcpy(mc_rt_dir, getenv("MCSTAS") ? getenv("MCSTAS") : MCSTAS);
        sprintf(mc_rt_path, "%s%c%s%c%s", mc_rt_dir, MC_PATHSEP_C, "contrib", MC_PATHSEP_C, mc_rt_File);
        mc_rt_hfile = fopen(mc_rt_path, "r");
      }
      if(!mc_rt_hfile)
      {
        fprintf(stderr, "Error: Could not open input file '%s' (Table_Read)\n", mc_rt_File);
        return (-1);
      }
    }
    stat(mc_rt_File,&mc_rt_stfile); mc_rt_filesize = mc_rt_stfile.st_size;
    if (mc_rt_offset && *mc_rt_offset) fseek(mc_rt_hfile, *mc_rt_offset, SEEK_SET);
    mc_rt_begin     = ftell(mc_rt_hfile);
    mc_rt_nelements = Table_Read_Handle(mc_rt_Table, mc_rt_hfile, mc_rt_block_number, mc_rt_max_lines);
    strncpy(mc_rt_Table->filename, mc_rt_File, 128);
    mc_rt_Table->begin = mc_rt_begin;
    mc_rt_Table->end   = ftell(mc_rt_hfile);
    mc_rt_Table->filesize = (mc_rt_filesize>0 ? mc_rt_filesize : 0);
    if (mc_rt_offset) *mc_rt_offset=mc_rt_Table->end;
    fclose(mc_rt_hfile);
    return(mc_rt_nelements);

  } /* end Table_Read_Offset */

/*******************************************************************************
* long Table_Read_Offset_Binary(t_Table *Table, char *File, char *type,
*                               long *offset, long rows, long columns)
*   ACTION: read a single Table from a binary file, starting at offset
*     Same as Table_Read_Offset(..) except that it handles binary files.
*   input   type: may be "float"/NULL or "double"
*           offset: pointer to an mc_rt_offset (*offset should be 0 at start)
*           rows   : number of rows (0 means read all)
*           columns: number of columns
*   return  initialized single Table t_Table structure containing data, header, ...
*           number of read elements (-1: error, 0:header only)
*           updated *offset position (where end of reading occured)
*******************************************************************************/
  long Table_Read_Offset_Binary(t_Table *mc_rt_Table, char *mc_rt_File, char *mc_rt_type,
                                long *mc_rt_offset, long mc_rt_rows, long mc_rt_columns)
  { /* reads all/a data block in binary 'file' and returns a Table structure  */
    long    mc_rt_nelements, mc_rt_sizeofelement;
    long    mc_rt_filesize;
    FILE   *mc_rt_hfile;
    struct stat mc_rt_stfile;
    double *mc_rt_data;
    long    mc_rt_i;
    long    mc_rt_begin;

    if (!mc_rt_Table) return(-1);

    Table_Init(mc_rt_Table, 0, 0);
    if (!mc_rt_File)  return(-1);

    mc_rt_hfile = fopen(mc_rt_File, "r");
    if(!mc_rt_hfile)
    {
      char mc_rt_path[256];
      char mc_rt_dir[256];

      if (!mc_rt_hfile)
      {
        strcpy(mc_rt_dir, getenv("MCSTAS") ? getenv("MCSTAS") : MCSTAS);
        sprintf(mc_rt_path, "%s%c%s%c%s", mc_rt_dir, MC_PATHSEP_C, "data", MC_PATHSEP_C, mc_rt_File);
        mc_rt_hfile = fopen(mc_rt_path, "r");
      }
      if (!mc_rt_hfile)
      {
        strcpy(mc_rt_dir, getenv("MCSTAS") ? getenv("MCSTAS") : MCSTAS);
        sprintf(mc_rt_path, "%s%c%s%c%s", mc_rt_dir, MC_PATHSEP_C, "contrib", MC_PATHSEP_C, mc_rt_File);
        mc_rt_hfile = fopen(mc_rt_path, "r");
      }
      if(!mc_rt_hfile)
      {
        fprintf(stderr, "Error: Could not open input file '%s' (Table_Read_Offset_Binary)\n", mc_rt_File);
        return (-1);
      }
    }
    stat(mc_rt_File,&mc_rt_stfile);
    mc_rt_filesize = mc_rt_stfile.st_size;
    mc_rt_Table->filesize=mc_rt_filesize;
    if (mc_rt_type && !strcmp(mc_rt_type,"double")) mc_rt_sizeofelement = sizeof(double);
    else  mc_rt_sizeofelement = sizeof(float);
    if (mc_rt_offset && *mc_rt_offset) fseek(mc_rt_hfile, *mc_rt_offset, SEEK_SET);
    mc_rt_begin     = ftell(mc_rt_hfile);
    if (mc_rt_rows && mc_rt_filesize > mc_rt_sizeofelement*mc_rt_columns*mc_rt_rows)
      mc_rt_nelements = mc_rt_columns*mc_rt_rows;
    else mc_rt_nelements = (long)(mc_rt_filesize/mc_rt_sizeofelement);
    if (!mc_rt_nelements || mc_rt_filesize <= *mc_rt_offset) return(0);
    mc_rt_data    = (double*)malloc(mc_rt_nelements*mc_rt_sizeofelement);
    if (!mc_rt_data) {
      fprintf(stderr,"Error: allocating %ld elements for %s file '%s'. Too big (Table_Read_Offset_Binary).\n", mc_rt_nelements, mc_rt_type, mc_rt_File);
      exit(-1);
    }
    mc_rt_nelements = fread(mc_rt_data, mc_rt_sizeofelement, mc_rt_nelements, mc_rt_hfile);

    if (!mc_rt_data || !mc_rt_nelements)
    {
      fprintf(stderr,"Error: reading %ld elements from %s file '%s' (Table_Read_Offset_Binary)\n", mc_rt_nelements, mc_rt_type, mc_rt_File);
      exit(-1);
    }
    mc_rt_Table->begin   = mc_rt_begin;
    mc_rt_Table->end     = ftell(mc_rt_hfile);
    if (mc_rt_offset) *mc_rt_offset=mc_rt_Table->end;
    fclose(mc_rt_hfile);
    mc_rt_data = (double*)realloc(mc_rt_data, (double)mc_rt_nelements*mc_rt_sizeofelement);
    /* copy file data into Table */
    if (mc_rt_type && !strcmp(mc_rt_type,"double")) mc_rt_Table->data = mc_rt_data;
    else {
      float  *mc_rt_s;
      double *mc_rt_dataf;
      mc_rt_s     = (float*)mc_rt_data;
      mc_rt_dataf = (double*)malloc(sizeof(double)*mc_rt_nelements);
      for (mc_rt_i=0; mc_rt_i<mc_rt_nelements; mc_rt_i++)
        mc_rt_dataf[mc_rt_i]=mc_rt_s[mc_rt_i];
      free(mc_rt_data);
      mc_rt_Table->data = mc_rt_dataf;
    }
    strcpy(mc_rt_Table->filename, mc_rt_File);
    mc_rt_Table->rows    = mc_rt_nelements/mc_rt_columns;
    mc_rt_Table->columns = mc_rt_columns;
    mc_rt_Table->array_length = 1;
    mc_rt_Table->block_number = 1;

    Table_Stat(mc_rt_Table);

    return(mc_rt_nelements);
  } /* end Table_Read_Offset_Binary */

/*******************************************************************************
* long Read_Table_Handle(t_Table *Table, FILE *fid, int block_number, long max_lines)
*   ACTION: read a single Table from a text file handle (private)
*   input   Table:pointer to a t_Table structure
*           fid:  pointer to FILE handle
*           block_number: if the file does contain more than one
*                 data block, then indicates which one to get (from index 1)
*                 a 0 value means append/catenate all
*           max_lines: if non 0, only reads that number of lines
*   return  initialized single Table t_Table structure containing data, header, ...
*           modified Table t_Table structure containing data, header, ...
*           number of read elements (-1: error, 0:header only)
* The routine stores any line starting with '#', '%' and ';' into the header
* Other lines are interpreted as numerical data, and stored.
* Data block should be a rectangular matrix or vector.
* Data block may be rebined with Table_Rebin (also sort in ascending order)
*******************************************************************************/
  long Table_Read_Handle(t_Table *mc_rt_Table, FILE *mc_rt_hfile,
                         long mc_rt_block_number, long mc_rt_max_lines)
  { /* reads all/a data block from 'file' handle and returns a Table structure  */
    double *mc_rt_Data;
    char *mc_rt_Header;
    long  mc_rt_malloc_size         = 1024;
    long  mc_rt_malloc_size_h       = 4096;
    long  mc_rt_Rows = 0,   mc_rt_Columns = 0;
    long  mc_rt_count_in_array      = 0;
    long  mc_rt_count_in_header     = 0;
    long  mc_rt_block_Current_index = 0;
    char  mc_rt_flag_In_array       = 0;
    char  mc_rt_flag_End_row_loop   = 0;

    if (!mc_rt_Table) return(-1);
    Table_Init(mc_rt_Table, 0, 0);

    if(!mc_rt_hfile) {
       fprintf(stderr, "Error: File handle is NULL (Table_Read_Handle).\n");
       return (-1);
    }
    mc_rt_Header = (char*)  malloc(mc_rt_malloc_size_h*sizeof(char));
    mc_rt_Data   = (double*)malloc(mc_rt_malloc_size  *sizeof(double));
    if ((mc_rt_Header == NULL) || (mc_rt_Data == NULL)) {
       fprintf(stderr, "Error: Could not allocate Table and Header (Table_Read_Handle).\n");
       return (-1);
    }
    mc_rt_Header[0] = '\0';

    do { /* while (!mc_rt_flag_End_row_loop) */
      char  mc_rt_line[64*1024];
      long  mc_rt_back_pos=0;   /* ftell start of line */

      mc_rt_back_pos = ftell(mc_rt_hfile);
      if (fgets(mc_rt_line, 64*1024, mc_rt_hfile) != NULL) { /* analyse line */
        int mc_rt_i=0;
        char  mc_rt_flag_Store_into_header=0;
        /* first skip blank and tabulation characters */
        while (mc_rt_line[mc_rt_i] == ' ' || mc_rt_line[mc_rt_i] == '\t') mc_rt_i++;
        /* handle comments: stored in header */
        if ((mc_rt_line[mc_rt_i] == '#') || (mc_rt_line[mc_rt_i] == '%')
        || (mc_rt_line[mc_rt_i] == ';') || (mc_rt_line[mc_rt_i] == '/'))
        { /* line is a comment */
          mc_rt_flag_Store_into_header=1;
          mc_rt_flag_In_array = 0;
        } else {
          double mc_rt_X;

          /* get the number of columns splitting line with strtok */
          if (sscanf(mc_rt_line,"%lg ",&mc_rt_X) == 1)
          { /* line begins at least with one num */
            char  *mc_rt_InputTokens, *mc_rt_lexeme;
            char   mc_rt_flag_End_Line= 0;
            long   mc_rt_block_Num_Columns     = 0;

            mc_rt_InputTokens            = mc_rt_line;

            do { /* while (!mc_rt_flag_End_Line) */
              mc_rt_lexeme      = (char *)strtok(mc_rt_InputTokens, " ,;\t\n\r");
              mc_rt_InputTokens = NULL;
              if ((mc_rt_lexeme != NULL) && (strlen(mc_rt_lexeme) != 0))
              { /* reading line: the token is not empty */
                if (sscanf(mc_rt_lexeme,"%lg ",&mc_rt_X) == 1)
                { /* reading line: the token is a number in the line */
                  if (!mc_rt_flag_In_array)
                  { /* reading num: not already in a block: starts a new data block */
                    mc_rt_block_Current_index++;
                    mc_rt_flag_In_array    = 1;
                    mc_rt_block_Num_Columns= 0;
                    if (mc_rt_block_number)
                    { /* initialise a new data block */
                      mc_rt_Rows = 0;
                      mc_rt_count_in_array = 0;
                    } /* else append */
                  }
                  /* reading num: all blocks or selected block */
                  if ( mc_rt_flag_In_array &&  ((mc_rt_block_number == 0) || (mc_rt_block_number == mc_rt_block_Current_index)) )
                  {
                    /* starting block: already the desired number of rows ? */
                    if (mc_rt_block_Num_Columns == 0
                      && mc_rt_max_lines && mc_rt_Rows >= mc_rt_max_lines) {
                      mc_rt_flag_End_Line      = 1;
                      mc_rt_flag_End_row_loop  = 1;
                      mc_rt_flag_In_array      = 0;
                      /* reposition to begining of line (ignore line) */
                      fseek(mc_rt_hfile, mc_rt_back_pos, SEEK_SET);
                    } else { /* store into data array */
                      if (mc_rt_count_in_array >= mc_rt_malloc_size)
                      { /* realloc data buffer if necessary */
                        mc_rt_malloc_size = mc_rt_count_in_array+1024;
                        mc_rt_Data     = (double*)realloc(mc_rt_Data, mc_rt_malloc_size*sizeof(double));
                        if (mc_rt_Data == NULL)
                        {
                          fprintf(stderr, "Error: Can not re-allocate memory %li (Table_Read_Handle).\n", mc_rt_malloc_size*sizeof(double));
                          return (-1);
                        }
                      }
                      if (mc_rt_block_Num_Columns == 0) mc_rt_Rows++;
                      mc_rt_Data[mc_rt_count_in_array] = mc_rt_X;
                      mc_rt_count_in_array++;
                      mc_rt_block_Num_Columns++;
                    }
                  } /* reading num: end if mc_rt_flag_In_array */
                  else
                  { /* reading num: passed selected block */
                    if (mc_rt_block_number < mc_rt_block_Current_index)
                    { /* we finished to extract block -> force end of file reading */
                      mc_rt_flag_End_Line      = 1;
                      mc_rt_flag_End_row_loop  = 1;
                      mc_rt_flag_In_array  = 0;
                    }
                    /* else (if read all blocks) continue */
                  }
                } /* end reading num: end if sscanf mc_rt_lexeme -> numerical */
                else
                { /* reading line: the token is not numerical in that line. end block */
                  mc_rt_flag_End_Line = 1;
                  mc_rt_flag_In_array = 0;
                }
              }
              else
              { /* no more tokens in mc_rt_line */
                mc_rt_flag_End_Line = 1;
                if (mc_rt_block_Num_Columns) mc_rt_Columns = mc_rt_block_Num_Columns;
              }
            } while (!mc_rt_flag_End_Line); /* end while mc_rt_flag_End_Line */
          }
          else
          { /* ascii line: does not begin with a number: ignore line */
            mc_rt_flag_In_array          = 0;
            mc_rt_flag_Store_into_header = 1;
          }
        } /* end: if not line comment else numerical */
        if (mc_rt_flag_Store_into_header) { /* add line into header */
          mc_rt_count_in_header += strlen(mc_rt_line);
          if (mc_rt_count_in_header+4096 > mc_rt_malloc_size_h)
          { /* if succeed and in array : add (and realloc if necessary) */
            mc_rt_malloc_size_h = mc_rt_count_in_header+4096;
            mc_rt_Header     = (char*)realloc(mc_rt_Header, mc_rt_malloc_size_h*sizeof(char));
          }
          strncat(mc_rt_Header, mc_rt_line, 4096);
          mc_rt_flag_In_array  = 0; /* will start a new data block */
          /* exit line and file if passed desired block */
          if (mc_rt_block_number && mc_rt_block_number == mc_rt_block_Current_index) {
            mc_rt_flag_End_row_loop  = 1;

          }
        }
      } /* end: if fgets */
      else mc_rt_flag_End_row_loop = 1; /* else fgets : end of file */
    } while (!mc_rt_flag_End_row_loop); /* end while mc_rt_flag_End_row_loop */

    mc_rt_Table->block_number = mc_rt_block_number;
    mc_rt_Table->array_length = 1;
    if (mc_rt_count_in_header) mc_rt_Header    = (char*)realloc(mc_rt_Header, mc_rt_count_in_header*sizeof(char));
    mc_rt_Table->header       = mc_rt_Header;
    if (mc_rt_count_in_array*mc_rt_Rows*mc_rt_Columns == 0)
    {
      mc_rt_Table->rows         = 0;
      mc_rt_Table->columns      = 0;
      free(mc_rt_Data);
      return (0);
    }
    if (mc_rt_Rows * mc_rt_Columns != mc_rt_count_in_array)
    {
      fprintf(stderr, "Warning: Read_Table :%s %s Data has %li values that should be %li x %li\n",
        (mc_rt_Table->filename ? mc_rt_Table->filename : ""),
        (!mc_rt_block_number ? " catenated" : ""),
        mc_rt_count_in_array, mc_rt_Rows, mc_rt_Columns);
      mc_rt_Columns = mc_rt_count_in_array; mc_rt_Rows = 1;
    }
    mc_rt_Data     = (double*)realloc(mc_rt_Data, mc_rt_count_in_array*sizeof(double));
    mc_rt_Table->data         = mc_rt_Data;
    mc_rt_Table->rows         = mc_rt_Rows;
    mc_rt_Table->columns      = mc_rt_Columns;

    Table_Stat(mc_rt_Table);
    return (mc_rt_count_in_array);

  } /* end Table_Read_Handle */

/*******************************************************************************
* long Rebin_Table(t_Table *Table)
*   ACTION: rebin a single Table, sorting 1st column in ascending order
*   input   Table: single table containing data.
*                  The data block is reallocated in this process
*   return  updated Table with increasing, evenly spaced first column (index 0)
*           number of data elements (-1: error, 0:empty data)
*******************************************************************************/
  long Table_Rebin(t_Table *mc_rt_Table)
  {
    double mc_rt_new_step=0;
    long   mc_rt_i;
    /* performs linear interpolation on X axis (0-th column) */

    if (!mc_rt_Table) return(-1);
    if (!mc_rt_Table->data
    || mc_rt_Table->rows*mc_rt_Table->columns == 0 || !mc_rt_Table->step_x)
      return(0);
    Table_Stat(mc_rt_Table); /* recompute statitstics and minimal step */
    mc_rt_new_step = mc_rt_Table->step_x; /* minimal step in 1st column */

    if (mc_rt_Table->constantstep) /* already evenly spaced */
      return (mc_rt_Table->rows*mc_rt_Table->columns);
    else {
      long mc_rt_Length_Table;
      double *mc_rt_New_Table;

      mc_rt_Length_Table = ceil(fabs(mc_rt_Table->max_x - mc_rt_Table->min_x)/mc_rt_new_step);
      mc_rt_New_Table    = (double*)malloc(mc_rt_Length_Table*mc_rt_Table->columns*sizeof(double));

      for (mc_rt_i=0; mc_rt_i < mc_rt_Length_Table; mc_rt_i++)
      {
        long   mc_rt_j;
        double mc_rt_X;
        mc_rt_X = mc_rt_Table->min_x + mc_rt_i*mc_rt_new_step;
        mc_rt_New_Table[mc_rt_i*mc_rt_Table->columns] = mc_rt_X;
        for (mc_rt_j=1; mc_rt_j < mc_rt_Table->columns; mc_rt_j++)
          mc_rt_New_Table[mc_rt_i*mc_rt_Table->columns+mc_rt_j]
                = Table_Value(*mc_rt_Table, mc_rt_X, mc_rt_j);
      } /* end for mc_rt_i */

      mc_rt_Table->rows = mc_rt_Length_Table;
      mc_rt_Table->step_x = mc_rt_new_step;
      free(mc_rt_Table->data);
      mc_rt_Table->data = mc_rt_New_Table;
    } /* end else (!constantstep) */
    return (mc_rt_Table->rows*mc_rt_Table->columns);
  } /* end Rebin_Table */

/*******************************************************************************
* double Table_Index(t_Table Table, long i, long j)
*   ACTION: read an element [i,j] of a single Table
*   input   Table: table containing data
*           i : index of row      (0:Rows-1)
*           j : index of column   (0:Columns-1)
*   return  Value = data[i][j]
* Returns Value from the i-th row, j-th column of Table
* Tests are performed on indexes i,j to avoid errors
*******************************************************************************/
  double Table_Index(t_Table mc_rt_Table, long mc_rt_i, long mc_rt_j)
  {
    long mc_rt_AbsIndex;

    if (mc_rt_i < 0)        mc_rt_i = 0;
    if (mc_rt_j < 0)        mc_rt_j = 0;
    /* handle vectors specifically */
    if (mc_rt_Table.columns == 1 || mc_rt_Table.rows == 1) {
      mc_rt_AbsIndex = mc_rt_i+mc_rt_j;
    } else {
      if (mc_rt_i >= mc_rt_Table.rows)    mc_rt_i = mc_rt_Table.rows-1;
      if (mc_rt_j >= mc_rt_Table.columns) mc_rt_j = mc_rt_Table.columns-1;
      mc_rt_AbsIndex = mc_rt_i*(mc_rt_Table.columns)+mc_rt_j;
    }
    if (mc_rt_AbsIndex >= mc_rt_Table.rows*mc_rt_Table.columns)
      mc_rt_AbsIndex = mc_rt_Table.rows*mc_rt_Table.columns-1;
    else if (mc_rt_AbsIndex < 0) mc_rt_AbsIndex=0;
    if (mc_rt_Table.data != NULL)
      return(mc_rt_Table.data[mc_rt_AbsIndex]);
    else
      return(0);
  } /* end Table_Index */

/*******************************************************************************
* void Table_SetElement(t_Table *Table, long i, long j, double value)
*   ACTION: set an element [i,j] of a single Table
*   input   Table: table containing data
*           i : index of row      (0:mc_rt_Rows-1)
*           j : index of column   (0:Columns-1)
*           value = data[i][j]
* Returns 0 in case of error
* Tests are performed on indexes i,j to avoid errors
*******************************************************************************/
  int Table_SetElement(t_Table *mc_rt_Table, long mc_rt_i, long mc_rt_j,
                        double mc_rt_value)
  {
    long mc_rt_AbsIndex;

    if (mc_rt_i < 0)        mc_rt_i = 0;
    if (mc_rt_i >= mc_rt_Table->rows)    mc_rt_i = mc_rt_Table->rows-1;
    if (mc_rt_j < 0)        mc_rt_j = 0;
    if (mc_rt_j >= mc_rt_Table->columns) mc_rt_j = mc_rt_Table->columns-1;
    mc_rt_AbsIndex = mc_rt_i*(mc_rt_Table->columns)+mc_rt_j;
    if (mc_rt_Table->data != NULL)
      mc_rt_Table->data[mc_rt_AbsIndex] = mc_rt_value;
    else return(0);
    return(1);
  } /* end Table_SetElement */

/*******************************************************************************
* double Table_Value(t_Table Table, double X, long j)
*   ACTION: read column [j] of a single Table at row which 1st column is X
*   input   Table: table containing data.
*           X : data value in the first column (index 0)
*           j : index of column from which is extracted the Value (0:Columns-1)
*   return  Value = data[index for X][j] with linear interpolation
* Returns Value from the j-th column of Table corresponding to the
* X value for the 1st column (index 0)
* Tests are performed (within Table_Index) on indexes i,j to avoid errors
* NOTE: data should rather be monotonic, and evenly sampled.
*******************************************************************************/
  double Table_Value(t_Table mc_rt_Table, double X, long j)
  {
    long   mc_rt_Index;
    double mc_rt_X1, mc_rt_Y1, mc_rt_X2, mc_rt_Y2;
    double ret=0;

    if (X > mc_rt_Table.max_x) return Table_Index(mc_rt_Table,mc_rt_Table.rows-1  ,j);
    if (X < mc_rt_Table.min_x) return Table_Index(mc_rt_Table,0  ,j);

    if (!mc_rt_Table.constantstep)
      /* look for index surrounding X in the table -> mc_rt_Index */
      for (mc_rt_Index=1; mc_rt_Index < mc_rt_Table.rows-1; mc_rt_Index++)
      {
        mc_rt_X2 = Table_Index(mc_rt_Table, mc_rt_Index  ,0);
        mc_rt_X1 = Table_Index(mc_rt_Table, mc_rt_Index-1,0);
        if ((mc_rt_X1 <= X) && (X < mc_rt_X2)) break;
      } /* end for mc_rt_Index */
    else {
        mc_rt_Index = (long)floor((X - mc_rt_Table.min_x)
                          /(mc_rt_Table.max_x - mc_rt_Table.min_x)
                           *mc_rt_Table.rows);
        mc_rt_X1 = Table_Index(mc_rt_Table,mc_rt_Index  ,0);
        mc_rt_X2 = Table_Index(mc_rt_Table,mc_rt_Index+1,0);
    }
    mc_rt_Y2 = Table_Index(mc_rt_Table,mc_rt_Index  ,j);
    mc_rt_Y1 = Table_Index(mc_rt_Table,mc_rt_Index-1,j);

    if (!strcmp(mc_rt_Table.method,"linear"))
      ret = Table_Interp1d(X, mc_rt_X1,mc_rt_Y1, mc_rt_X2,mc_rt_Y2);
    else if (!strcmp(mc_rt_Table.method,"nearest"))
      ret = Table_Interp1d_nearest(X, mc_rt_X1,mc_rt_Y1, mc_rt_X2,mc_rt_Y2);

    return (ret);
  } /* end Table_Value */

/*******************************************************************************
* double Table_Value2d(t_Table Table, double X, double Y)
*   ACTION: read element [X,Y] of a matrix Table
*   input   Table: table containing data.
*           X : row index, may be non integer
*           Y : column index, may be non integer
*   return  Value = data[index X][index Y] with bi-linear interpolation
* Returns Value for the indexes [X,Y]
* Tests are performed (within Table_Index) on indexes i,j to avoid errors
* NOTE: data should rather be monotonic, and evenly sampled.
*******************************************************************************/
  double Table_Value2d(t_Table Table, double X, double Y)
  {
    double x1,x2,y1,y2,z11,z12,z21,z22;
    double ret=0;
    x1 = floor(X);
    y1 = floor(Y);
    if (x1 > Table.rows-1 || x1 < 0)    x2 = x1;
    else x2=x1+1;
    if (y1 > Table.columns-1 || y1 < 0) y2 = y1;
    else y2=y1+1;
    z11=Table_Index(Table, x1, y1);
    z12=Table_Index(Table, x1, y2);
    z21=Table_Index(Table, x2, y1);
    z22=Table_Index(Table, x2, y2);

    if (!strcmp(Table.method,"linear"))
      ret = Table_Interp2d(X,Y, x1,y1,x2,y2, z11,z12,z21,z22);
    else {
      if (fabs(X-x1) < fabs(X-x2)) {
        if (fabs(Y-y1) < fabs(Y-y2)) ret = z11; else ret = z12;
      } else {
        if (fabs(Y-y1) < fabs(Y-y2)) ret = z21; else ret = z22;
      }
    }
    return ret;
  } /* end Table_Value2d */


/*******************************************************************************
* void Table_Free(t_Table *Table)
*   ACTION: free a single Table
*   return: empty Table
*******************************************************************************/
  void Table_Free(t_Table *mc_rt_Table)
  {
    if (!mc_rt_Table) return;
    if (mc_rt_Table->data   != NULL) free(mc_rt_Table->data);
    if (mc_rt_Table->header != NULL) free(mc_rt_Table->header);
    mc_rt_Table->data   = NULL;
    mc_rt_Table->header = NULL;
  } /* end Table_Free */

/******************************************************************************
* void Table_Info(t_Table Table)
*    ACTION: print informations about a single Table
*******************************************************************************/
  long Table_Info(t_Table mc_rt_Table)
  {
    char mc_rt_buffer[256];
    long ret=0;

    if (!mc_rt_Table.block_number) strcpy(mc_rt_buffer, "catenated");
    else sprintf(mc_rt_buffer, "block %li", mc_rt_Table.block_number);
    printf("Table from file '%s' (%s)", mc_rt_Table.filename, mc_rt_buffer);
    if ((mc_rt_Table.data   != NULL) && (mc_rt_Table.rows*mc_rt_Table.columns))
    {
      printf(" is %li x %li ", mc_rt_Table.rows, mc_rt_Table.columns);
      if (mc_rt_Table.rows*mc_rt_Table.columns > 1)
        printf("(x=%g:%g)", mc_rt_Table.min_x, mc_rt_Table.max_x);
      else printf("(x=%g) ", mc_rt_Table.min_x);
      ret = mc_rt_Table.rows*mc_rt_Table.columns;
      if (mc_rt_Table.monotonic)    printf(", monotonic");
      if (mc_rt_Table.constantstep) printf(", constant step");
      printf(". interpolation: %s\n", mc_rt_Table.method);
    }
    else printf(" is empty.\n");

    if (mc_rt_Table.header && strlen(mc_rt_Table.header)) {
      char *header;
      int  i;
      header = (char *)malloc(80);
      if (!header) return(ret);
      for (i=0; i<80; header[i++]=0);
      if (strlen(mc_rt_Table.header) > 75) {
        strncpy(header, mc_rt_Table.header, 75-5);
        strcat( header, " ...");
      } else strcpy(header, mc_rt_Table.header);
      for (i=0; i<strlen(header); i++)
        if (header[i] == '\n' || header[i] == '\r') header[i] = ';';
      printf("  '%s'\n", header);
      free(header);
    }
    return(ret);
  } /* end Table_Info */

/******************************************************************************
* long Table_Init(t_Table *Table, m, n)
*   ACTION: initialise a Table to empty m by n table
*   return: empty Table
******************************************************************************/
long Table_Init(t_Table *mc_rt_Table, long rows, long columns)
{
  double *mc_rt_data=NULL;
  long   i;

  if (!mc_rt_Table) return(0);

  mc_rt_Table->header  = NULL;
  mc_rt_Table->filename[0]= '\0';
  mc_rt_Table->filesize= 0;
  mc_rt_Table->min_x   = 0;
  mc_rt_Table->max_x   = 0;
  mc_rt_Table->step_x  = 0;
  mc_rt_Table->block_number = 0;
  mc_rt_Table->array_length = 0;
  mc_rt_Table->monotonic    = 0;
  mc_rt_Table->constantstep = 0;
  mc_rt_Table->begin   = 0;
  mc_rt_Table->end     = 0;
  strcpy(mc_rt_Table->method,"linear");

  if (rows*columns >= 1) {
    mc_rt_data    = (double*)malloc(rows*columns*sizeof(double));
    if (mc_rt_data) for (i=0; i < rows*columns; mc_rt_data[i++]=0);
    else {
      fprintf(stderr,"Error: allocating %ld double elements."
                     "Too big (Table_Init).\n", rows*columns);
      rows = columns = 0;
    }
  }
  mc_rt_Table->rows    = (rows >= 1 ? rows : 0);
  mc_rt_Table->columns = (columns >= 1 ? columns : 0);
  mc_rt_Table->data    = mc_rt_data;
  return(mc_rt_Table->rows*mc_rt_Table->columns);
} /* end Table_Init */


/******************************************************************************
* void Table_Stat(t_Table *Table)
*   ACTION: computes min/max/mean step of 1st column for a single table (private)
*   return: updated Table
*******************************************************************************/
  static void Table_Stat(t_Table *mc_rt_Table)
  {
    long   mc_rt_i;
    double mc_rt_max_x, mc_rt_min_x;
    double mc_rt_row=1;
    char   mc_rt_monotonic=1;
    char   mc_rt_constantstep=1;
    double mc_rt_step=0;
    double mc_rt_n;

    if (!mc_rt_Table) return;
    if (!mc_rt_Table->rows || !mc_rt_Table->columns) return;
    if (mc_rt_Table->rows == 1) mc_rt_row=0;
    mc_rt_max_x = -FLT_MAX;
    mc_rt_min_x =  FLT_MAX;
    mc_rt_n     = (mc_rt_row ? mc_rt_Table->rows : mc_rt_Table->columns);

    /* get min and max of first column/vector */
    for (mc_rt_i=0; mc_rt_i < mc_rt_n; mc_rt_i++)
    {
      double mc_rt_X;
      mc_rt_X = (mc_rt_row ? Table_Index(*mc_rt_Table,mc_rt_i  ,0)
                               : Table_Index(*mc_rt_Table,0, mc_rt_i));
      if (mc_rt_X < mc_rt_min_x) mc_rt_min_x = mc_rt_X;
      if (mc_rt_X > mc_rt_max_x) mc_rt_max_x = mc_rt_X;
    } /* for */
    if (mc_rt_n > 1) {
      /* mean step */
      mc_rt_step = (mc_rt_max_x - mc_rt_min_x)/(mc_rt_n-1);
      /* now test if table is monotonic on first column, and get minimal step size */
      for (mc_rt_i=0; mc_rt_i < mc_rt_n-1; mc_rt_i++) {
        double mc_rt_X, mc_rt_diff;;
        mc_rt_X = (mc_rt_row ? Table_Index(*mc_rt_Table,mc_rt_i  ,0)
                            : Table_Index(*mc_rt_Table,0, mc_rt_i));
        mc_rt_diff         = Table_Index(*mc_rt_Table,mc_rt_i+1,0) - mc_rt_X;
        if (fabs(mc_rt_diff) < fabs(mc_rt_step)) mc_rt_step = mc_rt_diff;
        /* change sign ? */
        if ((mc_rt_Table->max_x - mc_rt_Table->min_x)*mc_rt_diff < 0 && mc_rt_monotonic)
          mc_rt_monotonic = 0;
      } /* end for */
      /* now test if steps are constant within READ_TABLE_STEPTOL */
      if (mc_rt_monotonic) {
      for (mc_rt_i=0; mc_rt_i < mc_rt_n-1; mc_rt_i++) {
        double mc_rt_X, mc_rt_diff;
        mc_rt_X = (mc_rt_row ? Table_Index(*mc_rt_Table,mc_rt_i  ,0)
                            : Table_Index(*mc_rt_Table,0, mc_rt_i));
        mc_rt_diff         = Table_Index(*mc_rt_Table,mc_rt_i+1,0) - mc_rt_X;
        if ( !(fabs(mc_rt_step)*(1-READ_TABLE_STEPTOL) < fabs(mc_rt_diff)
               && fabs(mc_rt_diff) < fabs(mc_rt_step)*(1+READ_TABLE_STEPTOL)) )
        { mc_rt_constantstep = 0; break; }
      }
}
    }
    mc_rt_Table->step_x= mc_rt_step;
    mc_rt_Table->max_x = mc_rt_max_x;
    mc_rt_Table->min_x = mc_rt_min_x;
    mc_rt_Table->monotonic = mc_rt_monotonic;
    mc_rt_Table->constantstep = mc_rt_constantstep;
  } /* end Table_Stat */

/******************************************************************************
* t_Table *Table_Read_Array(char *File, long *blocks)
*   ACTION: read as many data blocks as available, iteratively from file
*   return: initialized t_Table array, last element is an empty Table.
*           the number of extracted blocks in non NULL pointer *blocks
*******************************************************************************/
  t_Table *Table_Read_Array(char *mc_rt_File, long *mc_rt_blocks)
  {
    t_Table *mc_rt_Table_Array=NULL;
    long mc_rt_offset=0;
    long mc_rt_block_number=0;
    long mc_rt_allocated=256;
    long mc_rt_nelements=1;

    /* fisrt allocate an initial empty t_Table array */
    mc_rt_Table_Array = (t_Table *)malloc(mc_rt_allocated*sizeof(t_Table));
    if (!mc_rt_Table_Array) {
      fprintf(stderr, "Error: Can not allocate memory %li (Table_Read_Array).\n",
         mc_rt_allocated*sizeof(t_Table));
      *mc_rt_blocks = 0;
      return (NULL);
    }

    while (mc_rt_nelements > 0)
    {
      t_Table mc_rt_Table;

      /* access file at mc_rt_offset and get following block */
      mc_rt_nelements = Table_Read_Offset(&mc_rt_Table, mc_rt_File, 1,
      &mc_rt_offset,0);
      /* if ok, set t_Table block number else exit loop */
      mc_rt_block_number++;
      mc_rt_Table.block_number = mc_rt_block_number;
      /* if t_Table array is not long enough, expand and realocate */
      if (mc_rt_block_number >= mc_rt_allocated-1) {
        mc_rt_allocated += 256;
        mc_rt_Table_Array = (t_Table *)realloc(mc_rt_Table_Array,
           mc_rt_allocated*sizeof(t_Table));
        if (!mc_rt_Table_Array) {
          fprintf(stderr, "Error: Can not re-allocate memory %li (Table_Read_Array).\n",
              mc_rt_allocated*sizeof(t_Table));
          *mc_rt_blocks = 0;
          return (NULL);
        }
      }
      /* store it into t_Table array */
      mc_rt_Table_Array[mc_rt_block_number-1] = mc_rt_Table;
      /* continues until we find an empty block */
    }
    /* send back number of extracted blocks */
    if (mc_rt_blocks) *mc_rt_blocks = mc_rt_block_number-1;

    /* now store total number of elements in Table array */
    for (mc_rt_offset=0; mc_rt_offset < mc_rt_block_number;
      mc_rt_Table_Array[mc_rt_offset++].array_length = mc_rt_block_number-1);

    return(mc_rt_Table_Array);
  } /* end Table_Read_Array */
/*******************************************************************************
* void Table_Free_Array(t_Table *Table)
*   ACTION: free a Table array
*******************************************************************************/
  void Table_Free_Array(t_Table *mc_rt_Table)
  {
    long mc_rt_index=0;
    if (!mc_rt_Table) return;
    do {
        if (mc_rt_Table[mc_rt_index].data || mc_rt_Table[mc_rt_index].header)
          Table_Free(&mc_rt_Table[mc_rt_index]);
        else mc_rt_index=-1;
    } while (mc_rt_index>= 0);
    free(mc_rt_Table);
  } /* end Table_Free_Array */

/******************************************************************************
* long Table_Info_Array(t_Table *Table)
*    ACTION: print informations about a Table array
*    return: number of elements in the Table array
*******************************************************************************/
  long Table_Info_Array(t_Table *mc_rt_Table)
  {
    long mc_rt_index=0;

    if (!mc_rt_Table) return(-1);
    while (mc_rt_index < mc_rt_Table[mc_rt_index].array_length
       && (mc_rt_Table[mc_rt_index].data || mc_rt_Table[mc_rt_index].header)
       && (mc_rt_Table[mc_rt_index].rows*mc_rt_Table[mc_rt_index].columns) ) {
      Table_Info(mc_rt_Table[mc_rt_index]);
      mc_rt_index++;
    }
    printf("This Table array contains %li elements\n", mc_rt_index);
    return(mc_rt_index);
  } /* end Table_Info_Array */

/******************************************************************************
* char **Table_ParseHeaderchar *header, symbol1, symbol2, ..., NULL)
*    ACTION: search for char* symbols in header and return their value or NULL
*            Last argument MUST be NULL
*    return: array of char* with line following each symbol, or NULL if not found
*******************************************************************************/
#ifndef MyNL_ARGMAX
#define MyNL_ARGMAX 50
#endif

char **Table_ParseHeader(char *header, ...){
  va_list ap;
  char exit_flag=0;
  int counter=0;
  char **ret;

  if (!header) return(NULL);

  ret = (char**)calloc(MyNL_ARGMAX, sizeof(char*));
  if (!ret) {
    printf("Table_ParseHeader: Cannot allocate %i values array for Parser (Table_ParseHeader).\n",
      MyNL_ARGMAX);
    return(NULL);
  }

  va_start(ap, header);
  while(!exit_flag && counter < MyNL_ARGMAX-1)
  {
    char *arg_char;
    char *pos;
    /* get variable argument value as a char */
    arg_char = va_arg(ap, char *);
    ret[counter] = NULL;
    if (!arg_char){
      exit_flag = 1; break;
    }
    /* search for the symbol in the header */
    pos = strstr(header, arg_char);
    if (pos) {
      char *eol_pos;
      eol_pos = strchr(pos+strlen(arg_char), '\n');
      if (!eol_pos)
        eol_pos = strchr(pos+strlen(arg_char), '\r');
      if (!eol_pos) eol_pos = pos+strlen(pos)-1;
      ret[counter] = (char*)malloc(eol_pos - pos);
      if (!ret[counter]) {
        printf("Table_ParseHeader: Cannot allocate value[%i] array for Parser (Table_ParseHeader).\n",
          counter);
        exit_flag = 1; break;
      }
      strncpy(ret[counter], pos+strlen(arg_char), eol_pos - pos - strlen(arg_char));
      ret[counter][eol_pos - pos - strlen(arg_char)]='\0';
    }
    counter++;
  }
  va_end(ap);
  return(ret);
} /* Table_ParseHeader */

/******************************************************************************
* double Table_Interp1d(x, x1, y1, x2, y2)
*    ACTION: interpolates linearly at x between y1=f(x1) and y2=f(x2)
*    return: y=f(x) value
*******************************************************************************/
double Table_Interp1d(double x,
  double x1, double y1,
  double x2, double y2)
{
  double slope;
  if (x2 == x1) return (y1+y2)/2;
  if (y1 == y2) return  y1;
  slope = (y2 - y1)/(x2 - x1);
  return y1+slope*(x - x1);
} /* Table_Interp1d */

/******************************************************************************
* double Table_Interp1d_nearest(x, x1, y1, x2, y2)
*    ACTION: table lookup with nearest method at x between y1=f(x1) and y2=f(x2)
*    return: y=f(x) value
*******************************************************************************/
double Table_Interp1d_nearest(double x,
  double x1, double y1,
  double x2, double y2)
{
  if (fabs(x-x1) < fabs(x-x2)) return (y1);
  else return(y2);
} /* Table_Interp1d_nearest */

/******************************************************************************
* double Table_Interp2d(x,y, x1,y1, x2,y2, z11,z12,z21,z22)
*    ACTION: interpolates bi-linearly at (x,y) between z1=f(x1,y1) and z2=f(x2,y2)
*    return: z=f(x,y) value
*    x,y |   x1   x2
*    ----------------
*     y1 |   z11  z21
*     y2 |   z12  z22
*******************************************************************************/
double Table_Interp2d(double x, double y,
  double x1, double y1,
  double x2, double y2,
  double z11, double z12, double z21, double z22)
{
  double ratio_x, ratio_y;
  if (x2 == x1) return Table_Interp1d(y, y1,z11, y2,z12);
  if (y1 == y2) return Table_Interp1d(x, x1,z11, x2,z21);

  ratio_y = (y - y1)/(y2 - y1);
  ratio_x = (x - x1)/(x2 - x1);
  return (1-ratio_x)*(1-ratio_y)*z11 + ratio_x*(1-ratio_y)*z21
    + ratio_x*ratio_y*z22         + (1-ratio_x)*ratio_y*z12;
} /* Table_Interp2d */

/* end of read_table-lib.c */
