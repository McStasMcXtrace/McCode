/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/read_table-lib.c
*
* %Identification
* Written by: EF
* Date: Aug 28, 2002
* Origin: ILL
* Release: McStas 1.6
* Version: 1.1
*
* This file is to be imported by components that may read data from table files
* It handles some shared functions. Embedded within instrument in runtime mode.
* Variable names have prefix 'mc_rt_' for 'McStas Read Table' to avoid conflicts
*
* Usage: within SHARE
* %include "read_table-lib"
*
* $Id: read_table-lib.c,v 1.5 2003-01-21 08:47:03 pkwi Exp $
*
*	$Log: not supported by cvs2svn $
* Revision 1.1 2002/08/29 11:39:00 ef
*	Initial revision extracted from lib/optics/Monochromators...
*******************************************************************************/

#ifndef READ_TABLE_LIB_H
#error McStas : please import this library with %include "read_table-lib"
#endif

/*******************************************************************************
* long Read_Table(t_Table *Table, char *name, int block_number)
*   input   Table: pointer to a t_Table structure
*           name: file name from which table should be extracted
*           block_number: if the file does contain more than one
*                 data block, then indicates which one to get (from index 1)
*                 a 0 value means append/catenate all
*   return  modified Table t_Table structure containing data, header, ...
*           number of read elements (-1: error, 0:header only)
* The routine stores any line starting with '#', '%' and ';' into the header
* File is opened, read and closed
* Other lines are interpreted as numerical data, and stored.
* Data block should be a rectangular matrix or vector.
* Data block may be rebined with Table_Rebin (also sort in ascending order)
*******************************************************************************/
  long Table_Read(t_Table *mc_rt_Table, char *mc_rt_File, long mc_rt_block_number)
  { /* reads all/a data block in 'file' and returns a Table structure  */
    FILE *mc_rt_hfile;
    long  mc_rt_nelements;
    
    if (!mc_rt_File) return (-1);
    if (strlen(mc_rt_File) == 0) return (-1);
    mc_rt_hfile = fopen(mc_rt_File, "r");
    if(!mc_rt_hfile)
    {
       fprintf(stderr, "Error: Could not open input file '%s' (Table_Read)\n", mc_rt_File);
       return (-1);
    }
    mc_rt_nelements = Table_Read_Handle(mc_rt_Table, mc_rt_hfile, mc_rt_block_number);
    strncpy(mc_rt_Table->filename, mc_rt_File, 128);
    fclose(mc_rt_hfile); 
    return(mc_rt_nelements);
    
  } /* end Table_Read */
  
/*******************************************************************************
* long Read_Table_Handle(t_Table *Table, FILE *fid, int block_number)
*   input   Table:pointer to a t_Table structure
*           fid:  pointer to FILE handle
*           block_number: if the file does contain more than one
*                 data block, then indicates which one to get (from index 1)
*                 a 0 value means append/catenate all
*   return  modified Table t_Table structure containing data, header, ...
*           number of read elements (-1: error, 0:header only)
* The routine stores any line starting with '#', '%' and ';' into the header
* Other lines are interpreted as numerical data, and stored.
* Data block should be a rectangular matrix or vector.
* Data block may be rebined with Table_Rebin (also sort in ascending order)
*******************************************************************************/
  long Table_Read_Handle(t_Table *mc_rt_Table, FILE *mc_rt_hfile, long mc_rt_block_number)
  { /* reads all/a data block in 'file' and returns a Table structure  */
    double *mc_rt_Data;
    char *mc_rt_Header;
    long  mc_rt_malloc_size         = 1024;
    long  mc_rt_malloc_size_h       = 4096;
    char  mc_rt_flag_exit_loop      = 0;
    long  mc_rt_Rows = 0,   mc_rt_Columns = 0;
    long  mc_rt_count_in_array      = 0;
    long  mc_rt_count_in_header     = 0;
    long  mc_rt_cur_block_number    = 0;
    char  mc_rt_flag_in_array       = 0;
        
    Table_Init(mc_rt_Table);
    
    if(!mc_rt_hfile)
    {
       fprintf(stderr, "Error: File handle is NULL (Table_Read_Handle).\n");
       return (-1);
    }
    mc_rt_Header = (char*)  malloc(mc_rt_malloc_size_h*sizeof(char));
    mc_rt_Data   = (double*)malloc(mc_rt_malloc_size  *sizeof(double));
    if ((mc_rt_Header == NULL) || (mc_rt_Data == NULL))
    {
       fprintf(stderr, "Error: Could not allocate Table and Header (Table_Read_Handle).\n");
       return (-1);
    }
    mc_rt_Header[0] = '\0';

    while (!mc_rt_flag_exit_loop)
    {
      char  mc_rt_line[4096];

      if (fgets(mc_rt_line, 4096, mc_rt_hfile) != NULL)
      { /* tries to read some informations from the file */
        int mc_rt_i=0;
        /* first skip blank and tabulation characters */
        while (mc_rt_line[mc_rt_i] == ' ' || mc_rt_line[mc_rt_i] == '\t') mc_rt_i++;
        if ((mc_rt_line[mc_rt_i] == '#') || (mc_rt_line[mc_rt_i] == '%') 
        || (mc_rt_line[mc_rt_i] == ';') || (mc_rt_line[mc_rt_i] == '/'))
        { 
          if (mc_rt_flag_in_array && mc_rt_block_number)
            mc_rt_count_in_header = 0; /* comment comes after a data block */
          mc_rt_count_in_header += strlen(mc_rt_line);
          if (mc_rt_count_in_header+4096 > mc_rt_malloc_size_h)
          { /* if succeed and in array : add (and realloc if necessary) */
            mc_rt_malloc_size_h = mc_rt_count_in_header+4096;
            mc_rt_Header     = (char*)realloc(mc_rt_Header, mc_rt_malloc_size_h*sizeof(char));
          }
          strncat(mc_rt_Header, mc_rt_line, 4096); 
          mc_rt_flag_in_array  = 0; /* will start a new data block */
        } /* line is a comment */
        else
        {
          double mc_rt_X;
          
          /* get the number of columns splitting mc_rt_line with strtok */
          if (sscanf(mc_rt_line,"%lg ",&mc_rt_X) == 1) /* mc_rt_line begins at least with one num */
          { 
            char  *mc_rt_InputTokens, *mc_rt_lexeme;
            char   mc_rt_End_Line_Scanning_Flag= 0;
            long   mc_rt_This_Line_Columns     = 0;
            
            mc_rt_InputTokens            = mc_rt_line;

            while (!mc_rt_End_Line_Scanning_Flag)
            {
              mc_rt_lexeme      = (char *)strtok(mc_rt_InputTokens, " ,;\t\n\r");
              mc_rt_InputTokens = NULL;
              if ((mc_rt_lexeme != NULL) && (strlen(mc_rt_lexeme) != 0)) 
              {
                if (sscanf(mc_rt_lexeme,"%lg ",&mc_rt_X) == 1)  /* found a number */
                {
                  if (mc_rt_flag_in_array == 0 
                  && (((mc_rt_block_number == 0) || (mc_rt_block_number > mc_rt_cur_block_number)))) /* not already in a block -> start */
                  { /* starts a new data block */
                    if (mc_rt_block_number)
                    { /* initialise a new data block */
                      mc_rt_Rows = 0; 
                      mc_rt_count_in_array = 0; 
                    } /* else append */
                    mc_rt_cur_block_number++;
                    mc_rt_flag_in_array    = 1;
                    mc_rt_This_Line_Columns= 0; /* starts the first data row of this block */
                    
                  }
                  if (mc_rt_flag_in_array && ((mc_rt_block_number == 0) || (mc_rt_block_number == mc_rt_cur_block_number)))
                  { /* append all, or within requested block -> store data in row */
                    if (mc_rt_count_in_array >= mc_rt_malloc_size)
                    { /* if succeed and in array : add (and realloc if necessary) */
                      mc_rt_malloc_size = mc_rt_count_in_array+1024;
                      mc_rt_Data     = (double*)realloc(mc_rt_Data, mc_rt_malloc_size*sizeof(double));
                      if (mc_rt_Data == NULL)
                      {
                        fprintf(stderr, "Error: Can not allocate memory %i (Table_Read_Handle).\n");
       return (-1);
                      }
                    }
                    mc_rt_Data[mc_rt_count_in_array] = mc_rt_X;
                    mc_rt_count_in_array++;
                    mc_rt_This_Line_Columns++;
                    if (mc_rt_This_Line_Columns == 1) mc_rt_Rows++;
                  }
                  else 
                  { /* not in a block to store */
                    if ((mc_rt_block_number) && (mc_rt_cur_block_number > mc_rt_block_number)) 
                    { /* we finished to extract block -> force end of file reading */
                      mc_rt_End_Line_Scanning_Flag        = 1; 
                      mc_rt_flag_exit_loop = 1; 
                      mc_rt_flag_in_array  = 0; 
                    } 
                  }
                } /* end if sscanf mc_rt_lexeme -> numerical */
                else 
                { /* token s not numerical in that line */
                  mc_rt_End_Line_Scanning_Flag = 1; mc_rt_flag_in_array  = 0; 
                }
              }
              else 
              { /* no more tokens in mc_rt_line */
                mc_rt_End_Line_Scanning_Flag = 1;   
                if (mc_rt_This_Line_Columns) mc_rt_Columns = mc_rt_This_Line_Columns; 
              }
            } /* end while mc_rt_End_Line_Scanning_Flag */
          } 
          else
          { /* non-comment line does not begin with a number: ignore line */
            mc_rt_flag_in_array  = 0;
          }
        } /* end: if not mc_rt_line comment else numerical */
      } /* end: if fgets */
      else mc_rt_flag_exit_loop = 1; /* else fgets : end of file */
    } /* end while mc_rt_flag_exit_loop */

    mc_rt_Table->block_number = mc_rt_block_number;
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
      fprintf(stderr, "Warning: Read_Table : Data has %li values that should be %li x %li\n", mc_rt_count_in_array, mc_rt_Rows, mc_rt_Columns);
      mc_rt_Columns = mc_rt_count_in_array; mc_rt_Rows = 1;
    } 
    mc_rt_Data     = (double*)realloc(mc_rt_Data, mc_rt_count_in_array*sizeof(double));
    mc_rt_Table->data         = mc_rt_Data; 
    mc_rt_Table->rows         = mc_rt_Rows;
    mc_rt_Table->columns      = mc_rt_Columns;
    mc_rt_Table->min_x        = mc_rt_Data[0];
    mc_rt_Table->max_x        = mc_rt_Data[(mc_rt_Rows-1)*mc_rt_Columns];
    if (mc_rt_Rows != 0) mc_rt_Table->step_x = (mc_rt_Table->max_x - mc_rt_Table->min_x)/mc_rt_Rows;
    else mc_rt_Table->step_x = 0;
    return (mc_rt_count_in_array);
    
  } /* end Table_Read_Handle */  

/*******************************************************************************
* long Rebin_Table(t_Table *Table)
*   input   Table: table containing data
*   return  new Table with increasing, evenly spaced first column (index 0)
*           number of data elements (-1: error, 0:header only)
*******************************************************************************/
  long Table_Rebin(t_Table *mc_rt_Table)
  {
    double mc_rt_new_step=0;
    long   mc_rt_i;
    long   mc_rt_tmp;
    double mc_rt_max_x, mc_rt_min_x;
    char   mc_rt_monotonic = 1;
    /* performs linear interpolation on X axis (0-th column) */
    
    if (mc_rt_Table->data == NULL 
    || mc_rt_Table->rows*mc_rt_Table->columns == 0)
      return(0);
    mc_rt_tmp   = mc_rt_Table->rows;
    mc_rt_max_x = mc_rt_Table->max_x;
    mc_rt_min_x = mc_rt_Table->min_x;
    mc_rt_new_step = (mc_rt_max_x - mc_rt_min_x);
    for (mc_rt_i=0; mc_rt_i < mc_rt_Table->rows - 1; mc_rt_i++)
    {
      double mc_rt_current_step;
      double mc_rt_X, mc_rt_diff;
      mc_rt_X            = Table_Index(*mc_rt_Table,mc_rt_i  ,0);
      mc_rt_diff         = Table_Index(*mc_rt_Table,mc_rt_i+1,0) - mc_rt_X;
      mc_rt_current_step = fabs(mc_rt_diff);
      if ((mc_rt_Table->max_x - mc_rt_Table->min_x)*mc_rt_diff < 0 && mc_rt_monotonic && mc_rt_Table->columns > 1)
      {
        fprintf(stderr, "Warning: Rebin_Table : Data from file '%s' (%li x %li) is not monotonic (at row %li)\n", mc_rt_Table->filename, mc_rt_Table->rows, mc_rt_Table->columns, mc_rt_i);
        mc_rt_monotonic = 0;
      }
      if (mc_rt_current_step > 0 && mc_rt_current_step < mc_rt_new_step) mc_rt_new_step = mc_rt_current_step; 
      else mc_rt_tmp--;
      if (mc_rt_X < mc_rt_min_x) mc_rt_min_x = mc_rt_X;
      if (mc_rt_X > mc_rt_max_x) mc_rt_max_x = mc_rt_X;
    } /* for */
    mc_rt_Table->min_x = mc_rt_min_x;
    mc_rt_Table->max_x = mc_rt_max_x;
    if (fabs(mc_rt_new_step/mc_rt_Table->step_x) >= 0.98) 
      return (mc_rt_Table->rows*mc_rt_Table->columns);
    if (mc_rt_tmp > 0 && mc_rt_new_step > 0 && mc_rt_Table->columns > 1)  /* table was not already evenly sampled */
    {
      long mc_rt_Length_Table;
      double *mc_rt_New_Table;
      
      mc_rt_Length_Table = ceil(fabs(mc_rt_Table->max_x - mc_rt_Table->min_x)/mc_rt_new_step);
      mc_rt_New_Table    = (double*)malloc(mc_rt_Length_Table*mc_rt_Table->columns*sizeof(double));


      for (mc_rt_i=0; mc_rt_i < mc_rt_Length_Table; mc_rt_i++)
      {
        long   mc_rt_j;
        long   mc_rt_old_i;
        double mc_rt_X;
        double mc_rt_X1, mc_rt_X2, mc_rt_Y1, mc_rt_Y2;
        char   mc_rt_test=0;
        mc_rt_X = mc_rt_Table->min_x + mc_rt_i*mc_rt_new_step;
        mc_rt_New_Table[mc_rt_i*mc_rt_Table->columns] = mc_rt_X;
        /* look for index surrounding X in the old table -> index old_i, old-1 */
        
        for (mc_rt_old_i=1; mc_rt_old_i < mc_rt_Table->rows-1; mc_rt_old_i++)
        {
          mc_rt_X2 = Table_Index(*mc_rt_Table,mc_rt_old_i  ,0);
          mc_rt_X1 = Table_Index(*mc_rt_Table,mc_rt_old_i-1,0);
          if (mc_rt_Table->min_x < mc_rt_Table->max_x)
            mc_rt_test = ((mc_rt_X1 <= mc_rt_X) && (mc_rt_X < mc_rt_X2));
          else
            mc_rt_test = ((mc_rt_X2 <= mc_rt_X) && (mc_rt_X < mc_rt_X1));
          if (mc_rt_test) break;
        } 

        for (mc_rt_j=1; mc_rt_j < mc_rt_Table->columns; mc_rt_j++)
        {
          mc_rt_Y2 = Table_Index(*mc_rt_Table,mc_rt_old_i  ,mc_rt_j);
          mc_rt_Y1 = Table_Index(*mc_rt_Table,mc_rt_old_i-1,mc_rt_j);
          if (mc_rt_X2-mc_rt_X1)
          {    
          /* linear interpolation */
            double mc_rt_slope = (mc_rt_Y2-mc_rt_Y1)/(mc_rt_X2-mc_rt_X1);
            mc_rt_New_Table[mc_rt_i*mc_rt_Table->columns+mc_rt_j] = mc_rt_Y1+mc_rt_slope*(mc_rt_X-mc_rt_X1);
          }
          else
            mc_rt_New_Table[mc_rt_i*mc_rt_Table->columns+mc_rt_j] = mc_rt_Y2;
        }
        
      } /* end for  i */
      mc_rt_Table->rows = mc_rt_Length_Table;
      mc_rt_Table->step_x = mc_rt_new_step;
      free(mc_rt_Table->data);
      mc_rt_Table->data = mc_rt_New_Table;
    } /* end if tmp */
    return (mc_rt_Table->rows*mc_rt_Table->columns);
  } /* end Rebin_Table */
  
/*******************************************************************************
* double Table_Index(t_Table Table, long i, long j)
*   input   Table: table containing data
*           i : index of row      (0:mc_rt_Rows-1)
*           j : index of column   (0:Columns-1)
*   return  Value = data[i][j]
* Returns Value from the i-th row, j-th column of Table
* Tests are performed on indexes i,j to avoid errors
*******************************************************************************/
  double Table_Index(t_Table mc_rt_Table, long mc_rt_i, long mc_rt_j)
  {
    long mc_rt_AbsIndex;
    
    if (mc_rt_i < 0)        mc_rt_i = 0;
    if (mc_rt_i >= mc_rt_Table.rows)    mc_rt_i = mc_rt_Table.rows-1;
    if (mc_rt_j < 0)        mc_rt_j = 0;
    if (mc_rt_j >= mc_rt_Table.columns) mc_rt_j = mc_rt_Table.columns-1;
    mc_rt_AbsIndex = mc_rt_i*(mc_rt_Table.columns)+mc_rt_j;
    if (mc_rt_Table.data != NULL)
      return(mc_rt_Table.data[mc_rt_AbsIndex]);
    else
      return(0);
  }
  
/*******************************************************************************
* double Table_Value(t_Table Table, double X, long j)
*   input   Table: table containing data
*           X : data value in the first column (index 0)
*           j : index of column from which is extracted the Value (0:Columns-1)
*   return  Value = data[index for X][j]
* Returns Value from the j-th column of Table corresponding to the 
* X value for the 1st column (index 0)
* Tests are performed (within Table_Index) on indexes i,j to avoid errors
* NOTE: data should rather be mc_rt_monotonic, and evenly sampled.
*******************************************************************************/
  double Table_Value(t_Table mc_rt_Table, double X, long j)
  {
    long   mc_rt_Index;
    double mc_rt_Value;
    
    if (mc_rt_Table.step_x != 0)
      mc_rt_Index = floor((X - mc_rt_Table.min_x)/mc_rt_Table.step_x);
    else mc_rt_Index=0;
    mc_rt_Value = Table_Index(mc_rt_Table, mc_rt_Index, j);
    
    return(mc_rt_Value);
  }
/*******************************************************************************
* void Table_Free(t_Table *Table)
*******************************************************************************/
  void Table_Free(t_Table *mc_rt_Table)
  {
    if (mc_rt_Table->data   != NULL) free(mc_rt_Table->data);
    if (mc_rt_Table->header != NULL) free(mc_rt_Table->header);
    mc_rt_Table->data   = NULL;
    mc_rt_Table->header = NULL; 
  }
/******************************************************************************
* void Table_Info(t_Table Table)
*   prints informations about a Table
*******************************************************************************/
  void Table_Info(t_Table mc_rt_Table)
  {
    printf("Table from file '%s'", mc_rt_Table.filename);
    if (mc_rt_Table.block_number) printf(" (block %li)", mc_rt_Table.block_number);
    if ((mc_rt_Table.data   != NULL) && (mc_rt_Table.rows*mc_rt_Table.columns))
    {
      printf(" is %li x %li.\n", mc_rt_Table.rows, mc_rt_Table.columns);
      /* printf("Data axis range %f-%f, step=%f\n", mc_rt_Table.min_x, mc_rt_Table.max_x, mc_rt_Table.step_x); */
    }
    else printf(" is empty.\n");
  }

/******************************************************************************
* void Table_Init(t_Table *Table)
*   initialise a Table to empty
*******************************************************************************/  
  void Table_Init(t_Table *mc_rt_Table)
  {
    mc_rt_Table->data    = NULL;
    mc_rt_Table->header  = NULL;
    mc_rt_Table->filename[0]= '\0';
    mc_rt_Table->rows    = 0;
    mc_rt_Table->columns = 0;
    mc_rt_Table->min_x   = 0;
    mc_rt_Table->max_x   = 0;
    mc_rt_Table->step_x  = 0;
    mc_rt_Table->block_number = 0;
  }

/* end of read_table-lib.c */
