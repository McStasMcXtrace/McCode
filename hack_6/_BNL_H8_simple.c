/* Automatically generated file. Do not edit. 
 * Format:     ANSI C source code
 * Creator:    McStas <http://www.mcstas.org>
 * Instrument: BNL_H8_simple.instr (BNL_H8)
 * Date:       Fri Oct 18 11:57:15 2019
 * File:       BNL_H8_simple.c
 * CFLAGS=
 */

#define MCCODE_STRING "McStas 3.0-dev - Oct. 18, 2019"
#define FLAVOR        "mcstas"
#define FLAVOR_UPPER  "MCSTAS"

#define MC_USE_DEFAULT_MAIN
#define PI 3.14159265358979323846
#ifndef M_PI
#define M_PI PI
#endif
#ifndef M_2_PI
#define M_2_PI 0.63661977236758134308
#endif
#ifndef M_PI_2
#define M_PI_2 1.57079632679489661923  /* pi/2 */
#endif
#ifndef M_SQRT2
#define M_SQRT2 1.4142135623730951  /* sqrt(2) */
#endif

#ifdef USE_PGI
#undef MC_TRACE_ENABLED
#include <openacc_curand.h>
#endif

struct _struct_particle {
  double x,y,z; /* position [m] */
  double vx,vy,vz; /* velocity [m/s] */
  double sx,sy,sz; /* spin [0-1] */
#ifdef USE_PGI
  curandState_t MCRANDstate; /* CUDA RNG state */
#endif
  double t, p;    /* time, event weight */
  long long _uid;  /* event ID */
  long _index;     /* component index where to send this event */
  long _absorbed;  /* flag set to TRUE when this event is to be removed/ignored */
  long _scattered; /* flag set to TRUE when this event has interacted with the last component instance */
  long _restore;   /* set to true if neutron event must be restored */
};
typedef struct _struct_particle _class_particle;

#define MC_EMBEDDED_RUNTIME
#include "_mccode-r.h"
/* End of file "mccode-r.h". */

#include "_mcstas-r.h"
/* End of file "mcstas-r.h". */

#include "_mccode-r.c"
/* End of file "mccode-r.c". */

#include "_mcstas-r.c"
/* End of file "mcstas-r.c". */


/* *****************************************************************************
* Start of instrument 'BNL_H8' generated code
***************************************************************************** */

#ifdef MC_TRACE_ENABLED
int traceenabled = 1;
#else
int traceenabled = 0;
#endif
#define MCSTAS "/usr/share/mcstas/3.0-dev/"
int   defaultmain         = 1;
char  instrument_name[]   = "BNL_H8";
char  instrument_source[] = "BNL_H8_simple.instr";
char *instrument_exe      = NULL; /* will be set to argv[0] in main */
char  instrument_code[]   = "Instrument BNL_H8 source code BNL_H8_simple.instr is not embedded in this executable.\n  Use --source option when running McStas.\n";

int main(int argc, char *argv[]){return mccode_main(argc, argv);}

/* *****************************************************************************
* instrument 'BNL_H8' and components DECLARE
***************************************************************************** */

/* Instrument parameters: structure and a table for the initialisation
   (Used in e.g. inputparse and I/O function (e.g. detector_out) */

struct _struct_instrument_parameters {
  MCNUM _lambda;
};
typedef struct _struct_instrument_parameters _class_instrument_parameters;

struct _instrument_struct {
  char   _name[256]; /* the name of this instrument e.g. 'BNL_H8' */
/* Counters per component instance */
  double counter_AbsorbProp[30]; /* absorbed events in PROP routines */
  double counter_N[30], counter_P[30], counter_P2[30]; /* event counters after each component instance */
  _class_particle _trajectory[30]; /* current trajectory for STORE/RESTORE */
/* Components position table (absolute and relative coords) */
  Coords _position_relative[30]; /* positions of all components */
  Coords _position_absolute[30];
  _class_instrument_parameters _parameters; /* instrument parameters */
} _instrument_var;
struct _instrument_struct *instrument = & _instrument_var;
#pragma acc declare create ( _instrument_var )
#pragma acc declare create ( instrument )

int numipar = 1;
struct mcinputtable_struct mcinputtable[] = {
  "lambda", &(_instrument_var._parameters._lambda), instr_type_double, "2.36", 
  NULL, NULL, instr_type_double, ""
};


/* ************************************************************************** */
/*             SHARE user declarations for all components                     */
/* ************************************************************************** */

/* Shared user declarations for all components types 'Guide_simple'. */
  //%include "read_table-lib"
/*****************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2006, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/ref-lib.h
*
* %Identification
* Written by: Peter Christiansen
* Date: August, 2006
* Origin: RISOE
* Release: McStas 1.10
* Version: $Revision$
*
* Commonly used reflection functions are declared in this file which
* are used by some guide and mirror components.
*
* Depends on read_table-lib
*
* Usage: within SHARE
* %include "ref-lib"
*
****************************************************************************/
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
* Version: $Revision$
*
* This file is to be imported by components that may read data from table files
* It handles some shared functions.
*
* This library may be used directly as an external library. It has no dependency
*
* Usage: within SHARE
* %include "read_table-lib"
*
*******************************************************************************/

#ifndef READ_TABLE_LIB_H
#define READ_TABLE_LIB_H "$Revision$"

#define READ_TABLE_STEPTOL  0.04 /* tolerancy for constant step approx */

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
    char    filename[1024];
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

typedef struct t_Read_table_file_item {
    int ref_count;
    t_Table *table_ref;
} t_Read_table_file_item;

typedef enum enum_Read_table_file_actions {STORE,FIND,GC}  t_Read_table_file_actions;

/* read_table-lib function prototypes */
/* ========================================================================= */

/* 'public' functions */
long     Table_Read              (t_Table *Table, char *File, long block_number);
long     Table_Read_Offset       (t_Table *Table, char *File, long block_number,
                                  long *offset, long max_lines);
long     Table_Read_Offset_Binary(t_Table *Table, char *File, char *Type,
                                  long *Offset, long Rows, long Columns);
long     Table_Rebin(t_Table *Table); /* rebin table with regular 1st column and interpolate all columns 2:end */
long     Table_Info (t_Table Table);
double   Table_Index(t_Table Table,   long i, long j); /* get indexed value */
double   Table_Value(t_Table Table, double X, long j); /* search X in 1st column and return interpolated value in j-column */
t_Table *Table_Read_Array(char *File, long *blocks);
void     Table_Free_Array(t_Table *Table);
long     Table_Info_Array(t_Table *Table);
int      Table_SetElement(t_Table *Table, long i, long j, double value);
long     Table_Init(t_Table *Table, long rows, long columns); /* create a Table */
double   Table_Value2d(t_Table Table, double X, double Y);    /* same as Table_Index with non-integer indices and 2d interpolation */
MCDETECTOR Table_Write(t_Table Table, char*file, char*xl, char*yl, 
           double x1, double x2, double y1, double y2); /* write Table to disk */
void * Table_File_List_Handler(t_Read_table_file_actions action, void *item, void *item_modifier);
t_Table *Table_File_List_find(char *name, int block, int offset);
int Table_File_List_gc(t_Table *tab);
void *Table_File_List_store(t_Table *tab);

#define Table_ParseHeader(header, ...) \
  Table_ParseHeader_backend(header,__VA_ARGS__,NULL);

char **Table_ParseHeader_backend(char *header, ...);

/* private functions */
void Table_Free(t_Table *Table);
long Table_Read_Handle(t_Table *Table, FILE *fid, long block_number, long max_lines, char *name);
static void Table_Stat(t_Table *Table);
double Table_Interp1d(double x, double x1, double y1, double x2, double y2);
double Table_Interp1d_nearest(double x, double x1, double y1, double x2, double y2);
double Table_Interp2d(double x, double y, double x1, double y1, double x2, double y2,
double z11, double z12, double z21, double z22);

#endif

/* end of read_table-lib.h */
/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright (C) 1997-2009, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/read_table-lib.c
*
* %Identification
* Written by: EF
* Date: Aug 28, 2002
* Origin: ILL
* Release: McStas CVS_090504
* Version: $Revision$
*
* This file is to be imported by components that may read data from table files
* It handles some shared functions. Embedded within instrument in runtime mode.
*
* Usage: within SHARE
* %include "read_table-lib"
*
*******************************************************************************/

#ifndef READ_TABLE_LIB_H
#include "read_table-lib.h"
#endif


/*******************************************************************************
 * void *Table_File_List_Handler(action, item, item_modifier)
 *   ACTION: handle file entries in the read_table-lib file list. If a file is read - it is supposed to be
 *   stored in a list such that we can avoid reading the same file many times.
 *   input  action: FIND, STORE, GC. check if file exists in the list, store an item in the list, or check if it can be garbage collected.
 *   input item: depends on the action.
 *    FIND)  item is a filename, and item_modifier is the block number
 *    STORE) item is the Table to store - item_modifier is ignored
 *    GC)    item is the Table to check. If it has a ref_count >1 then this is simply decremented.
 *   return  depends on the action
 *    FIND)  return a reference to a table+ref_count item if found - NULL otherwise. I.e. NULL means the file has not been read before and must be read again.
 *    STORE) return NULL always
 *    GC)    return NULL if no garbage collection is needed, return an adress to the t_Table which should be garbage collected. 0x1 is returned if
 *           the item is not found in the list
*******************************************************************************/
void * Table_File_List_Handler(t_Read_table_file_actions action, void *item, void *item_modifier){

    /* logic here is Read_Table should include a call to FIND. If found the return value shoud just be used as
     * if the table had been read. If not found then read the table and STORE.
     * Table_Free should include a call to GC. If this returns non-NULL then we shoudl proceed with freeing the memory
     * associated with the table item - otherwise do nothing since there are more references that may need it.*/

    static t_Read_table_file_item read_table_file_list[1024];
    static int read_table_file_count=0;

    t_Read_table_file_item *tr;
    switch(action){
        case FIND:
            /*interpret data item as a filename, if it is found return a pointer to the table and increment refcount.
             * if not found return the item itself*/
            tr=read_table_file_list;
            while ( tr->table_ref!=NULL ){
                int i=*((int*) item_modifier);
                int j=*( ((int*) item_modifier)+1);
                if ( !strcmp(tr->table_ref->filename,(char *) item) &&
                        tr->table_ref->block_number==i && tr->table_ref->begin==j ){
                    tr->ref_count++;
                    return (void *) tr;
                }
                tr++;
            }
            return NULL;
        case STORE:
            /*find an available slot and store references to table there*/
            tr=&(read_table_file_list[read_table_file_count++]);
            tr->table_ref=(t_Table *)calloc(1,sizeof(t_Table));
            /*copy the contents of the table handle*/
            *(tr->table_ref)= *((t_Table *) item);
            tr->ref_count++;
            return NULL;
        case GC:
            /* Should this item be garbage collected (freed) - if so scratch the entry and return the address of the item -
             * else decrement ref_count and return NULL.
             * A non-NULL return expects the item to actually be freed afterwards.*/
            tr=read_table_file_list;
            while ( tr->table_ref!=NULL ){
                if ( tr->table_ref->data ==((t_Table *)item)->data &&
                        tr->table_ref->block_number == ((t_Table *)item)->block_number){
                    /*matching item found*/
                    if (tr->ref_count>1){
                        /*the item is found - no garbage collection needed*/
                        tr->ref_count--;
                        return NULL;
                    }else{
                        /* The item is found - move remaining list items up one slot,
                         * and return the table for garbage collection by caller*/
                        while (tr->table_ref!=NULL){
                            *tr=*(tr+1);
                            tr++;
                        }
                        read_table_file_count--;
                        return (t_Table *) item;
                    }
                }
                tr++;
            }
            return (void *)0x1 ;/*item not found*/
    }

}

/* Access functions to the handler*/

/********************************************
 * t_Table *Table_File_List_find(char *name, int block, int offset)
 * input name: filename to search for in the file list
 * input block: data block in the file as each file may contain more than 1 data block.
 * return a ref. to a table if it is found (you may use this pointer and skip reading the file), NULL otherwise (i.e. go ahead and read the file)
*********************************************/
t_Table *Table_File_List_find(char *name, int block, int offset){
    int vars[2]={block,offset};
    t_Read_table_file_item *item = Table_File_List_Handler(FIND,name, vars);
    if (item == NULL){
        return NULL;
    }else{
        return item->table_ref;
    }
}
/********************************************
 * int Table_File_List_gc(t_Table *tab)
 * input tab: the table to check for references.
 * return 0: no garbage collection needed
 *        1: Table's data and header (at least) should be freed.
*********************************************/
int Table_File_List_gc(t_Table *tab){
    void *rval=Table_File_List_Handler(GC,tab,0);
    if (rval==NULL) return 0;
    else return 1;
}


/*****************************************************************************
 * void *Table_File_List_store(t_Table *tab)
 * input tab: pointer to table to store.
 * return None.
*******************************************************************************/
void *Table_File_List_store(t_Table *tab){
    Table_File_List_Handler(STORE,tab,0);
}


/*******************************************************************************
* FILE *Open_File(char *name, char *Mode, char *path)
*   ACTION: search for a file and open it. Optionally return the opened path.
*   input   name:  file name from which table should be extracted
*           mode: "r", "w", "a" or any valid fopen mode
*           path:  NULL or a pointer to at least 1024 allocated chars
*   return  initialized file handle or NULL in case of error
*******************************************************************************/

  FILE *Open_File(char *File, const char *Mode, char *Path)
  {
    char path[1024];
    FILE *hfile = NULL;

    if (!File || File[0]=='\0')                     return(NULL);
    if (!strcmp(File,"NULL") || !strcmp(File,"0"))  return(NULL);

    /* search in current or full path */
    strncpy(path, File, 1024);
    hfile = fopen(path, Mode);
    if(!hfile)
    {
      char dir[1024];

      if (!hfile && instrument_source && strlen(instrument_source)) /* search in instrument source location */
      {
        char *path_pos   = NULL;
        /* extract path: searches for last file separator */
        path_pos    = strrchr(instrument_source, MC_PATHSEP_C);  /* last PATHSEP */
        if (path_pos) {
          long path_length = path_pos +1 - instrument_source;  /* from start to path+sep */
          if (path_length) {
            strncpy(dir, instrument_source, path_length);
            dir[path_length] = '\0';
            snprintf(path, 1024, "%s%c%s", dir, MC_PATHSEP_C, File);
            hfile = fopen(path, Mode);
          }
        }
      }
      if (!hfile && instrument_exe && strlen(instrument_exe)) /* search in PWD instrument executable location */
      {
        char *path_pos   = NULL;
        /* extract path: searches for last file separator */
        path_pos    = strrchr(instrument_exe, MC_PATHSEP_C);  /* last PATHSEP */
        if (path_pos) {
          long path_length = path_pos +1 - instrument_exe;  /* from start to path+sep */
          if (path_length) {
            strncpy(dir, instrument_exe, path_length);
            dir[path_length] = '\0';
            snprintf(path, 1024, "%s%c%s", dir, MC_PATHSEP_C, File);
            hfile = fopen(path, Mode);
          }
        }
      }
      if (!hfile) /* search in HOME or . */
      {
        strcpy(dir, getenv("HOME") ? getenv("HOME") : ".");
        snprintf(path, 1024, "%s%c%s", dir, MC_PATHSEP_C, File);
        hfile = fopen(path, Mode);
      }
      if (!hfile) /* search in MCSTAS/data */
      {
        strcpy(dir, getenv(FLAVOR_UPPER) ? getenv(FLAVOR_UPPER) : MCSTAS);
        snprintf(path, 1024, "%s%c%s%c%s", dir, MC_PATHSEP_C, "data", MC_PATHSEP_C, File);
        hfile = fopen(path, Mode);
      }
      if (!hfile) /* search in MVCSTAS/contrib */
      {
        strcpy(dir, getenv(FLAVOR_UPPER) ? getenv(FLAVOR_UPPER) : MCSTAS);
        snprintf(path, 1024, "%s%c%s%c%s", dir, MC_PATHSEP_C, "contrib", MC_PATHSEP_C, File);
        hfile = fopen(path, Mode);
      }
      if(!hfile)
      {
        fprintf(stderr, "Error: Could not open input file '%s' (Open_File)\n", File);
        return (NULL);
      }
    }
    if (Path) strncpy(Path, path, 1024);
    return(hfile);
  } /* end Open_File */

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
* Data block may be rebinned with Table_Rebin (also sort in ascending order)
*******************************************************************************/
  long Table_Read(t_Table *Table, char *File, long block_number)
  { /* reads all or a single data block from 'file' and returns a Table structure  */
    return(Table_Read_Offset(Table, File, block_number, NULL, 0));
  } /* end Table_Read */

/*******************************************************************************
* long Table_Read_Offset(t_Table *Table, char *name, int block_number, long *offset
*                        long max_rows)
*   ACTION: read a single Table from a text file, starting at offset
*     Same as Table_Read(..) except:
*   input   offset:    pointer to an offset (*offset should be 0 at start)
*           max_rows: max number of data rows to read from file (0 means all)
*   return  initialized single Table t_Table structure containing data, header, ...
*           number of read elements (-1: error, 0:header only)
*           updated *offset position (where end of reading occured)
*******************************************************************************/
  long Table_Read_Offset(t_Table *Table, char *File,
                         long block_number, long *offset,
                         long max_rows)
  { /* reads all/a data block in 'file' and returns a Table structure  */
    FILE *hfile;
    long  nelements=0;
    long  begin=0;
    long  filesize=0;
    char  name[1024];
    char  path[1024];
    struct stat stfile;

    /*Need to be able to store the pointer*/
    if (!Table) return(-1);

    //if (offset && *offset) snprintf(name, 1024, "%s@%li", File, *offset);
    //else
    strncpy(name, File, 1024);
    if(offset && *offset){
        begin=*offset;
    }
    /* Check if the table has already been read from file.
     * If so just reuse the table, if not (this is flagged by returning NULL
     * set up a new table and read the data into it */
    t_Table *tab_p= Table_File_List_find(name,block_number,begin);
    if ( tab_p!=NULL ){
        /*table was found in the Table_File_List*/
        // printf("Reusing input file '%s' (Table_Read_Offset)\n", name);
        *Table=*tab_p;
        return Table->rows*Table->columns;
    }

    /* open the file */
    hfile = Open_File(File, "r", path);
    if (!hfile) return(-1);
    else {
      MPI_MASTER(
      printf("Opening input file '%s' (Table_Read_Offset)\n", path);
      );
    }

    /* read file state */
    stat(path,&stfile); filesize = stfile.st_size;
    if (offset && *offset) fseek(hfile, *offset, SEEK_SET);
    begin     = ftell(hfile);

    Table_Init(Table, 0, 0);

    /* read file content and set the Table */
    nelements = Table_Read_Handle(Table, hfile, block_number, max_rows, name);
    Table->begin = begin;
    Table->end   = ftell(hfile);
    Table->filesize = (filesize>0 ? filesize : 0);
    Table_Stat(Table);

    Table_File_List_store(Table);

    if (offset) *offset=Table->end;
    fclose(hfile);
    return(nelements);

  } /* end Table_Read_Offset */

/*******************************************************************************
* long Table_Read_Offset_Binary(t_Table *Table, char *File, char *type,
*                               long *offset, long rows, long columns)
*   ACTION: read a single Table from a binary file, starting at offset
*     Same as Table_Read_Offset(..) except that it handles binary files.
*   input   type: may be "float"/NULL or "double"
*           offset: pointer to an offset (*offset should be 0 at start)
*           rows   : number of rows (0 means read all)
*           columns: number of columns
*   return  initialized single Table t_Table structure containing data, header, ...
*           number of read elements (-1: error, 0:header only)
*           updated *offset position (where end of reading occured)
*******************************************************************************/
  long Table_Read_Offset_Binary(t_Table *Table, char *File, char *type,
                                long *offset, long rows, long columns)
  { /* reads all/a data block in binary 'file' and returns a Table structure  */
    long    nelements, sizeofelement;
    long    filesize;
    FILE   *hfile;
    char    path[1024];
    struct stat stfile;
    double *data;
    long    i;
    long    begin;

    if (!Table) return(-1);

    Table_Init(Table, 0, 0);

    /* open the file */
    hfile = Open_File(File, "r", path);
    if (!hfile) return(-1);
    else {
      MPI_MASTER(
      printf("Opening input file '%s' (Table_Read, Binary)\n", path);
      );
    }

    /* read file state */
    stat(File,&stfile);
    filesize = stfile.st_size;
    Table->filesize=filesize;

    /* read file content */
    if (type && !strcmp(type,"double")) sizeofelement = sizeof(double);
    else  sizeofelement = sizeof(float);
    if (offset && *offset) fseek(hfile, *offset, SEEK_SET);
    begin     = ftell(hfile);
    if (rows && filesize > sizeofelement*columns*rows)
      nelements = columns*rows;
    else nelements = (long)(filesize/sizeofelement);
    if (!nelements || filesize <= *offset) return(0);
    data    = (double*)malloc(nelements*sizeofelement);
    if (!data) {
      fprintf(stderr,"Error: allocating %ld elements for %s file '%s'. Too big (Table_Read_Offset_Binary).\n", nelements, type, File);
      exit(-1);
    }
    nelements = fread(data, sizeofelement, nelements, hfile);

    if (!data || !nelements)
    {
      fprintf(stderr,"Error: reading %ld elements from %s file '%s' (Table_Read_Offset_Binary)\n", nelements, type, File);
      exit(-1);
    }
    Table->begin   = begin;
    Table->end     = ftell(hfile);
    if (offset) *offset=Table->end;
    fclose(hfile);
    data = (double*)realloc(data, (double)nelements*sizeofelement);
    /* copy file data into Table */
    if (type && !strcmp(type,"double")) Table->data = data;
    else {
      float  *s;
      double *dataf;
      s     = (float*)data;
      dataf = (double*)malloc(sizeof(double)*nelements);
      for (i=0; i<nelements; i++)
        dataf[i]=s[i];
      free(data);
      Table->data = dataf;
    }
    strncpy(Table->filename, File, 1024);
    Table->rows    = nelements/columns;
    Table->columns = columns;
    Table->array_length = 1;
    Table->block_number = 1;

    Table_Stat(Table);

    return(nelements);
  } /* end Table_Read_Offset_Binary */

/*******************************************************************************
* long Table_Read_Handle(t_Table *Table, FILE *fid, int block_number, long max_rows, char *name)
*   ACTION: read a single Table from a text file handle (private)
*   input   Table:pointer to a t_Table structure
*           fid:  pointer to FILE handle
*           block_number: if the file does contain more than one
*                 data block, then indicates which one to get (from index 1)
*                 a 0 value means append/catenate all
*           max_rows: if non 0, only reads that number of lines
*   return  initialized single Table t_Table structure containing data, header, ...
*           modified Table t_Table structure containing data, header, ...
*           number of read elements (-1: error, 0:header only)
* The routine stores any line starting with '#', '%' and ';' into the header
* Other lines are interpreted as numerical data, and stored.
* Data block should be a rectangular matrix or vector.
* Data block may be rebined with Table_Rebin (also sort in ascending order)
*******************************************************************************/
  long Table_Read_Handle(t_Table *Table, FILE *hfile,
                         long block_number, long max_rows, char *name)
  { /* reads all/a data block from 'file' handle and returns a Table structure  */
    double *Data;
    char *Header              = NULL;
    long  malloc_size         = CHAR_BUF_LENGTH;
    long  malloc_size_h       = 4096;
    long  Rows = 0,   Columns = 0;
    long  count_in_array      = 0;
    long  count_in_header     = 0;
    long  block_Current_index = 0;
    char  flag_End_row_loop   = 0;

    if (!Table) return(-1);
    Table_Init(Table, 0, 0);
    if (name && name[0]!='\0') strncpy(Table->filename, name, 1024);

    if(!hfile) {
       fprintf(stderr, "Error: File handle is NULL (Table_Read_Handle).\n");
       return (-1);
    }
    Header = (char*)  calloc(malloc_size_h, sizeof(char));
    Data   = (double*)calloc(malloc_size,   sizeof(double));
    if ((Header == NULL) || (Data == NULL)) {
       fprintf(stderr, "Error: Could not allocate Table and Header (Table_Read_Handle).\n");
       return (-1);
    }

    int flag_In_array = 0;
    do { /* while (!flag_End_row_loop) */
      char  line[1024*CHAR_BUF_LENGTH];
      long  back_pos=0;   /* ftell start of line */

      back_pos = ftell(hfile);
      if (fgets(line, 1024*CHAR_BUF_LENGTH, hfile) != NULL) { /* analyse line */
        /* first skip blank and tabulation characters */
        int i = strspn(line, " \t");

        /* handle comments: stored in header */
        if (NULL != strchr("#%;/", line[i]))
        { /* line is a comment */
          count_in_header += strlen(line);
          if (count_in_header >= malloc_size_h) {
            /* if succeed and in array : add (and realloc if necessary) */
            malloc_size_h = count_in_header+4096;
            Header        = (char*)realloc(Header, malloc_size_h*sizeof(char));
          }
          strncat(Header, line, 4096);
          flag_In_array=0;
          /* exit line and file if passed desired block */
          if (block_number > 0 && block_number == block_Current_index) {
            flag_End_row_loop = 1;
          }

          /* Continue with next line */
          continue;
        }

        /* get the number of columns splitting line with strtok */
        char  *lexeme;
        char  flag_End_Line = 0;
        long  block_Num_Columns = 0;
        const char seps[] = " ,;\t\n\r";

        lexeme = strtok(line, seps);
        while (!flag_End_Line) {
          if ((lexeme != NULL) && (lexeme[0] != '\0')) {
            /* reading line: the token is not empty */
            double X;
            int    count=1;
            /* test if we have 'NaN','Inf' */
            if (!strncasecmp(lexeme,"NaN",3))
              X = 0;
            else if (!strncasecmp(lexeme,"Inf",3) || !strncasecmp(lexeme,"+Inf",4))
              X = FLT_MAX;
            else if (!strncasecmp(lexeme,"-Inf",4))
              X = -FLT_MAX;
            else
              count = sscanf(lexeme,"%lg",&X);
            if (count == 1) {
              /* reading line: the token is a number in the line */
              if (!flag_In_array) {
                /* reading num: not already in a block: starts a new data block */
                block_Current_index++;
                flag_In_array    = 1;
                block_Num_Columns= 0;
                if (block_number > 0) {
                  /* initialise a new data block */
                  Rows = 0;
                  count_in_array = 0;
                } /* else append */
              }
              /* reading num: all blocks or selected block */
              if (flag_In_array && (block_number == 0 ||
                  block_number == block_Current_index)) {
                /* starting block: already the desired number of rows ? */
                if (block_Num_Columns == 0 &&
                    max_rows > 0 && Rows >= max_rows) {
                  flag_End_Line      = 1;
                  flag_End_row_loop  = 1;
                  flag_In_array      = 0;
                  /* reposition to begining of line (ignore line) */
                  fseek(hfile, back_pos, SEEK_SET);
                } else { /* store into data array */
                  if (count_in_array >= malloc_size) {
                    /* realloc data buffer if necessary */
                    malloc_size = count_in_array+CHAR_BUF_LENGTH;
                    Data = (double*) realloc(Data, malloc_size*sizeof(double));
                    if (Data == NULL) {
                      fprintf(stderr, "Error: Can not re-allocate memory %li (Table_Read_Handle).\n",
                              malloc_size*sizeof(double));
                      return (-1);
                    }
                  }
                  if (0 == block_Num_Columns) Rows++;
                  Data[count_in_array] = X;
                  count_in_array++;
                  block_Num_Columns++;
                }
              } /* reading num: end if flag_In_array */
            } /* end reading num: end if sscanf lexeme -> numerical */
            else {
              /* reading line: the token is not numerical in that line. end block */
              if (block_Current_index == block_number) {
                flag_End_Line = 1;
                flag_End_row_loop = 1;
              } else {
                flag_In_array = 0;
                flag_End_Line = 1;
              }
            }
          }
          else {
            /* no more tokens in line */
            flag_End_Line = 1;
            if (block_Num_Columns > 0) Columns = block_Num_Columns;
          }

          // parse next token
          lexeme = strtok(NULL, seps);

        } /* while (!flag_End_Line) */
      } /* end: if fgets */
      else flag_End_row_loop = 1; /* else fgets : end of file */

    } while (!flag_End_row_loop); /* end while flag_End_row_loop */

    Table->block_number = block_number;
    Table->array_length = 1;

    // shrink header to actual size (plus terminating 0-byte)
    if (count_in_header) {
      Header = (char*)realloc(Header, count_in_header*sizeof(char) + 1);
    }
    Table->header = Header;

    if (count_in_array*Rows*Columns == 0)
    {
      Table->rows         = 0;
      Table->columns      = 0;
      free(Data);
      return (0);
    }
    if (Rows * Columns != count_in_array)
    {
      fprintf(stderr, "Warning: Read_Table :%s %s Data has %li values that should be %li x %li\n",
        (Table->filename ? Table->filename : ""),
        (!block_number ? " catenated" : ""),
        count_in_array, Rows, Columns);
      Columns = count_in_array; Rows = 1;
    }
    Data     = (double*)realloc(Data, count_in_array*sizeof(double));
    Table->data         = Data;
    Table->rows         = Rows;
    Table->columns      = Columns;

    return (count_in_array);

  } /* end Table_Read_Handle */

/*******************************************************************************
* long Table_Rebin(t_Table *Table)
*   ACTION: rebin a single Table, sorting 1st column in ascending order
*   input   Table: single table containing data.
*                  The data block is reallocated in this process
*   return  updated Table with increasing, evenly spaced first column (index 0)
*           number of data elements (-1: error, 0:empty data)
*******************************************************************************/
  long Table_Rebin(t_Table *Table)
  {
    double new_step=0;
    long   i;
    /* performs linear interpolation on X axis (0-th column) */

    if (!Table) return(-1);
    if (!Table->data
    || Table->rows*Table->columns == 0 || !Table->step_x)
      return(0);
    Table_Stat(Table); /* recompute statitstics and minimal step */
    new_step = Table->step_x; /* minimal step in 1st column */

    if (!(Table->constantstep)) /* not already evenly spaced */
    {
      long Length_Table;
      double *New_Table;

      Length_Table = ceil(fabs(Table->max_x - Table->min_x)/new_step)+1;
      New_Table    = (double*)malloc(Length_Table*Table->columns*sizeof(double));

      for (i=0; i < Length_Table; i++)
      {
        long   j;
        double X;
        X = Table->min_x + i*new_step;
        New_Table[i*Table->columns] = X;
        for (j=1; j < Table->columns; j++)
          New_Table[i*Table->columns+j]
                = Table_Value(*Table, X, j);
      } /* end for i */

      Table->rows = Length_Table;
      Table->step_x = new_step;
      Table->max_x = Table->min_x + (Length_Table-1)*new_step;
      /*max might not be the same anymore
       * Use Length_Table -1 since the first and laset rows are the limits of the defined interval.*/
      free(Table->data);
      Table->data = New_Table;
      Table->constantstep=1;
    } /* end else (!constantstep) */
    return (Table->rows*Table->columns);
  } /* end Table_Rebin */

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

#ifndef MIN
#define MIN(a, b)  (((a) < (b)) ? (a) : (b))
#endif
#ifndef MAX
#define MAX(a, b)  (((a) > (b)) ? (a) : (b))
#endif

double Table_Index(t_Table Table, long i, long j)
{
  long AbsIndex;

  if (Table.rows == 1 || Table.columns == 1) {
    /* vector */
    j = MIN(MAX(0, i+j), Table.columns*Table.rows - 1);
    i = 0;
  } else {
    /* matrix */
    i = MIN(MAX(0, i), Table.rows - 1);
    j = MIN(MAX(0, j), Table.columns - 1);
  }

  /* handle vectors specifically */
  AbsIndex = i*(Table.columns)+j;

  if (Table.data != NULL)
    return (Table.data[AbsIndex]);
  else
    return 0;
} /* end Table_Index */

/*******************************************************************************
* void Table_SetElement(t_Table *Table, long i, long j, double value)
*   ACTION: set an element [i,j] of a single Table
*   input   Table: table containing data
*           i : index of row      (0:Rows-1)
*           j : index of column   (0:Columns-1)
*           value = data[i][j]
* Returns 0 in case of error
* Tests are performed on indexes i,j to avoid errors
*******************************************************************************/
int Table_SetElement(t_Table *Table, long i, long j,
                     double value)
{
  long AbsIndex;

  if (Table->rows == 1 || Table->columns == 1) {
    /* vector */
    j = MIN(MAX(0, i+j), Table->columns*Table->rows - 1); i=0;
  } else {
    /* matrix */
    i = MIN(MAX(0, i), Table->rows - 1);
    j = MIN(MAX(0, j), Table->columns - 1);
  }

  AbsIndex = i*(Table->columns)+j;
  if (Table->data != NULL) {
    Table->data[AbsIndex] = value;
    return 1;
  }

  return 0;
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
double Table_Value(t_Table Table, double X, long j)
{
  long   Index = -1;
  double X1=0, Y1=0, X2=0, Y2=0;
  double ret=0;

  if (X > Table.max_x) return Table_Index(Table,Table.rows-1  ,j);
  if (X < Table.min_x) return Table_Index(Table,0  ,j);

  // Use constant-time lookup when possible
  if(Table.constantstep) {
    Index = (long)floor(
              (X - Table.min_x) / (Table.max_x - Table.min_x) * (Table.rows-1));
    X1 = Table_Index(Table,Index  ,0);
    X2 = Table_Index(Table,Index+1,0);
  }
  // Use binary search on large, monotonic tables
  else if(Table.monotonic && Table.rows > 100) {
    long left = Table.min_x;
    long right = Table.max_x;

    while (!((X1 <= X) && (X < X2)) && (right - left > 1)) {
      Index = (left + right) / 2;

      X1 = Table_Index(Table, Index-1, 0);
      X2 = Table_Index(Table, Index,   0);

      if (X < X1) {
        right = Index;
      } else {
        left  = Index;
      }
    }
  }

  // Fall back to linear search, if no-one else has set X1, X2 correctly
  if (!((X1 <= X) && (X < X2))) {
    /* look for index surrounding X in the table -> Index */
    for (Index=1; Index <= Table.rows-1; Index++) {
        X1 = Table_Index(Table, Index-1,0);
        X2 = Table_Index(Table, Index  ,0);
        if ((X1 <= X) && (X < X2)) break;
      } /* end for Index */
  }

  Y1 = Table_Index(Table,Index-1,j);
  Y2 = Table_Index(Table,Index  ,j);

  if (!strcmp(Table.method,"linear")) {
    ret = Table_Interp1d(X, X1,Y1, X2,Y2);
  }
  else if (!strcmp(Table.method,"nearest")) {
    ret = Table_Interp1d_nearest(X, X1,Y1, X2,Y2);
  }

  return ret;
} /* end Table_Value */

/*******************************************************************************
* double Table_Value2d(t_Table Table, double X, double Y)
*   ACTION: read element [X,Y] of a matrix Table
*   input   Table: table containing data.
*           X : row index, may be non integer
*           Y : column index, may be non integer
*   return  Value = data[index X][index Y] with bi-linear interpolation
* Returns Value for the indices [X,Y]
* Tests are performed (within Table_Index) on indexes i,j to avoid errors
* NOTE: data should rather be monotonic, and evenly sampled.
*******************************************************************************/
  double Table_Value2d(t_Table Table, double X, double Y)
  {
    long   x1,x2,y1,y2;
    double z11,z12,z21,z22;
    double ret=0;

    x1 = (long)floor(X);
    y1 = (long)floor(Y);

    if (x1 > Table.rows-1 || x1 < 0) {
      x2 = x1;
    } else {
      x2 = x1 + 1;
    }

    if (y1 > Table.columns-1 || y1 < 0) {
      y2 = y1;
    } else {
      y2 = y1 + 1;
    }

    z11 = Table_Index(Table, x1, y1);

    if (y2 != y1) z12=Table_Index(Table, x1, y2); else z12 = z11;
    if (x2 != x1) z21=Table_Index(Table, x2, y1); else z21 = z11;
    if (y2 != y1) z22=Table_Index(Table, x2, y2); else z22 = z21;

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
  void Table_Free(t_Table *Table)
  {
    if( !Table_File_List_gc(Table) ){
       return;
    }
    if (!Table) return;
    if (Table->data   != NULL) free(Table->data);
    if (Table->header != NULL) free(Table->header);
    Table->data   = NULL;
    Table->header = NULL;
  } /* end Table_Free */

/******************************************************************************
* void Table_Info(t_Table Table)
*    ACTION: print informations about a single Table
*******************************************************************************/
  long Table_Info(t_Table Table)
  {
    char buffer[256];
    long ret=0;

    if (!Table.block_number) strcpy(buffer, "catenated");
    else sprintf(buffer, "block %li", Table.block_number);
    printf("Table from file '%s' (%s)",
      Table.filename ? Table.filename : "", buffer);
    if ((Table.data != NULL) && (Table.rows*Table.columns))
    {
      printf(" is %li x %li ", Table.rows, Table.columns);
      if (Table.rows*Table.columns > 1)
        printf("(x=%g:%g)", Table.min_x, Table.max_x);
      else printf("(x=%g) ", Table.min_x);
      ret = Table.rows*Table.columns;
      if (Table.monotonic)    printf(", monotonic");
      if (Table.constantstep) printf(", constant step");
      printf(". interpolation: %s\n", Table.method);
    }
    else printf(" is empty.\n");

    if (Table.header && strlen(Table.header)) {
      char *header;
      int  i;
      header = malloc(80);
      if (!header) return(ret);
      for (i=0; i<80; header[i++]=0);
      strncpy(header, Table.header, 75);
      if (strlen(Table.header) > 75) {
        strcat( header, " ...");
      }
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
long Table_Init(t_Table *Table, long rows, long columns)
{
  double *data=NULL;
  long   i;

  if (!Table) return(0);

  Table->header  = NULL;
  Table->filename[0]= '\0';
  Table->filesize= 0;
  Table->min_x   = 0;
  Table->max_x   = 0;
  Table->step_x  = 0;
  Table->block_number = 0;
  Table->array_length = 0;
  Table->monotonic    = 0;
  Table->constantstep = 0;
  Table->begin   = 0;
  Table->end     = 0;
  strcpy(Table->method,"linear");

  if (rows*columns >= 1) {
    data    = (double*)malloc(rows*columns*sizeof(double));
    if (data) for (i=0; i < rows*columns; data[i++]=0);
    else {
      fprintf(stderr,"Error: allocating %ld double elements."
                     "Too big (Table_Init).\n", rows*columns);
      rows = columns = 0;
    }
  }
  Table->rows    = (rows >= 1 ? rows : 0);
  Table->columns = (columns >= 1 ? columns : 0);
  Table->data    = data;
  return(Table->rows*Table->columns);
} /* end Table_Init */

/******************************************************************************
* long Table_Write(t_Table Table, char *file, x1,x2, y1,y2)
*   ACTION: write a Table to disk (ascii).
*     when x1=x2=0 or y1=y2=0, the table default limits are used.
*   return: 0=all is fine, non-0: error
*******************************************************************************/
MCDETECTOR Table_Write(t_Table Table, char *file, char *xl, char *yl,
  double x1, double x2, double y1, double y2)
{
  long    i =0;
  MCDETECTOR detector;

  if ((Table.data == NULL) && (Table.rows*Table.columns)) {
    detector.m = 0;
    return(detector); /* Table is empty - nothing to do */
  }
  if (!x1 && !x2) {
    x1 = Table.min_x;
    x2 = Table.max_x;
  }
  if (!y1 && !y2) {
    y1 = 1;
    y2 = Table.columns;
  }

  /* transfer content of the Table into a 2D detector */
  Coords coords = { 0, 0, 0};

  if (Table.rows == 1 || Table.columns == 1) {
    detector = mcdetector_out_1D(Table.filename,
                      xl ? xl : "", yl ? yl : "",
                      "x", x1, x2,
                      Table.rows * Table.columns,
                      NULL, Table.data, NULL,
                      file, file, coords);
  } else {
    detector = mcdetector_out_2D(Table.filename,
                      xl ? xl : "", yl ? yl : "",
                      x1, x2, y1, y2,
                      Table.rows, Table.columns,
                      NULL, Table.data, NULL,
                      file, file, coords);
  }
  return(detector);
}

/******************************************************************************
* void Table_Stat(t_Table *Table)
*   ACTION: computes min/max/mean step of 1st column for a single table (private)
*   return: updated Table
*******************************************************************************/
  static void Table_Stat(t_Table *Table)
  {
    long   i;
    double max_x, min_x;
    double row=1;
    char   monotonic=1;
    char   constantstep=1;
    double step=0;
    long n;

    if (!Table) return;
    if (!Table->rows || !Table->columns) return;
    if (Table->rows == 1) row=0; // single row
    max_x = -FLT_MAX;
    min_x =  FLT_MAX;
    n     = (row ? Table->rows : Table->columns);
    /* get min and max of first column/vector */
    for (i=0; i < n; i++)
    {
      double X;
      X = (row ? Table_Index(*Table,i  ,0)
                               : Table_Index(*Table,0, i));
      if (X < min_x) min_x = X;
      if (X > max_x) max_x = X;
    } /* for */

    /* test for monotonicity and constant step if the table is an XY or single vector */
    if (n > 1) {
      /* mean step */
      step = (max_x - min_x)/(n-1);
      /* now test if table is monotonic on first column, and get minimal step size */
      for (i=0; i < n-1; i++) {
        double X, diff;;
        X    = (row ? Table_Index(*Table,i  ,0)
                    : Table_Index(*Table,0,  i));
        diff = (row ? Table_Index(*Table,i+1,0)
                    : Table_Index(*Table,0,  i+1)) - X;
        if (diff && fabs(diff) < fabs(step)) step = diff;
        /* change sign ? */
        if ((max_x - min_x)*diff < 0 && monotonic)
          monotonic = 0;
      } /* end for */

      /* now test if steps are constant within READ_TABLE_STEPTOL */
      if(!step){
        /*means there's a disconitnuity -> not constantstep*/
        constantstep=0;
      }else if (monotonic) {
        for (i=0; i < n-1; i++) {
          double X, diff;
          X    = (row ? Table_Index(*Table,i  ,0)
              : Table_Index(*Table,0,  i));
          diff = (row ? Table_Index(*Table,i+1,0)
              : Table_Index(*Table,0,  i+1)) - X;
          if ( fabs(step)*(1+READ_TABLE_STEPTOL) < fabs(diff) ||
                fabs(diff) < fabs(step)*(1-READ_TABLE_STEPTOL) )
          { constantstep = 0; break; }
        }
      }

    }
    Table->step_x= step;
    Table->max_x = max_x;
    Table->min_x = min_x;
    Table->monotonic = monotonic;
    Table->constantstep = constantstep;
  } /* end Table_Stat */

/******************************************************************************
* t_Table *Table_Read_Array(char *File, long *blocks)
*   ACTION: read as many data blocks as available, iteratively from file
*   return: initialized t_Table array, last element is an empty Table.
*           the number of extracted blocks in non NULL pointer *blocks
*******************************************************************************/
  t_Table *Table_Read_Array(char *File, long *blocks)
  {
    t_Table *Table_Array=NULL;
    long offset=0;
    long block_number=0;
    long allocated=256;
    long nelements=1;

    /* fisrt allocate an initial empty t_Table array */
    Table_Array = (t_Table *)malloc(allocated*sizeof(t_Table));
    if (!Table_Array) {
      fprintf(stderr, "Error: Can not allocate memory %li (Table_Read_Array).\n",
         allocated*sizeof(t_Table));
      *blocks = 0;
      return (NULL);
    }

    while (nelements > 0)
    {
      t_Table Table;

      /* if ok, set t_Table block number else exit loop */
      block_number++;
      Table.block_number = block_number;

      /* access file at offset and get following block. Block number is from the set offset
       * hence the hardcoded 1 - i.e. the next block counted from offset.*/
      nelements = Table_Read_Offset(&Table, File, 1, &offset,0);
      /* if t_Table array is not long enough, expand and realocate */
      if (block_number >= allocated-1) {
        allocated += 256;
        Table_Array = (t_Table *)realloc(Table_Array,
           allocated*sizeof(t_Table));
        if (!Table_Array) {
          fprintf(stderr, "Error: Can not re-allocate memory %li (Table_Read_Array).\n",
              allocated*sizeof(t_Table));
          *blocks = 0;
          return (NULL);
        }
      }
      /* store it into t_Table array */
      //snprintf(Table.filename, 1024, "%s#%li", File, block_number-1);
      Table_Array[block_number-1] = Table;
      /* continues until we find an empty block */
    }
    /* send back number of extracted blocks */
    if (blocks) *blocks = block_number-1;

    /* now store total number of elements in Table array */
    for (offset=0; offset < block_number;
      Table_Array[offset++].array_length = block_number-1);

    return(Table_Array);
  } /* end Table_Read_Array */
/*******************************************************************************
* void Table_Free_Array(t_Table *Table)
*   ACTION: free a Table array
*******************************************************************************/
  void Table_Free_Array(t_Table *Table)
  {
    long index=0;
    if (!Table) return;
    while (Table[index].data || Table[index].header){
            Table_Free(&Table[index]);
            index++;
    }
    free(Table);
  } /* end Table_Free_Array */

/******************************************************************************
* long Table_Info_Array(t_Table *Table)
*    ACTION: print informations about a Table array
*    return: number of elements in the Table array
*******************************************************************************/
  long Table_Info_Array(t_Table *Table)
  {
    long index=0;

    if (!Table) return(-1);
    while (index < Table[index].array_length
       && (Table[index].data || Table[index].header)
       && (Table[index].rows*Table[index].columns) ) {
      Table_Info(Table[index]);
      index++;
    }
    printf("This Table array contains %li elements\n", index);
    return(index);
  } /* end Table_Info_Array */

/******************************************************************************
* char **Table_ParseHeader(char *header, symbol1, symbol2, ..., NULL)
*    ACTION: search for char* symbols in header and return their value or NULL
*            the search is not case sensitive.
*            Last argument MUST be NULL
*    return: array of char* with line following each symbol, or NULL if not found
*******************************************************************************/
#ifndef MyNL_ARGMAX
#define MyNL_ARGMAX 50
#endif

char **Table_ParseHeader_backend(char *header, ...){
  va_list ap;
  char exit_flag=0;
  int counter   =0;
  char **ret    =NULL;
  if (!header || header[0]=='\0') return(NULL);

  ret = (char**)calloc(MyNL_ARGMAX, sizeof(char*));
  if (!ret) {
    printf("Table_ParseHeader: Cannot allocate %i values array for Parser (Table_ParseHeader).\n",
      MyNL_ARGMAX);
    return(NULL);
  }
  for (counter=0; counter < MyNL_ARGMAX; ret[counter++] = NULL);
  counter=0;

  va_start(ap, header);
  while(!exit_flag && counter < MyNL_ARGMAX-1)
  {
    char *arg_char=NULL;
    char *pos     =NULL;
    /* get variable argument value as a char */
    arg_char = va_arg(ap, char *);
    if (!arg_char || arg_char[0]=='\0'){
      exit_flag = 1; break;
    }
    /* search for the symbol in the header */
    pos = (char*)strcasestr(header, arg_char);
    if (pos) {
      char *eol_pos;
      eol_pos = strchr(pos+strlen(arg_char), '\n');
      if (!eol_pos)
        eol_pos = strchr(pos+strlen(arg_char), '\r');
      if (!eol_pos)
        eol_pos = pos+strlen(pos)-1;
      ret[counter] = (char*)malloc(eol_pos - pos);
      if (!ret[counter]) {
        printf("Table_ParseHeader: Cannot allocate value[%i] array for Parser searching for %s (Table_ParseHeader).\n",
          counter, arg_char);
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


#ifndef REF_LIB_H
#define REF_LIB_H "$Revision$"

void StdReflecFunc(double, double*, double*);
void TableReflecFunc(double, t_Table*, double*);

#endif

/* end of ref-lib.h */
/****************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2006, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/ref-lib.c
*
* %Identification
* Written by: Peter Christiansen
* Date: August, 2006
* Origin: RISOE
* Release: McStas 1.10
* Version: $Revision$
*
* Commonly used reflection functions are declared in this file which
* are used by some guide and mirror components.
*
* Variable names have prefix 'mc_ref_' for 'McStas Reflection' 
* to avoid conflicts
*
* Usage: within SHARE
* %include "ref-lib"
*
****************************************************************************/

#ifndef REF_LIB_H
#include "ref-lib.h"
#endif

#ifndef READ_TABLE_LIB_H
#include "read_table-lib.h"
#include "read_table-lib.c"
#endif

/****************************************************************************
* void StdReflecFunc(double q, double *par, double *r)
* 
* The McStas standard analytic parametrization of the reflectivity.
* The parameters are:
* R0:      [1]    Low-angle reflectivity
* Qc:      [AA-1] Critical scattering vector
* alpha:   [AA]   Slope of reflectivity
* m:       [1]    m-value of material. Zero means completely absorbing.
* W:       [AA-1] Width of supermirror cut-off
*****************************************************************************/
#pragma acc routine seq nohost
void StdReflecFunc(double mc_pol_q, double *mc_pol_par, double *mc_pol_r) {
    double R0    = mc_pol_par[0];
    double Qc    = mc_pol_par[1];
    double alpha = mc_pol_par[2];
    double m     = mc_pol_par[3];
    double W     = mc_pol_par[4];
    double beta  = 0;
    mc_pol_q     = fabs(mc_pol_q);
    double arg;
        
    /* Simpler parametrization from Henrik Jacobsen uses these values that depend on m only.
       double m_value=m*0.9853+0.1978;
       double W=-0.0002*m_value+0.0022;
       double alpha=0.2304*m_value+5.0944;
       double beta=-7.6251*m_value+68.1137; 
       If W and alpha are set to 0, use Henrik's approach for estimating these parameters
       and apply the formulation:
       arg = R0*0.5*(1-tanh(arg))*(1-alpha*(q-Qc)+beta*(q-Qc)*(q-Qc));
    */  
    if (W==0 && alpha==0) {
      m=m*0.9853+0.1978;
      W=-0.0002*m+0.0022;
      alpha=0.2304*m+5.0944;
      beta=-7.6251*m+68.1137;
      if (m<=3) {
	alpha=m;
	beta=0;
      }
    }
    
    arg = W > 0 ? (mc_pol_q - m*Qc)/W : 11;

    if (arg > 10 || m <= 0 || Qc <=0 || R0 <= 0) {
      *mc_pol_r = 0;
      return;
    }
    
    if (m < 1) { Qc *= m; m=1; }
    
    if(mc_pol_q <= Qc) {      
      *mc_pol_r = R0;
      return;
    }
    
    
    *mc_pol_r = R0*0.5*(1 - tanh(arg))*(1 - alpha*(mc_pol_q - Qc) + beta*(mc_pol_q - Qc)*(mc_pol_q - Qc));
    
    return;
  }

/****************************************************************************
* void TableReflecFunc(double q, t_Table *par, double *r) {
* 
* Looks up the reflectivity in a table using the routines in read_table-lib.
*****************************************************************************/
void TableReflecFunc(double mc_pol_q, t_Table *mc_pol_par, double *mc_pol_r) {
    
  *mc_pol_r = Table_Value(*mc_pol_par, mc_pol_q, 1);
  if(*mc_pol_r>1)
    *mc_pol_r = 1;
  return;
}

/* end of ref-lib.c */


/* Shared user declarations for all components types 'Monochromator_flat'. */
#ifndef GAUSS
/* Define these arrays only once for all instances. */
/* Values for Gauss quadrature. Taken from Brice Carnahan, H. A. Luther and
James O Wilkes, "Applied numerical methods", Wiley, 1969, page 103.
This reference is available from the Copenhagen UB2 library */
double Gauss_X[] = {-0.987992518020485, -0.937273392400706, -0.848206583410427,
-0.724417731360170, -0.570972172608539, -0.394151347077563,
-0.201194093997435, 0, 0.201194093997435,
0.394151347077563, 0.570972172608539, 0.724417731360170,
0.848206583410427, 0.937273392400706, 0.987992518020485};
double Gauss_W[] = {0.030753241996117, 0.070366047488108, 0.107159220467172,
0.139570677926154, 0.166269205816994, 0.186161000115562,
0.198431485327111, 0.202578241925561, 0.198431485327111,
0.186161000115562, 0.166269205816994, 0.139570677926154,
0.107159220467172, 0.070366047488108, 0.030753241996117};
#pragma acc declare create ( Gauss_X )
#pragma acc declare create ( Gauss_W )

#define GAUSS(x,mean,rms) \
  (exp(-((x)-(mean))*((x)-(mean))/(2*(rms)*(rms)))/(sqrt(2*PI)*(rms)))
#endif

/* Shared user declarations for all components types 'V_sample'. */
struct StructVarsV
{
double  sigma_a; /* Absorption cross section per atom (barns) */
    double  sigma_i; /* Incoherent scattering cross section per atom (barns) */
    double  rho;     /* Density of atoms (AA-3) */
    double  my_s;
    double  my_a_v;
    int     shapetyp;    /* 0 double well cylynder, 1 box,  3 sphere */
    double  distance;    /* when non zero, gives rect target distance */
    double  aw,ah;       /* rectangular angular dimensions */
    double  xw,yh;       /* rectangular metrical dimensions */
    double  tx,ty,tz;    /* target coords */
  };


/* ************************************************************************** */
/*             End of SHARE user declarations for all components              */
/* ************************************************************************** */


/* ********************** component definition declarations. **************** */

/* component Origin=Arm() [1] DECLARE */
/* Parameter definition for component type 'Arm' */
struct _struct_Arm_parameters {
  char Arm_has_no_parameters;
}; /* _struct_Arm_parameters */
typedef struct _struct_Arm_parameters _class_Arm_parameters;

/* Parameters for component type 'Arm' */
struct _struct_Arm {
  char     _name[256]; /* e.g. Origin */
  char     _type[256]; /* Arm */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_Arm_parameters _parameters;
};
typedef struct _struct_Arm _class_Arm;
_class_Arm _Origin_var;
#pragma acc declare create ( _Origin_var )

/* component Source=Source_simple() [2] DECLARE */
/* Parameter definition for component type 'Source_simple' */
struct _struct_Source_simple_parameters {
  /* Component type 'Source_simple' setting parameters */
  MCNUM radius;
  MCNUM yheight;
  MCNUM xwidth;
  MCNUM dist;
  MCNUM focus_xw;
  MCNUM focus_yh;
  MCNUM E0;
  MCNUM dE;
  MCNUM lambda0;
  MCNUM dlambda;
  MCNUM flux;
  MCNUM gauss;
  long target_index;
  /* Component type 'Source_simple' private parameters */
  /* Component type 'Source_simple' DECLARE code stored as structure members */
double pmul, srcArea;
int square;
}; /* _struct_Source_simple_parameters */
typedef struct _struct_Source_simple_parameters _class_Source_simple_parameters;

/* Parameters for component type 'Source_simple' */
struct _struct_Source_simple {
  char     _name[256]; /* e.g. Source */
  char     _type[256]; /* Source_simple */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_Source_simple_parameters _parameters;
};
typedef struct _struct_Source_simple _class_Source_simple;
_class_Source_simple _Source_var;
#pragma acc declare create ( _Source_var )

/* component D0_Source=PSD_monitor() [3] DECLARE */
/* Parameter definition for component type 'PSD_monitor' */
struct _struct_PSD_monitor_parameters {
  /* Component type 'PSD_monitor' setting parameters */
  MCNUM nx;
  MCNUM ny;
  char filename[16384];
  MCNUM xmin;
  MCNUM xmax;
  MCNUM ymin;
  MCNUM ymax;
  MCNUM xwidth;
  MCNUM yheight;
  MCNUM restore_neutron;
  /* Component type 'PSD_monitor' private parameters */
  /* Component type 'PSD_monitor' DECLARE code stored as structure members */
  DArray2d PSD_N;
  DArray2d PSD_p;
  DArray2d PSD_p2;
}; /* _struct_PSD_monitor_parameters */
typedef struct _struct_PSD_monitor_parameters _class_PSD_monitor_parameters;

/* Parameters for component type 'PSD_monitor' */
struct _struct_PSD_monitor {
  char     _name[256]; /* e.g. D0_Source */
  char     _type[256]; /* PSD_monitor */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_PSD_monitor_parameters _parameters;
};
typedef struct _struct_PSD_monitor _class_PSD_monitor;
_class_PSD_monitor _D0_Source_var;
#pragma acc declare create ( _D0_Source_var )

/* component SC1=Guide_simple() [4] DECLARE */
/* Parameter definition for component type 'Guide_simple' */
struct _struct_Guide_simple_parameters {
  /* Component type 'Guide_simple' setting parameters */
  MCNUM w1;
  MCNUM h1;
  MCNUM w2;
  MCNUM h2;
  MCNUM l;
  MCNUM R0;
  MCNUM Qc;
  MCNUM alpha;
  MCNUM m;
  MCNUM W;
  /* Component type 'Guide_simple' DECLARE code stored as structure members */
                   
}; /* _struct_Guide_simple_parameters */
typedef struct _struct_Guide_simple_parameters _class_Guide_simple_parameters;

/* Parameters for component type 'Guide_simple' */
struct _struct_Guide_simple {
  char     _name[256]; /* e.g. SC1 */
  char     _type[256]; /* Guide_simple */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_Guide_simple_parameters _parameters;
};
typedef struct _struct_Guide_simple _class_Guide_simple;
_class_Guide_simple _SC1_var;
#pragma acc declare create ( _SC1_var )

_class_PSD_monitor _D1_SC1_Out_var;
#pragma acc declare create ( _D1_SC1_Out_var )

/* component As1=Slit() [6] DECLARE */
/* Parameter definition for component type 'Slit' */
struct _struct_Slit_parameters {
  /* Component type 'Slit' setting parameters */
  MCNUM xmin;
  MCNUM xmax;
  MCNUM ymin;
  MCNUM ymax;
  MCNUM radius;
  MCNUM xwidth;
  MCNUM yheight;
}; /* _struct_Slit_parameters */
typedef struct _struct_Slit_parameters _class_Slit_parameters;

/* Parameters for component type 'Slit' */
struct _struct_Slit {
  char     _name[256]; /* e.g. As1 */
  char     _type[256]; /* Slit */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_Slit_parameters _parameters;
};
typedef struct _struct_Slit _class_Slit;
_class_Slit _As1_var;
#pragma acc declare create ( _As1_var )

_class_Slit _As2_var;
#pragma acc declare create ( _As2_var )

_class_Slit _As3_var;
#pragma acc declare create ( _As3_var )

_class_Slit _As4_var;
#pragma acc declare create ( _As4_var )

_class_PSD_monitor _D2_A4_var;
#pragma acc declare create ( _D2_A4_var )

_class_Arm _Mono_Cradle_var;
#pragma acc declare create ( _Mono_Cradle_var )

/* component PG1Xtal=Monochromator_flat() [12] DECLARE */
/* Parameter definition for component type 'Monochromator_flat' */
struct _struct_Monochromator_flat_parameters {
  /* Component type 'Monochromator_flat' setting parameters */
  MCNUM zmin;
  MCNUM zmax;
  MCNUM ymin;
  MCNUM ymax;
  MCNUM zwidth;
  MCNUM yheight;
  MCNUM mosaich;
  MCNUM mosaicv;
  MCNUM r0;
  MCNUM Q;
  MCNUM DM;
  /* Component type 'Monochromator_flat' private parameters */
  /* Component type 'Monochromator_flat' DECLARE code stored as structure members */
  double mos_rms_y;                                             
  double mos_rms_z;
  double mos_rms_max;
  double mono_Q;
}; /* _struct_Monochromator_flat_parameters */
typedef struct _struct_Monochromator_flat_parameters _class_Monochromator_flat_parameters;

/* Parameters for component type 'Monochromator_flat' */
struct _struct_Monochromator_flat {
  char     _name[256]; /* e.g. PG1Xtal */
  char     _type[256]; /* Monochromator_flat */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_Monochromator_flat_parameters _parameters;
};
typedef struct _struct_Monochromator_flat _class_Monochromator_flat;
_class_Monochromator_flat _PG1Xtal_var;
#pragma acc declare create ( _PG1Xtal_var )

_class_Arm _Mono_Out_var;
#pragma acc declare create ( _Mono_Out_var )

_class_PSD_monitor _D4_SC2_In_var;
#pragma acc declare create ( _D4_SC2_In_var )

_class_Guide_simple _SC2_var;
#pragma acc declare create ( _SC2_var )

_class_PSD_monitor _D5_SC2_Out_var;
#pragma acc declare create ( _D5_SC2_Out_var )

_class_Arm _Sample_Cradle_var;
#pragma acc declare create ( _Sample_Cradle_var )

_class_Arm _Sample_Out_var;
#pragma acc declare create ( _Sample_Out_var )

/* component Sample=V_sample() [19] DECLARE */
/* Parameter definition for component type 'V_sample' */
struct _struct_V_sample_parameters {
  /* Component type 'V_sample' setting parameters */
  MCNUM radius;
  MCNUM thickness;
  MCNUM zdepth;
  MCNUM Vc;
  MCNUM sigma_abs;
  MCNUM sigma_inc;
  MCNUM radius_i;
  MCNUM radius_o;
  MCNUM h;
  MCNUM focus_r;
  MCNUM pack;
  MCNUM frac;
  MCNUM f_QE;
  MCNUM gamma;
  MCNUM target_x;
  MCNUM target_y;
  MCNUM target_z;
  MCNUM focus_xw;
  MCNUM focus_yh;
  MCNUM focus_aw;
  MCNUM focus_ah;
  MCNUM xwidth;
  MCNUM yheight;
  MCNUM zthick;
  MCNUM rad_sphere;
  MCNUM sig_a;
  MCNUM sig_i;
  MCNUM V0;
  long target_index;
  MCNUM multiples;
  /* Component type 'V_sample' private parameters */
  /* Component type 'V_sample' DECLARE code stored as structure members */
  struct StructVarsV VarsV;
}; /* _struct_V_sample_parameters */
typedef struct _struct_V_sample_parameters _class_V_sample_parameters;

/* Parameters for component type 'V_sample' */
struct _struct_V_sample {
  char     _name[256]; /* e.g. Sample */
  char     _type[256]; /* V_sample */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_V_sample_parameters _parameters;
};
typedef struct _struct_V_sample _class_V_sample;
_class_V_sample _Sample_var;
#pragma acc declare create ( _Sample_var )

_class_PSD_monitor _D7_SC3_In_var;
#pragma acc declare create ( _D7_SC3_In_var )

_class_Guide_simple _SC3_var;
#pragma acc declare create ( _SC3_var )

_class_PSD_monitor _D8_SC3_Out_var;
#pragma acc declare create ( _D8_SC3_Out_var )

_class_Arm _Ana_Cradle_var;
#pragma acc declare create ( _Ana_Cradle_var )

_class_Monochromator_flat _PG2Xtal_var;
#pragma acc declare create ( _PG2Xtal_var )

_class_Arm _Ana_Out_var;
#pragma acc declare create ( _Ana_Out_var )

_class_PSD_monitor _D10_SC4_In_var;
#pragma acc declare create ( _D10_SC4_In_var )

_class_Guide_simple _SC4_var;
#pragma acc declare create ( _SC4_var )

_class_PSD_monitor _He3H_var;
#pragma acc declare create ( _He3H_var )

int mcNUMCOMP = 28;

/* User declarations from instrument definition. Can define functions. */
  double DM         = 3.3539;   /* Monochromator d-spacing in Angs */
                                /* PG002 Orders : 1st 3.355 2e 1.6775, 3e 1.1183 */

/* to compute */
  double A1,A2;
  double A3,A4;
  double A5,A6;
  double mono_q, Ei;

#undef compcurname
#undef compcurtype
#undef compcurindex
/* end of instrument 'BNL_H8' and components DECLARE */

/* *****************************************************************************
* instrument 'BNL_H8' and components INITIALISE
***************************************************************************** */

/* component Origin=Arm() SETTING, POSITION/ROTATION */
int _Origin_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Origin_setpos] component Origin=Arm() SETTING [Arm:0]");
  stracpy(_Origin_var._name, "Origin", 16384);
  stracpy(_Origin_var._type, "Arm", 16384);
  _Origin_var._index=1;
  /* component Origin=Arm() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(_Origin_var._rotation_absolute,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_copy(_Origin_var._rotation_relative, _Origin_var._rotation_absolute);
    _Origin_var._rotation_is_identity =  rot_test_identity(_Origin_var._rotation_relative);
    _Origin_var._position_absolute = coords_set(
      0, 0, 0);
    tc1 = coords_neg(_Origin_var._position_absolute);
    _Origin_var._position_relative = rot_apply(_Origin_var._rotation_absolute, tc1);
  } /* Origin=Arm() AT ROTATED */
  DEBUG_COMPONENT("Origin", _Origin_var._position_absolute, _Origin_var._rotation_absolute);
  instrument->_position_absolute[1] = _Origin_var._position_absolute;
  instrument->_position_relative[1] = _Origin_var._position_relative;
  instrument->counter_N[1]  = instrument->counter_P[1] = instrument->counter_P2[1] = 0;
  instrument->counter_AbsorbProp[1]= 0;
  return(0);
} /* _Origin_setpos */

/* component Source=Source_simple() SETTING, POSITION/ROTATION */
int _Source_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Source_setpos] component Source=Source_simple() SETTING [/usr/share/mcstas/3.0-dev/sources/Source_simple.comp:64]");
  stracpy(_Source_var._name, "Source", 16384);
  stracpy(_Source_var._type, "Source_simple", 16384);
  _Source_var._index=2;
  _Source_var._parameters.radius = 0.10;
  #define radius (_Source_var._parameters.radius)
  _Source_var._parameters.yheight = 0;
  #define yheight (_Source_var._parameters.yheight)
  _Source_var._parameters.xwidth = 0;
  #define xwidth (_Source_var._parameters.xwidth)
  _Source_var._parameters.dist = 2.7473;
  #define dist (_Source_var._parameters.dist)
  _Source_var._parameters.focus_xw = 0.031;
  #define focus_xw (_Source_var._parameters.focus_xw)
  _Source_var._parameters.focus_yh = 0.054;
  #define focus_yh (_Source_var._parameters.focus_yh)
  _Source_var._parameters.E0 = Ei;
  #define E0 (_Source_var._parameters.E0)
  _Source_var._parameters.dE = 0.03 * Ei;
  #define dE (_Source_var._parameters.dE)
  _Source_var._parameters.lambda0 = 0;
  #define lambda0 (_Source_var._parameters.lambda0)
  _Source_var._parameters.dlambda = 0;
  #define dlambda (_Source_var._parameters.dlambda)
  _Source_var._parameters.flux = 1;
  #define flux (_Source_var._parameters.flux)
  _Source_var._parameters.gauss = 0;
  #define gauss (_Source_var._parameters.gauss)
  _Source_var._parameters.target_index = + 1;
  #define target_index (_Source_var._parameters.target_index)

  #define pmul (_Source_var._parameters.pmul)
  #define square (_Source_var._parameters.square)
  #define srcArea (_Source_var._parameters.srcArea)

  #undef radius
  #undef yheight
  #undef xwidth
  #undef dist
  #undef focus_xw
  #undef focus_yh
  #undef E0
  #undef dE
  #undef lambda0
  #undef dlambda
  #undef flux
  #undef gauss
  #undef target_index
  #undef pmul
  #undef square
  #undef srcArea
  /* component Source=Source_simple() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(_Source_var._rotation_absolute,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_transpose(_Origin_var._rotation_absolute, tr1);
    rot_mul(_Source_var._rotation_absolute, tr1, _Source_var._rotation_relative);
    _Source_var._rotation_is_identity =  rot_test_identity(_Source_var._rotation_relative);
    _Source_var._position_absolute = coords_set(
      0, 0, 0);
    tc1 = coords_sub(_Origin_var._position_absolute, _Source_var._position_absolute);
    _Source_var._position_relative = rot_apply(_Source_var._rotation_absolute, tc1);
  } /* Source=Source_simple() AT ROTATED */
  DEBUG_COMPONENT("Source", _Source_var._position_absolute, _Source_var._rotation_absolute);
  instrument->_position_absolute[2] = _Source_var._position_absolute;
  instrument->_position_relative[2] = _Source_var._position_relative;
  instrument->counter_N[2]  = instrument->counter_P[2] = instrument->counter_P2[2] = 0;
  instrument->counter_AbsorbProp[2]= 0;
  return(0);
} /* _Source_setpos */

/* component D0_Source=PSD_monitor() SETTING, POSITION/ROTATION */
int _D0_Source_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_D0_Source_setpos] component D0_Source=PSD_monitor() SETTING [/usr/share/mcstas/3.0-dev/monitors/PSD_monitor.comp:64]");
  stracpy(_D0_Source_var._name, "D0_Source", 16384);
  stracpy(_D0_Source_var._type, "PSD_monitor", 16384);
  _D0_Source_var._index=3;
  _D0_Source_var._parameters.nx = 20;
  #define nx (_D0_Source_var._parameters.nx)
  _D0_Source_var._parameters.ny = 20;
  #define ny (_D0_Source_var._parameters.ny)
  if("D0_Source.psd" && strlen("D0_Source.psd"))
    stracpy(_D0_Source_var._parameters.filename, "D0_Source.psd" ? "D0_Source.psd" : "", 16384);
  else 
  _D0_Source_var._parameters.filename[0]='\0';
  #define filename (_D0_Source_var._parameters.filename)
  _D0_Source_var._parameters.xmin = -0.05;
  #define xmin (_D0_Source_var._parameters.xmin)
  _D0_Source_var._parameters.xmax = 0.05;
  #define xmax (_D0_Source_var._parameters.xmax)
  _D0_Source_var._parameters.ymin = -0.05;
  #define ymin (_D0_Source_var._parameters.ymin)
  _D0_Source_var._parameters.ymax = 0.05;
  #define ymax (_D0_Source_var._parameters.ymax)
  _D0_Source_var._parameters.xwidth = 0.03;
  #define xwidth (_D0_Source_var._parameters.xwidth)
  _D0_Source_var._parameters.yheight = 0.054;
  #define yheight (_D0_Source_var._parameters.yheight)
  _D0_Source_var._parameters.restore_neutron = 0;
  #define restore_neutron (_D0_Source_var._parameters.restore_neutron)

  #define PSD_N (_D0_Source_var._parameters.PSD_N)
  #define PSD_p (_D0_Source_var._parameters.PSD_p)
  #define PSD_p2 (_D0_Source_var._parameters.PSD_p2)

  #undef nx
  #undef ny
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  /* component D0_Source=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Source_var._rotation_absolute, _D0_Source_var._rotation_absolute);
    rot_transpose(_Source_var._rotation_absolute, tr1);
    rot_mul(_D0_Source_var._rotation_absolute, tr1, _D0_Source_var._rotation_relative);
    _D0_Source_var._rotation_is_identity =  rot_test_identity(_D0_Source_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.0001);
    rot_transpose(_Source_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _D0_Source_var._position_absolute = coords_add(_Source_var._position_absolute, tc2);
    tc1 = coords_sub(_Source_var._position_absolute, _D0_Source_var._position_absolute);
    _D0_Source_var._position_relative = rot_apply(_D0_Source_var._rotation_absolute, tc1);
  } /* D0_Source=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("D0_Source", _D0_Source_var._position_absolute, _D0_Source_var._rotation_absolute);
  instrument->_position_absolute[3] = _D0_Source_var._position_absolute;
  instrument->_position_relative[3] = _D0_Source_var._position_relative;
  instrument->counter_N[3]  = instrument->counter_P[3] = instrument->counter_P2[3] = 0;
  instrument->counter_AbsorbProp[3]= 0;
  return(0);
} /* _D0_Source_setpos */

/* component SC1=Guide_simple() SETTING, POSITION/ROTATION */
int _SC1_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_SC1_setpos] component SC1=Guide_simple() SETTING [/usr/share/mcstas/3.0-dev/optics/Guide_simple.comp:72]");
  stracpy(_SC1_var._name, "SC1", 16384);
  stracpy(_SC1_var._type, "Guide_simple", 16384);
  _SC1_var._index=4;
  _SC1_var._parameters.w1 = 0.031;
  #define w1 (_SC1_var._parameters.w1)
  _SC1_var._parameters.h1 = 0.054;
  #define h1 (_SC1_var._parameters.h1)
  _SC1_var._parameters.w2 = 0;
  #define w2 (_SC1_var._parameters.w2)
  _SC1_var._parameters.h2 = 0;
  #define h2 (_SC1_var._parameters.h2)
  _SC1_var._parameters.l = 0.9144;
  #define l (_SC1_var._parameters.l)
  _SC1_var._parameters.R0 = 1.0;
  #define R0 (_SC1_var._parameters.R0)
  _SC1_var._parameters.Qc = 0.021;
  #define Qc (_SC1_var._parameters.Qc)
  _SC1_var._parameters.alpha = 6;
  #define alpha (_SC1_var._parameters.alpha)
  _SC1_var._parameters.m = 1;
  #define m (_SC1_var._parameters.m)
  _SC1_var._parameters.W = 0.0003;
  #define W (_SC1_var._parameters.W)

  #undef w1
  #undef h1
  #undef w2
  #undef h2
  #undef l
  #undef R0
  #undef Qc
  #undef alpha
  #undef m
  #undef W
  /* component SC1=Guide_simple() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Source_var._rotation_absolute, _SC1_var._rotation_absolute);
    rot_transpose(_D0_Source_var._rotation_absolute, tr1);
    rot_mul(_SC1_var._rotation_absolute, tr1, _SC1_var._rotation_relative);
    _SC1_var._rotation_is_identity =  rot_test_identity(_SC1_var._rotation_relative);
    tc1 = coords_set(
      0.0, 0, 2.7473);
    rot_transpose(_Source_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _SC1_var._position_absolute = coords_add(_Source_var._position_absolute, tc2);
    tc1 = coords_sub(_D0_Source_var._position_absolute, _SC1_var._position_absolute);
    _SC1_var._position_relative = rot_apply(_SC1_var._rotation_absolute, tc1);
  } /* SC1=Guide_simple() AT ROTATED */
  DEBUG_COMPONENT("SC1", _SC1_var._position_absolute, _SC1_var._rotation_absolute);
  instrument->_position_absolute[4] = _SC1_var._position_absolute;
  instrument->_position_relative[4] = _SC1_var._position_relative;
  instrument->counter_N[4]  = instrument->counter_P[4] = instrument->counter_P2[4] = 0;
  instrument->counter_AbsorbProp[4]= 0;
  return(0);
} /* _SC1_setpos */

/* component D1_SC1_Out=PSD_monitor() SETTING, POSITION/ROTATION */
int _D1_SC1_Out_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_D1_SC1_Out_setpos] component D1_SC1_Out=PSD_monitor() SETTING [/usr/share/mcstas/3.0-dev/monitors/PSD_monitor.comp:64]");
  stracpy(_D1_SC1_Out_var._name, "D1_SC1_Out", 16384);
  stracpy(_D1_SC1_Out_var._type, "PSD_monitor", 16384);
  _D1_SC1_Out_var._index=5;
  _D1_SC1_Out_var._parameters.nx = 20;
  #define nx (_D1_SC1_Out_var._parameters.nx)
  _D1_SC1_Out_var._parameters.ny = 20;
  #define ny (_D1_SC1_Out_var._parameters.ny)
  if("D1_SC1_Out.psd" && strlen("D1_SC1_Out.psd"))
    stracpy(_D1_SC1_Out_var._parameters.filename, "D1_SC1_Out.psd" ? "D1_SC1_Out.psd" : "", 16384);
  else 
  _D1_SC1_Out_var._parameters.filename[0]='\0';
  #define filename (_D1_SC1_Out_var._parameters.filename)
  _D1_SC1_Out_var._parameters.xmin = -0.05;
  #define xmin (_D1_SC1_Out_var._parameters.xmin)
  _D1_SC1_Out_var._parameters.xmax = 0.05;
  #define xmax (_D1_SC1_Out_var._parameters.xmax)
  _D1_SC1_Out_var._parameters.ymin = -0.05;
  #define ymin (_D1_SC1_Out_var._parameters.ymin)
  _D1_SC1_Out_var._parameters.ymax = 0.05;
  #define ymax (_D1_SC1_Out_var._parameters.ymax)
  _D1_SC1_Out_var._parameters.xwidth = 0.03;
  #define xwidth (_D1_SC1_Out_var._parameters.xwidth)
  _D1_SC1_Out_var._parameters.yheight = 0.054;
  #define yheight (_D1_SC1_Out_var._parameters.yheight)
  _D1_SC1_Out_var._parameters.restore_neutron = 0;
  #define restore_neutron (_D1_SC1_Out_var._parameters.restore_neutron)

  #define PSD_N (_D1_SC1_Out_var._parameters.PSD_N)
  #define PSD_p (_D1_SC1_Out_var._parameters.PSD_p)
  #define PSD_p2 (_D1_SC1_Out_var._parameters.PSD_p2)

  #undef nx
  #undef ny
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  /* component D1_SC1_Out=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _SC1_var._rotation_absolute, _D1_SC1_Out_var._rotation_absolute);
    rot_transpose(_SC1_var._rotation_absolute, tr1);
    rot_mul(_D1_SC1_Out_var._rotation_absolute, tr1, _D1_SC1_Out_var._rotation_relative);
    _D1_SC1_Out_var._rotation_is_identity =  rot_test_identity(_D1_SC1_Out_var._rotation_relative);
    tc1 = coords_set(
      0.0, 0, 0.9145);
    rot_transpose(_SC1_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _D1_SC1_Out_var._position_absolute = coords_add(_SC1_var._position_absolute, tc2);
    tc1 = coords_sub(_SC1_var._position_absolute, _D1_SC1_Out_var._position_absolute);
    _D1_SC1_Out_var._position_relative = rot_apply(_D1_SC1_Out_var._rotation_absolute, tc1);
  } /* D1_SC1_Out=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("D1_SC1_Out", _D1_SC1_Out_var._position_absolute, _D1_SC1_Out_var._rotation_absolute);
  instrument->_position_absolute[5] = _D1_SC1_Out_var._position_absolute;
  instrument->_position_relative[5] = _D1_SC1_Out_var._position_relative;
  instrument->counter_N[5]  = instrument->counter_P[5] = instrument->counter_P2[5] = 0;
  instrument->counter_AbsorbProp[5]= 0;
  return(0);
} /* _D1_SC1_Out_setpos */

/* component As1=Slit() SETTING, POSITION/ROTATION */
int _As1_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_As1_setpos] component As1=Slit() SETTING [/usr/share/mcstas/3.0-dev/optics/Slit.comp:47]");
  stracpy(_As1_var._name, "As1", 16384);
  stracpy(_As1_var._type, "Slit", 16384);
  _As1_var._index=6;
  _As1_var._parameters.xmin = -0.01;
  #define xmin (_As1_var._parameters.xmin)
  _As1_var._parameters.xmax = 0.01;
  #define xmax (_As1_var._parameters.xmax)
  _As1_var._parameters.ymin = -0.01;
  #define ymin (_As1_var._parameters.ymin)
  _As1_var._parameters.ymax = 0.01;
  #define ymax (_As1_var._parameters.ymax)
  _As1_var._parameters.radius = 0;
  #define radius (_As1_var._parameters.radius)
  _As1_var._parameters.xwidth = 0.04450;
  #define xwidth (_As1_var._parameters.xwidth)
  _As1_var._parameters.yheight = 0.0635;
  #define yheight (_As1_var._parameters.yheight)

  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef radius
  #undef xwidth
  #undef yheight
  /* component As1=Slit() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Source_var._rotation_absolute, _As1_var._rotation_absolute);
    rot_transpose(_D1_SC1_Out_var._rotation_absolute, tr1);
    rot_mul(_As1_var._rotation_absolute, tr1, _As1_var._rotation_relative);
    _As1_var._rotation_is_identity =  rot_test_identity(_As1_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 3.6998);
    rot_transpose(_Source_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _As1_var._position_absolute = coords_add(_Source_var._position_absolute, tc2);
    tc1 = coords_sub(_D1_SC1_Out_var._position_absolute, _As1_var._position_absolute);
    _As1_var._position_relative = rot_apply(_As1_var._rotation_absolute, tc1);
  } /* As1=Slit() AT ROTATED */
  DEBUG_COMPONENT("As1", _As1_var._position_absolute, _As1_var._rotation_absolute);
  instrument->_position_absolute[6] = _As1_var._position_absolute;
  instrument->_position_relative[6] = _As1_var._position_relative;
  instrument->counter_N[6]  = instrument->counter_P[6] = instrument->counter_P2[6] = 0;
  instrument->counter_AbsorbProp[6]= 0;
  return(0);
} /* _As1_setpos */

/* component As2=Slit() SETTING, POSITION/ROTATION */
int _As2_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_As2_setpos] component As2=Slit() SETTING [/usr/share/mcstas/3.0-dev/optics/Slit.comp:47]");
  stracpy(_As2_var._name, "As2", 16384);
  stracpy(_As2_var._type, "Slit", 16384);
  _As2_var._index=7;
  _As2_var._parameters.xmin = -0.01;
  #define xmin (_As2_var._parameters.xmin)
  _As2_var._parameters.xmax = 0.01;
  #define xmax (_As2_var._parameters.xmax)
  _As2_var._parameters.ymin = -0.01;
  #define ymin (_As2_var._parameters.ymin)
  _As2_var._parameters.ymax = 0.01;
  #define ymax (_As2_var._parameters.ymax)
  _As2_var._parameters.radius = 0;
  #define radius (_As2_var._parameters.radius)
  _As2_var._parameters.xwidth = 0.04450;
  #define xwidth (_As2_var._parameters.xwidth)
  _As2_var._parameters.yheight = 0.0635;
  #define yheight (_As2_var._parameters.yheight)

  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef radius
  #undef xwidth
  #undef yheight
  /* component As2=Slit() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Source_var._rotation_absolute, _As2_var._rotation_absolute);
    rot_transpose(_As1_var._rotation_absolute, tr1);
    rot_mul(_As2_var._rotation_absolute, tr1, _As2_var._rotation_relative);
    _As2_var._rotation_is_identity =  rot_test_identity(_As2_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 4.0808);
    rot_transpose(_Source_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _As2_var._position_absolute = coords_add(_Source_var._position_absolute, tc2);
    tc1 = coords_sub(_As1_var._position_absolute, _As2_var._position_absolute);
    _As2_var._position_relative = rot_apply(_As2_var._rotation_absolute, tc1);
  } /* As2=Slit() AT ROTATED */
  DEBUG_COMPONENT("As2", _As2_var._position_absolute, _As2_var._rotation_absolute);
  instrument->_position_absolute[7] = _As2_var._position_absolute;
  instrument->_position_relative[7] = _As2_var._position_relative;
  instrument->counter_N[7]  = instrument->counter_P[7] = instrument->counter_P2[7] = 0;
  instrument->counter_AbsorbProp[7]= 0;
  return(0);
} /* _As2_setpos */

/* component As3=Slit() SETTING, POSITION/ROTATION */
int _As3_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_As3_setpos] component As3=Slit() SETTING [/usr/share/mcstas/3.0-dev/optics/Slit.comp:47]");
  stracpy(_As3_var._name, "As3", 16384);
  stracpy(_As3_var._type, "Slit", 16384);
  _As3_var._index=8;
  _As3_var._parameters.xmin = -0.01;
  #define xmin (_As3_var._parameters.xmin)
  _As3_var._parameters.xmax = 0.01;
  #define xmax (_As3_var._parameters.xmax)
  _As3_var._parameters.ymin = -0.01;
  #define ymin (_As3_var._parameters.ymin)
  _As3_var._parameters.ymax = 0.01;
  #define ymax (_As3_var._parameters.ymax)
  _As3_var._parameters.radius = 0;
  #define radius (_As3_var._parameters.radius)
  _As3_var._parameters.xwidth = 0.04450;
  #define xwidth (_As3_var._parameters.xwidth)
  _As3_var._parameters.yheight = 0.0635;
  #define yheight (_As3_var._parameters.yheight)

  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef radius
  #undef xwidth
  #undef yheight
  /* component As3=Slit() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Source_var._rotation_absolute, _As3_var._rotation_absolute);
    rot_transpose(_As2_var._rotation_absolute, tr1);
    rot_mul(_As3_var._rotation_absolute, tr1, _As3_var._rotation_relative);
    _As3_var._rotation_is_identity =  rot_test_identity(_As3_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 4.1189);
    rot_transpose(_Source_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _As3_var._position_absolute = coords_add(_Source_var._position_absolute, tc2);
    tc1 = coords_sub(_As2_var._position_absolute, _As3_var._position_absolute);
    _As3_var._position_relative = rot_apply(_As3_var._rotation_absolute, tc1);
  } /* As3=Slit() AT ROTATED */
  DEBUG_COMPONENT("As3", _As3_var._position_absolute, _As3_var._rotation_absolute);
  instrument->_position_absolute[8] = _As3_var._position_absolute;
  instrument->_position_relative[8] = _As3_var._position_relative;
  instrument->counter_N[8]  = instrument->counter_P[8] = instrument->counter_P2[8] = 0;
  instrument->counter_AbsorbProp[8]= 0;
  return(0);
} /* _As3_setpos */

/* component As4=Slit() SETTING, POSITION/ROTATION */
int _As4_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_As4_setpos] component As4=Slit() SETTING [/usr/share/mcstas/3.0-dev/optics/Slit.comp:47]");
  stracpy(_As4_var._name, "As4", 16384);
  stracpy(_As4_var._type, "Slit", 16384);
  _As4_var._index=9;
  _As4_var._parameters.xmin = -0.01;
  #define xmin (_As4_var._parameters.xmin)
  _As4_var._parameters.xmax = 0.01;
  #define xmax (_As4_var._parameters.xmax)
  _As4_var._parameters.ymin = -0.01;
  #define ymin (_As4_var._parameters.ymin)
  _As4_var._parameters.ymax = 0.01;
  #define ymax (_As4_var._parameters.ymax)
  _As4_var._parameters.radius = 0;
  #define radius (_As4_var._parameters.radius)
  _As4_var._parameters.xwidth = 0.04450;
  #define xwidth (_As4_var._parameters.xwidth)
  _As4_var._parameters.yheight = 0.0635;
  #define yheight (_As4_var._parameters.yheight)

  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef radius
  #undef xwidth
  #undef yheight
  /* component As4=Slit() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Source_var._rotation_absolute, _As4_var._rotation_absolute);
    rot_transpose(_As3_var._rotation_absolute, tr1);
    rot_mul(_As4_var._rotation_absolute, tr1, _As4_var._rotation_relative);
    _As4_var._rotation_is_identity =  rot_test_identity(_As4_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 4.4141);
    rot_transpose(_Source_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _As4_var._position_absolute = coords_add(_Source_var._position_absolute, tc2);
    tc1 = coords_sub(_As3_var._position_absolute, _As4_var._position_absolute);
    _As4_var._position_relative = rot_apply(_As4_var._rotation_absolute, tc1);
  } /* As4=Slit() AT ROTATED */
  DEBUG_COMPONENT("As4", _As4_var._position_absolute, _As4_var._rotation_absolute);
  instrument->_position_absolute[9] = _As4_var._position_absolute;
  instrument->_position_relative[9] = _As4_var._position_relative;
  instrument->counter_N[9]  = instrument->counter_P[9] = instrument->counter_P2[9] = 0;
  instrument->counter_AbsorbProp[9]= 0;
  return(0);
} /* _As4_setpos */

/* component D2_A4=PSD_monitor() SETTING, POSITION/ROTATION */
int _D2_A4_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_D2_A4_setpos] component D2_A4=PSD_monitor() SETTING [/usr/share/mcstas/3.0-dev/monitors/PSD_monitor.comp:64]");
  stracpy(_D2_A4_var._name, "D2_A4", 16384);
  stracpy(_D2_A4_var._type, "PSD_monitor", 16384);
  _D2_A4_var._index=10;
  _D2_A4_var._parameters.nx = 20;
  #define nx (_D2_A4_var._parameters.nx)
  _D2_A4_var._parameters.ny = 20;
  #define ny (_D2_A4_var._parameters.ny)
  if("D2_A4.psd" && strlen("D2_A4.psd"))
    stracpy(_D2_A4_var._parameters.filename, "D2_A4.psd" ? "D2_A4.psd" : "", 16384);
  else 
  _D2_A4_var._parameters.filename[0]='\0';
  #define filename (_D2_A4_var._parameters.filename)
  _D2_A4_var._parameters.xmin = -0.05;
  #define xmin (_D2_A4_var._parameters.xmin)
  _D2_A4_var._parameters.xmax = 0.05;
  #define xmax (_D2_A4_var._parameters.xmax)
  _D2_A4_var._parameters.ymin = -0.05;
  #define ymin (_D2_A4_var._parameters.ymin)
  _D2_A4_var._parameters.ymax = 0.05;
  #define ymax (_D2_A4_var._parameters.ymax)
  _D2_A4_var._parameters.xwidth = 0.04450;
  #define xwidth (_D2_A4_var._parameters.xwidth)
  _D2_A4_var._parameters.yheight = 0.0635;
  #define yheight (_D2_A4_var._parameters.yheight)
  _D2_A4_var._parameters.restore_neutron = 0;
  #define restore_neutron (_D2_A4_var._parameters.restore_neutron)

  #define PSD_N (_D2_A4_var._parameters.PSD_N)
  #define PSD_p (_D2_A4_var._parameters.PSD_p)
  #define PSD_p2 (_D2_A4_var._parameters.PSD_p2)

  #undef nx
  #undef ny
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  /* component D2_A4=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _As4_var._rotation_absolute, _D2_A4_var._rotation_absolute);
    rot_transpose(_As4_var._rotation_absolute, tr1);
    rot_mul(_D2_A4_var._rotation_absolute, tr1, _D2_A4_var._rotation_relative);
    _D2_A4_var._rotation_is_identity =  rot_test_identity(_D2_A4_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.0001);
    rot_transpose(_As4_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _D2_A4_var._position_absolute = coords_add(_As4_var._position_absolute, tc2);
    tc1 = coords_sub(_As4_var._position_absolute, _D2_A4_var._position_absolute);
    _D2_A4_var._position_relative = rot_apply(_D2_A4_var._rotation_absolute, tc1);
  } /* D2_A4=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("D2_A4", _D2_A4_var._position_absolute, _D2_A4_var._rotation_absolute);
  instrument->_position_absolute[10] = _D2_A4_var._position_absolute;
  instrument->_position_relative[10] = _D2_A4_var._position_relative;
  instrument->counter_N[10]  = instrument->counter_P[10] = instrument->counter_P2[10] = 0;
  instrument->counter_AbsorbProp[10]= 0;
  return(0);
} /* _D2_A4_setpos */

/* component Mono_Cradle=Arm() SETTING, POSITION/ROTATION */
int _Mono_Cradle_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Mono_Cradle_setpos] component Mono_Cradle=Arm() SETTING [Arm:0]");
  stracpy(_Mono_Cradle_var._name, "Mono_Cradle", 16384);
  stracpy(_Mono_Cradle_var._type, "Arm", 16384);
  _Mono_Cradle_var._index=11;
  /* component Mono_Cradle=Arm() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (A1)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _Source_var._rotation_absolute, _Mono_Cradle_var._rotation_absolute);
    rot_transpose(_D2_A4_var._rotation_absolute, tr1);
    rot_mul(_Mono_Cradle_var._rotation_absolute, tr1, _Mono_Cradle_var._rotation_relative);
    _Mono_Cradle_var._rotation_is_identity =  rot_test_identity(_Mono_Cradle_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 5.2746);
    rot_transpose(_Source_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _Mono_Cradle_var._position_absolute = coords_add(_Source_var._position_absolute, tc2);
    tc1 = coords_sub(_D2_A4_var._position_absolute, _Mono_Cradle_var._position_absolute);
    _Mono_Cradle_var._position_relative = rot_apply(_Mono_Cradle_var._rotation_absolute, tc1);
  } /* Mono_Cradle=Arm() AT ROTATED */
  DEBUG_COMPONENT("Mono_Cradle", _Mono_Cradle_var._position_absolute, _Mono_Cradle_var._rotation_absolute);
  instrument->_position_absolute[11] = _Mono_Cradle_var._position_absolute;
  instrument->_position_relative[11] = _Mono_Cradle_var._position_relative;
  instrument->counter_N[11]  = instrument->counter_P[11] = instrument->counter_P2[11] = 0;
  instrument->counter_AbsorbProp[11]= 0;
  return(0);
} /* _Mono_Cradle_setpos */

/* component PG1Xtal=Monochromator_flat() SETTING, POSITION/ROTATION */
int _PG1Xtal_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_PG1Xtal_setpos] component PG1Xtal=Monochromator_flat() SETTING [/usr/share/mcstas/3.0-dev/optics/Monochromator_flat.comp:103]");
  stracpy(_PG1Xtal_var._name, "PG1Xtal", 16384);
  stracpy(_PG1Xtal_var._type, "Monochromator_flat", 16384);
  _PG1Xtal_var._index=12;
  _PG1Xtal_var._parameters.zmin = -0.05;
  #define zmin (_PG1Xtal_var._parameters.zmin)
  _PG1Xtal_var._parameters.zmax = 0.05;
  #define zmax (_PG1Xtal_var._parameters.zmax)
  _PG1Xtal_var._parameters.ymin = -0.05;
  #define ymin (_PG1Xtal_var._parameters.ymin)
  _PG1Xtal_var._parameters.ymax = 0.05;
  #define ymax (_PG1Xtal_var._parameters.ymax)
  _PG1Xtal_var._parameters.zwidth = 0.1;
  #define zwidth (_PG1Xtal_var._parameters.zwidth)
  _PG1Xtal_var._parameters.yheight = 0.08;
  #define yheight (_PG1Xtal_var._parameters.yheight)
  _PG1Xtal_var._parameters.mosaich = 40;
  #define mosaich (_PG1Xtal_var._parameters.mosaich)
  _PG1Xtal_var._parameters.mosaicv = 40;
  #define mosaicv (_PG1Xtal_var._parameters.mosaicv)
  _PG1Xtal_var._parameters.r0 = 0.7;
  #define r0 (_PG1Xtal_var._parameters.r0)
  _PG1Xtal_var._parameters.Q = mono_q;
  #define Q (_PG1Xtal_var._parameters.Q)
  _PG1Xtal_var._parameters.DM = 0;
  #define DM (_PG1Xtal_var._parameters.DM)

  #define mos_rms_y (_PG1Xtal_var._parameters.mos_rms_y)
  #define mos_rms_z (_PG1Xtal_var._parameters.mos_rms_z)
  #define mos_rms_max (_PG1Xtal_var._parameters.mos_rms_max)
  #define mono_Q (_PG1Xtal_var._parameters.mono_Q)

  #undef zmin
  #undef zmax
  #undef ymin
  #undef ymax
  #undef zwidth
  #undef yheight
  #undef mosaich
  #undef mosaicv
  #undef r0
  #undef Q
  #undef DM
  #undef mos_rms_y
  #undef mos_rms_z
  #undef mos_rms_max
  #undef mono_Q
  /* component PG1Xtal=Monochromator_flat() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Mono_Cradle_var._rotation_absolute, _PG1Xtal_var._rotation_absolute);
    rot_transpose(_Mono_Cradle_var._rotation_absolute, tr1);
    rot_mul(_PG1Xtal_var._rotation_absolute, tr1, _PG1Xtal_var._rotation_relative);
    _PG1Xtal_var._rotation_is_identity =  rot_test_identity(_PG1Xtal_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.0001);
    rot_transpose(_Mono_Cradle_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _PG1Xtal_var._position_absolute = coords_add(_Mono_Cradle_var._position_absolute, tc2);
    tc1 = coords_sub(_Mono_Cradle_var._position_absolute, _PG1Xtal_var._position_absolute);
    _PG1Xtal_var._position_relative = rot_apply(_PG1Xtal_var._rotation_absolute, tc1);
  } /* PG1Xtal=Monochromator_flat() AT ROTATED */
  DEBUG_COMPONENT("PG1Xtal", _PG1Xtal_var._position_absolute, _PG1Xtal_var._rotation_absolute);
  instrument->_position_absolute[12] = _PG1Xtal_var._position_absolute;
  instrument->_position_relative[12] = _PG1Xtal_var._position_relative;
  instrument->counter_N[12]  = instrument->counter_P[12] = instrument->counter_P2[12] = 0;
  instrument->counter_AbsorbProp[12]= 0;
  return(0);
} /* _PG1Xtal_setpos */

/* component Mono_Out=Arm() SETTING, POSITION/ROTATION */
int _Mono_Out_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Mono_Out_setpos] component Mono_Out=Arm() SETTING [Arm:0]");
  stracpy(_Mono_Out_var._name, "Mono_Out", 16384);
  stracpy(_Mono_Out_var._type, "Arm", 16384);
  _Mono_Out_var._index=13;
  /* component Mono_Out=Arm() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (A2)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _Source_var._rotation_absolute, _Mono_Out_var._rotation_absolute);
    rot_transpose(_PG1Xtal_var._rotation_absolute, tr1);
    rot_mul(_Mono_Out_var._rotation_absolute, tr1, _Mono_Out_var._rotation_relative);
    _Mono_Out_var._rotation_is_identity =  rot_test_identity(_Mono_Out_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.0002);
    rot_transpose(_Mono_Cradle_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _Mono_Out_var._position_absolute = coords_add(_Mono_Cradle_var._position_absolute, tc2);
    tc1 = coords_sub(_PG1Xtal_var._position_absolute, _Mono_Out_var._position_absolute);
    _Mono_Out_var._position_relative = rot_apply(_Mono_Out_var._rotation_absolute, tc1);
  } /* Mono_Out=Arm() AT ROTATED */
  DEBUG_COMPONENT("Mono_Out", _Mono_Out_var._position_absolute, _Mono_Out_var._rotation_absolute);
  instrument->_position_absolute[13] = _Mono_Out_var._position_absolute;
  instrument->_position_relative[13] = _Mono_Out_var._position_relative;
  instrument->counter_N[13]  = instrument->counter_P[13] = instrument->counter_P2[13] = 0;
  instrument->counter_AbsorbProp[13]= 0;
  return(0);
} /* _Mono_Out_setpos */

/* component D4_SC2_In=PSD_monitor() SETTING, POSITION/ROTATION */
int _D4_SC2_In_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_D4_SC2_In_setpos] component D4_SC2_In=PSD_monitor() SETTING [/usr/share/mcstas/3.0-dev/monitors/PSD_monitor.comp:64]");
  stracpy(_D4_SC2_In_var._name, "D4_SC2_In", 16384);
  stracpy(_D4_SC2_In_var._type, "PSD_monitor", 16384);
  _D4_SC2_In_var._index=14;
  _D4_SC2_In_var._parameters.nx = 20;
  #define nx (_D4_SC2_In_var._parameters.nx)
  _D4_SC2_In_var._parameters.ny = 20;
  #define ny (_D4_SC2_In_var._parameters.ny)
  if("D4_SC2_In.psd" && strlen("D4_SC2_In.psd"))
    stracpy(_D4_SC2_In_var._parameters.filename, "D4_SC2_In.psd" ? "D4_SC2_In.psd" : "", 16384);
  else 
  _D4_SC2_In_var._parameters.filename[0]='\0';
  #define filename (_D4_SC2_In_var._parameters.filename)
  _D4_SC2_In_var._parameters.xmin = -0.05;
  #define xmin (_D4_SC2_In_var._parameters.xmin)
  _D4_SC2_In_var._parameters.xmax = 0.05;
  #define xmax (_D4_SC2_In_var._parameters.xmax)
  _D4_SC2_In_var._parameters.ymin = -0.05;
  #define ymin (_D4_SC2_In_var._parameters.ymin)
  _D4_SC2_In_var._parameters.ymax = 0.05;
  #define ymax (_D4_SC2_In_var._parameters.ymax)
  _D4_SC2_In_var._parameters.xwidth = 0.0318;
  #define xwidth (_D4_SC2_In_var._parameters.xwidth)
  _D4_SC2_In_var._parameters.yheight = 0.0495;
  #define yheight (_D4_SC2_In_var._parameters.yheight)
  _D4_SC2_In_var._parameters.restore_neutron = 0;
  #define restore_neutron (_D4_SC2_In_var._parameters.restore_neutron)

  #define PSD_N (_D4_SC2_In_var._parameters.PSD_N)
  #define PSD_p (_D4_SC2_In_var._parameters.PSD_p)
  #define PSD_p2 (_D4_SC2_In_var._parameters.PSD_p2)

  #undef nx
  #undef ny
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  /* component D4_SC2_In=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Mono_Out_var._rotation_absolute, _D4_SC2_In_var._rotation_absolute);
    rot_transpose(_Mono_Out_var._rotation_absolute, tr1);
    rot_mul(_D4_SC2_In_var._rotation_absolute, tr1, _D4_SC2_In_var._rotation_relative);
    _D4_SC2_In_var._rotation_is_identity =  rot_test_identity(_D4_SC2_In_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.2222);
    rot_transpose(_Mono_Out_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _D4_SC2_In_var._position_absolute = coords_add(_Mono_Out_var._position_absolute, tc2);
    tc1 = coords_sub(_Mono_Out_var._position_absolute, _D4_SC2_In_var._position_absolute);
    _D4_SC2_In_var._position_relative = rot_apply(_D4_SC2_In_var._rotation_absolute, tc1);
  } /* D4_SC2_In=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("D4_SC2_In", _D4_SC2_In_var._position_absolute, _D4_SC2_In_var._rotation_absolute);
  instrument->_position_absolute[14] = _D4_SC2_In_var._position_absolute;
  instrument->_position_relative[14] = _D4_SC2_In_var._position_relative;
  instrument->counter_N[14]  = instrument->counter_P[14] = instrument->counter_P2[14] = 0;
  instrument->counter_AbsorbProp[14]= 0;
  return(0);
} /* _D4_SC2_In_setpos */

/* component SC2=Guide_simple() SETTING, POSITION/ROTATION */
int _SC2_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_SC2_setpos] component SC2=Guide_simple() SETTING [/usr/share/mcstas/3.0-dev/optics/Guide_simple.comp:72]");
  stracpy(_SC2_var._name, "SC2", 16384);
  stracpy(_SC2_var._type, "Guide_simple", 16384);
  _SC2_var._index=15;
  _SC2_var._parameters.w1 = 0.0318;
  #define w1 (_SC2_var._parameters.w1)
  _SC2_var._parameters.h1 = 0.0495;
  #define h1 (_SC2_var._parameters.h1)
  _SC2_var._parameters.w2 = 0;
  #define w2 (_SC2_var._parameters.w2)
  _SC2_var._parameters.h2 = 0;
  #define h2 (_SC2_var._parameters.h2)
  _SC2_var._parameters.l = 0.6096;
  #define l (_SC2_var._parameters.l)
  _SC2_var._parameters.R0 = 1.0;
  #define R0 (_SC2_var._parameters.R0)
  _SC2_var._parameters.Qc = 0.021;
  #define Qc (_SC2_var._parameters.Qc)
  _SC2_var._parameters.alpha = 6;
  #define alpha (_SC2_var._parameters.alpha)
  _SC2_var._parameters.m = 1;
  #define m (_SC2_var._parameters.m)
  _SC2_var._parameters.W = 0.0003;
  #define W (_SC2_var._parameters.W)

  #undef w1
  #undef h1
  #undef w2
  #undef h2
  #undef l
  #undef R0
  #undef Qc
  #undef alpha
  #undef m
  #undef W
  /* component SC2=Guide_simple() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Mono_Out_var._rotation_absolute, _SC2_var._rotation_absolute);
    rot_transpose(_D4_SC2_In_var._rotation_absolute, tr1);
    rot_mul(_SC2_var._rotation_absolute, tr1, _SC2_var._rotation_relative);
    _SC2_var._rotation_is_identity =  rot_test_identity(_SC2_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.2223);
    rot_transpose(_Mono_Out_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _SC2_var._position_absolute = coords_add(_Mono_Out_var._position_absolute, tc2);
    tc1 = coords_sub(_D4_SC2_In_var._position_absolute, _SC2_var._position_absolute);
    _SC2_var._position_relative = rot_apply(_SC2_var._rotation_absolute, tc1);
  } /* SC2=Guide_simple() AT ROTATED */
  DEBUG_COMPONENT("SC2", _SC2_var._position_absolute, _SC2_var._rotation_absolute);
  instrument->_position_absolute[15] = _SC2_var._position_absolute;
  instrument->_position_relative[15] = _SC2_var._position_relative;
  instrument->counter_N[15]  = instrument->counter_P[15] = instrument->counter_P2[15] = 0;
  instrument->counter_AbsorbProp[15]= 0;
  return(0);
} /* _SC2_setpos */

/* component D5_SC2_Out=PSD_monitor() SETTING, POSITION/ROTATION */
int _D5_SC2_Out_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_D5_SC2_Out_setpos] component D5_SC2_Out=PSD_monitor() SETTING [/usr/share/mcstas/3.0-dev/monitors/PSD_monitor.comp:64]");
  stracpy(_D5_SC2_Out_var._name, "D5_SC2_Out", 16384);
  stracpy(_D5_SC2_Out_var._type, "PSD_monitor", 16384);
  _D5_SC2_Out_var._index=16;
  _D5_SC2_Out_var._parameters.nx = 20;
  #define nx (_D5_SC2_Out_var._parameters.nx)
  _D5_SC2_Out_var._parameters.ny = 20;
  #define ny (_D5_SC2_Out_var._parameters.ny)
  if("D5_SC2_Out.psd" && strlen("D5_SC2_Out.psd"))
    stracpy(_D5_SC2_Out_var._parameters.filename, "D5_SC2_Out.psd" ? "D5_SC2_Out.psd" : "", 16384);
  else 
  _D5_SC2_Out_var._parameters.filename[0]='\0';
  #define filename (_D5_SC2_Out_var._parameters.filename)
  _D5_SC2_Out_var._parameters.xmin = -0.05;
  #define xmin (_D5_SC2_Out_var._parameters.xmin)
  _D5_SC2_Out_var._parameters.xmax = 0.05;
  #define xmax (_D5_SC2_Out_var._parameters.xmax)
  _D5_SC2_Out_var._parameters.ymin = -0.05;
  #define ymin (_D5_SC2_Out_var._parameters.ymin)
  _D5_SC2_Out_var._parameters.ymax = 0.05;
  #define ymax (_D5_SC2_Out_var._parameters.ymax)
  _D5_SC2_Out_var._parameters.xwidth = 0.0318;
  #define xwidth (_D5_SC2_Out_var._parameters.xwidth)
  _D5_SC2_Out_var._parameters.yheight = 0.0495;
  #define yheight (_D5_SC2_Out_var._parameters.yheight)
  _D5_SC2_Out_var._parameters.restore_neutron = 0;
  #define restore_neutron (_D5_SC2_Out_var._parameters.restore_neutron)

  #define PSD_N (_D5_SC2_Out_var._parameters.PSD_N)
  #define PSD_p (_D5_SC2_Out_var._parameters.PSD_p)
  #define PSD_p2 (_D5_SC2_Out_var._parameters.PSD_p2)

  #undef nx
  #undef ny
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  /* component D5_SC2_Out=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _SC2_var._rotation_absolute, _D5_SC2_Out_var._rotation_absolute);
    rot_transpose(_SC2_var._rotation_absolute, tr1);
    rot_mul(_D5_SC2_Out_var._rotation_absolute, tr1, _D5_SC2_Out_var._rotation_relative);
    _D5_SC2_Out_var._rotation_is_identity =  rot_test_identity(_D5_SC2_Out_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.6097);
    rot_transpose(_SC2_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _D5_SC2_Out_var._position_absolute = coords_add(_SC2_var._position_absolute, tc2);
    tc1 = coords_sub(_SC2_var._position_absolute, _D5_SC2_Out_var._position_absolute);
    _D5_SC2_Out_var._position_relative = rot_apply(_D5_SC2_Out_var._rotation_absolute, tc1);
  } /* D5_SC2_Out=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("D5_SC2_Out", _D5_SC2_Out_var._position_absolute, _D5_SC2_Out_var._rotation_absolute);
  instrument->_position_absolute[16] = _D5_SC2_Out_var._position_absolute;
  instrument->_position_relative[16] = _D5_SC2_Out_var._position_relative;
  instrument->counter_N[16]  = instrument->counter_P[16] = instrument->counter_P2[16] = 0;
  instrument->counter_AbsorbProp[16]= 0;
  return(0);
} /* _D5_SC2_Out_setpos */

/* component Sample_Cradle=Arm() SETTING, POSITION/ROTATION */
int _Sample_Cradle_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Sample_Cradle_setpos] component Sample_Cradle=Arm() SETTING [Arm:0]");
  stracpy(_Sample_Cradle_var._name, "Sample_Cradle", 16384);
  stracpy(_Sample_Cradle_var._type, "Arm", 16384);
  _Sample_Cradle_var._index=17;
  /* component Sample_Cradle=Arm() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (A3)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _Mono_Out_var._rotation_absolute, _Sample_Cradle_var._rotation_absolute);
    rot_transpose(_D5_SC2_Out_var._rotation_absolute, tr1);
    rot_mul(_Sample_Cradle_var._rotation_absolute, tr1, _Sample_Cradle_var._rotation_relative);
    _Sample_Cradle_var._rotation_is_identity =  rot_test_identity(_Sample_Cradle_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.7811);
    rot_transpose(_D5_SC2_Out_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _Sample_Cradle_var._position_absolute = coords_add(_D5_SC2_Out_var._position_absolute, tc2);
    tc1 = coords_sub(_D5_SC2_Out_var._position_absolute, _Sample_Cradle_var._position_absolute);
    _Sample_Cradle_var._position_relative = rot_apply(_Sample_Cradle_var._rotation_absolute, tc1);
  } /* Sample_Cradle=Arm() AT ROTATED */
  DEBUG_COMPONENT("Sample_Cradle", _Sample_Cradle_var._position_absolute, _Sample_Cradle_var._rotation_absolute);
  instrument->_position_absolute[17] = _Sample_Cradle_var._position_absolute;
  instrument->_position_relative[17] = _Sample_Cradle_var._position_relative;
  instrument->counter_N[17]  = instrument->counter_P[17] = instrument->counter_P2[17] = 0;
  instrument->counter_AbsorbProp[17]= 0;
  return(0);
} /* _Sample_Cradle_setpos */

/* component Sample_Out=Arm() SETTING, POSITION/ROTATION */
int _Sample_Out_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Sample_Out_setpos] component Sample_Out=Arm() SETTING [Arm:0]");
  stracpy(_Sample_Out_var._name, "Sample_Out", 16384);
  stracpy(_Sample_Out_var._type, "Arm", 16384);
  _Sample_Out_var._index=18;
  /* component Sample_Out=Arm() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (A4)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _Mono_Out_var._rotation_absolute, _Sample_Out_var._rotation_absolute);
    rot_transpose(_Sample_Cradle_var._rotation_absolute, tr1);
    rot_mul(_Sample_Out_var._rotation_absolute, tr1, _Sample_Out_var._rotation_relative);
    _Sample_Out_var._rotation_is_identity =  rot_test_identity(_Sample_Out_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0);
    rot_transpose(_Sample_Cradle_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _Sample_Out_var._position_absolute = coords_add(_Sample_Cradle_var._position_absolute, tc2);
    tc1 = coords_sub(_Sample_Cradle_var._position_absolute, _Sample_Out_var._position_absolute);
    _Sample_Out_var._position_relative = rot_apply(_Sample_Out_var._rotation_absolute, tc1);
  } /* Sample_Out=Arm() AT ROTATED */
  DEBUG_COMPONENT("Sample_Out", _Sample_Out_var._position_absolute, _Sample_Out_var._rotation_absolute);
  instrument->_position_absolute[18] = _Sample_Out_var._position_absolute;
  instrument->_position_relative[18] = _Sample_Out_var._position_relative;
  instrument->counter_N[18]  = instrument->counter_P[18] = instrument->counter_P2[18] = 0;
  instrument->counter_AbsorbProp[18]= 0;
  return(0);
} /* _Sample_Out_setpos */

/* component Sample=V_sample() SETTING, POSITION/ROTATION */
int _Sample_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Sample_setpos] component Sample=V_sample() SETTING [/usr/share/mcstas/3.0-dev/obsolete/V_sample.comp:121]");
  stracpy(_Sample_var._name, "Sample", 16384);
  stracpy(_Sample_var._type, "V_sample", 16384);
  _Sample_var._index=19;
  _Sample_var._parameters.radius = 0.0064;
  #define radius (_Sample_var._parameters.radius)
  _Sample_var._parameters.thickness = 0;
  #define thickness (_Sample_var._parameters.thickness)
  _Sample_var._parameters.zdepth = 0;
  #define zdepth (_Sample_var._parameters.zdepth)
  _Sample_var._parameters.Vc = 13.827;
  #define Vc (_Sample_var._parameters.Vc)
  _Sample_var._parameters.sigma_abs = 5.08;
  #define sigma_abs (_Sample_var._parameters.sigma_abs)
  _Sample_var._parameters.sigma_inc = 5.08;
  #define sigma_inc (_Sample_var._parameters.sigma_inc)
  _Sample_var._parameters.radius_i = 0;
  #define radius_i (_Sample_var._parameters.radius_i)
  _Sample_var._parameters.radius_o = 0;
  #define radius_o (_Sample_var._parameters.radius_o)
  _Sample_var._parameters.h = 0;
  #define h (_Sample_var._parameters.h)
  _Sample_var._parameters.focus_r = 0;
  #define focus_r (_Sample_var._parameters.focus_r)
  _Sample_var._parameters.pack = 1;
  #define pack (_Sample_var._parameters.pack)
  _Sample_var._parameters.frac = 1;
  #define frac (_Sample_var._parameters.frac)
  _Sample_var._parameters.f_QE = 0;
  #define f_QE (_Sample_var._parameters.f_QE)
  _Sample_var._parameters.gamma = 0;
  #define gamma (_Sample_var._parameters.gamma)
  _Sample_var._parameters.target_x = 0;
  #define target_x (_Sample_var._parameters.target_x)
  _Sample_var._parameters.target_y = 0;
  #define target_y (_Sample_var._parameters.target_y)
  _Sample_var._parameters.target_z = 0;
  #define target_z (_Sample_var._parameters.target_z)
  _Sample_var._parameters.focus_xw = 0.0478;
  #define focus_xw (_Sample_var._parameters.focus_xw)
  _Sample_var._parameters.focus_yh = 0.049;
  #define focus_yh (_Sample_var._parameters.focus_yh)
  _Sample_var._parameters.focus_aw = 0;
  #define focus_aw (_Sample_var._parameters.focus_aw)
  _Sample_var._parameters.focus_ah = 0;
  #define focus_ah (_Sample_var._parameters.focus_ah)
  _Sample_var._parameters.xwidth = 0;
  #define xwidth (_Sample_var._parameters.xwidth)
  _Sample_var._parameters.yheight = 0.0254;
  #define yheight (_Sample_var._parameters.yheight)
  _Sample_var._parameters.zthick = 0;
  #define zthick (_Sample_var._parameters.zthick)
  _Sample_var._parameters.rad_sphere = 0;
  #define rad_sphere (_Sample_var._parameters.rad_sphere)
  _Sample_var._parameters.sig_a = 0;
  #define sig_a (_Sample_var._parameters.sig_a)
  _Sample_var._parameters.sig_i = 0;
  #define sig_i (_Sample_var._parameters.sig_i)
  _Sample_var._parameters.V0 = 0;
  #define V0 (_Sample_var._parameters.V0)
  _Sample_var._parameters.target_index = + 1;
  #define target_index (_Sample_var._parameters.target_index)
  _Sample_var._parameters.multiples = 1;
  #define multiples (_Sample_var._parameters.multiples)

  #define VarsV (_Sample_var._parameters.VarsV)

  #undef radius
  #undef thickness
  #undef zdepth
  #undef Vc
  #undef sigma_abs
  #undef sigma_inc
  #undef radius_i
  #undef radius_o
  #undef h
  #undef focus_r
  #undef pack
  #undef frac
  #undef f_QE
  #undef gamma
  #undef target_x
  #undef target_y
  #undef target_z
  #undef focus_xw
  #undef focus_yh
  #undef focus_aw
  #undef focus_ah
  #undef xwidth
  #undef yheight
  #undef zthick
  #undef rad_sphere
  #undef sig_a
  #undef sig_i
  #undef V0
  #undef target_index
  #undef multiples
  #undef VarsV
  /* component Sample=V_sample() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Sample_Out_var._rotation_absolute, _Sample_var._rotation_absolute);
    rot_transpose(_Sample_Out_var._rotation_absolute, tr1);
    rot_mul(_Sample_var._rotation_absolute, tr1, _Sample_var._rotation_relative);
    _Sample_var._rotation_is_identity =  rot_test_identity(_Sample_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0);
    rot_transpose(_Sample_Out_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _Sample_var._position_absolute = coords_add(_Sample_Out_var._position_absolute, tc2);
    tc1 = coords_sub(_Sample_Out_var._position_absolute, _Sample_var._position_absolute);
    _Sample_var._position_relative = rot_apply(_Sample_var._rotation_absolute, tc1);
  } /* Sample=V_sample() AT ROTATED */
  DEBUG_COMPONENT("Sample", _Sample_var._position_absolute, _Sample_var._rotation_absolute);
  instrument->_position_absolute[19] = _Sample_var._position_absolute;
  instrument->_position_relative[19] = _Sample_var._position_relative;
  instrument->counter_N[19]  = instrument->counter_P[19] = instrument->counter_P2[19] = 0;
  instrument->counter_AbsorbProp[19]= 0;
  return(0);
} /* _Sample_setpos */

/* component D7_SC3_In=PSD_monitor() SETTING, POSITION/ROTATION */
int _D7_SC3_In_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_D7_SC3_In_setpos] component D7_SC3_In=PSD_monitor() SETTING [/usr/share/mcstas/3.0-dev/monitors/PSD_monitor.comp:64]");
  stracpy(_D7_SC3_In_var._name, "D7_SC3_In", 16384);
  stracpy(_D7_SC3_In_var._type, "PSD_monitor", 16384);
  _D7_SC3_In_var._index=20;
  _D7_SC3_In_var._parameters.nx = 20;
  #define nx (_D7_SC3_In_var._parameters.nx)
  _D7_SC3_In_var._parameters.ny = 20;
  #define ny (_D7_SC3_In_var._parameters.ny)
  if("D7_SC3_In.psd" && strlen("D7_SC3_In.psd"))
    stracpy(_D7_SC3_In_var._parameters.filename, "D7_SC3_In.psd" ? "D7_SC3_In.psd" : "", 16384);
  else 
  _D7_SC3_In_var._parameters.filename[0]='\0';
  #define filename (_D7_SC3_In_var._parameters.filename)
  _D7_SC3_In_var._parameters.xmin = -0.05;
  #define xmin (_D7_SC3_In_var._parameters.xmin)
  _D7_SC3_In_var._parameters.xmax = 0.05;
  #define xmax (_D7_SC3_In_var._parameters.xmax)
  _D7_SC3_In_var._parameters.ymin = -0.05;
  #define ymin (_D7_SC3_In_var._parameters.ymin)
  _D7_SC3_In_var._parameters.ymax = 0.05;
  #define ymax (_D7_SC3_In_var._parameters.ymax)
  _D7_SC3_In_var._parameters.xwidth = 0.0478;
  #define xwidth (_D7_SC3_In_var._parameters.xwidth)
  _D7_SC3_In_var._parameters.yheight = 0.049;
  #define yheight (_D7_SC3_In_var._parameters.yheight)
  _D7_SC3_In_var._parameters.restore_neutron = 0;
  #define restore_neutron (_D7_SC3_In_var._parameters.restore_neutron)

  #define PSD_N (_D7_SC3_In_var._parameters.PSD_N)
  #define PSD_p (_D7_SC3_In_var._parameters.PSD_p)
  #define PSD_p2 (_D7_SC3_In_var._parameters.PSD_p2)

  #undef nx
  #undef ny
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  /* component D7_SC3_In=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Sample_Out_var._rotation_absolute, _D7_SC3_In_var._rotation_absolute);
    rot_transpose(_Sample_var._rotation_absolute, tr1);
    rot_mul(_D7_SC3_In_var._rotation_absolute, tr1, _D7_SC3_In_var._rotation_relative);
    _D7_SC3_In_var._rotation_is_identity =  rot_test_identity(_D7_SC3_In_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.2349);
    rot_transpose(_Sample_Out_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _D7_SC3_In_var._position_absolute = coords_add(_Sample_Out_var._position_absolute, tc2);
    tc1 = coords_sub(_Sample_var._position_absolute, _D7_SC3_In_var._position_absolute);
    _D7_SC3_In_var._position_relative = rot_apply(_D7_SC3_In_var._rotation_absolute, tc1);
  } /* D7_SC3_In=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("D7_SC3_In", _D7_SC3_In_var._position_absolute, _D7_SC3_In_var._rotation_absolute);
  instrument->_position_absolute[20] = _D7_SC3_In_var._position_absolute;
  instrument->_position_relative[20] = _D7_SC3_In_var._position_relative;
  instrument->counter_N[20]  = instrument->counter_P[20] = instrument->counter_P2[20] = 0;
  instrument->counter_AbsorbProp[20]= 0;
  return(0);
} /* _D7_SC3_In_setpos */

/* component SC3=Guide_simple() SETTING, POSITION/ROTATION */
int _SC3_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_SC3_setpos] component SC3=Guide_simple() SETTING [/usr/share/mcstas/3.0-dev/optics/Guide_simple.comp:72]");
  stracpy(_SC3_var._name, "SC3", 16384);
  stracpy(_SC3_var._type, "Guide_simple", 16384);
  _SC3_var._index=21;
  _SC3_var._parameters.w1 = 0.0478;
  #define w1 (_SC3_var._parameters.w1)
  _SC3_var._parameters.h1 = 0.0490;
  #define h1 (_SC3_var._parameters.h1)
  _SC3_var._parameters.w2 = 0;
  #define w2 (_SC3_var._parameters.w2)
  _SC3_var._parameters.h2 = 0;
  #define h2 (_SC3_var._parameters.h2)
  _SC3_var._parameters.l = 0.3048;
  #define l (_SC3_var._parameters.l)
  _SC3_var._parameters.R0 = 1.0;
  #define R0 (_SC3_var._parameters.R0)
  _SC3_var._parameters.Qc = 0.021;
  #define Qc (_SC3_var._parameters.Qc)
  _SC3_var._parameters.alpha = 6;
  #define alpha (_SC3_var._parameters.alpha)
  _SC3_var._parameters.m = 1;
  #define m (_SC3_var._parameters.m)
  _SC3_var._parameters.W = 0.0003;
  #define W (_SC3_var._parameters.W)

  #undef w1
  #undef h1
  #undef w2
  #undef h2
  #undef l
  #undef R0
  #undef Qc
  #undef alpha
  #undef m
  #undef W
  /* component SC3=Guide_simple() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Sample_Out_var._rotation_absolute, _SC3_var._rotation_absolute);
    rot_transpose(_D7_SC3_In_var._rotation_absolute, tr1);
    rot_mul(_SC3_var._rotation_absolute, tr1, _SC3_var._rotation_relative);
    _SC3_var._rotation_is_identity =  rot_test_identity(_SC3_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.2350);
    rot_transpose(_Sample_Out_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _SC3_var._position_absolute = coords_add(_Sample_Out_var._position_absolute, tc2);
    tc1 = coords_sub(_D7_SC3_In_var._position_absolute, _SC3_var._position_absolute);
    _SC3_var._position_relative = rot_apply(_SC3_var._rotation_absolute, tc1);
  } /* SC3=Guide_simple() AT ROTATED */
  DEBUG_COMPONENT("SC3", _SC3_var._position_absolute, _SC3_var._rotation_absolute);
  instrument->_position_absolute[21] = _SC3_var._position_absolute;
  instrument->_position_relative[21] = _SC3_var._position_relative;
  instrument->counter_N[21]  = instrument->counter_P[21] = instrument->counter_P2[21] = 0;
  instrument->counter_AbsorbProp[21]= 0;
  return(0);
} /* _SC3_setpos */

/* component D8_SC3_Out=PSD_monitor() SETTING, POSITION/ROTATION */
int _D8_SC3_Out_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_D8_SC3_Out_setpos] component D8_SC3_Out=PSD_monitor() SETTING [/usr/share/mcstas/3.0-dev/monitors/PSD_monitor.comp:64]");
  stracpy(_D8_SC3_Out_var._name, "D8_SC3_Out", 16384);
  stracpy(_D8_SC3_Out_var._type, "PSD_monitor", 16384);
  _D8_SC3_Out_var._index=22;
  _D8_SC3_Out_var._parameters.nx = 20;
  #define nx (_D8_SC3_Out_var._parameters.nx)
  _D8_SC3_Out_var._parameters.ny = 20;
  #define ny (_D8_SC3_Out_var._parameters.ny)
  if("D8_SC3_Out.psd" && strlen("D8_SC3_Out.psd"))
    stracpy(_D8_SC3_Out_var._parameters.filename, "D8_SC3_Out.psd" ? "D8_SC3_Out.psd" : "", 16384);
  else 
  _D8_SC3_Out_var._parameters.filename[0]='\0';
  #define filename (_D8_SC3_Out_var._parameters.filename)
  _D8_SC3_Out_var._parameters.xmin = -0.05;
  #define xmin (_D8_SC3_Out_var._parameters.xmin)
  _D8_SC3_Out_var._parameters.xmax = 0.05;
  #define xmax (_D8_SC3_Out_var._parameters.xmax)
  _D8_SC3_Out_var._parameters.ymin = -0.05;
  #define ymin (_D8_SC3_Out_var._parameters.ymin)
  _D8_SC3_Out_var._parameters.ymax = 0.05;
  #define ymax (_D8_SC3_Out_var._parameters.ymax)
  _D8_SC3_Out_var._parameters.xwidth = 0.0478;
  #define xwidth (_D8_SC3_Out_var._parameters.xwidth)
  _D8_SC3_Out_var._parameters.yheight = 0.049;
  #define yheight (_D8_SC3_Out_var._parameters.yheight)
  _D8_SC3_Out_var._parameters.restore_neutron = 0;
  #define restore_neutron (_D8_SC3_Out_var._parameters.restore_neutron)

  #define PSD_N (_D8_SC3_Out_var._parameters.PSD_N)
  #define PSD_p (_D8_SC3_Out_var._parameters.PSD_p)
  #define PSD_p2 (_D8_SC3_Out_var._parameters.PSD_p2)

  #undef nx
  #undef ny
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  /* component D8_SC3_Out=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _SC3_var._rotation_absolute, _D8_SC3_Out_var._rotation_absolute);
    rot_transpose(_SC3_var._rotation_absolute, tr1);
    rot_mul(_D8_SC3_Out_var._rotation_absolute, tr1, _D8_SC3_Out_var._rotation_relative);
    _D8_SC3_Out_var._rotation_is_identity =  rot_test_identity(_D8_SC3_Out_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.3047);
    rot_transpose(_SC3_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _D8_SC3_Out_var._position_absolute = coords_add(_SC3_var._position_absolute, tc2);
    tc1 = coords_sub(_SC3_var._position_absolute, _D8_SC3_Out_var._position_absolute);
    _D8_SC3_Out_var._position_relative = rot_apply(_D8_SC3_Out_var._rotation_absolute, tc1);
  } /* D8_SC3_Out=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("D8_SC3_Out", _D8_SC3_Out_var._position_absolute, _D8_SC3_Out_var._rotation_absolute);
  instrument->_position_absolute[22] = _D8_SC3_Out_var._position_absolute;
  instrument->_position_relative[22] = _D8_SC3_Out_var._position_relative;
  instrument->counter_N[22]  = instrument->counter_P[22] = instrument->counter_P2[22] = 0;
  instrument->counter_AbsorbProp[22]= 0;
  return(0);
} /* _D8_SC3_Out_setpos */

/* component Ana_Cradle=Arm() SETTING, POSITION/ROTATION */
int _Ana_Cradle_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Ana_Cradle_setpos] component Ana_Cradle=Arm() SETTING [Arm:0]");
  stracpy(_Ana_Cradle_var._name, "Ana_Cradle", 16384);
  stracpy(_Ana_Cradle_var._type, "Arm", 16384);
  _Ana_Cradle_var._index=23;
  /* component Ana_Cradle=Arm() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (A5)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _Sample_Out_var._rotation_absolute, _Ana_Cradle_var._rotation_absolute);
    rot_transpose(_D8_SC3_Out_var._rotation_absolute, tr1);
    rot_mul(_Ana_Cradle_var._rotation_absolute, tr1, _Ana_Cradle_var._rotation_relative);
    _Ana_Cradle_var._rotation_is_identity =  rot_test_identity(_Ana_Cradle_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.1397);
    rot_transpose(_D8_SC3_Out_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _Ana_Cradle_var._position_absolute = coords_add(_D8_SC3_Out_var._position_absolute, tc2);
    tc1 = coords_sub(_D8_SC3_Out_var._position_absolute, _Ana_Cradle_var._position_absolute);
    _Ana_Cradle_var._position_relative = rot_apply(_Ana_Cradle_var._rotation_absolute, tc1);
  } /* Ana_Cradle=Arm() AT ROTATED */
  DEBUG_COMPONENT("Ana_Cradle", _Ana_Cradle_var._position_absolute, _Ana_Cradle_var._rotation_absolute);
  instrument->_position_absolute[23] = _Ana_Cradle_var._position_absolute;
  instrument->_position_relative[23] = _Ana_Cradle_var._position_relative;
  instrument->counter_N[23]  = instrument->counter_P[23] = instrument->counter_P2[23] = 0;
  instrument->counter_AbsorbProp[23]= 0;
  return(0);
} /* _Ana_Cradle_setpos */

/* component PG2Xtal=Monochromator_flat() SETTING, POSITION/ROTATION */
int _PG2Xtal_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_PG2Xtal_setpos] component PG2Xtal=Monochromator_flat() SETTING [/usr/share/mcstas/3.0-dev/optics/Monochromator_flat.comp:103]");
  stracpy(_PG2Xtal_var._name, "PG2Xtal", 16384);
  stracpy(_PG2Xtal_var._type, "Monochromator_flat", 16384);
  _PG2Xtal_var._index=24;
  _PG2Xtal_var._parameters.zmin = -0.05;
  #define zmin (_PG2Xtal_var._parameters.zmin)
  _PG2Xtal_var._parameters.zmax = 0.05;
  #define zmax (_PG2Xtal_var._parameters.zmax)
  _PG2Xtal_var._parameters.ymin = -0.05;
  #define ymin (_PG2Xtal_var._parameters.ymin)
  _PG2Xtal_var._parameters.ymax = 0.05;
  #define ymax (_PG2Xtal_var._parameters.ymax)
  _PG2Xtal_var._parameters.zwidth = 0.10;
  #define zwidth (_PG2Xtal_var._parameters.zwidth)
  _PG2Xtal_var._parameters.yheight = 0.08;
  #define yheight (_PG2Xtal_var._parameters.yheight)
  _PG2Xtal_var._parameters.mosaich = 40;
  #define mosaich (_PG2Xtal_var._parameters.mosaich)
  _PG2Xtal_var._parameters.mosaicv = 40;
  #define mosaicv (_PG2Xtal_var._parameters.mosaicv)
  _PG2Xtal_var._parameters.r0 = 0.7;
  #define r0 (_PG2Xtal_var._parameters.r0)
  _PG2Xtal_var._parameters.Q = mono_q;
  #define Q (_PG2Xtal_var._parameters.Q)
  _PG2Xtal_var._parameters.DM = 0;
  #define DM (_PG2Xtal_var._parameters.DM)

  #define mos_rms_y (_PG2Xtal_var._parameters.mos_rms_y)
  #define mos_rms_z (_PG2Xtal_var._parameters.mos_rms_z)
  #define mos_rms_max (_PG2Xtal_var._parameters.mos_rms_max)
  #define mono_Q (_PG2Xtal_var._parameters.mono_Q)

  #undef zmin
  #undef zmax
  #undef ymin
  #undef ymax
  #undef zwidth
  #undef yheight
  #undef mosaich
  #undef mosaicv
  #undef r0
  #undef Q
  #undef DM
  #undef mos_rms_y
  #undef mos_rms_z
  #undef mos_rms_max
  #undef mono_Q
  /* component PG2Xtal=Monochromator_flat() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Ana_Cradle_var._rotation_absolute, _PG2Xtal_var._rotation_absolute);
    rot_transpose(_Ana_Cradle_var._rotation_absolute, tr1);
    rot_mul(_PG2Xtal_var._rotation_absolute, tr1, _PG2Xtal_var._rotation_relative);
    _PG2Xtal_var._rotation_is_identity =  rot_test_identity(_PG2Xtal_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0);
    rot_transpose(_Ana_Cradle_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _PG2Xtal_var._position_absolute = coords_add(_Ana_Cradle_var._position_absolute, tc2);
    tc1 = coords_sub(_Ana_Cradle_var._position_absolute, _PG2Xtal_var._position_absolute);
    _PG2Xtal_var._position_relative = rot_apply(_PG2Xtal_var._rotation_absolute, tc1);
  } /* PG2Xtal=Monochromator_flat() AT ROTATED */
  DEBUG_COMPONENT("PG2Xtal", _PG2Xtal_var._position_absolute, _PG2Xtal_var._rotation_absolute);
  instrument->_position_absolute[24] = _PG2Xtal_var._position_absolute;
  instrument->_position_relative[24] = _PG2Xtal_var._position_relative;
  instrument->counter_N[24]  = instrument->counter_P[24] = instrument->counter_P2[24] = 0;
  instrument->counter_AbsorbProp[24]= 0;
  return(0);
} /* _PG2Xtal_setpos */

/* component Ana_Out=Arm() SETTING, POSITION/ROTATION */
int _Ana_Out_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Ana_Out_setpos] component Ana_Out=Arm() SETTING [Arm:0]");
  stracpy(_Ana_Out_var._name, "Ana_Out", 16384);
  stracpy(_Ana_Out_var._type, "Arm", 16384);
  _Ana_Out_var._index=25;
  /* component Ana_Out=Arm() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (A6)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _Sample_Out_var._rotation_absolute, _Ana_Out_var._rotation_absolute);
    rot_transpose(_PG2Xtal_var._rotation_absolute, tr1);
    rot_mul(_Ana_Out_var._rotation_absolute, tr1, _Ana_Out_var._rotation_relative);
    _Ana_Out_var._rotation_is_identity =  rot_test_identity(_Ana_Out_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0);
    rot_transpose(_Ana_Cradle_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _Ana_Out_var._position_absolute = coords_add(_Ana_Cradle_var._position_absolute, tc2);
    tc1 = coords_sub(_PG2Xtal_var._position_absolute, _Ana_Out_var._position_absolute);
    _Ana_Out_var._position_relative = rot_apply(_Ana_Out_var._rotation_absolute, tc1);
  } /* Ana_Out=Arm() AT ROTATED */
  DEBUG_COMPONENT("Ana_Out", _Ana_Out_var._position_absolute, _Ana_Out_var._rotation_absolute);
  instrument->_position_absolute[25] = _Ana_Out_var._position_absolute;
  instrument->_position_relative[25] = _Ana_Out_var._position_relative;
  instrument->counter_N[25]  = instrument->counter_P[25] = instrument->counter_P2[25] = 0;
  instrument->counter_AbsorbProp[25]= 0;
  return(0);
} /* _Ana_Out_setpos */

/* component D10_SC4_In=PSD_monitor() SETTING, POSITION/ROTATION */
int _D10_SC4_In_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_D10_SC4_In_setpos] component D10_SC4_In=PSD_monitor() SETTING [/usr/share/mcstas/3.0-dev/monitors/PSD_monitor.comp:64]");
  stracpy(_D10_SC4_In_var._name, "D10_SC4_In", 16384);
  stracpy(_D10_SC4_In_var._type, "PSD_monitor", 16384);
  _D10_SC4_In_var._index=26;
  _D10_SC4_In_var._parameters.nx = 20;
  #define nx (_D10_SC4_In_var._parameters.nx)
  _D10_SC4_In_var._parameters.ny = 20;
  #define ny (_D10_SC4_In_var._parameters.ny)
  if("D10_SC4_In.psd" && strlen("D10_SC4_In.psd"))
    stracpy(_D10_SC4_In_var._parameters.filename, "D10_SC4_In.psd" ? "D10_SC4_In.psd" : "", 16384);
  else 
  _D10_SC4_In_var._parameters.filename[0]='\0';
  #define filename (_D10_SC4_In_var._parameters.filename)
  _D10_SC4_In_var._parameters.xmin = -0.05;
  #define xmin (_D10_SC4_In_var._parameters.xmin)
  _D10_SC4_In_var._parameters.xmax = 0.05;
  #define xmax (_D10_SC4_In_var._parameters.xmax)
  _D10_SC4_In_var._parameters.ymin = -0.05;
  #define ymin (_D10_SC4_In_var._parameters.ymin)
  _D10_SC4_In_var._parameters.ymax = 0.05;
  #define ymax (_D10_SC4_In_var._parameters.ymax)
  _D10_SC4_In_var._parameters.xwidth = 0.0478;
  #define xwidth (_D10_SC4_In_var._parameters.xwidth)
  _D10_SC4_In_var._parameters.yheight = 0.049;
  #define yheight (_D10_SC4_In_var._parameters.yheight)
  _D10_SC4_In_var._parameters.restore_neutron = 0;
  #define restore_neutron (_D10_SC4_In_var._parameters.restore_neutron)

  #define PSD_N (_D10_SC4_In_var._parameters.PSD_N)
  #define PSD_p (_D10_SC4_In_var._parameters.PSD_p)
  #define PSD_p2 (_D10_SC4_In_var._parameters.PSD_p2)

  #undef nx
  #undef ny
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  /* component D10_SC4_In=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Ana_Out_var._rotation_absolute, _D10_SC4_In_var._rotation_absolute);
    rot_transpose(_Ana_Out_var._rotation_absolute, tr1);
    rot_mul(_D10_SC4_In_var._rotation_absolute, tr1, _D10_SC4_In_var._rotation_relative);
    _D10_SC4_In_var._rotation_is_identity =  rot_test_identity(_D10_SC4_In_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.3365);
    rot_transpose(_Ana_Out_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _D10_SC4_In_var._position_absolute = coords_add(_Ana_Out_var._position_absolute, tc2);
    tc1 = coords_sub(_Ana_Out_var._position_absolute, _D10_SC4_In_var._position_absolute);
    _D10_SC4_In_var._position_relative = rot_apply(_D10_SC4_In_var._rotation_absolute, tc1);
  } /* D10_SC4_In=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("D10_SC4_In", _D10_SC4_In_var._position_absolute, _D10_SC4_In_var._rotation_absolute);
  instrument->_position_absolute[26] = _D10_SC4_In_var._position_absolute;
  instrument->_position_relative[26] = _D10_SC4_In_var._position_relative;
  instrument->counter_N[26]  = instrument->counter_P[26] = instrument->counter_P2[26] = 0;
  instrument->counter_AbsorbProp[26]= 0;
  return(0);
} /* _D10_SC4_In_setpos */

/* component SC4=Guide_simple() SETTING, POSITION/ROTATION */
int _SC4_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_SC4_setpos] component SC4=Guide_simple() SETTING [/usr/share/mcstas/3.0-dev/optics/Guide_simple.comp:72]");
  stracpy(_SC4_var._name, "SC4", 16384);
  stracpy(_SC4_var._type, "Guide_simple", 16384);
  _SC4_var._index=27;
  _SC4_var._parameters.w1 = 0.0478;
  #define w1 (_SC4_var._parameters.w1)
  _SC4_var._parameters.h1 = 0.0490;
  #define h1 (_SC4_var._parameters.h1)
  _SC4_var._parameters.w2 = 0;
  #define w2 (_SC4_var._parameters.w2)
  _SC4_var._parameters.h2 = 0;
  #define h2 (_SC4_var._parameters.h2)
  _SC4_var._parameters.l = 0.3048;
  #define l (_SC4_var._parameters.l)
  _SC4_var._parameters.R0 = 1.0;
  #define R0 (_SC4_var._parameters.R0)
  _SC4_var._parameters.Qc = 0.021;
  #define Qc (_SC4_var._parameters.Qc)
  _SC4_var._parameters.alpha = 6;
  #define alpha (_SC4_var._parameters.alpha)
  _SC4_var._parameters.m = 1;
  #define m (_SC4_var._parameters.m)
  _SC4_var._parameters.W = 0.0003;
  #define W (_SC4_var._parameters.W)

  #undef w1
  #undef h1
  #undef w2
  #undef h2
  #undef l
  #undef R0
  #undef Qc
  #undef alpha
  #undef m
  #undef W
  /* component SC4=Guide_simple() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Ana_Out_var._rotation_absolute, _SC4_var._rotation_absolute);
    rot_transpose(_D10_SC4_In_var._rotation_absolute, tr1);
    rot_mul(_SC4_var._rotation_absolute, tr1, _SC4_var._rotation_relative);
    _SC4_var._rotation_is_identity =  rot_test_identity(_SC4_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.3366);
    rot_transpose(_Ana_Out_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _SC4_var._position_absolute = coords_add(_Ana_Out_var._position_absolute, tc2);
    tc1 = coords_sub(_D10_SC4_In_var._position_absolute, _SC4_var._position_absolute);
    _SC4_var._position_relative = rot_apply(_SC4_var._rotation_absolute, tc1);
  } /* SC4=Guide_simple() AT ROTATED */
  DEBUG_COMPONENT("SC4", _SC4_var._position_absolute, _SC4_var._rotation_absolute);
  instrument->_position_absolute[27] = _SC4_var._position_absolute;
  instrument->_position_relative[27] = _SC4_var._position_relative;
  instrument->counter_N[27]  = instrument->counter_P[27] = instrument->counter_P2[27] = 0;
  instrument->counter_AbsorbProp[27]= 0;
  return(0);
} /* _SC4_setpos */

/* component He3H=PSD_monitor() SETTING, POSITION/ROTATION */
int _He3H_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_He3H_setpos] component He3H=PSD_monitor() SETTING [/usr/share/mcstas/3.0-dev/monitors/PSD_monitor.comp:64]");
  stracpy(_He3H_var._name, "He3H", 16384);
  stracpy(_He3H_var._type, "PSD_monitor", 16384);
  _He3H_var._index=28;
  _He3H_var._parameters.nx = 20;
  #define nx (_He3H_var._parameters.nx)
  _He3H_var._parameters.ny = 20;
  #define ny (_He3H_var._parameters.ny)
  if("He3.psd" && strlen("He3.psd"))
    stracpy(_He3H_var._parameters.filename, "He3.psd" ? "He3.psd" : "", 16384);
  else 
  _He3H_var._parameters.filename[0]='\0';
  #define filename (_He3H_var._parameters.filename)
  _He3H_var._parameters.xmin = -0.05;
  #define xmin (_He3H_var._parameters.xmin)
  _He3H_var._parameters.xmax = 0.05;
  #define xmax (_He3H_var._parameters.xmax)
  _He3H_var._parameters.ymin = -0.05;
  #define ymin (_He3H_var._parameters.ymin)
  _He3H_var._parameters.ymax = 0.05;
  #define ymax (_He3H_var._parameters.ymax)
  _He3H_var._parameters.xwidth = 0.0508;
  #define xwidth (_He3H_var._parameters.xwidth)
  _He3H_var._parameters.yheight = 0.0857;
  #define yheight (_He3H_var._parameters.yheight)
  _He3H_var._parameters.restore_neutron = 0;
  #define restore_neutron (_He3H_var._parameters.restore_neutron)

  #define PSD_N (_He3H_var._parameters.PSD_N)
  #define PSD_p (_He3H_var._parameters.PSD_p)
  #define PSD_p2 (_He3H_var._parameters.PSD_p2)

  #undef nx
  #undef ny
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  /* component He3H=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _SC4_var._rotation_absolute, _He3H_var._rotation_absolute);
    rot_transpose(_SC4_var._rotation_absolute, tr1);
    rot_mul(_He3H_var._rotation_absolute, tr1, _He3H_var._rotation_relative);
    _He3H_var._rotation_is_identity =  rot_test_identity(_He3H_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.3049);
    rot_transpose(_SC4_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _He3H_var._position_absolute = coords_add(_SC4_var._position_absolute, tc2);
    tc1 = coords_sub(_SC4_var._position_absolute, _He3H_var._position_absolute);
    _He3H_var._position_relative = rot_apply(_He3H_var._rotation_absolute, tc1);
  } /* He3H=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("He3H", _He3H_var._position_absolute, _He3H_var._rotation_absolute);
  instrument->_position_absolute[28] = _He3H_var._position_absolute;
  instrument->_position_relative[28] = _He3H_var._position_relative;
  instrument->counter_N[28]  = instrument->counter_P[28] = instrument->counter_P2[28] = 0;
  instrument->counter_AbsorbProp[28]= 0;
  return(0);
} /* _He3H_setpos */

_class_Source_simple *class_Source_simple_init(_class_Source_simple *_comp
) {
  #define radius (_comp->_parameters.radius)
  #define yheight (_comp->_parameters.yheight)
  #define xwidth (_comp->_parameters.xwidth)
  #define dist (_comp->_parameters.dist)
  #define focus_xw (_comp->_parameters.focus_xw)
  #define focus_yh (_comp->_parameters.focus_yh)
  #define E0 (_comp->_parameters.E0)
  #define dE (_comp->_parameters.dE)
  #define lambda0 (_comp->_parameters.lambda0)
  #define dlambda (_comp->_parameters.dlambda)
  #define flux (_comp->_parameters.flux)
  #define gauss (_comp->_parameters.gauss)
  #define target_index (_comp->_parameters.target_index)
  #define pmul (_comp->_parameters.pmul)
  #define square (_comp->_parameters.square)
  #define srcArea (_comp->_parameters.srcArea)
  SIG_MESSAGE("[_Source_init] component Source=Source_simple() INITIALISE [/usr/share/mcstas/3.0-dev/sources/Source_simple.comp:64]");
square = 0;
/* Determine source area */
if (radius && !yheight && !xwidth ) {
    square = 0;
    srcArea = PI*radius*radius;
  } else if(yheight && xwidth) {
    square = 1;
    srcArea = xwidth * yheight;
  }

  if (flux) {
    pmul=flux*1e4*srcArea/mcget_ncount();
    if (dlambda)
      pmul *= 2*dlambda;
    else if (dE)
      pmul *= 2*dE;
  } else {
    gauss = 0;
    pmul=1.0/(mcget_ncount()*4*PI);
  }

  if (target_index && !dist)
  {
    Coords ToTarget;
    double tx,ty,tz;
    ToTarget = coords_sub(POS_A_COMP_INDEX(INDEX_CURRENT_COMP+target_index),POS_A_CURRENT_COMP);
    ToTarget = rot_apply(ROT_A_CURRENT_COMP, ToTarget);
    coords_get(ToTarget, &tx, &ty, &tz);
    dist=sqrt(tx*tx+ty*ty+tz*tz);
  }

  if (srcArea <= 0) {
    printf("Source_simple: %s: Source area is <= 0 !\n ERROR - Exiting\n",
           NAME_CURRENT_COMP);
    exit(0);
  }
  if (dist <= 0 || focus_xw <= 0 || focus_yh <= 0) {
    printf("Source_simple: %s: Target area unmeaningful! (negative dist / focus_xw / focus_yh)\n ERROR - Exiting\n",
           NAME_CURRENT_COMP);
    exit(0);
  }

  if ((!lambda0 && !E0 && !dE && !dlambda)) {
    printf("Source_simple: %s: You must specify either a wavelength or energy range!\n ERROR - Exiting\n",
           NAME_CURRENT_COMP);
    exit(0);
  }
  if ((!lambda0 && !dlambda && (E0 <= 0 || dE < 0 || E0-dE <= 0))
    || (!E0 && !dE && (lambda0 <= 0 || dlambda < 0 || lambda0-dlambda <= 0))) {
    printf("Source_simple: %s: Unmeaningful definition of wavelength or energy range!\n ERROR - Exiting\n",
           NAME_CURRENT_COMP);
      exit(0);
  }
  #undef radius
  #undef yheight
  #undef xwidth
  #undef dist
  #undef focus_xw
  #undef focus_yh
  #undef E0
  #undef dE
  #undef lambda0
  #undef dlambda
  #undef flux
  #undef gauss
  #undef target_index
  #undef pmul
  #undef square
  #undef srcArea
  return(_comp);
} /* class_Source_simple_init */

_class_PSD_monitor *class_PSD_monitor_init(_class_PSD_monitor *_comp
) {
  #define nx (_comp->_parameters.nx)
  #define ny (_comp->_parameters.ny)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define PSD_N (_comp->_parameters.PSD_N)
  #define PSD_p (_comp->_parameters.PSD_p)
  #define PSD_p2 (_comp->_parameters.PSD_p2)
  SIG_MESSAGE("[_D0_Source_init] component D0_Source=PSD_monitor() INITIALISE [/usr/share/mcstas/3.0-dev/monitors/PSD_monitor.comp:64]");
  if (xwidth  > 0) { xmax = xwidth/2;  xmin = -xmax; }
  if (yheight > 0) { ymax = yheight/2; ymin = -ymax; }

  if ((xmin >= xmax) || (ymin >= ymax)){
    printf("PSD_monitor: %s: Null detection area !\n"
           "ERROR        (xwidth,yheight,xmin,xmax,ymin,ymax). Exiting",
    NAME_CURRENT_COMP);
    exit(0);
  }

  PSD_N = create_darr2d(nx, ny);
  PSD_p = create_darr2d(nx, ny);
  PSD_p2 = create_darr2d(nx, ny);

  int i, j;
  for (i=0; i<nx; i++){
    for (j=0; j<ny; j++){
      PSD_N[i][j] = 0;
      PSD_p[i][j] = 0;
      PSD_p2[i][j] = 0;
    }
  }
  #undef nx
  #undef ny
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  return(_comp);
} /* class_PSD_monitor_init */

_class_Guide_simple *class_Guide_simple_init(_class_Guide_simple *_comp
) {
  #define w1 (_comp->_parameters.w1)
  #define h1 (_comp->_parameters.h1)
  #define w2 (_comp->_parameters.w2)
  #define h2 (_comp->_parameters.h2)
  #define l (_comp->_parameters.l)
  #define R0 (_comp->_parameters.R0)
  #define Qc (_comp->_parameters.Qc)
  #define alpha (_comp->_parameters.alpha)
  #define m (_comp->_parameters.m)
  #define W (_comp->_parameters.W)
  SIG_MESSAGE("[_SC1_init] component SC1=Guide_simple() INITIALISE [/usr/share/mcstas/3.0-dev/optics/Guide_simple.comp:72]");
if (mcgravitation) fprintf(stderr,"WARNING: Guide: %s: "
    "This component produces wrong results with gravitation !\n"
    "Use Guide_gravity.\n",
    NAME_CURRENT_COMP);

  if (!w2) w2=w1;
  if (!h2) h2=h1;

  //  if (reflect && strlen(reflect) && strcmp(reflect,"NULL") && strcmp(reflect,"0")) {
  //  if (Table_Read(&pTable, reflect, 1) <= 0) /* read 1st block data from file into pTable */
  //    exit(fprintf(stderr,"Guide: %s: can not read file %s\n", NAME_CURRENT_COMP, reflect));
  //} else {
    if (W < 0 || R0 < 0 || Qc < 0 || m < 0)
    { fprintf(stderr,"Guide: %s: W R0 Qc must be >0.\n", NAME_CURRENT_COMP);
      exit(-1); }
    //}
  #undef w1
  #undef h1
  #undef w2
  #undef h2
  #undef l
  #undef R0
  #undef Qc
  #undef alpha
  #undef m
  #undef W
  return(_comp);
} /* class_Guide_simple_init */

_class_Slit *class_Slit_init(_class_Slit *_comp
) {
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define radius (_comp->_parameters.radius)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  SIG_MESSAGE("[_As1_init] component As1=Slit() INITIALISE [/usr/share/mcstas/3.0-dev/optics/Slit.comp:47]");
if (xwidth > 0)  { xmax=xwidth/2;  xmin=-xmax; }
  if (yheight > 0) { ymax=yheight/2; ymin=-ymax; }
  if (xmin == 0 && xmax == 0 && ymin == 0 && ymax == 0 && radius == 0)
    { fprintf(stderr,"Slit: %s: Error: give geometry\n", NAME_CURRENT_COMP); exit(-1); }

  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef radius
  #undef xwidth
  #undef yheight
  return(_comp);
} /* class_Slit_init */

_class_Monochromator_flat *class_Monochromator_flat_init(_class_Monochromator_flat *_comp
) {
  #define zmin (_comp->_parameters.zmin)
  #define zmax (_comp->_parameters.zmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define zwidth (_comp->_parameters.zwidth)
  #define yheight (_comp->_parameters.yheight)
  #define mosaich (_comp->_parameters.mosaich)
  #define mosaicv (_comp->_parameters.mosaicv)
  #define r0 (_comp->_parameters.r0)
  #define Q (_comp->_parameters.Q)
  #define DM (_comp->_parameters.DM)
  #define mos_rms_y (_comp->_parameters.mos_rms_y)
  #define mos_rms_z (_comp->_parameters.mos_rms_z)
  #define mos_rms_max (_comp->_parameters.mos_rms_max)
  #define mono_Q (_comp->_parameters.mono_Q)
  SIG_MESSAGE("[_PG1Xtal_init] component PG1Xtal=Monochromator_flat() INITIALISE [/usr/share/mcstas/3.0-dev/optics/Monochromator_flat.comp:103]");
  mos_rms_y = MIN2RAD*mosaicv/sqrt(8*log(2));
  mos_rms_z = MIN2RAD*mosaich/sqrt(8*log(2));
  mos_rms_max = mos_rms_y > mos_rms_z ? mos_rms_y : mos_rms_z;

  mono_Q = Q;
  if (DM != 0) mono_Q = 2*PI/DM;

  if (zwidth>0)  { zmax = zwidth/2;  zmin=-zmax; }
  if (yheight>0) { ymax = yheight/2; ymin=-ymax; }

  if (zmin==zmax || ymin==ymax)
    exit(fprintf(stderr, "Monochromator_flat: %s : Surface is null (zmin,zmax,ymin,ymax)\n", NAME_CURRENT_COMP));
  #undef zmin
  #undef zmax
  #undef ymin
  #undef ymax
  #undef zwidth
  #undef yheight
  #undef mosaich
  #undef mosaicv
  #undef r0
  #undef Q
  #undef DM
  #undef mos_rms_y
  #undef mos_rms_z
  #undef mos_rms_max
  #undef mono_Q
  return(_comp);
} /* class_Monochromator_flat_init */

_class_V_sample *class_V_sample_init(_class_V_sample *_comp
) {
  #define radius (_comp->_parameters.radius)
  #define thickness (_comp->_parameters.thickness)
  #define zdepth (_comp->_parameters.zdepth)
  #define Vc (_comp->_parameters.Vc)
  #define sigma_abs (_comp->_parameters.sigma_abs)
  #define sigma_inc (_comp->_parameters.sigma_inc)
  #define radius_i (_comp->_parameters.radius_i)
  #define radius_o (_comp->_parameters.radius_o)
  #define h (_comp->_parameters.h)
  #define focus_r (_comp->_parameters.focus_r)
  #define pack (_comp->_parameters.pack)
  #define frac (_comp->_parameters.frac)
  #define f_QE (_comp->_parameters.f_QE)
  #define gamma (_comp->_parameters.gamma)
  #define target_x (_comp->_parameters.target_x)
  #define target_y (_comp->_parameters.target_y)
  #define target_z (_comp->_parameters.target_z)
  #define focus_xw (_comp->_parameters.focus_xw)
  #define focus_yh (_comp->_parameters.focus_yh)
  #define focus_aw (_comp->_parameters.focus_aw)
  #define focus_ah (_comp->_parameters.focus_ah)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define zthick (_comp->_parameters.zthick)
  #define rad_sphere (_comp->_parameters.rad_sphere)
  #define sig_a (_comp->_parameters.sig_a)
  #define sig_i (_comp->_parameters.sig_i)
  #define V0 (_comp->_parameters.V0)
  #define target_index (_comp->_parameters.target_index)
  #define multiples (_comp->_parameters.multiples)
  #define VarsV (_comp->_parameters.VarsV)
  SIG_MESSAGE("[_Sample_init] component Sample=V_sample() INITIALISE [/usr/share/mcstas/3.0-dev/obsolete/V_sample.comp:121]");
  /* Backward compatibility */
  if (radius) radius_o = radius;
  if (thickness) radius_i = radius_o - thickness;
  if (zdepth) zthick = zdepth;
  if (yheight) h = yheight;
  if (Vc) V0 = Vc;
  if (sigma_abs) sig_a = sigma_abs;
  if (sigma_inc) sig_i = sigma_inc;

  VarsV.shapetyp = -1;
  if (xwidth && yheight && zdepth)  VarsV.shapetyp=1; /* box */
  else if (radius > 0 && yheight)        VarsV.shapetyp=0; /* cylinder */
  else if (radius && !yheight)           VarsV.shapetyp=2; /* sphere */
  
  if (VarsV.shapetyp < 0)
    exit(fprintf(stderr,"V_sample: %s: sample has invalid dimensions. Please check parameter values.\n", NAME_CURRENT_COMP));

  VarsV.sigma_a=sig_a;
  VarsV.sigma_i=sig_i;
  VarsV.rho = (pack/V0);
  VarsV.my_s=(VarsV.rho * 100 * VarsV.sigma_i);
  VarsV.my_a_v=(VarsV.rho * 100 * VarsV.sigma_a);

  /* now compute target coords if a component index is supplied */
  VarsV.tx= VarsV.ty=VarsV.tz=0;
  if (target_index)
  {
    Coords ToTarget;
    ToTarget = coords_sub(POS_A_COMP_INDEX(INDEX_CURRENT_COMP+target_index),POS_A_CURRENT_COMP);
    ToTarget = rot_apply(ROT_A_CURRENT_COMP, ToTarget);
    coords_get(ToTarget, &VarsV.tx, &VarsV.ty, &VarsV.tz);
  }
  else
  { VarsV.tx = target_x; VarsV.ty = target_y; VarsV.tz = target_z; }

  if (!(VarsV.tx || VarsV.ty || VarsV.tz))
    printf("V_sample: %s: The target is not defined. Using direct beam (Z-axis).\n",
      NAME_CURRENT_COMP);

  VarsV.distance=sqrt(VarsV.tx*VarsV.tx+VarsV.ty*VarsV.ty+VarsV.tz*VarsV.tz);

  /* different ways of setting rectangular area */
  VarsV.aw  = VarsV.ah = 0;
  if (focus_xw) {
  VarsV.xw = focus_xw;
  }
  if (focus_yh) {
    VarsV.yh = focus_yh;
  }
  if (focus_aw) {
    VarsV.aw = DEG2RAD*focus_aw;
  }
  if (focus_ah) {
    VarsV.ah = DEG2RAD*focus_ah;
  }
  #undef radius
  #undef thickness
  #undef zdepth
  #undef Vc
  #undef sigma_abs
  #undef sigma_inc
  #undef radius_i
  #undef radius_o
  #undef h
  #undef focus_r
  #undef pack
  #undef frac
  #undef f_QE
  #undef gamma
  #undef target_x
  #undef target_y
  #undef target_z
  #undef focus_xw
  #undef focus_yh
  #undef focus_aw
  #undef focus_ah
  #undef xwidth
  #undef yheight
  #undef zthick
  #undef rad_sphere
  #undef sig_a
  #undef sig_i
  #undef V0
  #undef target_index
  #undef multiples
  #undef VarsV
  return(_comp);
} /* class_V_sample_init */



int init(void) { /* called by mccode_main for BNL_H8:INITIALISE */
  DEBUG_INSTR();

  /* code_main/parseoptions/readparams sets instrument parameters value */
  stracpy(instrument->_name, "BNL_H8", 256);

  /* Instrument 'BNL_H8' INITIALISE */
  SIG_MESSAGE("[BNL_H8] INITIALISE [BNL_H8_simple.instr:52]");
  #define lambda (instrument->_parameters._lambda)
{
  int    ORDER = 1;
  double vi, Ki;
  int    SM,SS,SA;
  char hostname[256];

  /* SM : scattering at mono to the right (-1)/left(+1) */
  /* SS : scattering at sample to the right (-1)/left(+1) */
  /* SA : scattering at analyser to the right (-1)/left(+1) */
  SM = 1; SS = -1; SA = 1;

/*  SM = 0; SS = -0; SA = 0; */

  mono_q = 2*PI*ORDER/DM;  /* Q mono in Angs-1 */

  Ki = 2*PI/lambda;
  vi = K2V*fabs(Ki);
  Ei = VS2E*vi*vi;

  A2 = asin(mono_q/2/Ki)*RAD2DEG*2;
  A4 = A2; A6 = A2;

  A2 *= SM;       /* A1 : mono theta (crystal) */
  A1 = A2/2;    /* A2 : mono 2 theta (arm to sample) */
  A4 *= SS;       /* A3 : sample theta */
  A3 = A4/2;    /* A4 : sample 2 theta (arm to analyser) */
  A6 *= SA;       /* A5 : analyser theta (crystal) */
  A5 = A6/2;    /* A6 : analyser 2 theta (arm to Dector) */

  strcpy(hostname, getenv("HOSTNAME") ? getenv("HOSTNAME") : "localhost");

  printf("Instrument:     BNL_H8 on %s.\n", hostname);
  printf("Monochromator : DM = %g\n",DM);
  printf("A1 = %.2f, A2 = %.2f (deg)\n",A1,A2);
  printf("Ki = %.4g Angs-1 Energy = %.4g meV\nVelocity = %.4g m/s, lambda = %.4g Angs\n", Ki, Ei, vi,
lambda);
}
  #undef lambda
  _Origin_setpos(); /* type Arm */
  _Source_setpos(); /* type Source_simple */
  _D0_Source_setpos(); /* type PSD_monitor */
  _SC1_setpos(); /* type Guide_simple */
  _D1_SC1_Out_setpos(); /* type PSD_monitor */
  _As1_setpos(); /* type Slit */
  _As2_setpos(); /* type Slit */
  _As3_setpos(); /* type Slit */
  _As4_setpos(); /* type Slit */
  _D2_A4_setpos(); /* type PSD_monitor */
  _Mono_Cradle_setpos(); /* type Arm */
  _PG1Xtal_setpos(); /* type Monochromator_flat */
  _Mono_Out_setpos(); /* type Arm */
  _D4_SC2_In_setpos(); /* type PSD_monitor */
  _SC2_setpos(); /* type Guide_simple */
  _D5_SC2_Out_setpos(); /* type PSD_monitor */
  _Sample_Cradle_setpos(); /* type Arm */
  _Sample_Out_setpos(); /* type Arm */
  _Sample_setpos(); /* type V_sample */
  _D7_SC3_In_setpos(); /* type PSD_monitor */
  _SC3_setpos(); /* type Guide_simple */
  _D8_SC3_Out_setpos(); /* type PSD_monitor */
  _Ana_Cradle_setpos(); /* type Arm */
  _PG2Xtal_setpos(); /* type Monochromator_flat */
  _Ana_Out_setpos(); /* type Arm */
  _D10_SC4_In_setpos(); /* type PSD_monitor */
  _SC4_setpos(); /* type Guide_simple */
  _He3H_setpos(); /* type PSD_monitor */

  /* call iteratively all components INITIALISE */

  class_Source_simple_init(&_Source_var);

  class_PSD_monitor_init(&_D0_Source_var);

  class_Guide_simple_init(&_SC1_var);

  class_PSD_monitor_init(&_D1_SC1_Out_var);

  class_Slit_init(&_As1_var);

  class_Slit_init(&_As2_var);

  class_Slit_init(&_As3_var);

  class_Slit_init(&_As4_var);

  class_PSD_monitor_init(&_D2_A4_var);


  class_Monochromator_flat_init(&_PG1Xtal_var);


  class_PSD_monitor_init(&_D4_SC2_In_var);

  class_Guide_simple_init(&_SC2_var);

  class_PSD_monitor_init(&_D5_SC2_Out_var);



  class_V_sample_init(&_Sample_var);

  class_PSD_monitor_init(&_D7_SC3_In_var);

  class_Guide_simple_init(&_SC3_var);

  class_PSD_monitor_init(&_D8_SC3_Out_var);


  class_Monochromator_flat_init(&_PG2Xtal_var);


  class_PSD_monitor_init(&_D10_SC4_In_var);

  class_Guide_simple_init(&_SC4_var);

  class_PSD_monitor_init(&_He3H_var);

  if (mcdotrace) display();
  DEBUG_INSTR_END();

  return(0);
} /* init */

/*******************************************************************************
* components TRACE
*******************************************************************************/

#define x (_particle->x)
#define y (_particle->y)
#define z (_particle->z)
#define vx (_particle->vx)
#define vy (_particle->vy)
#define vz (_particle->vz)
#define t (_particle->t)
#define sx (_particle->sx)
#define sy (_particle->sy)
#define sz (_particle->sz)
#define p (_particle->p)
// user variables:

#define SCATTERED (_particle->_scattered)
#define RESTORE (_particle->_restore)
#define RESTORE_NEUTRON(_index, ...) _particle->_restore = _index;
#define ABSORBED (_particle->_absorbed)
#define ABSORB0 do { DEBUG_STATE(); DEBUG_ABSORB(); MAGNET_OFF; ABSORBED++; return(_comp); } while(0)
#define ABSORB ABSORB0
#pragma acc routine seq nohost
_class_Source_simple *class_Source_simple_trace(_class_Source_simple *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;

  #define radius (_comp->_parameters.radius)
  #define yheight (_comp->_parameters.yheight)
  #define xwidth (_comp->_parameters.xwidth)
  #define dist (_comp->_parameters.dist)
  #define focus_xw (_comp->_parameters.focus_xw)
  #define focus_yh (_comp->_parameters.focus_yh)
  #define E0 (_comp->_parameters.E0)
  #define dE (_comp->_parameters.dE)
  #define lambda0 (_comp->_parameters.lambda0)
  #define dlambda (_comp->_parameters.dlambda)
  #define flux (_comp->_parameters.flux)
  #define gauss (_comp->_parameters.gauss)
  #define target_index (_comp->_parameters.target_index)
  #define pmul (_comp->_parameters.pmul)
  #define square (_comp->_parameters.square)
  #define srcArea (_comp->_parameters.srcArea)
  SIG_MESSAGE("[_Source_trace] component Source=Source_simple() TRACE [/usr/share/mcstas/3.0-dev/sources/Source_simple.comp:120]");
 double chi,E,lambda,v,r, xf, yf, rf, dx, dy, pdir;

 t=0;
 z=0;

 if (square == 1) {
   x = xwidth * (rand01() - 0.5);
   y = yheight * (rand01() - 0.5);
 } else {
   chi=2*PI*rand01();                          /* Choose point on source */
   r=sqrt(rand01())*radius;                    /* with uniform distribution. */
   x=r*cos(chi);
   y=r*sin(chi);
 }
 randvec_target_rect_real(&xf, &yf, &rf, &pdir,
			  0, 0, dist, focus_xw, focus_yh, ROT_A_CURRENT_COMP, x, y, z, 2);

 dx = xf-x;
 dy = yf-y;
 rf = sqrt(dx*dx+dy*dy+dist*dist);

 p = pdir*pmul;

 if(lambda0==0) {
   if (!gauss) {
     E=E0+dE*randpm1();              /*  Choose from uniform distribution */
   } else {
     E=E0+randnorm()*dE;
   }
   v=sqrt(E)*SE2V;
 } else {
   if (!gauss) {
     lambda=lambda0+dlambda*randpm1();
   } else {
     lambda=lambda0+randnorm()*dlambda;
   }
   v = K2V*(2*PI/lambda);
 }

 vz=v*dist/rf;
 vy=v*dy/rf;
 vx=v*dx/rf;
  #undef radius
  #undef yheight
  #undef xwidth
  #undef dist
  #undef focus_xw
  #undef focus_yh
  #undef E0
  #undef dE
  #undef lambda0
  #undef dlambda
  #undef flux
  #undef gauss
  #undef target_index
  #undef pmul
  #undef square
  #undef srcArea
  return(_comp);
} /* class_Source_simple_trace */

#pragma acc routine seq nohost
_class_PSD_monitor *class_PSD_monitor_trace(_class_PSD_monitor *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;

  #define nx (_comp->_parameters.nx)
  #define ny (_comp->_parameters.ny)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define PSD_N (_comp->_parameters.PSD_N)
  #define PSD_p (_comp->_parameters.PSD_p)
  #define PSD_p2 (_comp->_parameters.PSD_p2)
  SIG_MESSAGE("[_D0_Source_trace] component D0_Source=PSD_monitor() TRACE [/usr/share/mcstas/3.0-dev/monitors/PSD_monitor.comp:90]");
  PROP_Z0;
  if (x>xmin && x<xmax && y>ymin && y<ymax){
    int i = floor((x - xmin)*nx/(xmax - xmin));
    int j = floor((y - ymin)*ny/(ymax - ymin));
    PSD_N[i][j]++;
    PSD_p[i][j] += p;
    PSD_p2[i][j] += p*p;
    SCATTER;
  }
  if (restore_neutron) {
    RESTORE_NEUTRON(INDEX_CURRENT_COMP, x, y, z, vx, vy, vz, t, sx, sy, sz, p);
  }
  #undef nx
  #undef ny
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  return(_comp);
} /* class_PSD_monitor_trace */

#pragma acc routine seq nohost
_class_Guide_simple *class_Guide_simple_trace(_class_Guide_simple *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;

  #define w1 (_comp->_parameters.w1)
  #define h1 (_comp->_parameters.h1)
  #define w2 (_comp->_parameters.w2)
  #define h2 (_comp->_parameters.h2)
  #define l (_comp->_parameters.l)
  #define R0 (_comp->_parameters.R0)
  #define Qc (_comp->_parameters.Qc)
  #define alpha (_comp->_parameters.alpha)
  #define m (_comp->_parameters.m)
  #define W (_comp->_parameters.W)
  SIG_MESSAGE("[_SC1_trace] component SC1=Guide_simple() TRACE [/usr/share/mcstas/3.0-dev/optics/Guide_simple.comp:92]");
  double t1,t2;                                 /* Intersection times. */
  double av,ah,bv,bh,cv1,cv2,ch1,ch2,d;         /* Intermediate values */
  double weight;                                /* Internal probability weight */
  double vdotn_v1,vdotn_v2,vdotn_h1,vdotn_h2;   /* Dot products. */
  int i;                                        /* Which mirror hit? */
  double q;                                     /* Q [1/AA] of reflection */
  double nlen2;                                 /* Vector lengths squared */

  /* ToDo: These could be precalculated. */
  double ww = .5*(w2 - w1), hh = .5*(h2 - h1);
  double whalf = .5*w1, hhalf = .5*h1;

  /* Propagate neutron to guide entrance. */
  PROP_Z0;
  /* Scatter here to ensure that fully transmitted neutrons will not be
     absorbed in a GROUP construction, e.g. all neutrons - even the
     later absorbed ones are scattered at the guide entry. */
  SCATTER;
  if(x <= -whalf || x >= whalf || y <= -hhalf || y >= hhalf)
    ABSORB;
  for(;;)
  {
    /* Compute the dot products of v and n for the four mirrors. */
    av = l*vx; bv = ww*vz;
    ah = l*vy; bh = hh*vz;
    vdotn_v1 = bv + av;         /* Left vertical */
    vdotn_v2 = bv - av;         /* Right vertical */
    vdotn_h1 = bh + ah;         /* Lower horizontal */
    vdotn_h2 = bh - ah;         /* Upper horizontal */
    /* Compute the dot products of (O - r) and n as c1+c2 and c1-c2 */
    cv1 = -whalf*l - z*ww; cv2 = x*l;
    ch1 = -hhalf*l - z*hh; ch2 = y*l;
    /* Compute intersection times. */
    t1 = (l - z)/vz;
    i = 0;
    if(vdotn_v1 < 0 && (t2 = (cv1 - cv2)/vdotn_v1) < t1)
    {
      t1 = t2;
      i = 1;
    }
    if(vdotn_v2 < 0 && (t2 = (cv1 + cv2)/vdotn_v2) < t1)
    {
      t1 = t2;
      i = 2;
    }
    if(vdotn_h1 < 0 && (t2 = (ch1 - ch2)/vdotn_h1) < t1)
    {
      t1 = t2;
      i = 3;
    }
    if(vdotn_h2 < 0 && (t2 = (ch1 + ch2)/vdotn_h2) < t1)
    {
      t1 = t2;
      i = 4;
    }
    if(i == 0)
      break;                    /* Neutron left guide. */
    PROP_DT(t1);
    switch(i)
    {
      case 1:                   /* Left vertical mirror */
        nlen2 = l*l + ww*ww;
        q = V2Q*(-2)*vdotn_v1/sqrt(nlen2);
        d = 2*vdotn_v1/nlen2;
        vx = vx - d*l;
        vz = vz - d*ww;
        break;
      case 2:                   /* Right vertical mirror */
        nlen2 = l*l + ww*ww;
        q = V2Q*(-2)*vdotn_v2/sqrt(nlen2);
        d = 2*vdotn_v2/nlen2;
        vx = vx + d*l;
        vz = vz - d*ww;
        break;
      case 3:                   /* Lower horizontal mirror */
        nlen2 = l*l + hh*hh;
        q = V2Q*(-2)*vdotn_h1/sqrt(nlen2);
        d = 2*vdotn_h1/nlen2;
        vy = vy - d*l;
        vz = vz - d*hh;
        break;
      case 4:                   /* Upper horizontal mirror */
        nlen2 = l*l + hh*hh;
        q = V2Q*(-2)*vdotn_h2/sqrt(nlen2);
        d = 2*vdotn_h2/nlen2;
        vy = vy + d*l;
        vz = vz - d*hh;
        break;
    }
    /* Now compute reflectivity. */
    weight = 1.0; /* Initial internal weight factor */
    if(m == 0)
      ABSORB;
    //    if (reflect && strlen(reflect) && strcmp(reflect,"NULL") && strcmp(reflect,"0"))
    //   TableReflecFunc(q, &pTable, &weight);
    //else {
      double par[] = {R0, Qc, alpha, m, W};
      StdReflecFunc(q, par, &weight);
      //}
    if (weight > 0)
      p *= weight;
    else ABSORB;
    SCATTER;
  }
  #undef w1
  #undef h1
  #undef w2
  #undef h2
  #undef l
  #undef R0
  #undef Qc
  #undef alpha
  #undef m
  #undef W
  return(_comp);
} /* class_Guide_simple_trace */

#pragma acc routine seq nohost
_class_Slit *class_Slit_trace(_class_Slit *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;

  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define radius (_comp->_parameters.radius)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  SIG_MESSAGE("[_As1_trace] component As1=Slit() TRACE [/usr/share/mcstas/3.0-dev/optics/Slit.comp:56]");
    mcPROP_Z0;
    if (((radius == 0) && (x<xmin || x>xmax || y<ymin || y>ymax))
    || ((radius != 0) && (x*x + y*y > radius*radius)))
      ABSORB;
    else
        SCATTER;
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef radius
  #undef xwidth
  #undef yheight
  return(_comp);
} /* class_Slit_trace */

#pragma acc routine seq nohost
_class_Monochromator_flat *class_Monochromator_flat_trace(_class_Monochromator_flat *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;

  #define zmin (_comp->_parameters.zmin)
  #define zmax (_comp->_parameters.zmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define zwidth (_comp->_parameters.zwidth)
  #define yheight (_comp->_parameters.yheight)
  #define mosaich (_comp->_parameters.mosaich)
  #define mosaicv (_comp->_parameters.mosaicv)
  #define r0 (_comp->_parameters.r0)
  #define Q (_comp->_parameters.Q)
  #define DM (_comp->_parameters.DM)
  #define mos_rms_y (_comp->_parameters.mos_rms_y)
  #define mos_rms_z (_comp->_parameters.mos_rms_z)
  #define mos_rms_max (_comp->_parameters.mos_rms_max)
  #define mono_Q (_comp->_parameters.mono_Q)
  SIG_MESSAGE("[_PG1Xtal_trace] component PG1Xtal=Monochromator_flat() TRACE [/usr/share/mcstas/3.0-dev/optics/Monochromator_flat.comp:119]");
  double y1,z1,t1,dt,kix,kiy,kiz,ratio,order,q0x,k,q0,theta;
  double bx,by,bz,kux,kuy,kuz,ax,ay,az,phi;
  double cos_2theta,k_sin_2theta,cos_phi,sin_phi,q_x,q_y,q_z;
  double delta,p_reflect,total,c1x,c1y,c1z,width,mos_sample;
  int i;

  if(vx != 0.0 && (dt = -x/vx) >= 0.0)
  {                             /* Moving towards crystal? */
    y1 = y + vy*dt;             /* Propagate to crystal plane */
    z1 = z + vz*dt;
    t1 = t + dt;
    if (z1>zmin && z1<zmax && y1>ymin && y1<ymax)
    {                           /* Intersect the crystal? */
      kix = V2K*vx;             /* Initial wave vector */
      kiy = V2K*vy;
      kiz = V2K*vz;
      /* Get reflection order and corresponding nominal scattering vector q0
         of correct length and direction. Only the order with the closest
         scattering vector is considered */
      ratio = -2*kix/mono_Q;
      order = floor(ratio + .5);
      if(order == 0.0)
        order = ratio < 0 ? -1 : 1;
      /* Order will be negative when the neutron enters from the back, in
         which case the direction of Q0 is flipped. */
      if(order < 0)
        order = -order;
      /* Make sure the order is small enough to allow Bragg scattering at the
         given neutron wavelength */
      k = sqrt(kix*kix + kiy*kiy + kiz*kiz);
      kux = kix/k;              /* Unit vector along ki */
      kuy = kiy/k;
      kuz = kiz/k;
      if(order > 2*k/mono_Q)
        order--;
      if(order > 0)             /* Bragg scattering possible? */
      {
        q0 = order*mono_Q;
        q0x = ratio < 0 ? -q0 : q0;
        theta = asin(q0/(2*k)); /* Actual bragg angle */
        /* Make MC choice: reflect or transmit? */
        delta = asin(fabs(kux)) - theta;
        p_reflect = r0*exp(-kiy*kiy/(kiy*kiy + kiz*kiz)*(delta*delta)/
                           (2*mos_rms_y*mos_rms_y))*
                       exp(-kiz*kiz/(kiy*kiy + kiz*kiz)*(delta*delta)/
                           (2*mos_rms_z*mos_rms_z));
        if(rand01() < p_reflect)
        {                       /* Reflect */
          cos_2theta = cos(2*theta);
          k_sin_2theta = k*sin(2*theta);
          /* Get unit normal to plane containing ki and most probable kf */
          vec_prod(bx, by, bz, kix, kiy, kiz, q0x, 0, 0);
          NORM(bx,by,bz);
          bx *= k_sin_2theta;
          by *= k_sin_2theta;
          bz *= k_sin_2theta;
          /* Get unit vector normal to ki and b */
          vec_prod(ax, ay, az, bx, by, bz, kux, kuy, kuz);
          /* Compute the total scattering probability at this ki */
          total = 0;
          /* Choose width of Gaussian distribution to sample the angle
           * phi on the Debye-Scherrer cone for the scattered neutron.
           * The radius of the Debye-Scherrer cone is smaller by a
           * factor 1/cos(theta) than the radius of the (partial) sphere
           * describing the possible orientations of Q due to mosaicity, so we
           * start with a width 1/cos(theta) greater than the largest of
           * the two mosaics. */
          mos_sample = mos_rms_max/cos(theta);
          c1x = kix*(cos_2theta-1);
          c1y = kiy*(cos_2theta-1);
          c1z = kiz*(cos_2theta-1);
          /* Loop, repeatedly reducing the sample width until it is small
           * enough to avoid sampling scattering directions with
           * ridiculously low scattering probability.
           * Use a cut-off at 5 times the gauss width for considering
           * scattering probability as well as for integration limits
           * when integrating the sampled distribution below. */
          for(i=0; i<100; i++) {
            width = 5*mos_sample;
            cos_phi = cos(width);
            sin_phi = sin(width);
            q_x = c1x + cos_phi*ax + sin_phi*bx;
            q_y = (c1y + cos_phi*ay + sin_phi*by)/mos_rms_y;
            q_z = (c1z + cos_phi*az + sin_phi*bz)/mos_rms_z;
            /* Stop when we get near a factor of 25=5^2. */
            if(q_z*q_z + q_y*q_y < (25/(2.0/3.0))*(q_x*q_x))
              break;
            mos_sample *= (2.0/3.0);
          }
          /* Now integrate the chosen sampling distribution, using a
           * cut-off at five times sigma. */
          for(i = 0; i < (sizeof(Gauss_X)/sizeof(double)); i++)
          {
            phi = width*Gauss_X[i];
            cos_phi = cos(phi);
            sin_phi = sin(phi);
            q_x = c1x + cos_phi*ax + sin_phi*bx;
            q_y = c1y + cos_phi*ay + sin_phi*by;
            q_z = c1z + cos_phi*az + sin_phi*bz;
            p_reflect = GAUSS((q_y/q_x),0,mos_rms_y)*
                        GAUSS((q_z/q_x),0,mos_rms_z);
            total += Gauss_W[i]*p_reflect;
          }
          total *= width;
          /* Choose point on Debye-Scherrer cone. Sample from a Gaussian of
           * width 1/cos(theta) greater than the mosaic and correct for any
           * error by adjusting the neutron weight later. */
          phi = mos_sample*randnorm();
          /* Compute final wave vector kf and scattering vector q = ki - kf */
          cos_phi = cos(phi);
          sin_phi = sin(phi);
          q_x = c1x + cos_phi*ax + sin_phi*bx;
          q_y = c1y + cos_phi*ay + sin_phi*by;
          q_z = c1z + cos_phi*az + sin_phi*bz;
          p_reflect = GAUSS((q_y/q_x),0,mos_rms_y)*
                      GAUSS((q_z/q_x),0,mos_rms_z);
          x = 0;
          y = y1;
          z = z1;
          t = t1;
          vx = K2V*(kix+q_x);
          vy = K2V*(kiy+q_y);
          vz = K2V*(kiz+q_z);
          p_reflect /= total*GAUSS(phi,0,mos_sample);
          if (p_reflect <= 0) ABSORB;
          if (p_reflect > 1)  p_reflect = 1;
          p *= p_reflect;
          SCATTER;
        } /* End MC choice to reflect or transmit neutron */
      } /* End bragg scattering possible */
    } /* End intersect the crystal */
  } /* End neutron moving towards crystal */
  #undef zmin
  #undef zmax
  #undef ymin
  #undef ymax
  #undef zwidth
  #undef yheight
  #undef mosaich
  #undef mosaicv
  #undef r0
  #undef Q
  #undef DM
  #undef mos_rms_y
  #undef mos_rms_z
  #undef mos_rms_max
  #undef mono_Q
  return(_comp);
} /* class_Monochromator_flat_trace */

#pragma acc routine seq nohost
_class_V_sample *class_V_sample_trace(_class_V_sample *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;

  #define radius (_comp->_parameters.radius)
  #define thickness (_comp->_parameters.thickness)
  #define zdepth (_comp->_parameters.zdepth)
  #define Vc (_comp->_parameters.Vc)
  #define sigma_abs (_comp->_parameters.sigma_abs)
  #define sigma_inc (_comp->_parameters.sigma_inc)
  #define radius_i (_comp->_parameters.radius_i)
  #define radius_o (_comp->_parameters.radius_o)
  #define h (_comp->_parameters.h)
  #define focus_r (_comp->_parameters.focus_r)
  #define pack (_comp->_parameters.pack)
  #define frac (_comp->_parameters.frac)
  #define f_QE (_comp->_parameters.f_QE)
  #define gamma (_comp->_parameters.gamma)
  #define target_x (_comp->_parameters.target_x)
  #define target_y (_comp->_parameters.target_y)
  #define target_z (_comp->_parameters.target_z)
  #define focus_xw (_comp->_parameters.focus_xw)
  #define focus_yh (_comp->_parameters.focus_yh)
  #define focus_aw (_comp->_parameters.focus_aw)
  #define focus_ah (_comp->_parameters.focus_ah)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define zthick (_comp->_parameters.zthick)
  #define rad_sphere (_comp->_parameters.rad_sphere)
  #define sig_a (_comp->_parameters.sig_a)
  #define sig_i (_comp->_parameters.sig_i)
  #define V0 (_comp->_parameters.V0)
  #define target_index (_comp->_parameters.target_index)
  #define multiples (_comp->_parameters.multiples)
  #define VarsV (_comp->_parameters.VarsV)
  SIG_MESSAGE("[_Sample_trace] component Sample=V_sample() TRACE [/usr/share/mcstas/3.0-dev/obsolete/V_sample.comp:180]");
  double t0, t3;                /* Entry/exit time for outer cylinder */
  double t1, t2;                /* Entry/exit time for inner cylinder */
  double v;                     /* Neutron velocity */
  double dt0, dt1, dt2, dt;     /* Flight times through sample */
  double l_full;                /* Flight path length for non-scattered neutron */
  double l_i, l_o=0;            /* Flight path lenght in/out for scattered neutron */
  double my_a=0;                  /* Velocity-dependent attenuation factor */
  double solid_angle=0;         /* Solid angle of target as seen from scattering point */
  double aim_x=0, aim_y=0, aim_z=1;   /* Position of target relative to scattering point */
  double v_i, v_f, E_i, E_f; /* initial and final energies and velocities */
  double dE;                 /* Energy transfer */
  int    intersect=0;

  if (VarsV.shapetyp == 2)
    intersect = sphere_intersect(&t0, &t3, x, y, z, vx, vy, vz, rad_sphere);
  else
    if (VarsV.shapetyp == 1)
      intersect = box_intersect(&t0, &t3, x, y, z, vx, vy, vz, xwidth, yheight, zthick);
  else
    intersect = cylinder_intersect(&t0, &t3, x, y, z, vx, vy, vz, radius_o, h);
  if(intersect)
  {
    if(t0 < 0) ABSORB; /* we already passed the sample; this is illegal */
    /* Neutron enters at t=t0. */
    if(VarsV.shapetyp == 1 || VarsV.shapetyp == 2)
      t1 = t2 = t3;
    else
      if(!radius_i || !cylinder_intersect(&t1, &t2, x, y, z, vx, vy, vz, radius_i, h))
        t1 = t2 = t3;

    dt0 = t1-t0;                /* Time in sample, ingoing */
    dt1 = t2-t1;                /* Time in hole */
    dt2 = t3-t2;                /* Time in sample, outgoing */
    v = sqrt(vx*vx + vy*vy + vz*vz);
    l_full = v * (dt0 + dt2);   /* Length of full path through sample */
    if (v) my_a = VarsV.my_a_v*(2200/v);

    if (frac >= 1 || rand01()<frac)          /* Scattering */
    {
      dt = rand01()*(dt0+dt2);    /* Time of scattering (relative to t0) */
      l_i = v*dt;                 /* Penetration in sample: scattering+abs */
      if (dt > dt0)
        dt += dt1;                /* jump to 2nd side of cylinder */

      PROP_DT(dt+t0);             /* Point of scattering */

      if ((VarsV.tx || VarsV.ty || VarsV.tz)) {
        aim_x = VarsV.tx-x;       /* Vector pointing at target (anal./det.) */
        aim_y = VarsV.ty-y;
        aim_z = VarsV.tz-z;
      }
      if(VarsV.aw && VarsV.ah) {
        randvec_target_rect_angular(&vx, &vy, &vz, &solid_angle,
          aim_x, aim_y, aim_z, VarsV.aw, VarsV.ah, ROT_A_CURRENT_COMP);
      } else if(VarsV.xw && VarsV.yh) {
        randvec_target_rect(&vx, &vy, &vz, &solid_angle,
          aim_x, aim_y, aim_z, VarsV.xw, VarsV.yh, ROT_A_CURRENT_COMP);
      } else {
        randvec_target_circle(&vx, &vy, &vz, &solid_angle, aim_x, aim_y, aim_z, focus_r);
      }
      NORM(vx, vy, vz);

      v_i = v;          /* Store initial velocity in case of quasielastic */
      if (rand01()<f_QE)	/* Quasielastic contribution */
	{
          E_i = VS2E*v_i*v_i;
          dE = gamma*tan(PI/2*randpm1());
          E_f = E_i + dE;
          if (E_f <= 0)
            ABSORB;
	  v_f = SE2V*sqrt(E_f);
          v = v_f;
	  /*          printf("vi: %g Ei: %g dE: %g Ef %g vf: %g v: %g \n",
		      v_i,E_i,dE,E_f,v_f,v); */
	}

      vx *= v;
      vy *= v;
      vz *= v;

      if(VarsV.shapetyp == 0) {
        if(!cylinder_intersect(&t0, &t3, x, y, z, vx, vy, vz, radius_o, h)) {
          /* ??? did not hit cylinder */
          printf("FATAL ERROR: Did not hit cylinder from inside.\n");
          //exit(1);
        }
        dt = t3; /* outgoing point */
        if(cylinder_intersect(&t1, &t2, x, y, z, vx, vy, vz, radius_i, h) &&
           t2 > 0)
          dt -= (t2-t1);            /* Subtract hollow part */
      }
      else {
        if(VarsV.shapetyp == 1) {
	      if(!box_intersect(&t0, &t3, x, y, z, vx, vy, vz, xwidth, yheight, zthick)) {
            /* ??? did not hit box */
            printf("FATAL ERROR: Did not hit box from inside.\n");
            //exit(1);
          }
          dt = t3;
        }
        else {
	      if(!sphere_intersect(&t0, &t3, x, y, z, vx, vy, vz, rad_sphere)) {
            /* ??? did not hit sphere */
            printf("FATAL ERROR: Did not hit sphere from inside.\n");
            //exit(1);
          }
          dt = t3;  
        }
      }
      l_o = v*dt; /* trajectory after scattering point: absorption only */

      p *= v/v_i*l_full*VarsV.my_s*exp(-my_a*(l_i+v_i/v*l_o)-VarsV.my_s*l_i);
      if (!multiples) {
	/* If no "multiples", correct by applying scattering cross-sec and
	   implicitly "absorb" further scattering (as in PowderN) 
	   We are currently (august 2007) having a debate on which solution 
	   is the most reasonable */
	p *= exp(-VarsV.my_s*l_o);
      }
      /* We do not consider scattering from 2nd part (outgoing) */
      p /= 4*PI/solid_angle;
      p /= frac;

      /* Polarisation part (1/3 NSF, 2/3 SF) */
      sx *= -1.0/3.0;
      sy *= -1.0/3.0;
      sz *= -1.0/3.0;

      SCATTER;
    }
    else /* Transmitting; always elastic */
    {
      p *= exp(-(my_a+VarsV.my_s)*l_full);
      p /= (1-frac);
    }
  }
  #undef radius
  #undef thickness
  #undef zdepth
  #undef Vc
  #undef sigma_abs
  #undef sigma_inc
  #undef radius_i
  #undef radius_o
  #undef h
  #undef focus_r
  #undef pack
  #undef frac
  #undef f_QE
  #undef gamma
  #undef target_x
  #undef target_y
  #undef target_z
  #undef focus_xw
  #undef focus_yh
  #undef focus_aw
  #undef focus_ah
  #undef xwidth
  #undef yheight
  #undef zthick
  #undef rad_sphere
  #undef sig_a
  #undef sig_i
  #undef V0
  #undef target_index
  #undef multiples
  #undef VarsV
  return(_comp);
} /* class_V_sample_trace */

/* *****************************************************************************
* instrument 'BNL_H8' TRACE
***************************************************************************** */

#pragma acc routine seq nohost
int raytrace(_class_particle* _particle) { /* called by mccode_main for BNL_H8:TRACE */

  /* init variables and counters for TRACE */
  #undef ABSORB0
  #undef ABSORB
  #define ABSORB0 do { DEBUG_ABSORB(); MAGNET_OFF; ABSORBED++; return(ABSORBED);} while(0)
  #define ABSORB ABSORB0
  DEBUG_ENTER();
  DEBUG_STATE();
  /* the main iteration loop for one incoming neutron */
  while (!ABSORBED) { /* iterate neutron event until absorbed */
    _class_particle _particle_save;
    /* send neutron event to component instance, one after the other */
    char flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 1) {
      /* begin component Origin=Arm() [1] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_Origin_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _Origin_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_Origin_var._position_relative, _Origin_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_Origin_var._name);
      DEBUG_STATE();
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component Origin [1] */
    if (!ABSORBED && _particle->_index == 2) {
      /* begin component Source=Source_simple() [2] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_Source_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _Source_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_Source_var._position_relative, _Source_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_Source_var._name);
      DEBUG_STATE();
      class_Source_simple_trace(&_Source_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component Source [2] */
    if (!ABSORBED && _particle->_index == 3) {
      /* begin component D0_Source=PSD_monitor() [3] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_D0_Source_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _D0_Source_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_D0_Source_var._position_relative, _D0_Source_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_D0_Source_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_D0_Source_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component D0_Source [3] */
    if (!ABSORBED && _particle->_index == 4) {
      /* begin component SC1=Guide_simple() [4] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_SC1_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _SC1_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_SC1_var._position_relative, _SC1_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_SC1_var._name);
      DEBUG_STATE();
      class_Guide_simple_trace(&_SC1_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component SC1 [4] */
    if (!ABSORBED && _particle->_index == 5) {
      /* begin component D1_SC1_Out=PSD_monitor() [5] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_D1_SC1_Out_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _D1_SC1_Out_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_D1_SC1_Out_var._position_relative, _D1_SC1_Out_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_D1_SC1_Out_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_D1_SC1_Out_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component D1_SC1_Out [5] */
    if (!ABSORBED && _particle->_index == 6) {
      /* begin component As1=Slit() [6] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_As1_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _As1_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_As1_var._position_relative, _As1_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_As1_var._name);
      DEBUG_STATE();
      class_Slit_trace(&_As1_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component As1 [6] */
    if (!ABSORBED && _particle->_index == 7) {
      /* begin component As2=Slit() [7] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_As2_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _As2_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_As2_var._position_relative, _As2_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_As2_var._name);
      DEBUG_STATE();
      class_Slit_trace(&_As2_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component As2 [7] */
    if (!ABSORBED && _particle->_index == 8) {
      /* begin component As3=Slit() [8] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_As3_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _As3_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_As3_var._position_relative, _As3_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_As3_var._name);
      DEBUG_STATE();
      class_Slit_trace(&_As3_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component As3 [8] */
    if (!ABSORBED && _particle->_index == 9) {
      /* begin component As4=Slit() [9] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_As4_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _As4_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_As4_var._position_relative, _As4_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_As4_var._name);
      DEBUG_STATE();
      class_Slit_trace(&_As4_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component As4 [9] */
    if (!ABSORBED && _particle->_index == 10) {
      /* begin component D2_A4=PSD_monitor() [10] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_D2_A4_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _D2_A4_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_D2_A4_var._position_relative, _D2_A4_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_D2_A4_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_D2_A4_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component D2_A4 [10] */
    if (!ABSORBED && _particle->_index == 11) {
      /* begin component Mono_Cradle=Arm() [11] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_Mono_Cradle_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _Mono_Cradle_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_Mono_Cradle_var._position_relative, _Mono_Cradle_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_Mono_Cradle_var._name);
      DEBUG_STATE();
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component Mono_Cradle [11] */
    if (!ABSORBED && _particle->_index == 12) {
      /* begin component PG1Xtal=Monochromator_flat() [12] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_PG1Xtal_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _PG1Xtal_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_PG1Xtal_var._position_relative, _PG1Xtal_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_PG1Xtal_var._name);
      DEBUG_STATE();
      class_Monochromator_flat_trace(&_PG1Xtal_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component PG1Xtal [12] */
    if (!ABSORBED && _particle->_index == 13) {
      /* begin component Mono_Out=Arm() [13] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_Mono_Out_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _Mono_Out_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_Mono_Out_var._position_relative, _Mono_Out_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_Mono_Out_var._name);
      DEBUG_STATE();
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component Mono_Out [13] */
    if (!ABSORBED && _particle->_index == 14) {
      /* begin component D4_SC2_In=PSD_monitor() [14] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_D4_SC2_In_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _D4_SC2_In_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_D4_SC2_In_var._position_relative, _D4_SC2_In_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_D4_SC2_In_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_D4_SC2_In_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component D4_SC2_In [14] */
    if (!ABSORBED && _particle->_index == 15) {
      /* begin component SC2=Guide_simple() [15] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_SC2_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _SC2_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_SC2_var._position_relative, _SC2_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_SC2_var._name);
      DEBUG_STATE();
      class_Guide_simple_trace(&_SC2_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component SC2 [15] */
    if (!ABSORBED && _particle->_index == 16) {
      /* begin component D5_SC2_Out=PSD_monitor() [16] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_D5_SC2_Out_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _D5_SC2_Out_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_D5_SC2_Out_var._position_relative, _D5_SC2_Out_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_D5_SC2_Out_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_D5_SC2_Out_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component D5_SC2_Out [16] */
    if (!ABSORBED && _particle->_index == 17) {
      /* begin component Sample_Cradle=Arm() [17] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_Sample_Cradle_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _Sample_Cradle_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_Sample_Cradle_var._position_relative, _Sample_Cradle_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_Sample_Cradle_var._name);
      DEBUG_STATE();
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component Sample_Cradle [17] */
    if (!ABSORBED && _particle->_index == 18) {
      /* begin component Sample_Out=Arm() [18] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_Sample_Out_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _Sample_Out_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_Sample_Out_var._position_relative, _Sample_Out_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_Sample_Out_var._name);
      DEBUG_STATE();
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component Sample_Out [18] */
    if (!ABSORBED && _particle->_index == 19) {
      /* begin component Sample=V_sample() [19] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_Sample_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _Sample_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_Sample_var._position_relative, _Sample_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_Sample_var._name);
      DEBUG_STATE();
      class_V_sample_trace(&_Sample_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component Sample [19] */
    if (!ABSORBED && _particle->_index == 20) {
      /* begin component D7_SC3_In=PSD_monitor() [20] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_D7_SC3_In_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _D7_SC3_In_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_D7_SC3_In_var._position_relative, _D7_SC3_In_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_D7_SC3_In_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_D7_SC3_In_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component D7_SC3_In [20] */
    if (!ABSORBED && _particle->_index == 21) {
      /* begin component SC3=Guide_simple() [21] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_SC3_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _SC3_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_SC3_var._position_relative, _SC3_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_SC3_var._name);
      DEBUG_STATE();
      class_Guide_simple_trace(&_SC3_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component SC3 [21] */
    if (!ABSORBED && _particle->_index == 22) {
      /* begin component D8_SC3_Out=PSD_monitor() [22] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_D8_SC3_Out_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _D8_SC3_Out_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_D8_SC3_Out_var._position_relative, _D8_SC3_Out_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_D8_SC3_Out_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_D8_SC3_Out_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component D8_SC3_Out [22] */
    if (!ABSORBED && _particle->_index == 23) {
      /* begin component Ana_Cradle=Arm() [23] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_Ana_Cradle_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _Ana_Cradle_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_Ana_Cradle_var._position_relative, _Ana_Cradle_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_Ana_Cradle_var._name);
      DEBUG_STATE();
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component Ana_Cradle [23] */
    if (!ABSORBED && _particle->_index == 24) {
      /* begin component PG2Xtal=Monochromator_flat() [24] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_PG2Xtal_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _PG2Xtal_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_PG2Xtal_var._position_relative, _PG2Xtal_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_PG2Xtal_var._name);
      DEBUG_STATE();
      class_Monochromator_flat_trace(&_PG2Xtal_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component PG2Xtal [24] */
    if (!ABSORBED && _particle->_index == 25) {
      /* begin component Ana_Out=Arm() [25] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_Ana_Out_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _Ana_Out_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_Ana_Out_var._position_relative, _Ana_Out_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_Ana_Out_var._name);
      DEBUG_STATE();
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component Ana_Out [25] */
    if (!ABSORBED && _particle->_index == 26) {
      /* begin component D10_SC4_In=PSD_monitor() [26] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_D10_SC4_In_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _D10_SC4_In_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_D10_SC4_In_var._position_relative, _D10_SC4_In_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_D10_SC4_In_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_D10_SC4_In_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component D10_SC4_In [26] */
    if (!ABSORBED && _particle->_index == 27) {
      /* begin component SC4=Guide_simple() [27] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_SC4_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _SC4_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_SC4_var._position_relative, _SC4_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_SC4_var._name);
      DEBUG_STATE();
      class_Guide_simple_trace(&_SC4_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component SC4 [27] */
    if (!ABSORBED && _particle->_index == 28) {
      /* begin component He3H=PSD_monitor() [28] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_He3H_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _He3H_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_He3H_var._position_relative, _He3H_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_He3H_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_He3H_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component He3H [28] */
    if (_particle->_index > 28)
      ABSORBED++; /* absorbed when passed all components */
  } /* while !ABSORBED */

  DEBUG_LEAVE()
  DEBUG_STATE()

  return(_particle->_index);
} /* raytrace */
#undef x
#undef y
#undef z
#undef vx
#undef vy
#undef vz
#undef t
#undef sx
#undef sy
#undef sz
#undef p
// user variables:

#undef SCATTERED
#undef RESTORE
#undef RESTORE_NEUTRON
#undef STORE_NEUTRON
#undef ABSORBED
#undef ABSORB
#undef ABSORB0
/* *****************************************************************************
* instrument 'BNL_H8' and components SAVE
***************************************************************************** */

_class_PSD_monitor *class_PSD_monitor_save(_class_PSD_monitor *_comp
) {
  #define nx (_comp->_parameters.nx)
  #define ny (_comp->_parameters.ny)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define PSD_N (_comp->_parameters.PSD_N)
  #define PSD_p (_comp->_parameters.PSD_p)
  #define PSD_p2 (_comp->_parameters.PSD_p2)
  SIG_MESSAGE("[_D0_Source_save] component D0_Source=PSD_monitor() SAVE [/usr/share/mcstas/3.0-dev/monitors/PSD_monitor.comp:106]");
  DETECTOR_OUT_2D(
    "PSD monitor",
    "X position [cm]",
    "Y position [cm]",
    xmin*100.0, xmax*100.0, ymin*100.0, ymax*100.0,
    nx, ny,
    &PSD_N[0][0],&PSD_p[0][0],&PSD_p2[0][0],
    filename);
  #undef nx
  #undef ny
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  return(_comp);
} /* class_PSD_monitor_save */



int save(FILE *handle) { /* called by mccode_main for BNL_H8:SAVE */
  if (!handle) siminfo_init(NULL);

  /* call iteratively all components SAVE */


  class_PSD_monitor_save(&_D0_Source_var);


  class_PSD_monitor_save(&_D1_SC1_Out_var);





  class_PSD_monitor_save(&_D2_A4_var);




  class_PSD_monitor_save(&_D4_SC2_In_var);


  class_PSD_monitor_save(&_D5_SC2_Out_var);




  class_PSD_monitor_save(&_D7_SC3_In_var);


  class_PSD_monitor_save(&_D8_SC3_Out_var);




  class_PSD_monitor_save(&_D10_SC4_In_var);


  class_PSD_monitor_save(&_He3H_var);

  if (!handle) siminfo_close(); 

  return(0);
} /* save */

/* *****************************************************************************
* instrument 'BNL_H8' and components FINALLY
***************************************************************************** */

_class_PSD_monitor *class_PSD_monitor_finally(_class_PSD_monitor *_comp
) {
  #define nx (_comp->_parameters.nx)
  #define ny (_comp->_parameters.ny)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define PSD_N (_comp->_parameters.PSD_N)
  #define PSD_p (_comp->_parameters.PSD_p)
  #define PSD_p2 (_comp->_parameters.PSD_p2)
  SIG_MESSAGE("[_D0_Source_finally] component D0_Source=PSD_monitor() FINALLY [/usr/share/mcstas/3.0-dev/monitors/PSD_monitor.comp:118]");
  destroy_darr2d(PSD_N);
  destroy_darr2d(PSD_p);
  destroy_darr2d(PSD_p2);
  #undef nx
  #undef ny
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  return(_comp);
} /* class_PSD_monitor_finally */



int finally(void) { /* called by mccode_main for BNL_H8:FINALLY */
  siminfo_init(NULL);
  save(siminfo_file); /* save data when simulation ends */

  /* call iteratively all components FINALLY */


  class_PSD_monitor_finally(&_D0_Source_var);


  class_PSD_monitor_finally(&_D1_SC1_Out_var);





  class_PSD_monitor_finally(&_D2_A4_var);




  class_PSD_monitor_finally(&_D4_SC2_In_var);


  class_PSD_monitor_finally(&_D5_SC2_Out_var);




  class_PSD_monitor_finally(&_D7_SC3_In_var);


  class_PSD_monitor_finally(&_D8_SC3_Out_var);




  class_PSD_monitor_finally(&_D10_SC4_In_var);


  class_PSD_monitor_finally(&_He3H_var);

  siminfo_close(); 

  return(0);
} /* finally */

/* *****************************************************************************
* instrument 'BNL_H8' and components DISPLAY
***************************************************************************** */

  #define magnify     mcdis_magnify
  #define line        mcdis_line
  #define dashed_line mcdis_dashed_line
  #define multiline   mcdis_multiline
  #define rectangle   mcdis_rectangle
  #define box         mcdis_box
  #define circle      mcdis_circle
  #define cylinder    mcdis_cylinder
  #define sphere      mcdis_sphere
_class_Arm *class_Arm_display(_class_Arm *_comp
) {
  SIG_MESSAGE("[_Origin_display] component Origin=Arm() DISPLAY [/usr/share/mcstas/3.0-dev/optics/Arm.comp:40]");
  printf("MCDISPLAY: component %s\n", _comp->_name);
  /* A bit ugly; hard-coded dimensions. */
  
  line(0,0,0,0.2,0,0);
  line(0,0,0,0,0.2,0);
  line(0,0,0,0,0,0.2);
  return(_comp);
} /* class_Arm_display */

_class_Source_simple *class_Source_simple_display(_class_Source_simple *_comp
) {
  #define radius (_comp->_parameters.radius)
  #define yheight (_comp->_parameters.yheight)
  #define xwidth (_comp->_parameters.xwidth)
  #define dist (_comp->_parameters.dist)
  #define focus_xw (_comp->_parameters.focus_xw)
  #define focus_yh (_comp->_parameters.focus_yh)
  #define E0 (_comp->_parameters.E0)
  #define dE (_comp->_parameters.dE)
  #define lambda0 (_comp->_parameters.lambda0)
  #define dlambda (_comp->_parameters.dlambda)
  #define flux (_comp->_parameters.flux)
  #define gauss (_comp->_parameters.gauss)
  #define target_index (_comp->_parameters.target_index)
  #define pmul (_comp->_parameters.pmul)
  #define square (_comp->_parameters.square)
  #define srcArea (_comp->_parameters.srcArea)
  SIG_MESSAGE("[_Source_display] component Source=Source_simple() DISPLAY [/usr/share/mcstas/3.0-dev/sources/Source_simple.comp:166]");
  printf("MCDISPLAY: component %s\n", _comp->_name);
  if (square == 1) {
    
    rectangle("xy",0,0,0,xwidth,yheight);
  } else {
    
    circle("xy",0,0,0,radius);
  }
  if (dist) {
    dashed_line(0,0,0, -focus_xw/2,-focus_yh/2,dist, 4);
    dashed_line(0,0,0,  focus_xw/2,-focus_yh/2,dist, 4);
    dashed_line(0,0,0,  focus_xw/2, focus_yh/2,dist, 4);
    dashed_line(0,0,0, -focus_xw/2, focus_yh/2,dist, 4);
  }
  #undef radius
  #undef yheight
  #undef xwidth
  #undef dist
  #undef focus_xw
  #undef focus_yh
  #undef E0
  #undef dE
  #undef lambda0
  #undef dlambda
  #undef flux
  #undef gauss
  #undef target_index
  #undef pmul
  #undef square
  #undef srcArea
  return(_comp);
} /* class_Source_simple_display */

_class_PSD_monitor *class_PSD_monitor_display(_class_PSD_monitor *_comp
) {
  #define nx (_comp->_parameters.nx)
  #define ny (_comp->_parameters.ny)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define PSD_N (_comp->_parameters.PSD_N)
  #define PSD_p (_comp->_parameters.PSD_p)
  #define PSD_p2 (_comp->_parameters.PSD_p2)
  SIG_MESSAGE("[_D0_Source_display] component D0_Source=PSD_monitor() DISPLAY [/usr/share/mcstas/3.0-dev/monitors/PSD_monitor.comp:125]");
  printf("MCDISPLAY: component %s\n", _comp->_name);
  multiline(5,
    (double)xmin, (double)ymin, 0.0,
    (double)xmax, (double)ymin, 0.0,
    (double)xmax, (double)ymax, 0.0,
    (double)xmin, (double)ymax, 0.0,
    (double)xmin, (double)ymin, 0.0);
  #undef nx
  #undef ny
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  return(_comp);
} /* class_PSD_monitor_display */

_class_Guide_simple *class_Guide_simple_display(_class_Guide_simple *_comp
) {
  #define w1 (_comp->_parameters.w1)
  #define h1 (_comp->_parameters.h1)
  #define w2 (_comp->_parameters.w2)
  #define h2 (_comp->_parameters.h2)
  #define l (_comp->_parameters.l)
  #define R0 (_comp->_parameters.R0)
  #define Qc (_comp->_parameters.Qc)
  #define alpha (_comp->_parameters.alpha)
  #define m (_comp->_parameters.m)
  #define W (_comp->_parameters.W)
  SIG_MESSAGE("[_SC1_display] component SC1=Guide_simple() DISPLAY [/usr/share/mcstas/3.0-dev/optics/Guide_simple.comp:200]");
  printf("MCDISPLAY: component %s\n", _comp->_name);
  
  multiline(5,
            -w1/2.0, -h1/2.0, 0.0,
             w1/2.0, -h1/2.0, 0.0,
             w1/2.0,  h1/2.0, 0.0,
            -w1/2.0,  h1/2.0, 0.0,
            -w1/2.0, -h1/2.0, 0.0);
  multiline(5,
            -w2/2.0, -h2/2.0, (double)l,
             w2/2.0, -h2/2.0, (double)l,
             w2/2.0,  h2/2.0, (double)l,
            -w2/2.0,  h2/2.0, (double)l,
            -w2/2.0, -h2/2.0, (double)l);
  line(-w1/2.0, -h1/2.0, 0, -w2/2.0, -h2/2.0, (double)l);
  line( w1/2.0, -h1/2.0, 0,  w2/2.0, -h2/2.0, (double)l);
  line( w1/2.0,  h1/2.0, 0,  w2/2.0,  h2/2.0, (double)l);
  line(-w1/2.0,  h1/2.0, 0, -w2/2.0,  h2/2.0, (double)l);
  #undef w1
  #undef h1
  #undef w2
  #undef h2
  #undef l
  #undef R0
  #undef Qc
  #undef alpha
  #undef m
  #undef W
  return(_comp);
} /* class_Guide_simple_display */

_class_Slit *class_Slit_display(_class_Slit *_comp
) {
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define radius (_comp->_parameters.radius)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  SIG_MESSAGE("[_As1_display] component As1=Slit() DISPLAY [/usr/share/mcstas/3.0-dev/optics/Slit.comp:66]");
  printf("MCDISPLAY: component %s\n", _comp->_name);
  
  if (radius == 0) {
    double xw, yh;
    xw = (xmax - xmin)/2.0;
    yh = (ymax - ymin)/2.0;
    multiline(3, xmin-xw, (double)ymax, 0.0,
              (double)xmin, (double)ymax, 0.0,
              (double)xmin, ymax+yh, 0.0);
    multiline(3, xmax+xw, (double)ymax, 0.0,
              (double)xmax, (double)ymax, 0.0,
              (double)xmax, ymax+yh, 0.0);
    multiline(3, xmin-xw, (double)ymin, 0.0,
              (double)xmin, (double)ymin, 0.0,
              (double)xmin, ymin-yh, 0.0);
    multiline(3, xmax+xw, (double)ymin, 0.0,
              (double)xmax, (double)ymin, 0.0,
              (double)xmax, ymin-yh, 0.0);
  } else {
    circle("xy",0,0,0,radius);
  }
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef radius
  #undef xwidth
  #undef yheight
  return(_comp);
} /* class_Slit_display */

_class_Monochromator_flat *class_Monochromator_flat_display(_class_Monochromator_flat *_comp
) {
  #define zmin (_comp->_parameters.zmin)
  #define zmax (_comp->_parameters.zmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define zwidth (_comp->_parameters.zwidth)
  #define yheight (_comp->_parameters.yheight)
  #define mosaich (_comp->_parameters.mosaich)
  #define mosaicv (_comp->_parameters.mosaicv)
  #define r0 (_comp->_parameters.r0)
  #define Q (_comp->_parameters.Q)
  #define DM (_comp->_parameters.DM)
  #define mos_rms_y (_comp->_parameters.mos_rms_y)
  #define mos_rms_z (_comp->_parameters.mos_rms_z)
  #define mos_rms_max (_comp->_parameters.mos_rms_max)
  #define mono_Q (_comp->_parameters.mono_Q)
  SIG_MESSAGE("[_PG1Xtal_display] component PG1Xtal=Monochromator_flat() DISPLAY [/usr/share/mcstas/3.0-dev/optics/Monochromator_flat.comp:255]");
  printf("MCDISPLAY: component %s\n", _comp->_name);
  
  multiline(5, 0.0, (double)ymin, (double)zmin,
               0.0, (double)ymax, (double)zmin,
               0.0, (double)ymax, (double)zmax,
               0.0, (double)ymin, (double)zmax,
               0.0, (double)ymin, (double)zmin);
  #undef zmin
  #undef zmax
  #undef ymin
  #undef ymax
  #undef zwidth
  #undef yheight
  #undef mosaich
  #undef mosaicv
  #undef r0
  #undef Q
  #undef DM
  #undef mos_rms_y
  #undef mos_rms_z
  #undef mos_rms_max
  #undef mono_Q
  return(_comp);
} /* class_Monochromator_flat_display */

_class_V_sample *class_V_sample_display(_class_V_sample *_comp
) {
  #define radius (_comp->_parameters.radius)
  #define thickness (_comp->_parameters.thickness)
  #define zdepth (_comp->_parameters.zdepth)
  #define Vc (_comp->_parameters.Vc)
  #define sigma_abs (_comp->_parameters.sigma_abs)
  #define sigma_inc (_comp->_parameters.sigma_inc)
  #define radius_i (_comp->_parameters.radius_i)
  #define radius_o (_comp->_parameters.radius_o)
  #define h (_comp->_parameters.h)
  #define focus_r (_comp->_parameters.focus_r)
  #define pack (_comp->_parameters.pack)
  #define frac (_comp->_parameters.frac)
  #define f_QE (_comp->_parameters.f_QE)
  #define gamma (_comp->_parameters.gamma)
  #define target_x (_comp->_parameters.target_x)
  #define target_y (_comp->_parameters.target_y)
  #define target_z (_comp->_parameters.target_z)
  #define focus_xw (_comp->_parameters.focus_xw)
  #define focus_yh (_comp->_parameters.focus_yh)
  #define focus_aw (_comp->_parameters.focus_aw)
  #define focus_ah (_comp->_parameters.focus_ah)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define zthick (_comp->_parameters.zthick)
  #define rad_sphere (_comp->_parameters.rad_sphere)
  #define sig_a (_comp->_parameters.sig_a)
  #define sig_i (_comp->_parameters.sig_i)
  #define V0 (_comp->_parameters.V0)
  #define target_index (_comp->_parameters.target_index)
  #define multiples (_comp->_parameters.multiples)
  #define VarsV (_comp->_parameters.VarsV)
  SIG_MESSAGE("[_Sample_display] component Sample=V_sample() DISPLAY [/usr/share/mcstas/3.0-dev/obsolete/V_sample.comp:320]");
  printf("MCDISPLAY: component %s\n", _comp->_name);
  
  if (VarsV.shapetyp == 0) {
    circle("xz", 0,  h/2.0, 0, radius_i);
    circle("xz", 0,  h/2.0, 0, radius_o);
    circle("xz", 0, -h/2.0, 0, radius_i);
    circle("xz", 0, -h/2.0, 0, radius_o);
    line(-radius_i, -h/2.0, 0, -radius_i, +h/2.0, 0);
    line(+radius_i, -h/2.0, 0, +radius_i, +h/2.0, 0);
    line(0, -h/2.0, -radius_i, 0, +h/2.0, -radius_i);
    line(0, -h/2.0, +radius_i, 0, +h/2.0, +radius_i);
    line(-radius_o, -h/2.0, 0, -radius_o, +h/2.0, 0);
    line(+radius_o, -h/2.0, 0, +radius_o, +h/2.0, 0);
    line(0, -h/2.0, -radius_o, 0, +h/2.0, -radius_o);
    line(0, -h/2.0, +radius_o, 0, +h/2.0, +radius_o);
  }
  else { 
	if (VarsV.shapetyp == 1) {
      double xmin = -0.5*xwidth;
      double xmax =  0.5*xwidth;
      double ymin = -0.5*yheight;
      double ymax =  0.5*yheight;
      double zmin = -0.5*zthick;
      double zmax =  0.5*zthick;
      multiline(5, xmin, ymin, zmin,
                   xmax, ymin, zmin,
                   xmax, ymax, zmin,
                   xmin, ymax, zmin,
                   xmin, ymin, zmin);
      multiline(5, xmin, ymin, zmax,
                   xmax, ymin, zmax,
                   xmax, ymax, zmax,
                   xmin, ymax, zmax,
                   xmin, ymin, zmax);
      line(xmin, ymin, zmin, xmin, ymin, zmax);
      line(xmax, ymin, zmin, xmax, ymin, zmax);
      line(xmin, ymax, zmin, xmin, ymax, zmax);
      line(xmax, ymax, zmin, xmax, ymax, zmax);
    }
    else {
      circle("xy", 0,  0.0, 0, rad_sphere);
      circle("xz", 0,  0.0, 0, rad_sphere);
      circle("yz", 0,  0.0, 0, rad_sphere);        
    }
  }
  #undef radius
  #undef thickness
  #undef zdepth
  #undef Vc
  #undef sigma_abs
  #undef sigma_inc
  #undef radius_i
  #undef radius_o
  #undef h
  #undef focus_r
  #undef pack
  #undef frac
  #undef f_QE
  #undef gamma
  #undef target_x
  #undef target_y
  #undef target_z
  #undef focus_xw
  #undef focus_yh
  #undef focus_aw
  #undef focus_ah
  #undef xwidth
  #undef yheight
  #undef zthick
  #undef rad_sphere
  #undef sig_a
  #undef sig_i
  #undef V0
  #undef target_index
  #undef multiples
  #undef VarsV
  return(_comp);
} /* class_V_sample_display */


  #undef magnify
  #undef line
  #undef dashed_line
  #undef multiline
  #undef rectangle
  #undef box
  #undef circle
  #undef cylinder
  #undef sphere

int display(void) { /* called by mccode_main for BNL_H8:DISPLAY */
  printf("MCDISPLAY: start\n");

  /* call iteratively all components DISPLAY */
  class_Arm_display(&_Origin_var);

  class_Source_simple_display(&_Source_var);

  class_PSD_monitor_display(&_D0_Source_var);

  class_Guide_simple_display(&_SC1_var);

  class_PSD_monitor_display(&_D1_SC1_Out_var);

  class_Slit_display(&_As1_var);

  class_Slit_display(&_As2_var);

  class_Slit_display(&_As3_var);

  class_Slit_display(&_As4_var);

  class_PSD_monitor_display(&_D2_A4_var);

  class_Arm_display(&_Mono_Cradle_var);

  class_Monochromator_flat_display(&_PG1Xtal_var);

  class_Arm_display(&_Mono_Out_var);

  class_PSD_monitor_display(&_D4_SC2_In_var);

  class_Guide_simple_display(&_SC2_var);

  class_PSD_monitor_display(&_D5_SC2_Out_var);

  class_Arm_display(&_Sample_Cradle_var);

  class_Arm_display(&_Sample_Out_var);

  class_V_sample_display(&_Sample_var);

  class_PSD_monitor_display(&_D7_SC3_In_var);

  class_Guide_simple_display(&_SC3_var);

  class_PSD_monitor_display(&_D8_SC3_Out_var);

  class_Arm_display(&_Ana_Cradle_var);

  class_Monochromator_flat_display(&_PG2Xtal_var);

  class_Arm_display(&_Ana_Out_var);

  class_PSD_monitor_display(&_D10_SC4_In_var);

  class_Guide_simple_display(&_SC4_var);

  class_PSD_monitor_display(&_He3H_var);

  printf("MCDISPLAY: end\n");

  return(0);
} /* display */

void* _getvar_parameters(char* compname)
/* enables settings parameters based use of the GETPAR macro */
{
  if (!strcmp(compname, "Origin")) return (void *) &(_Origin_var._parameters);
  if (!strcmp(compname, "Source")) return (void *) &(_Source_var._parameters);
  if (!strcmp(compname, "D0_Source")) return (void *) &(_D0_Source_var._parameters);
  if (!strcmp(compname, "SC1")) return (void *) &(_SC1_var._parameters);
  if (!strcmp(compname, "D1_SC1_Out")) return (void *) &(_D1_SC1_Out_var._parameters);
  if (!strcmp(compname, "As1")) return (void *) &(_As1_var._parameters);
  if (!strcmp(compname, "As2")) return (void *) &(_As2_var._parameters);
  if (!strcmp(compname, "As3")) return (void *) &(_As3_var._parameters);
  if (!strcmp(compname, "As4")) return (void *) &(_As4_var._parameters);
  if (!strcmp(compname, "D2_A4")) return (void *) &(_D2_A4_var._parameters);
  if (!strcmp(compname, "Mono_Cradle")) return (void *) &(_Mono_Cradle_var._parameters);
  if (!strcmp(compname, "PG1Xtal")) return (void *) &(_PG1Xtal_var._parameters);
  if (!strcmp(compname, "Mono_Out")) return (void *) &(_Mono_Out_var._parameters);
  if (!strcmp(compname, "D4_SC2_In")) return (void *) &(_D4_SC2_In_var._parameters);
  if (!strcmp(compname, "SC2")) return (void *) &(_SC2_var._parameters);
  if (!strcmp(compname, "D5_SC2_Out")) return (void *) &(_D5_SC2_Out_var._parameters);
  if (!strcmp(compname, "Sample_Cradle")) return (void *) &(_Sample_Cradle_var._parameters);
  if (!strcmp(compname, "Sample_Out")) return (void *) &(_Sample_Out_var._parameters);
  if (!strcmp(compname, "Sample")) return (void *) &(_Sample_var._parameters);
  if (!strcmp(compname, "D7_SC3_In")) return (void *) &(_D7_SC3_In_var._parameters);
  if (!strcmp(compname, "SC3")) return (void *) &(_SC3_var._parameters);
  if (!strcmp(compname, "D8_SC3_Out")) return (void *) &(_D8_SC3_Out_var._parameters);
  if (!strcmp(compname, "Ana_Cradle")) return (void *) &(_Ana_Cradle_var._parameters);
  if (!strcmp(compname, "PG2Xtal")) return (void *) &(_PG2Xtal_var._parameters);
  if (!strcmp(compname, "Ana_Out")) return (void *) &(_Ana_Out_var._parameters);
  if (!strcmp(compname, "D10_SC4_In")) return (void *) &(_D10_SC4_In_var._parameters);
  if (!strcmp(compname, "SC4")) return (void *) &(_SC4_var._parameters);
  if (!strcmp(compname, "He3H")) return (void *) &(_He3H_var._parameters);
}

void* _get_particle_var(char *token, _class_particle *p)
/* enables setpars based use of GET_PARTICLE_DVAR macro and similar */
{
  return 0;
}

#include "_mccode_main.c"
/* End of file "mccode_main.c". */

/* end of generated C code BNL_H8_simple.c */