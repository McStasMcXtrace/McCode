/* Automatically generated file. Do not edit. 
 * Format:     ANSI C source code
 * Creator:    McStas <http://www.mcstas.org>
 * Instrument: PSI_DMC.instr (PSI_DMC)
 * Date:       Fri Jan 31 11:37:02 2020
 * File:       PSI_DMC.c
 * CFLAGS=
 */

#define MCCODE_STRING "McStas 3.0-dev - Jan. 30, 2020"
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

#ifdef DISABLE_TRACE
#undef MC_TRACE_ENABLED
#endif

#ifdef USE_PGI
#undef MC_TRACE_ENABLED
#include <openacc_curand.h>
#endif

struct _struct_particle {
  double x,y,z; /* position [m] */
  double vx,vy,vz; /* velocity [m/s] */
  double sx,sy,sz; /* spin [0-1] */
  unsigned long randstate[7];
  double t, p;    /* time, event weight */
  long long _uid;  /* event ID */
  long _index;     /* component index where to send this event */
  long _absorbed;  /* flag set to TRUE when this event is to be removed/ignored */
  long _scattered; /* flag set to TRUE when this event has interacted with the last component instance */
  long _restore;   /* set to true if neutron event must be restored */
};
typedef struct _struct_particle _class_particle;

_class_particle _particle_global_randnbuse_var;
_class_particle* _particle = &_particle_global_randnbuse_var;

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
* Start of instrument 'PSI_DMC' generated code
***************************************************************************** */

#ifdef MC_TRACE_ENABLED
int traceenabled = 1;
#else
int traceenabled = 0;
#endif
#define MCSTAS "/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/"
int   defaultmain         = 1;
char  instrument_name[]   = "PSI_DMC";
char  instrument_source[] = "PSI_DMC.instr";
char *instrument_exe      = NULL; /* will be set to argv[0] in main */
char  instrument_code[]   = "Instrument PSI_DMC source code PSI_DMC.instr is not embedded in this executable.\n  Use --source option when running McStas.\n";

int main(int argc, char *argv[]){return mccode_main(argc, argv);}

/* *****************************************************************************
* instrument 'PSI_DMC' and components DECLARE
***************************************************************************** */

/* Instrument parameters: structure and a table for the initialisation
   (Used in e.g. inputparse and I/O function (e.g. detector_out) */

struct _struct_instrument_parameters {
  MCNUM lambda;
  MCNUM R;
  MCNUM R_curve;
  char* filename;
  MCNUM D_PHI;
  MCNUM SHIFT;
  MCNUM PACK;
  MCNUM Dw;
  MCNUM BARNS;
};
typedef struct _struct_instrument_parameters _class_instrument_parameters;

/* instrument SPLIT, GROUP and JUMP control logic */
struct instrument_logic_struct {
  long Split_foc_mono; /* this is the SPLIT counter decremented down to 0 */
  _class_particle Split_foc_mono_particle; /* this is the particle to duplicate */
  long Split_sample; /* this is the SPLIT counter decremented down to 0 */
  _class_particle Split_sample_particle; /* this is the particle to duplicate */
};

struct _instrument_struct {
  char   _name[256]; /* the name of this instrument e.g. 'PSI_DMC' */
/* Counters per component instance */
  double counter_AbsorbProp[34]; /* absorbed events in PROP routines */
  double counter_N[34], counter_P[34], counter_P2[34]; /* event counters after each component instance */
  _class_particle _trajectory[34]; /* current trajectory for STORE/RESTORE */
/* Components position table (absolute and relative coords) */
  Coords _position_relative[34]; /* positions of all components */
  Coords _position_absolute[34];
  _class_instrument_parameters _parameters; /* instrument parameters */
  struct instrument_logic_struct logic; /* instrument logic */
} _instrument_var;
struct _instrument_struct *instrument = & _instrument_var;
#pragma acc declare create ( _instrument_var )
#pragma acc declare create ( instrument )

int numipar = 9;
struct mcinputtable_struct mcinputtable[] = {
  "lambda", &(_instrument_var._parameters.lambda), instr_type_double, "2.5666", 
  "R", &(_instrument_var._parameters.R), instr_type_double, "0.87", 
  "R_curve", &(_instrument_var._parameters.R_curve), instr_type_double, "0.87", 
  "filename", &(_instrument_var._parameters.filename), instr_type_string, "Na2Ca3Al2F14.laz", 
  "D_PHI", &(_instrument_var._parameters.D_PHI), instr_type_double, "6", 
  "SHIFT", &(_instrument_var._parameters.SHIFT), instr_type_double, "0", 
  "PACK", &(_instrument_var._parameters.PACK), instr_type_double, "0.7", 
  "Dw", &(_instrument_var._parameters.Dw), instr_type_double, "0.8", 
  "BARNS", &(_instrument_var._parameters.BARNS), instr_type_double, "1", 
  NULL, NULL, instr_type_double, ""
};


/* ************************************************************************** */
/*             SHARE user declarations for all components                     */
/* ************************************************************************** */

/* Shared user declarations for all components types 'Source_Maxwell_3'. */
/* A normalised Maxwellian distribution : Integral over all l = 1 */
#pragma acc routine seq
double SM3_Maxwell(double l, double temp)
  {
    double a=949.0/temp;
    return 2*a*a*exp(-a/(l*l))/(l*l*l*l*l);
  }

/* Shared user declarations for all components types 'Guide'. */
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
void Table_File_List_store(t_Table *tab);

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
void Table_File_List_store(t_Table *tab){
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

#pragma acc routine seq
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
#pragma acc routine seq
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

#ifdef USE_PGI
#define strcmp(a,b) str_comp(a,b)
#endif

  if (!strcmp(Table.method,"linear")) {
    ret = Table_Interp1d(X, X1,Y1, X2,Y2);
  }
  else if (!strcmp(Table.method,"nearest")) {
    ret = Table_Interp1d_nearest(X, X1,Y1, X2,Y2);
  }

#ifdef USE_PGI
#ifdef strcmp
#undef strcmp
#endif
#endif

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
#pragma acc routine seq
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

#ifdef USE_PGI
#define strcmp(a,b) str_comp(a,b)
#endif

    if (!strcmp(Table.method,"linear"))
      ret = Table_Interp2d(X,Y, x1,y1,x2,y2, z11,z12,z21,z22);
#ifdef USE_PGI
#ifdef strcmp
#undef strcmp
#endif
#endif
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
#pragma acc routine seq
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
#pragma acc routine seq
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
#pragma acc routine seq
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
#pragma acc routine seq
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
#pragma acc routine seq
void TableReflecFunc(double mc_pol_q, t_Table *mc_pol_par, double *mc_pol_r) {

  *mc_pol_r = Table_Value(*mc_pol_par, mc_pol_q, 1);
  if(*mc_pol_r>1)
    *mc_pol_r = 1;
  return;
}

/* end of ref-lib.c */


/* Shared user declarations for all components types 'Bender'. */


/* Shared user declarations for all components types 'Al_window'. */
/* ToDo: Should be component local names. */
#ifndef AL_WINDOW
#define avogadro 6.022 /* 10E23 Atoms per mole (mol-1) */
#define Al_sigma_a .231 /* Absorption cross section per atom (barns) at 2200m/s */
#define Al_sigma_i .0082 /* Incoherent scattering cross section per atom (barns) */
#define Al_rho 2.7 /* density (gcm-3) */
#define Al_mmol 27 /* molar mass Al (gmol-1) */
#define Al_my_s (Al_rho / Al_mmol * Al_sigma_i * avogadro * 10) /* inc. XS (barn) */
#define Al_my_a_v (Al_rho / Al_mmol * Al_sigma_a * avogadro * 10 * 2200 )
/* Define Constants for Polynomial Fit of
  sigma_tot(lambda)=A+B1*X+B2*X^2+B3*X^3+B4*X^4+... */
#define Al_pf_A 1.34722
#define Al_pf_B1 .12409
#define Al_pf_B2 .01078
#define Al_pf_B3 -3.25895e-5
#define Al_pf_B4 3.74731e-6
#define AL_WINDOW
#endif

/* Shared user declarations for all components types 'Monochromator_2foc'. */

#ifndef DIV_CUTOFF
#define DIV_CUTOFF 2            /* ~ 10^-5 cutoff. */
#endif

/* Shared user declarations for all components types 'PowderN'. */
/* used for reading data table from file */

#ifndef USE_PGI
/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright (C) 1997-2008, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Runtime: share/interoff.h
*
* %Identification
* Written by: Reynald Arnerin
* Date:    Jun 12, 2008
* Release:
* Version:
*
* Object File Format intersection header for McStas. Requires the qsort function.
*
* Such files may be obtained with e.g.
*   qhull < points.xyz Qx Qv Tv o > points.off
* where points.xyz has format:
*   3
*   <nb_points>
*   <x> <y> <z>
*   ...
* The resulting file should have its first line being changed from '3' into 'OFF'.
* It can then be displayed with geomview.
* A similar, but somewhat older solution is to use 'powercrust' with e.g.
*   powercrust -i points.xyz
* which will generate a 'pc.off' file to be renamed as suited.
*
*******************************************************************************/

#ifndef INTEROFF_LIB_H
#define INTEROFF_LIB_H "$Revision$"

#ifndef EPSILON
#define EPSILON 1e-13
#endif

#define OFF_INTERSECT_MAX 100

//#include <float.h>

#define N_VERTEX_DISPLAYED    200000

typedef struct intersection {
	MCNUM time;  	  //time of the intersection
	Coords v;	      //intersection point
	Coords normal;  //normal vector of the surface intersected
	short in_out;	  //1 if the ray enters the volume, -1 otherwise
	short edge;	    //1 if the intersection is on the boundary of the polygon, and error is possible
	unsigned long index; // index of the face
} intersection;

typedef struct polygon {
  MCNUM* p;       //vertices of the polygon in adjacent order, this way : x1 | y1 | z1 | x2 | y2 | z2 ...
  int npol;       //number of vertices
  Coords normal;
} polygon;

typedef struct off_struct {
    long vtxSize;
    long polySize;
    long faceSize;
    Coords* vtxArray;
    Coords* normalArray;
    unsigned long* faceArray;
    char *filename;
    int mantidflag;
    long mantidoffset;
    intersection intersects[OFF_INTERSECT_MAX]; // After a call to off_intersect_all contains the list of intersections.
    int nextintersect;                 // 'Next' intersection (first t>0) solution after call to off_intersect_all
    int numintersect;               // Number of intersections after call to off_intersect_all
} off_struct;

/*******************************************************************************
* long off_init(  char *offfile, double xwidth, double yheight, double zdepth, off_struct* data)
* ACTION: read an OFF file, optionally center object and rescale, initialize OFF data structure
* INPUT: 'offfile' OFF file to read
*        'xwidth,yheight,zdepth' if given as non-zero, apply bounding box.
*           Specifying only one of these will also use the same ratio on all axes
*        'notcenter' center the object to the (0,0,0) position in local frame when set to zero
* RETURN: number of polyhedra and 'data' OFF structure
*******************************************************************************/
long off_init(  char *offfile, double xwidth, double yheight, double zdepth,
                int notcenter, off_struct* data);

/*******************************************************************************
* int off_intersect_all(double* t0, double* t3,
     Coords *n0, Coords *n3,
     double x, double y, double z,
     double vx, double vy, double vz,
     off_struct *data )
* ACTION: computes intersection of neutron trajectory with an object.
* INPUT:  x,y,z and vx,vy,vz are the position and velocity of the neutron
*         data points to the OFF data structure
* RETURN: the number of polyhedra which trajectory intersects
*         t0 and t3 are the smallest incoming and outgoing intersection times
*         n0 and n3 are the corresponding normal vectors to the surface
*         data is the full OFF structure, including a list intersection type
*******************************************************************************/
int off_intersect_all(double* t0, double* t3,
     Coords *n0, Coords *n3,
     double x, double y, double z,
     double vx, double vy, double vz,
     off_struct *data );

/*******************************************************************************
* int off_intersect(double* t0, double* t3,
     Coords *n0, Coords *n3,
     double x, double y, double z,
     double vx, double vy, double vz,
     off_struct data )
* ACTION: computes intersection of neutron trajectory with an object.
* INPUT:  x,y,z and vx,vy,vz are the position and velocity of the neutron
*         data points to the OFF data structure
* RETURN: the number of polyhedra which trajectory intersects
*         t0 and t3 are the smallest incoming and outgoing intersection times
*         n0 and n3 are the corresponding normal vectors to the surface
*******************************************************************************/
int off_intersect(double* t0, double* t3,
     Coords *n0, Coords *n3,
     double x, double y, double z,
     double vx, double vy, double vz,
     off_struct data );

/*****************************************************************************
* int off_intersectx(double* l0, double* l3,
     Coords *n0, Coords *n3,
     double x, double y, double z,
     double kx, double ky, double kz,
     off_struct data )
* ACTION: computes intersection of an xray trajectory with an object.
* INPUT:  x,y,z and kx,ky,kz, are spatial coordinates and wavevector of the x-ray
*         respectively. data points to the OFF data structure.
* RETURN: the number of polyhedra the trajectory intersects
*         l0 and l3 are the smallest incoming and outgoing intersection lengths
*         n0 and n3 are the corresponding normal vectors to the surface
*******************************************************************************/
int off_x_intersect(double *l0,double *l3,
     Coords *n0, Coords *n3,
     double x,  double y,  double z,
     double kx, double ky, double kz,
     off_struct data );

/*******************************************************************************
* void off_display(off_struct data)
* ACTION: display up to N_VERTEX_DISPLAYED points from the object
*******************************************************************************/
void off_display(off_struct);

#endif

/* end of interoff-lib.h */
/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright (C) 1997-2008, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Runtime: share/interoff-lib.c
*
* %Identification
* Written by: Reynald Arnerin
* Date:    Jun 12, 2008
* Origin: ILL
* Release: $Revision$
* Version: McStas X.Y
*
* Object File Format intersection library for McStas. Requires the qsort function.
*
* Such files may be obtained with e.g.
*   qhull < points.xyz Qx Qv Tv o > points.off
* where points.xyz has format (it supports comments):
*   3
*   <nb_points>
*   <x> <y> <z>
*   ...
* The resulting file should have its first line being changed from '3' into 'OFF'.
* It can then be displayed with geomview.
* A similar, but somewhat older solution is to use 'powercrust' with e.g.
*   powercrust -i points.xyz
* which will generate a 'pc.off' file to be renamed as suited.
*
*******************************************************************************/

#ifndef INTEROFF_LIB_H
#include "interoff-lib.h"
#endif

#pragma acc routine seq
double off_F(double x, double y,double z,double A,double B,double C,double D) {
  return ( A*x + B*y + C*z + D );
}

#pragma acc routine seq
char off_sign(double a) {
  if (a<0)       return(-1);
  else if (a==0) return(0);
  else           return(1);
}

// off_normal ******************************************************************
//gives the normal vector of p
#pragma acc routine seq
void off_normal(Coords* n, polygon p)
{
  //using Newell method
  int i=0,j=0;
  n->x=0;n->y=0;n->z=0;
  for (i = 0, j = p.npol-1; i < p.npol; j = i++)
  {
    MCNUM x1=p.p[3*i],
          y1=p.p[3*i+1],
          z1=p.p[3*i+2];
    MCNUM x2=p.p[3*j],
          y2=p.p[3*j+1],
          z2=p.p[3*j+2];
    // n is the cross product of v1*v2
    n->x += (y1 - y2) * (z1 + z2);
    n->y += (z1 - z2) * (x1 + x2);
    n->z += (x1 - x2) * (y1 + y2);
  }
} /* off_normal */

// off_pnpoly ******************************************************************
//based on http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
//return 0 if the vertex is out
//    1 if it is in
//   -1 if on the boundary
#pragma acc routine seq
int off_pnpoly(polygon p, Coords v)
{
  int i=0, c = 0;
  MCNUM minx=FLT_MAX,maxx=-FLT_MAX,miny=FLT_MAX,maxy=-FLT_MAX,minz=FLT_MAX,maxz=-FLT_MAX;
  MCNUM rangex=0,rangey=0,rangez=0;

  int pol2dx=0,pol2dy=1;          //2d restriction of the poly
  MCNUM x=v.x,y=v.y;


  //take the most relevant 2D projection (prevent from instability)
  for (i=0; i<p.npol; ++i)
  {
    if (p.p[3*i]<minx)   minx=p.p[3*i];
    if (p.p[3*i]>maxx)   maxx=p.p[3*i];
    if (p.p[3*i+1]<miny) miny=p.p[3*i+1];
    if (p.p[3*i+1]>maxy) maxy=p.p[3*i+1];
    if (p.p[3*i+2]<minz) minz=p.p[3*i+2];
    if (p.p[3*i+2]>maxz) maxz=p.p[3*i+2];
  }
  rangex=maxx-minx;
  rangey=maxy-miny;
  rangez=maxz-minz;

  if (rangex<rangez)
  {
    if (rangex<rangey) {
      pol2dx=2;
      x=v.z;
    } else {
      pol2dy=2;
      y=v.z;
    }
  }
  else if (rangey<rangez) {
    pol2dy=2;
    y=v.z;
  }

  //trace rays and test number of intersection
  int j;
  for (i = 0, j = p.npol-1; i < p.npol; j = i++) {
    if (((((p.p[3*i+pol2dy])<=y) && (y<(p.p[3*j+pol2dy]))) ||
         (((p.p[3*j+pol2dy])<=y) && (y<(p.p[3*i+pol2dy])))) &&
        (x < ( (p.p[3*j+pol2dx] - p.p[3*i+pol2dx]) * (y - p.p[3*i+pol2dy])
             / (p.p[3*j+pol2dy] - p.p[3*i+pol2dy]) + p.p[3*i+pol2dx]) ))
      c = !c;

    if (((fabs(p.p[3*i+pol2dy]-y)<=EPSILON) || ((fabs(p.p[3*j+pol2dy]-y)<=EPSILON))) &&
        fabs(x -((p.p[3*j+pol2dx] - p.p[3*i+pol2dx]) * (y - p.p[3*i+pol2dy])
          / (p.p[3*j+pol2dy] - p.p[3*i+pol2dy]) + p.p[3*i+pol2dx])) < EPSILON)
    {
      //the point lies on the edge
      c=-1;
      break;
    }
  }

  return c;
} /* off_pnpoly */

// off_intersectPoly ***********************************************************
//gives the intersection vertex between ray [a,b) and polygon p and its parametric value on (a b)
//based on http://geometryalgorithms.com/Archive/algorithm_0105/algorithm_0105.htm
#pragma acc routine seq
int off_intersectPoly(intersection *inter, Coords a, Coords b, polygon p)
{
  //direction vector of [a,b]
  Coords dir = {b.x-a.x, b.y-a.y, b.z-a.z};

  //the normal vector to the polygon
  Coords normale=p.normal;
  //off_normal(&normale, p); done at the init stage

  //direction vector from a to a vertex of the polygon
  Coords w0 = {a.x-p.p[0], a.y-p.p[1], a.z-p.p[2]};

  //scalar product
  MCNUM nw0  =-scalar_prod(normale.x,normale.y,normale.z,w0.x,w0.y,w0.z);
  MCNUM ndir = scalar_prod(normale.x,normale.y,normale.z,dir.x,dir.y,dir.z);
  inter->time = inter->edge = inter->in_out=0;
  inter->v = inter->normal = coords_set(0,0,1);

  if (fabs(ndir) < EPSILON)    // ray is parallel to polygon plane
  {
    if (nw0 == 0)              // ray lies in polygon plane (infinite number of solution)
      return 0;
    else return 0;             // ray disjoint from plane (no solution)
  }

  // get intersect point of ray with polygon plane
  inter->time = nw0 / ndir;            //parametric value the point on line (a,b)

  inter->v = coords_set(a.x + inter->time * dir.x,// intersect point of ray and plane
    a.y + inter->time * dir.y,
    a.z + inter->time * dir.z);

  int res=off_pnpoly(p,inter->v);

  inter->edge=(res==-1);
  if (ndir<0)
    inter->in_out=1;  //the negative dot product means we enter the surface
  else
    inter->in_out=-1;

  inter->normal=p.normal;

  return res;         //true if the intersection point lies inside the poly
} /* off_intersectPoly */


// off_getBlocksIndex **********************************************************
/*reads the indexes at the beginning of the off file as this :
line 1  OFF
line 2  nbVertex nbFaces nbEdges
*/
FILE *off_getBlocksIndex(char* filename, long* vtxSize, long* polySize )
{
  FILE* f = Open_File(filename,"r", NULL); /* from read_table-lib: FILE *Open_File(char *name, char *Mode, char *path) */
  if (!f) return (f);

  char line[CHAR_BUF_LENGTH];
  char *ret=0;
  *vtxSize = *polySize = 0;

  /* **************** start to read the file header */
  /* OFF file:
     'OFF' or '3'
   */

  ret=fgets(line,CHAR_BUF_LENGTH , f);// line 1 = "OFF"
  if (ret == NULL)
  {
    fprintf(stderr, "Error: Can not read 1st line in file %s (interoff/off_getBlocksIndex)\n", filename);
    exit(1);
  }
  if (strlen(line)>5)
  {
      fprintf(stderr,"Error: First line in %s is too long (=%lu). Possibly the line is not terminated by '\\n'.\n"
              "       The first line is required to be exactly 'OFF', '3' or 'ply'.\n",
              filename,(long unsigned)strlen(line));
      fclose(f);
      return(NULL);
  }

  if (strncmp(line,"OFF",3) && strncmp(line,"3",1) && strncmp(line,"ply",1))
  {
    fprintf(stderr, "Error: %s is probably not an OFF, NOFF or PLY file (interoff/off_getBlocksIndex).\n"
                    "       Requires first line to be 'OFF', '3' or 'ply'.\n",filename);
    fclose(f);
    return(NULL);
  }

  if (!strncmp(line,"OFF",3) || !strncmp(line,"3",1)) {
    do  /* OFF file: skip # comments which may be there */
    {
      ret=fgets(line,CHAR_BUF_LENGTH , f);
      if (ret == NULL)
      {
        fprintf(stderr, "Error: Can not read line in file %s (interoff/off_getBlocksIndex)\n", filename);
        exit(1);
      }
    } while (line[0]=='#');
    //line = nblines of vertex,faces and edges arrays
    sscanf(line,"%lu %lu",vtxSize,polySize);
  } else {
    do  /* PLY file: read all lines until find 'end_header'
           and locate 'element faces' and 'element vertex' */
    {
      ret=fgets(line,CHAR_BUF_LENGTH , f);
      if (ret == NULL)
      {
        fprintf(stderr, "Error: Can not read line in file %s (interoff/off_getBlocksIndex)\n", filename);
        exit(1);
      }
      if (!strncmp(line,"element face",12))
        sscanf(line,"element face %lu",polySize);
      else if (!strncmp(line,"element vertex",14))
        sscanf(line,"element vertex %lu",vtxSize);
      else if (!strncmp(line,"format binary",13))
        exit(fprintf(stderr,
          "Error: Can not read binary PLY file %s, only 'format ascii' (interoff/off_getBlocksIndex)\n%s\n",
          filename, line));
    } while (strncmp(line,"end_header",10));
  }

  /* The FILE is left opened ready to read 'vtxSize' vertices (vtxSize *3 numbers)
     and then polySize polygons (rows) */

  return(f);
} /* off_getBlocksIndex */

// off_init_planes *************************************************************
//gives the equations of 2 perpandicular planes of [ab]
#pragma acc routine seq
void off_init_planes(Coords a, Coords b,
  MCNUM* A1, MCNUM* C1, MCNUM* D1, MCNUM *A2, MCNUM* B2, MCNUM* C2, MCNUM* D2)
{
  //direction vector of [a b]
  Coords dir={b.x-a.x, b.y-a.y, b.z-a.z};

  //the plane parallel to the 'y' is computed with the normal vector of the projection of [ab] on plane 'xz'
  *A1= dir.z;
  *C1=-dir.x;
  if(*A1!=0 || *C1!=0)
    *D1=-(a.x)*(*A1)-(a.z)*(*C1);
  else
  {
    //the plane does not support the vector, take the one parallel to 'z''
    *A1=1;
    //B1=dir.x=0
    *D1=-(a.x);
  }
  //the plane parallel to the 'x' is computed with the normal vector of the projection of [ab] on plane 'yz'
  *B2= dir.z;
  *C2=-dir.y;
  *A2= 0;
  if (*B2==0 && *C2==0)
  {
    //the plane does not support the vector, take the one parallel to 'z'
    *B2=1;
    //B1=dir.x=0
    *D2=-(a.y);
  }
  else {
    if (dir.z==0)
    {
      //the planes are the same, take the one parallel to 'z'
      *A2= dir.y;
      *B2=-dir.x;
      *D2=-(a.x)*(*A2)-(a.y)*(*B2);
    }
    else
      *D2=-(a.y)**B2-(a.z)**C2;
  }
} /* off_init_planes */

// off_clip_3D_mod *************************************************************
#pragma acc routine seq
int off_clip_3D_mod(intersection* t, Coords a, Coords b,
  Coords* vtxArray, unsigned long vtxSize, unsigned long* faceArray,
  unsigned long faceSize, Coords* normalArray)
{
  MCNUM A1=0, C1=0, D1=0, A2=0, B2=0, C2=0, D2=0;      //perpendicular plane equations to [a,b]
  off_init_planes(a, b, &A1, &C1, &D1, &A2, &B2, &C2, &D2);

  int t_size=0;
  //unsigned long vtxSize=vtxTable.rows, faceSize=faceTable.columns;  //Size of the corresponding tables
  char sg[vtxSize];  //array telling if vertex is left or right of the plane
  MCNUM popol[3*CHAR_BUF_LENGTH];
  unsigned long i=0,indPoly=0;
  for (i=0; i < vtxSize; ++i)
  {
    sg[i]=off_sign(off_F(vtxArray[i].x,vtxArray[i].y,vtxArray[i].z,A1,0,C1,D1));
  }

  //exploring the polygons :
  i=indPoly=0;
  while (i<faceSize)
  {
    polygon pol;
    pol.npol  = faceArray[i];                //nb vertex of polygon
    pol.p     = popol;
    pol.normal= coords_set(0,0,1);
    unsigned long indVertP1=faceArray[++i];  //polygon's first vertex index in vtxTable
    int j=1;
    while (j<pol.npol)
    {
      //polygon's j-th vertex index in vtxTable
      if (sg[indVertP1]!=sg[faceArray[i+j]]) //if the plane intersect the polygon
        break;

      ++j;
    }

    if (j<pol.npol)          //ok, let's test with the second plane
    {
      char sg1=off_sign(off_F(vtxArray[indVertP1].x,vtxArray[indVertP1].y,vtxArray[indVertP1].z,A2,B2,C2,D2));//tells if vertex is left or right of the plane

      j=1;
      while (j<pol.npol)
      {
        //unsigned long indVertPi=faceArray[i+j];  //polyg's j-th vertex index in vtxTable
        Coords vertPi=vtxArray[faceArray[i+j]];
        if (sg1!=off_sign(off_F(vertPi.x,vertPi.y,vertPi.z,A2,B2,C2,D2)))//if the plane intersect the polygon
          break;
        ++j;
      }
      if (j<pol.npol)
      {
        if (t_size>CHAR_BUF_LENGTH)
        {
#ifndef USE_PGI
          fprintf(stderr, "Warning: number of intersection exceeded (%d) (interoff-lib/off_clip_3D_mod)\n", CHAR_BUF_LENGTH);
#endif
            return (t_size);
        }
        //both planes intersect the polygon, let's find the intersection point
        //our polygon :
        int k;
        for (k=0; k<pol.npol; ++k)
        {
          Coords vertPk=vtxArray[faceArray[i+k]];
          pol.p[3*k]  =vertPk.x;
          pol.p[3*k+1]=vertPk.y;
          pol.p[3*k+2]=vertPk.z;
        }
        pol.normal=normalArray[indPoly];
        intersection x;
        if (off_intersectPoly(&x, a, b, pol))
        {
          x.index = indPoly;
          t[t_size++]=x;
        }
      } /* if (j<pol.npol) */
    } /* if (j<pol.npol) */
    i += pol.npol;
    indPoly++;
  } /* while i<faceSize */
  return t_size;
} /* off_clip_3D_mod */


// off_compare *****************************************************************
#pragma acc routine seq
int off_compare (void const *a, void const *b)
{
   intersection const *pa = a;
   intersection const *pb = b;

   return off_sign(pa->time - pb->time);
} /* off_compare */

// off_cleanDouble *************************************************************
//given an array of intersections throw those which appear several times
//returns 1 if there is a possibility of error
#pragma acc routine seq
int off_cleanDouble(intersection* t, int* t_size)
{
  int i=1;
  intersection prev=t[0];
  while (i<*t_size)
  {
    int j=i;
    //for each intersection with the same time
    while (j<*t_size && fabs(prev.time-t[j].time)<EPSILON)
    {
      //if the intersection is the exact same erase it
      if (prev.in_out==t[j].in_out)
      {
        int k;
        for (k=j+1; k<*t_size; ++k)
        {
          t[k-1]=t[k];
        }
        *t_size-=1;
      }
      else
        ++j;
    }
    prev=t[i];
    ++i;

  }
  return 1;
} /* off_cleanDouble */

// off_cleanInOut **************************************************************
//given an array of intesections throw those which enter and exit in the same time
//Meaning the ray passes very close to the volume
//returns 1 if there is a possibility of error
#pragma acc routine seq
int off_cleanInOut(intersection* t, int* t_size)
{
  int i=1;
  intersection prev=t[0];
  while (i<*t_size)
  {
    //if two intersection have the same time but one enters and the other exits erase both
    //(such intersections must be adjacent in the array : run off_cleanDouble before)
    if (fabs(prev.time-t[i].time)<EPSILON && prev.in_out!=t[i].in_out)
    {
      int j=0;
      for (j=i+1; j<*t_size; ++j)
      {
        t[j-2]=t[j];
      }
      *t_size-=2;
      prev=t[i-1];
    }
    else
    {
      prev=t[i];
      ++i;
    }
  }
  return (*t_size);
} /* off_cleanInOut */

/* PUBLIC functions ******************************************************** */

/*******************************************************************************
* long off_init(  char *offfile, double xwidth, double yheight, double zdepth, off_struct* data)
* ACTION: read an OFF file, optionally center object and rescale, initialize OFF data structure
* INPUT: 'offfile' OFF file to read
*        'xwidth,yheight,zdepth' if given as non-zero, apply bounding box.
*           Specifying only one of these will also use the same ratio on all axes
*        'notcenter' center the object to the (0,0,0) position in local frame when set to zero
* RETURN: number of polyhedra and 'data' OFF structure
*******************************************************************************/
long off_init(  char *offfile, double xwidth, double yheight, double zdepth,
                int notcenter, off_struct* data)
{
  // data to be initialized
  long    vtxSize =0, polySize=0, i=0, ret=0, faceSize=0;
  Coords* vtxArray        =NULL;
  Coords* normalArray     =NULL;
  unsigned long* faceArray=NULL;
  FILE*   f               =NULL; /* the FILE with vertices and polygons */
  double minx=FLT_MAX,maxx=-FLT_MAX,miny=FLT_MAX,maxy=-FLT_MAX,minz=FLT_MAX,maxz=-FLT_MAX;

  // get the indexes
  if (!data) return(0);

  MPI_MASTER(
  printf("Loading geometry file (OFF/PLY): %s\n", offfile);
  );

  f=off_getBlocksIndex(offfile,&vtxSize,&polySize);
  if (!f) return(0);

  // read vertex table = [x y z | x y z | ...] =================================
  // now we read the vertices as 'vtxSize*3' numbers and store it in vtxArray
  MPI_MASTER(
  printf("  Number of vertices: %ld\n", vtxSize);
  );
  vtxArray   = malloc(vtxSize*sizeof(Coords));
  if (!vtxArray) return(0);
  i=0;
  while (i<vtxSize && ~feof(f))
  {
    double x,y,z;
    ret=fscanf(f, "%lg%lg%lg", &x,&y,&z);
    if (!ret) {
      // invalid line: we skip it (probably a comment)
      char line[CHAR_BUF_LENGTH];
      char *s=fgets(line, CHAR_BUF_LENGTH, f);
      continue;
    }
    if (ret != 3) {
      fprintf(stderr, "Error: can not read [xyz] coordinates for vertex %li in file %s (interoff/off_init). Read %li values.\n",
        i, offfile, ret);
      exit(2);
    }
    vtxArray[i].x=x;
    vtxArray[i].y=y;
    vtxArray[i].z=z;

    //bounding box
    if (vtxArray[i].x<minx) minx=vtxArray[i].x;
    if (vtxArray[i].x>maxx) maxx=vtxArray[i].x;
    if (vtxArray[i].y<miny) miny=vtxArray[i].y;
    if (vtxArray[i].y>maxy) maxy=vtxArray[i].y;
    if (vtxArray[i].z<minz) minz=vtxArray[i].z;
    if (vtxArray[i].z>maxz) maxz=vtxArray[i].z;
    i++; // inquire next vertex
  }

  // resizing and repositioning params
  double centerx=0, centery=0, centerz=0;
  if (!notcenter) {
    centerx=(minx+maxx)*0.5;
    centery=(miny+maxy)*0.5;
    centerz=(minz+maxz)*0.5;
  }

  double rangex=-minx+maxx,
         rangey=-miny+maxy,
         rangez=-minz+maxz;

  double ratiox=1,ratioy=1,ratioz=1;

  if (xwidth && rangex)
  {
    ratiox=xwidth/rangex;
    ratioy=ratiox;
    ratioz=ratiox;
  }

  if (yheight && rangey)
  {
    ratioy=yheight/rangey;
    if(!xwidth)  ratiox=ratioy;
    ratioz=ratioy;
  }

  if (zdepth && rangez)
  {
    ratioz=zdepth/rangez;
    if(!xwidth)  ratiox=ratioz;
    if(!yheight) ratioy=ratioz;
  }

  rangex *= ratiox;
  rangey *= ratioy;
  rangez *= ratioz;

  //center and resize the object
  for (i=0; i<vtxSize; ++i)
  {
    vtxArray[i].x=(vtxArray[i].x-centerx)*ratiox+(!notcenter ? 0 : centerx);
    vtxArray[i].y=(vtxArray[i].y-centery)*ratioy+(!notcenter ? 0 : centery);
    vtxArray[i].z=(vtxArray[i].z-centerz)*ratioz+(!notcenter ? 0 : centerz);
  }

  // read face table = [nbvertex v1 v2 vn | nbvertex v1 v2 vn ...] =============
  MPI_MASTER(
  printf("  Number of polygons: %ld\n", polySize);
  );
  normalArray= malloc(polySize*sizeof(Coords));
  faceArray  = malloc(polySize*10*sizeof(unsigned long)); // we assume polygons have less than 9 vertices
  if (!normalArray || !faceArray) return(0);

  // fill faces
  faceSize=0;
  i=0;
  while (i<polySize && ~feof(f)) {
    int  nbVertex=0, j=0;
    // read the length of this polygon
    ret=fscanf(f, "%d", &nbVertex);
    if (!ret) {
      // invalid line: we skip it (probably a comment)
      char line[CHAR_BUF_LENGTH];
      char *s=fgets(line, CHAR_BUF_LENGTH, f);
      continue;
    }
    if (ret != 1) {
      fprintf(stderr, "Error: can not read polygon %li length in file %s (interoff/off_init)\n",
        i, offfile);
      exit(3);
    }
    if (faceSize > polySize*10) {
      fprintf(stderr, "Error: %li exceeded allocated polygon array[%li] in file %s (interoff/off_init)\n",
        faceSize, polySize*10, offfile);
    }
    faceArray[faceSize++] = nbVertex; // length of the polygon/face
    // then read the vertex ID's
    for (j=0; j<nbVertex; j++) {
      double vtx=0;
      ret=fscanf(f, "%lg", &vtx);
      faceArray[faceSize++] = vtx;   // add vertices index after length of polygon
    }
    i++;
  }

  // precomputes normals
  long indNormal=0;//index in polyArray
  i=0;    //index in faceArray
  while (i<faceSize)
  {
    int    nbVertex=faceArray[i];//nb of vertices of this polygon
    double vertices[3*nbVertex];
    int j;

    for (j=0; j<nbVertex; ++j)
    {
      unsigned long indVertPj=faceArray[i+j+1];
      vertices[3*j]  =vtxArray[indVertPj].x;
      vertices[3*j+1]=vtxArray[indVertPj].y;
      vertices[3*j+2]=vtxArray[indVertPj].z;
    }

    polygon p;
    p.p   =vertices;
    p.npol=nbVertex;
    off_normal(&(p.normal),p);

    normalArray[indNormal]=p.normal;

    i += nbVertex+1;
    indNormal++;

  }

  MPI_MASTER(
  if (ratiox!=ratioy || ratiox!=ratioz || ratioy!=ratioz)
    printf("Warning: Aspect ratio of the geometry %s was modified.\n"
           "         If you want to keep the original proportions, specifiy only one of the dimensions.\n",
           offfile);
  if ( xwidth==0 && yheight==0 && zdepth==0 ) {
    printf("Warning: Neither xwidth, yheight or zdepth are defined.\n"
	   "           The file-defined (non-scaled) geometry the OFF geometry %s will be applied!\n",
           offfile);
  }
  printf("  Bounding box dimensions for geometry %s:\n", offfile);
  printf("    Length=%f (%.3f%%)\n", rangex, ratiox*100);
  printf("    Width= %f (%.3f%%)\n", rangey, ratioy*100);
  printf("    Depth= %f (%.3f%%)\n", rangez, ratioz*100);
  );

  data->vtxArray   = vtxArray;
  data->normalArray= normalArray;
  data->faceArray  = faceArray;
  data->vtxSize    = vtxSize;
  data->polySize   = polySize;
  data->faceSize   = faceSize;
  data->filename   = offfile;
  return(polySize);
} /* off_init */

#pragma acc routine seq
int Min_int(int x, int y) {
  return (x<y)? x :y;
}

#pragma acc routine(merge)
 void merge(int arr[], int l, int m, int r)
{
int i, j, k;
int n1 = m - l + 1;
int n2 =  r - m;

/* create temp arrays */
int *L, *R;
 L = (int *)malloc(sizeof(int) * n1);
 R = (int *)malloc(sizeof(int) * n2);
/* Copy data to temp arrays L[] and R[] */
 #pragma acc loop independent
for (i = 0; i < n1; i++)
    L[i] = arr[l + i];
 #pragma acc loop independent
for (j = 0; j < n2; j++)
    R[j] = arr[m + 1+ j];

/* Merge the temp arrays back into arr[l..r]*/
i = 0;
j = 0;
k = l;

while (i < n1 && j < n2)
{
    if (L[i] <= R[j])
    {
        arr[k] = L[i];
        i++;
    }
    else
    {
        arr[k] = R[j];
        j++;
    }
    k++;
}

/* Copy the remaining elements of L[], if there are any */

while (i < n1)
{
    arr[k] = L[i];
    i++;
    k++;
}

/* Copy the remaining elements of R[], if there are any */
while (j < n2)
{
    arr[k] = R[j];
    j++;
    k++;
}
free(L);
free(R);
}

#pragma acc routine seq
void q2sort(void *base, size_t nmemb, size_t size,
	    int (*compar)(const void *, const void *))
{
  int curr_size;  // For current size of subarrays to be merged
  // curr_size varies from 1 to n/2
  int left_start; // For picking starting index of left subarray
  // to be merged
  // pcopying (R[0:n2])
  {
    for (curr_size=1; curr_size<=size-1; curr_size = 2*curr_size)
      {
	// Pick starting point of different subarrays of current size
	for (left_start=0; left_start<size-1; left_start += 2*curr_size)
	  {
	    // Find ending point of left subarray. mid+1 is starting
	    // point of right
	    int mid = left_start + curr_size - 1;

	    int right_end = Min_int(left_start + 2*curr_size - 1, size-1);

	    // Merge Subarrays arr[left_start...mid] & arr[mid+1...right_end]
	    if (mid < right_end) merge(base, left_start, mid, right_end);
	  }
      }
  }
}


/*******************************************************************************
* int off_intersect_all(double* t0, double* t3,
     Coords *n0, Coords *n3,
     double x, double y, double z,
     double vx, double vy, double vz,
     off_struct *data )
* ACTION: computes intersection of neutron trajectory with an object.
* INPUT:  x,y,z and vx,vy,vz are the position and velocity of the neutron
*         data points to the OFF data structure
* RETURN: the number of polyhedra which trajectory intersects
*         t0 and t3 are the smallest incoming and outgoing intersection times
*         n0 and n3 are the corresponding normal vectors to the surface
*         data is the full OFF structure, including a list intersection type
*******************************************************************************/
#pragma acc routine seq
int off_intersect_all(double* t0, double* t3,
     Coords *n0, Coords *n3,
     double x,  double y,  double z,
     double vx, double vy, double vz,
     off_struct *data )
{
    Coords A={x, y, z};
    Coords B={x+vx, y+vy, z+vz};
    int t_size=off_clip_3D_mod(data->intersects, A, B,
      data->vtxArray, data->vtxSize, data->faceArray, data->faceSize, data->normalArray );
    q2sort(data->intersects, t_size, sizeof(intersection),  off_compare);
    off_cleanDouble(data->intersects, &t_size);
    off_cleanInOut(data->intersects,  &t_size);

    /*find intersections "closest" to 0 (favouring positive ones)*/
    if(t_size>0){
      int i=0;
      if(t_size>1) {
        for (i=1; i < t_size-1; i++){
          if (data->intersects[i-1].time > 0 && data->intersects[i].time > 0)
            break;
        }

	data->nextintersect=i-1;
	data->numintersect=t_size;

        if (t0) *t0 = data->intersects[i-1].time;
        if (n0) *n0 = data->intersects[i-1].normal;
        if (t3) *t3 = data->intersects[i].time;
        if (n3) *n3 = data->intersects[i].normal;
      } else {
        if (t0) *t0 = data->intersects[0].time;
	      if (n0) *n0 = data->intersects[0].normal;
      }
      /* should also return t[0].index and t[i].index as polygon ID */
      return t_size;
    }
    return 0;
} /* off_intersect */

/*******************************************************************************
* int off_intersect(double* t0, double* t3,
     Coords *n0, Coords *n3,
     double x, double y, double z,
     double vx, double vy, double vz,
     off_struct data )
* ACTION: computes intersection of neutron trajectory with an object.
* INPUT:  x,y,z and vx,vy,vz are the position and velocity of the neutron
*         data points to the OFF data structure
* RETURN: the number of polyhedra which trajectory intersects
*         t0 and t3 are the smallest incoming and outgoing intersection times
*         n0 and n3 are the corresponding normal vectors to the surface
*******************************************************************************/
#pragma acc routine seq
int off_intersect(double* t0, double* t3,
     Coords *n0, Coords *n3,
     double x,  double y,  double z,
     double vx, double vy, double vz,
     off_struct data )
{
  return off_intersect_all(t0, t3, n0, n3, x, y, z, vx, vy, vz, &data );
} /* off_intersect */

/*****************************************************************************
* int off_x_intersect(double* l0, double* l3,
     Coords *n0, Coords *n3,
     double x, double y, double z,
     double kx, double ky, double kz,
     off_struct data )
* ACTION: computes intersection of an xray trajectory with an object.
* INPUT:  x,y,z and kx,ky,kz, are spatial coordinates and wavevector of the x-ray
*         respectively. data points to the OFF data structure.
* RETURN: the number of polyhedra the trajectory intersects
*         l0 and l3 are the smallest incoming and outgoing intersection lengths
*         n0 and n3 are the corresponding normal vectors to the surface
*******************************************************************************/
#pragma acc routine seq
int off_x_intersect(double *l0,double *l3,
     Coords *n0, Coords *n3,
     double x,  double y,  double z,
     double kx, double ky, double kz,
     off_struct data )
{
  /*This function simply reformats and calls off_intersect (as for neutrons)
   *by normalizing the wavevector - this will yield the intersection lengths
   *in m*/
  double jx,jy,jz,invk;
  int n;
  invk=1/sqrt(scalar_prod(kx,ky,kz,kx,ky,kz));
  jx=kx*invk;jy=ky*invk;jz=kz*invk;
  n=off_intersect(l0,l3,n0,n3,x,y,z,jx,jy,jz,data);
  return n;
}


/*******************************************************************************
* void off_display(off_struct data)
* ACTION: display up to N_VERTEX_DISPLAYED polygons from the object
*******************************************************************************/
void off_display(off_struct data)
{
#ifndef USE_PGI
  unsigned int i;
  double ratio=(double)(N_VERTEX_DISPLAYED)/(double)data.faceSize;
  unsigned int pixel=0;
  for (i=0; i<data.faceSize-1; i++) {
    int j;
    int nbVertex = data.faceArray[i];
    double x0,y0,z0;
    x0 = data.vtxArray[data.faceArray[i+1]].x;
    y0 = data.vtxArray[data.faceArray[i+1]].y;
    z0 = data.vtxArray[data.faceArray[i+1]].z;
    double x1=x0,y1=y0,z1=z0;
    double cmx=0,cmy=0,cmz=0;

    int drawthis = rand01() < ratio;
    // First pass, calculate center of mass location...
    for (j=1; j<=nbVertex; j++) {
      cmx = cmx+data.vtxArray[data.faceArray[i+j]].x;
      cmy = cmy+data.vtxArray[data.faceArray[i+j]].y;
      cmz = cmz+data.vtxArray[data.faceArray[i+j]].z;
    }
    cmx /= nbVertex;
    cmy /= nbVertex;
    cmz /= nbVertex;

    char pixelinfo[1024];
    sprintf(pixelinfo, "%li,%li,%li,%i,%g,%g,%g,%g,%g,%g", data.mantidoffset+pixel, data.mantidoffset, data.mantidoffset+data.polySize-1, nbVertex, cmx, cmy, cmz, x1-cmx, y1-cmy, z1-cmz);
    for (j=2; j<=nbVertex; j++) {
      double x2,y2,z2;
      x2 = data.vtxArray[data.faceArray[i+j]].x;
      y2 = data.vtxArray[data.faceArray[i+j]].y;
      z2 = data.vtxArray[data.faceArray[i+j]].z;
      sprintf(pixelinfo, "%s,%g,%g,%g", pixelinfo, x2-cmx, y2-cmy, z2-cmz);
      if (ratio > 1 || drawthis) {
	mcdis_line(x1,y1,z1,x2,y2,z2);
      }
      x1 = x2; y1 = y2; z1 = z2;
    }
    if (ratio > 1 || drawthis) {
	mcdis_line(x1,y1,z1,x0,y0,z0);
      }
    if (data.mantidflag) {
      printf("MANTID_PIXEL: %s\n", pixelinfo);
      pixel++;
    }
    i += nbVertex;
  }
#endif
} /* off_display */

/* end of interoff-lib.c */

#endif
/* Declare structures and functions only once in each instrument. */
#ifndef POWDERN_DECL
#define POWDERN_DECL
/* format definitions in the order {j d F2 DW Dd inv2d q F strain} */
#ifndef Crystallographica
#define Crystallographica { 4,5,7,0,0,0,0,0,0 }
#define Fullprof          { 4,0,8,0,0,5,0,0,0 }
#define Lazy              {17,6,0,0,0,0,0,13,0 }
#define Undefined         { 0,0,0,0,0,0,0,0,0 }
#endif

struct line_data
{
double F2;                  /* Value of structure factor */
double q;                   /* Qvector */
int j;                      /* Multiplicity */
double DWfactor;            /* Debye-Waller factor */
double w;                   /* Intrinsic line width */
double Epsilon;             /* Strain=delta_d_d/d shift in ppm */
};

struct line_info_struct
{
struct line_data *list;     /* Reflection array */
int  count;                  /* Number of reflections */
double Dd;
double DWfactor;
double V_0;
double rho;
double at_weight;
double at_nb;
double sigma_a;
double sigma_i;
char   compname[256];
double flag_barns;
int    shape; /* 0 cylinder, 1 box, 2 sphere, 3 OFF file */
int    column_order[9]; /* column signification */
int    flag_warning;
char   type;  /* interaction type of event t=Transmit, i=Incoherent, c=Coherent */
double dq;    /* wavevector transfer [Angs-1] */
double Epsilon; /* global strain in ppm */
double XsectionFactor;
double my_s_v2_sum;
double my_a_v;
double my_inc;
double *w_v,*q_v, *my_s_v2;
double radius_i,xwidth_i,yheight_i,zdepth_i;
double v; /* last velocity (cached) */
      double Nq;
      int    nb_reuses, nb_refl, nb_refl_count;
      double v_min, v_max;
      double xs_Nq[CHAR_BUF_LENGTH];
      double xs_sum[CHAR_BUF_LENGTH];
      double neutron_passed;
      long   xs_compute, xs_reuse, xs_calls;
    };
#ifndef USE_PGI
  off_struct offdata;
#endif
  // PN_list_compare *****************************************************************

  int PN_list_compare (void const *a, void const *b)
  {
     struct line_data const *pa = a;
     struct line_data const *pb = b;
     double s = pa->q - pb->q;

     if (!s) return 0;
     else    return (s < 0 ? -1 : 1);
  } /* PN_list_compare */

  int read_line_data(char *SC_file, struct line_info_struct *info)
  {
    struct line_data *list = NULL;
    int    size = 0;
    t_Table sTable; /* sample data table structure from SC_file */
    int    i=0;
    int    mult_count  =0;
    char   flag=0;
    double q_count=0, j_count=0, F2_count=0;
    char **parsing;
    int    list_count=0;

    if (!SC_file || !strlen(SC_file) || !strcmp(SC_file, "NULL")) {
      MPI_MASTER(
      printf("PowderN: %s: Using incoherent elastic scattering only.\n",
          info->compname);
      );
      info->count = 0;
      return(0);
    }
    Table_Read(&sTable, SC_file, 1); /* read 1st block data from SC_file into sTable*/

    /* parsing of header */
    parsing = Table_ParseHeader(sTable.header,
      "Vc","V_0",
      "sigma_abs","sigma_a ",
      "sigma_inc","sigma_i ",
      "column_j",
      "column_d",
      "column_F2",
      "column_DW",
      "column_Dd",
      "column_inv2d", "column_1/2d", "column_sintheta/lambda",
      "column_q", /* 14 */
      "DW", "Debye_Waller",
      "delta_d_d/d",
      "column_F ",
      "V_rho",
      "density",
      "weight",
      "nb_atoms","multiplicity", /* 23 */
      "column_ppm","column_strain",
      NULL);

    if (parsing) {
      if (parsing[0] && !info->V_0)     info->V_0    =atof(parsing[0]);
      if (parsing[1] && !info->V_0)     info->V_0    =atof(parsing[1]);
      if (parsing[2] && !info->sigma_a) info->sigma_a=atof(parsing[2]);
      if (parsing[3] && !info->sigma_a) info->sigma_a=atof(parsing[3]);
      if (parsing[4] && !info->sigma_i) info->sigma_i=atof(parsing[4]);
      if (parsing[5] && !info->sigma_i) info->sigma_i=atof(parsing[5]);
      if (parsing[6])                   info->column_order[0]=atoi(parsing[6]);
      if (parsing[7])                   info->column_order[1]=atoi(parsing[7]);
      if (parsing[8])                   info->column_order[2]=atoi(parsing[8]);
      if (parsing[9])                   info->column_order[3]=atoi(parsing[9]);
      if (parsing[10])                  info->column_order[4]=atoi(parsing[10]);
      if (parsing[11])                  info->column_order[5]=atoi(parsing[11]);
      if (parsing[12])                  info->column_order[5]=atoi(parsing[12]);
      if (parsing[13])                  info->column_order[5]=atoi(parsing[13]);
      if (parsing[14])                  info->column_order[6]=atoi(parsing[14]);
      if (parsing[15] && info->DWfactor<=0)    info->DWfactor=atof(parsing[15]);
      if (parsing[16] && info->DWfactor<=0)    info->DWfactor=atof(parsing[16]);
      if (parsing[17] && info->Dd <0)          info->Dd      =atof(parsing[17]);
      if (parsing[18])                  info->column_order[7]=atoi(parsing[18]);
      if (parsing[19] && !info->V_0)    info->V_0    =1/atof(parsing[19]);
      if (parsing[20] && !info->rho)    info->rho    =atof(parsing[20]);
      if (parsing[21] && !info->at_weight)     info->at_weight    =atof(parsing[21]);
      if (parsing[22] && info->at_nb <= 1)  info->at_nb    =atof(parsing[22]);
      if (parsing[23] && info->at_nb <= 1)  info->at_nb    =atof(parsing[23]);
      if (parsing[24])                  info->column_order[8]=atoi(parsing[24]);
      if (parsing[25])                  info->column_order[8]=atoi(parsing[25]);
      for (i=0; i<=25; i++) if (parsing[i]) free(parsing[i]);
      free(parsing);
    }

    if (!sTable.rows)
      exit(fprintf(stderr, "PowderN: %s: Error: The number of rows in %s "
       "should be at least %d\n", info->compname, SC_file, 1));
    else
      size = sTable.rows;

    MPI_MASTER(
    Table_Info(sTable);
    printf("PowderN: %s: Reading %d rows from %s\n",
          info->compname, size, SC_file);
    );

    if (info->column_order[0] == 4 && info->flag_barns !=0)
    MPI_MASTER(
      printf("PowderN: %s: Powder file probably of type Crystallographica/Fullprof (lau)\n"
           "WARNING: but F2 unit is set to barns=1 (barns). Intensity might be 100 times too high.\n",
           info->compname);
    );
    if (info->column_order[0] == 17 && info->flag_barns == 0)
    MPI_MASTER(
      printf("PowderN: %s: Powder file probably of type Lazy Pulver (laz)\n"
           "WARNING: but F2 unit is set to barns=0 (fm^2). Intensity might be 100 times too low.\n",
           info->compname);
    );
    /* allocate line_data array */
    list = (struct line_data*)malloc(size*sizeof(struct line_data));

    for (i=0; i<size; i++)
    {
      /*      printf("Reading in line %i\n",i);*/
      double j=0, d=0, w=0, q=0, DWfactor=0, F2=0, Epsilon=0;
      int index;

      if (info->Dd >= 0)      w         = info->Dd;
      if (info->DWfactor > 0) DWfactor  = info->DWfactor;
      if (info->Epsilon)      Epsilon   = info->Epsilon*1e-6;

      /* get data from table using columns {j d F2 DW Dd inv2d q F} */
      /* column indexes start at 1, thus need to substract 1 */
      if (info->column_order[0] >0)
        j = Table_Index(sTable, i, info->column_order[0]-1);
      if (info->column_order[1] >0)
        d = Table_Index(sTable, i, info->column_order[1]-1);
      if (info->column_order[2] >0)
        F2 = Table_Index(sTable, i, info->column_order[2]-1);
      if (info->column_order[3] >0)
        DWfactor = Table_Index(sTable, i, info->column_order[3]-1);
      if (info->column_order[4] >0)
        w = Table_Index(sTable, i, info->column_order[4]-1);
      if (info->column_order[5] >0)
        { d = Table_Index(sTable, i, info->column_order[5]-1);
          d = (d > 0? 1/d/2 : 0); }
      if (info->column_order[6] >0)
        { q = Table_Index(sTable, i, info->column_order[6]-1);
          d = (q > 0 ? 2*PI/q : 0); }
      if (info->column_order[7] >0  && !F2)
        { F2 = Table_Index(sTable, i, info->column_order[7]-1); F2 *= F2; }
      if (info->column_order[8] >0  && !Epsilon)
        { Epsilon = Table_Index(sTable, i, info->column_order[8]-1)*1e-6; }

      /* assign and check values */
      j        = (j > 0 ? j : 0);
      q        = (d > 0 ? 2*PI/d : 0); /* this is q */
      if (Epsilon && fabs(Epsilon) < 1e6) {
        q     -= Epsilon*q; /* dq/q = -delta_d_d/d = -Epsilon */
      }
      DWfactor = (DWfactor > 0 ? DWfactor : 1);
      w        = (w>0 ? w : 0); /* this is q and d relative spreading */
      F2       = (F2 >= 0 ? F2 : 0);
      if (j == 0 || q == 0) {
        MPI_MASTER(
        printf("PowderN: %s: line %i has invalid definition\n"
               "         (mult=0 or q=0 or d=0)\n", info->compname, i);
        );
        continue;
      }
      list[list_count].j = j;
      list[list_count].q = q;
      list[list_count].DWfactor = DWfactor;
      list[list_count].w = w;
      list[list_count].F2= F2;
      list[list_count].Epsilon = Epsilon;

      /* adjust multiplicity if j-column + multiple d-spacing lines */
      /* if  d = previous d, increase line duplication index */
      if (!q_count)      q_count  = q;
      if (!j_count)      j_count  = j;
      if (!F2_count)     F2_count = F2;
      if (fabs(q_count-q) < 0.0001*fabs(q)
       && fabs(F2_count-F2) < 0.0001*fabs(F2) && j_count == j) {
       mult_count++; flag=0; }
      else flag=1;
      if (i == size-1) flag=1;
      /* else if d != previous d : just passed equivalent lines */
      if (flag) {
        if (i == size-1) list_count++;
      /*   if duplication index == previous multiplicity */
      /*      set back multiplicity of previous lines to 1 */
        if ((mult_count && list_count>0)
            && (mult_count == list[list_count-1].j
                || ((list_count < size) && (i == size - 1)
                    && (mult_count == list[list_count].j))) ) {
          MPI_MASTER(
          printf("PowderN: %s: Set multiplicity to 1 for lines [%i:%i]\n"
                  "         (d-spacing %g is duplicated %i times)\n",
            info->compname, list_count-mult_count, list_count-1, list[list_count-1].q, mult_count);
          );
          for (index=list_count-mult_count; index<list_count; list[index++].j = 1);
          mult_count = 1;
          q_count   = q;
          j_count   = j;
          F2_count  = F2;
        }
        if (i == size-1) list_count--;
        flag=0;
      }
      list_count++;
    } /* end for */

    Table_Free(&sTable);

    /* sort the list with increasing q */
    qsort(list, list_count, sizeof(struct line_data),  PN_list_compare);

    MPI_MASTER(
    printf("PowderN: %s: Read %i reflections from file '%s'\n",
      info->compname, list_count, SC_file);
    );

    info->list  = list;
    info->count = list_count;

    return(list_count);
  } /* read_line_data */


/* computes the number of possible reflections (return value), and the total xsection 'sum' */
/* this routine looks for a pre-computed value in the Nq and sum cache tables               */
/* when found, the earch starts from the corresponding lower element in the table           */
#pragma acc routine seq
int calc_xsect(double v, double *qv, double *my_sv2, int count, double *sum,
  struct line_info_struct *line_info) {
  int    Nq = 0, line=0, line0=0;
  *sum=0;

  /* check if a line_info element has been recorded already */
  if (v >= line_info->v_min && v <= line_info->v_max && line_info->neutron_passed >= CHAR_BUF_LENGTH) {
    line = (int)floor(v - line_info->v_min)*CHAR_BUF_LENGTH/(line_info->v_max - line_info->v_min);
    Nq    = line_info->xs_Nq[line];
    *sum  = line_info->xs_sum[line];
    if (!Nq && *sum == 0) {
      /* not yet set: we compute the sum up to the corresponding speed in the table cache */
      double line_v = line_info->v_min + line*(line_info->v_max - line_info->v_min)/CHAR_BUF_LENGTH;
      for(line0=0; line0<count; line0++) {
        if (qv[line0] <= 2*line_v) { /* q < 2*kf: restrict structural range */
          *sum += my_sv2[line0];
          if (Nq < line0+1) Nq=line0+1; /* determine maximum line index which can scatter */
        } else break;
      }
      line_info->xs_Nq[line] = Nq;
      line_info->xs_sum[line]= *sum;
      line_info->xs_compute++;
    } else line_info->xs_reuse++;
    line0 = Nq - 1;
  }

  line_info->xs_calls++;

  for(line=line0; line<count; line++) {
    if (qv[line] <= 2*v) { /* q < 2*kf: restrict structural range */
      *sum += my_sv2[line];
      if (Nq < line+1) Nq=line+1; /* determine maximum line index which can scatter */
    } else break;
  }

  return(Nq);
} /* calc_xsect */

#endif /* !POWDERN_DECL */


/* Shared user declarations for all components types 'Monitor_nD'. */
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
* Version: $Revision$
*
* This file is to be imported by the monitor_nd related components
* It handles some shared functions.
*
* Usage: within SHARE
* %include "monitor_nd-lib"
*
*******************************************************************************/

#ifndef MONITOR_ND_LIB_H

#define MONITOR_ND_LIB_H "$Revision$"
#define MONnD_COORD_NMAX  30  /* max number of variables to record */

  typedef struct MonitornD_Defines
  {
    int COORD_NONE  ;
    int COORD_X     ;
    int COORD_Y     ;
    int COORD_Z     ;
    int COORD_RADIUS; 
    int COORD_VX    ;
    int COORD_VY    ;
    int COORD_VZ    ;
    int COORD_V     ;
    int COORD_T     ;
    int COORD_P     ;
    int COORD_SX    ;
    int COORD_SY    ;
    int COORD_SZ    ;
    int COORD_KX    ;
    int COORD_KY    ;
    int COORD_KZ    ;
    int COORD_K     ;
    int COORD_ENERGY;
    int COORD_LAMBDA;
    int COORD_KXY   ;
    int COORD_KYZ   ;
    int COORD_KXZ   ;
    int COORD_VXY   ;
    int COORD_VYZ   ;
    int COORD_VXZ   ;
    int COORD_HDIV  ;
    int COORD_VDIV  ;
    int COORD_ANGLE ;
    int COORD_NCOUNT;
    int COORD_THETA ;
    int COORD_PHI   ;
    int COORD_USER1 ;
    int COORD_USER2 ;
    int COORD_USER3 ;
    int COORD_XY    ;
    int COORD_XZ    ;
    int COORD_YZ    ;
    int COORD_PIXELID;

    /* token modifiers */
    int COORD_VAR   ; /* next token should be a variable or normal option */
    int COORD_MIN   ; /* next token is a min value */
    int COORD_MAX   ; /* next token is a max value */
    int COORD_DIM   ; /* next token is a bin value */
    int COORD_FIL   ; /* next token is a filename */
    int COORD_EVNT  ; /* next token is a buffer size value */
    int COORD_3HE   ; /* next token is a 3He pressure value */
    int COORD_LOG   ; /* next variable will be in log scale */
    int COORD_ABS   ; /* next variable will be in abs scale */
    int COORD_SIGNAL; /* next variable will be the signal var */
    int COORD_AUTO  ; /* set auto limits */

    char TOKEN_DEL[32]; /* token separators */

    char SHAPE_SQUARE; /* shape of the monitor */
    char SHAPE_DISK  ;
    char SHAPE_SPHERE;
    char SHAPE_CYLIND;
    char SHAPE_BANANA; /* cylinder without top/bottom, on restricted angular area */
    char SHAPE_BOX   ;
    char SHAPE_PREVIOUS;
    char SHAPE_OFF;

  } MonitornD_Defines_type;

  typedef struct MonitornD_Variables
  {
    double area;
    double Sphere_Radius     ;
    double Cylinder_Height   ;
    char   Flag_With_Borders ;   /* 2 means xy borders too */
    char   Flag_List         ;   /* 1 store 1 buffer, 2 is list all, 3 list all+append */
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
    int    Flag_signal       ;   /* 0:monitor p, else monitor a mean value */
    int    Flag_mantid       ;   /* 0:normal monitor, else do mantid-event specifics */
    int    Flag_OFF          ;   /* Flag to indicate external geometry from OFF file */
    unsigned long OFF_polyidx;   /* When intersection is done externally by off_intersect, this gives the 
				    polygon number, i.e. pixel index */

    unsigned long Coord_Number      ;   /* total number of variables to monitor, plus intensity (0) */
    unsigned long Coord_NumberNoPixel;  /* same but without counting PixelID */
    unsigned long Buffer_Block      ;   /* Buffer size for list or auto limits */
    unsigned long Neutron_Counter   ;   /* event counter, simulation total counts is mcget_ncount() */
    unsigned long Buffer_Counter    ;   /* index in Buffer size (for realloc) */
    unsigned long Buffer_Size       ;
    int    Coord_Type[MONnD_COORD_NMAX];      /* type of variable */
    char   Coord_Label[MONnD_COORD_NMAX][30]; /* label of variable */
    char   Coord_Var[MONnD_COORD_NMAX][30];   /* short id of variable */
    long   Coord_Bin[MONnD_COORD_NMAX];       /* bins of variable array */
    long   Coord_BinProd[MONnD_COORD_NMAX];   /* product of bins of variable array */
    double Coord_Min[MONnD_COORD_NMAX];
    double Coord_Max[MONnD_COORD_NMAX];
    char   Monitor_Label[MONnD_COORD_NMAX*30];/* Label for monitor */
    char   Mon_File[128];                     /* output file name */

    double cx,cy,cz;
    double cvx, cvy, cvz;
    double ckx, cky, ckz;
    double csx, csy, csz;
    double cEx, cEy, cEz;
    double cs1, cs2, ct, cphi, cp;
    double He3_pressure;
    char   Flag_UsePreMonitor    ;   /* use a previously stored neutron parameter set */
    char   UserName1[128];
    char   UserName2[128];
    char   UserName3[128];
    double UserVariable1;
    double UserVariable2;
    double UserVariable3;
    char   option[CHAR_BUF_LENGTH];

    long long int Nsum;
    double psum, p2sum;
    double **Mon2D_N;
    double **Mon2D_p;
    double **Mon2D_p2;
    double *Mon2D_Buffer;
    unsigned long PixelID;

    double mxmin,mxmax,mymin,mymax,mzmin,mzmax;
    double mean_dx, mean_dy, min_x, min_y, max_x, max_y, mean_p;

    char   compcurname[128];
    Coords compcurpos;

  } MonitornD_Variables_type;

/* monitor_nd-lib function prototypes */
/* ========================================================================= */

void Monitor_nD_Init(MonitornD_Defines_type *, MonitornD_Variables_type *, MCNUM, MCNUM, MCNUM, MCNUM, MCNUM, MCNUM, MCNUM, MCNUM, MCNUM, int);
int Monitor_nD_Trace(MonitornD_Defines_type *, MonitornD_Variables_type *, _class_particle* _particle);
MCDETECTOR Monitor_nD_Save(MonitornD_Defines_type *, MonitornD_Variables_type *);
void Monitor_nD_Finally(MonitornD_Defines_type *, MonitornD_Variables_type *);
void Monitor_nD_McDisplay(MonitornD_Defines_type *,
 MonitornD_Variables_type *);
 
#endif

/* end of monitor_nd-lib.h */
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
* Version: $Revision$
*
* This file is to be imported by the monitor_nd related components
* It handles some shared functions. Embedded within instrument in runtime mode.
*
* Usage: within SHARE
* %include "monitor_nd-lib"
*
*******************************************************************************/

#ifndef MONITOR_ND_LIB_H
#error McStas : please import this library with %include "monitor_nd-lib"
#endif

/* ========================================================================= */
/* Monitor_nD_Init: this routine is used to parse options                    */
/* ========================================================================= */

void Monitor_nD_Init(MonitornD_Defines_type *DEFS,
  MonitornD_Variables_type *Vars,
  MCNUM xwidth,
  MCNUM yheight,
  MCNUM zdepth,
  MCNUM xmin,
  MCNUM xmax,
  MCNUM ymin,
  MCNUM ymax,
  MCNUM zmin,
  MCNUM zmax,
  int offflag)
  {
    long carg = 1;
    char *option_copy, *token;
    char Flag_New_token = 1;
    char Flag_End       = 1;
    char Flag_All       = 0;
    char Flag_No        = 0;
    char Flag_abs       = 0;
    int  Flag_auto      = 0;  /* -1: all, 1: the current variable */
    int  Set_Vars_Coord_Type;
    char Set_Vars_Coord_Label[64];
    char Set_Vars_Coord_Var[64];
    char Short_Label[MONnD_COORD_NMAX][64];
    int  Set_Coord_Mode;
    long i=0, j=0;
    double lmin, lmax, XY=0;
    long t;


    t = (long)time(NULL);

/* initialize DEFS */
/* Variables to monitor */
    DEFS->COORD_NONE   =0;
    DEFS->COORD_X      =1;
    DEFS->COORD_Y      =2;
    DEFS->COORD_Z      =3;
    DEFS->COORD_RADIUS =19;
    DEFS->COORD_VX     =4;
    DEFS->COORD_VY     =5;
    DEFS->COORD_VZ     =6;
    DEFS->COORD_V      =16;
    DEFS->COORD_T      =7;
    DEFS->COORD_P      =8;
    DEFS->COORD_SX     =9;
    DEFS->COORD_SY     =10;
    DEFS->COORD_SZ     =11;
    DEFS->COORD_KX     =12;
    DEFS->COORD_KY     =13;
    DEFS->COORD_KZ     =14;
    DEFS->COORD_K      =15;
    DEFS->COORD_ENERGY =17;
    DEFS->COORD_LAMBDA =18;
    DEFS->COORD_HDIV   =20;
    DEFS->COORD_VDIV   =21;
    DEFS->COORD_ANGLE  =22;
    DEFS->COORD_NCOUNT =23;
    DEFS->COORD_THETA  =24;
    DEFS->COORD_PHI    =25;
    DEFS->COORD_USER1  =26;
    DEFS->COORD_USER2  =27;
    DEFS->COORD_USER3  =28;
    DEFS->COORD_XY     =37;
    DEFS->COORD_YZ     =31;
    DEFS->COORD_XZ     =32;
    DEFS->COORD_VXY    =30;
    DEFS->COORD_VYZ    =34;
    DEFS->COORD_VXZ    =36;
    DEFS->COORD_KXY    =29;
    DEFS->COORD_KYZ    =33;
    DEFS->COORD_KXZ    =35;
    DEFS->COORD_PIXELID=38;

/* token modifiers */
    DEFS->COORD_VAR    =0;    /* next token should be a variable or normal option */
    DEFS->COORD_MIN    =1;    /* next token is a min value */
    DEFS->COORD_MAX    =2;    /* next token is a max value */
    DEFS->COORD_DIM    =3;    /* next token is a bin value */
    DEFS->COORD_FIL    =4;    /* next token is a filename */
    DEFS->COORD_EVNT   =5;    /* next token is a buffer size value */
    DEFS->COORD_3HE    =6;    /* next token is a 3He pressure value */
    DEFS->COORD_LOG    =64;   /* next variable will be in log scale */
    DEFS->COORD_ABS    =128;  /* next variable will be in abs scale */
    DEFS->COORD_SIGNAL =256;  /* next variable will be the signal var */
    DEFS->COORD_AUTO   =512;  /* set auto limits */

    strcpy(DEFS->TOKEN_DEL, " =,;[](){}:");  /* token separators */

    DEFS->SHAPE_SQUARE =0;    /* shape of the monitor */
    DEFS->SHAPE_DISK   =1;
    DEFS->SHAPE_SPHERE =2;
    DEFS->SHAPE_CYLIND =3;
    DEFS->SHAPE_BANANA =4;
    DEFS->SHAPE_BOX    =5;
    DEFS->SHAPE_PREVIOUS=6;
    DEFS->SHAPE_OFF=7;

    Vars->Sphere_Radius     = 0;
    Vars->Cylinder_Height   = 0;
    Vars->Flag_With_Borders = 0;   /* 2 means xy borders too */
    Vars->Flag_List         = 0;   /* 1=store 1 buffer, 2=list all, 3=re-use buffer */
    Vars->Flag_Multiple     = 0;   /* 1 when n1D, 0 for 2D */
    Vars->Flag_Verbose      = 0;
    Vars->Flag_Shape        = DEFS->SHAPE_SQUARE;
    Vars->Flag_Auto_Limits  = 0;   /* get limits from first Buffer */
    Vars->Flag_Absorb       = 0;   /* monitor is also a slit */
    Vars->Flag_per_cm2      = 0;   /* flux is per cm2 */
    Vars->Flag_log          = 0;   /* log10 of the flux */
    Vars->Flag_parallel     = 0;   /* set neutron state back after detection (parallel components) */
    Vars->Flag_Binary_List  = 0;   /* save list as a binary file (smaller) */
    Vars->Coord_Number      = 0;   /* total number of variables to monitor, plus intensity (0) */
    Vars->Coord_NumberNoPixel=0;   /* same but without counting PixelID */

/* Allow to specify size of Monitor_nD buffer via a define*/
#ifndef MONND_BUFSIZ
    Vars->Buffer_Block      = 100000;     /* Buffer size for list or auto limits */
#else
	Vars->Buffer_Block      = MONND_BUFSIZ;     /* Buffer size for list or auto limits */	
#endif
    Vars->Neutron_Counter   = 0;   /* event counter, simulation total counts is mcget_ncount() */
    Vars->Buffer_Counter    = 0;   /* index in Buffer size (for realloc) */
    Vars->Buffer_Size       = 0;
    Vars->UserVariable1     = 0;
    Vars->UserVariable2     = 0;
    Vars->He3_pressure      = 0;
    Vars->Flag_capture      = 0;
    Vars->Flag_signal       = DEFS->COORD_P;
    Vars->Flag_mantid       = 0;
    Vars->Flag_OFF          = offflag;
    Vars->OFF_polyidx       = -1;
    Vars->mean_dx=Vars->mean_dy=0;
    Vars->min_x = Vars->max_x  =0;
    Vars->min_y = Vars->max_y  =0;

    Set_Vars_Coord_Type = DEFS->COORD_NONE;
    Set_Coord_Mode = DEFS->COORD_VAR;

    /* handle size parameters */
    /* normal use is with xwidth, yheight, zdepth */
    /* if xmin,xmax,ymin,ymax,zmin,zmax are non 0, use them */
    if (fabs(xmin-xmax) == 0)
      { Vars->mxmin = -fabs(xwidth)/2; Vars->mxmax = fabs(xwidth)/2; }
    else
      { if (xmin < xmax) {Vars->mxmin = xmin; Vars->mxmax = xmax;}
        else {Vars->mxmin = xmax; Vars->mxmax = xmin;}
      }
    if (fabs(ymin-ymax) == 0)
      { Vars->mymin = -fabs(yheight)/2; Vars->mymax = fabs(yheight)/2; }
    else
      { if (ymin < ymax) {Vars->mymin = ymin; Vars->mymax = ymax;}
        else {Vars->mymin = ymax; Vars->mymax = ymin;}
      }
    if (fabs(zmin-zmax) == 0)
      { Vars->mzmin = -fabs(zdepth)/2; Vars->mzmax = fabs(zdepth)/2; }
    else
      { if (zmin < zmax) {Vars->mzmin = zmin; Vars->mzmax = zmax; }
        else {Vars->mzmin = zmax; Vars->mzmax = zmin; }
      }

    if (fabs(Vars->mzmax-Vars->mzmin) == 0)
      Vars->Flag_Shape        = DEFS->SHAPE_SQUARE;
    else
      Vars->Flag_Shape        = DEFS->SHAPE_BOX;

    if (Vars->Flag_OFF) {
      Vars->Flag_Shape        = DEFS->SHAPE_OFF;
    }
    
    /* parse option string */

    option_copy = (char*)malloc(strlen(Vars->option)+1);
    if (option_copy == NULL)
    {
      fprintf(stderr,"Monitor_nD: %s cannot allocate 'options' copy (%li). Fatal.\n", Vars->compcurname, (long)strlen(Vars->option));
      exit(-1);
    }

    if (strlen(Vars->option))
    {
      Flag_End = 0;
      strcpy(option_copy, Vars->option);
    }

    if (strstr(Vars->option, "cm2") || strstr(Vars->option, "cm^2")) Vars->Flag_per_cm2 = 1;

    if (strstr(Vars->option, "binary") || strstr(Vars->option, "float"))
      Vars->Flag_Binary_List  = 1;
    if (strstr(Vars->option, "double"))
      Vars->Flag_Binary_List  = 2;

    strcpy(Vars->Coord_Label[0],"Intensity");
    strncpy(Vars->Coord_Var[0],"p",30);
    Vars->Coord_Type[0] = DEFS->COORD_P;
    Vars->Coord_Bin[0] = 1;
    Vars->Coord_Min[0] = 0;
    Vars->Coord_Max[0] = FLT_MAX;

    /* default file name is comp_name+dateID */
    sprintf(Vars->Mon_File, "%s_%li", Vars->compcurname, t);

    carg = 1;
    while((Flag_End == 0) && (carg < 128))
    {

      if (Flag_New_token) /* retain previous token or get a new one */
      {
        if (carg == 1) token=(char *)strtok(option_copy,DEFS->TOKEN_DEL);
        else token=(char *)strtok(NULL,DEFS->TOKEN_DEL);
        if (token == NULL) Flag_End=1;
      }
      Flag_New_token = 1;
      if ((token != NULL) && (strlen(token) != 0))
      {
        char iskeyword=0; /* left at 0 when variables are processed, 1 for modifiers */
        int  old_Mode;
        /* change token to lower case */
        for (i=0; i<strlen(token); i++) token[i]=tolower(token[i]);
        /* first handle option values from preceeding keyword token detected */
        old_Mode = Set_Coord_Mode;
        if (Set_Coord_Mode == DEFS->COORD_MAX)  /* max=%i */
        {
          if (!Flag_All)
            Vars->Coord_Max[Vars->Coord_Number] = atof(token);
          else
            for (i = 0; i <= Vars->Coord_Number; Vars->Coord_Max[i++] = atof(token));
          Set_Coord_Mode = DEFS->COORD_VAR; Flag_All = 0;
        }
        if (Set_Coord_Mode == DEFS->COORD_MIN)  /* min=%i */
        {
          if (!Flag_All)
            Vars->Coord_Min[Vars->Coord_Number] = atof(token);
          else
            for (i = 0; i <= Vars->Coord_Number; Vars->Coord_Min[i++] = atof(token));
          Set_Coord_Mode = DEFS->COORD_MAX;
        }
        if (Set_Coord_Mode == DEFS->COORD_DIM)  /* bins=%i */
        {
          if (!Flag_All)
            Vars->Coord_Bin[Vars->Coord_Number] = atoi(token);
          else
            for (i = 0; i <= Vars->Coord_Number; Vars->Coord_Bin[i++] = atoi(token));
          Set_Coord_Mode = DEFS->COORD_VAR; Flag_All = 0;
        }
        if (Set_Coord_Mode == DEFS->COORD_FIL)  /* file=%s */
        {
          if (!Flag_No) strncpy(Vars->Mon_File,token,128);
          else { strcpy(Vars->Mon_File,""); Vars->Coord_Number = 0; Flag_End = 1;}
          Set_Coord_Mode = DEFS->COORD_VAR;
        }
        if (Set_Coord_Mode == DEFS->COORD_EVNT) /* list=%i */
        {
          if (!strcmp(token, "all") || Flag_All) Vars->Flag_List = 2;
          else { i = (long)ceil(atof(token)); if (i) Vars->Buffer_Block = i;
            Vars->Flag_List = 1; }
          Set_Coord_Mode = DEFS->COORD_VAR; Flag_All = 0;
        }
        if (Set_Coord_Mode == DEFS->COORD_3HE)  /* pressure=%g */
        {
            Vars->He3_pressure = atof(token);
            Set_Coord_Mode = DEFS->COORD_VAR; Flag_All = 0;
        }

        /* now look for general option keywords */
        if (!strcmp(token, "borders"))  {Vars->Flag_With_Borders = 1; iskeyword=1; }
        if (!strcmp(token, "verbose"))  {Vars->Flag_Verbose      = 1; iskeyword=1; }
        if (!strcmp(token, "log"))      {Vars->Flag_log          = 1; iskeyword=1; }
        if (!strcmp(token, "abs"))      {Flag_abs                = 1; iskeyword=1; }
        if (!strcmp(token, "multiple")) {Vars->Flag_Multiple     = 1; iskeyword=1; }
        if (!strcmp(token, "list") || !strcmp(token, "events")) {
          Vars->Flag_List = 1; Set_Coord_Mode = DEFS->COORD_EVNT;  }
        if (!strcmp(token, "limits") || !strcmp(token, "min"))
          Set_Coord_Mode = DEFS->COORD_MIN;
        if (!strcmp(token, "slit") || !strcmp(token, "absorb")) {
          Vars->Flag_Absorb = 1;  iskeyword=1; }
        if (!strcmp(token, "max"))  Set_Coord_Mode = DEFS->COORD_MAX;
        if (!strcmp(token, "bins") || !strcmp(token, "dim")) Set_Coord_Mode = DEFS->COORD_DIM;
        if (!strcmp(token, "file") || !strcmp(token, "filename")) {
          Set_Coord_Mode = DEFS->COORD_FIL;
          if (Flag_No) { strcpy(Vars->Mon_File,""); Vars->Coord_Number = 0; Flag_End = 1; }
        }
        if (!strcmp(token, "unactivate")) {
          Flag_End = 1; Vars->Coord_Number = 0; iskeyword=1; }
        if (!strcmp(token, "all"))    { Flag_All = 1;  iskeyword=1; }
        if (!strcmp(token, "sphere")) { Vars->Flag_Shape = DEFS->SHAPE_SPHERE; iskeyword=1; }
        if (!strcmp(token, "cylinder")) { Vars->Flag_Shape = DEFS->SHAPE_CYLIND; iskeyword=1; }
        if (!strcmp(token, "banana")) { Vars->Flag_Shape = DEFS->SHAPE_BANANA; iskeyword=1; }
        if (!strcmp(token, "square")) { Vars->Flag_Shape = DEFS->SHAPE_SQUARE; iskeyword=1; }
        if (!strcmp(token, "disk"))   { Vars->Flag_Shape = DEFS->SHAPE_DISK; iskeyword=1; }
        if (!strcmp(token, "box"))     { Vars->Flag_Shape = DEFS->SHAPE_BOX; iskeyword=1; }
        if (!strcmp(token, "previous")) { Vars->Flag_Shape = DEFS->SHAPE_PREVIOUS; iskeyword=1; }
        if (!strcmp(token, "parallel")){ Vars->Flag_parallel = 1; iskeyword=1; }
        if (!strcmp(token, "capture")) { Vars->Flag_capture = 1; iskeyword=1; }
        if (!strcmp(token, "auto") && (Flag_auto != -1)) {
          Vars->Flag_Auto_Limits = 1;
          if (Flag_All) Flag_auto = -1;
          else          Flag_auto = 1;
          iskeyword=1; Flag_All=0; }
        if (!strcmp(token, "premonitor")) {
          Vars->Flag_UsePreMonitor = 1; iskeyword=1; }
        if (!strcmp(token, "3He_pressure") || !strcmp(token, "pressure")) {
          Vars->He3_pressure = 3; iskeyword=1; }
        if (!strcmp(token, "no") || !strcmp(token, "not")) { Flag_No = 1;  iskeyword=1; }
        if (!strcmp(token, "signal")) Set_Coord_Mode = DEFS->COORD_SIGNAL;
        if (!strcmp(token, "mantid")) { Vars->Flag_mantid = 1; iskeyword=1; }

        /* Mode has changed: this was a keyword or value  ? */
        if (Set_Coord_Mode != old_Mode) iskeyword=1;

        /* now look for variable names to monitor */
        Set_Vars_Coord_Type = DEFS->COORD_NONE; lmin = 0; lmax = 0;

        if (!strcmp(token, "x"))
          { Set_Vars_Coord_Type = DEFS->COORD_X; strcpy(Set_Vars_Coord_Label,"x [m]"); strcpy(Set_Vars_Coord_Var,"x");
          lmin = Vars->mxmin; lmax = Vars->mxmax;
          Vars->Coord_Min[Vars->Coord_Number+1] = Vars->mxmin;
          Vars->Coord_Max[Vars->Coord_Number+1] = Vars->mxmax;}
        if (!strcmp(token, "y"))
          { Set_Vars_Coord_Type = DEFS->COORD_Y; strcpy(Set_Vars_Coord_Label,"y [m]"); strcpy(Set_Vars_Coord_Var,"y");
          lmin = Vars->mymin; lmax = Vars->mymax;
          Vars->Coord_Min[Vars->Coord_Number+1] = Vars->mymin;
          Vars->Coord_Max[Vars->Coord_Number+1] = Vars->mymax;}
        if (!strcmp(token, "z"))
          { Set_Vars_Coord_Type = DEFS->COORD_Z; strcpy(Set_Vars_Coord_Label,"z [m]"); strcpy(Set_Vars_Coord_Var,"z"); lmin = Vars->mzmin; lmax = Vars->mzmax; }
        if (!strcmp(token, "k") || !strcmp(token, "wavevector"))
          { Set_Vars_Coord_Type = DEFS->COORD_K; strcpy(Set_Vars_Coord_Label,"|k| [Angs-1]"); strcpy(Set_Vars_Coord_Var,"k"); lmin = 0; lmax = 10; }
        if (!strcmp(token, "v"))
          { Set_Vars_Coord_Type = DEFS->COORD_V; strcpy(Set_Vars_Coord_Label,"Velocity [m/s]"); strcpy(Set_Vars_Coord_Var,"v"); lmin = 0; lmax = 10000; }
        if (!strcmp(token, "t") || !strcmp(token, "time") || !strcmp(token, "tof"))
          { Set_Vars_Coord_Type = DEFS->COORD_T; strcpy(Set_Vars_Coord_Label,"TOF [s]"); strcpy(Set_Vars_Coord_Var,"t"); lmin = 0; lmax = .1; }
        if ((!strcmp(token, "p") || !strcmp(token, "i") || !strcmp(token, "intensity") || !strcmp(token, "flux")))
          { Set_Vars_Coord_Type = DEFS->COORD_P;
            strcpy(Set_Vars_Coord_Label,"Intensity");
            strncat(Set_Vars_Coord_Label, " [n/s", 30);
            if (Vars->Flag_per_cm2) strncat(Set_Vars_Coord_Label, "/cm2", 30);
            if (XY > 1 && Vars->Coord_Number)
              strncat(Set_Vars_Coord_Label, "/bin", 30);
            strncat(Set_Vars_Coord_Label, "]", 30);
            strcpy(Set_Vars_Coord_Var,"I");
            lmin = 0; lmax = FLT_MAX;
            if (Flag_auto>0) Flag_auto=0;
          }

        if (!strcmp(token, "vx"))
          { Set_Vars_Coord_Type = DEFS->COORD_VX; strcpy(Set_Vars_Coord_Label,"vx [m/s]"); strcpy(Set_Vars_Coord_Var,"vx"); lmin = -1000; lmax = 1000; }
        if (!strcmp(token, "vy"))
          { Set_Vars_Coord_Type = DEFS->COORD_VY; strcpy(Set_Vars_Coord_Label,"vy [m/s]"); strcpy(Set_Vars_Coord_Var,"vy"); lmin = -1000; lmax = 1000; }
        if (!strcmp(token, "vz"))
          { Set_Vars_Coord_Type = DEFS->COORD_VZ; strcpy(Set_Vars_Coord_Label,"vz [m/s]"); strcpy(Set_Vars_Coord_Var,"vz"); lmin = -10000; lmax = 10000; }
        if (!strcmp(token, "kx"))
          { Set_Vars_Coord_Type = DEFS->COORD_KX; strcpy(Set_Vars_Coord_Label,"kx [Angs-1]"); strcpy(Set_Vars_Coord_Var,"kx"); lmin = -1; lmax = 1; }
        if (!strcmp(token, "ky"))
          { Set_Vars_Coord_Type = DEFS->COORD_KY; strcpy(Set_Vars_Coord_Label,"ky [Angs-1]"); strcpy(Set_Vars_Coord_Var,"ky"); lmin = -1; lmax = 1; }
        if (!strcmp(token, "kz"))
          { Set_Vars_Coord_Type = DEFS->COORD_KZ; strcpy(Set_Vars_Coord_Label,"kz [Angs-1]"); strcpy(Set_Vars_Coord_Var,"kz"); lmin = -10; lmax = 10; }
        if (!strcmp(token, "sx"))
          { Set_Vars_Coord_Type = DEFS->COORD_SX; strcpy(Set_Vars_Coord_Label,"sx [1]"); strcpy(Set_Vars_Coord_Var,"sx"); lmin = -1; lmax = 1; }
        if (!strcmp(token, "sy"))
          { Set_Vars_Coord_Type = DEFS->COORD_SY; strcpy(Set_Vars_Coord_Label,"sy [1]"); strcpy(Set_Vars_Coord_Var,"sy"); lmin = -1; lmax = 1; }
        if (!strcmp(token, "sz"))
          { Set_Vars_Coord_Type = DEFS->COORD_SZ; strcpy(Set_Vars_Coord_Label,"sz [1]"); strcpy(Set_Vars_Coord_Var,"sz"); lmin = -1; lmax = 1; }

        if (!strcmp(token, "energy") || !strcmp(token, "omega") || !strcmp(token, "e"))
          { Set_Vars_Coord_Type = DEFS->COORD_ENERGY; strcpy(Set_Vars_Coord_Label,"Energy [meV]"); strcpy(Set_Vars_Coord_Var,"E"); lmin = 0; lmax = 100; }
        if (!strcmp(token, "lambda") || !strcmp(token, "wavelength") || !strcmp(token, "l"))
          { Set_Vars_Coord_Type = DEFS->COORD_LAMBDA; strcpy(Set_Vars_Coord_Label,"Wavelength [Angs]"); strcpy(Set_Vars_Coord_Var,"L"); lmin = 0; lmax = 100; }
        if (!strcmp(token, "radius") || !strcmp(token, "r"))
          { Set_Vars_Coord_Type = DEFS->COORD_RADIUS; strcpy(Set_Vars_Coord_Label,"Radius [m]"); strcpy(Set_Vars_Coord_Var,"xy"); lmin = 0; lmax = xmax; }
        if (!strcmp(token, "xy"))
          { Set_Vars_Coord_Type = DEFS->COORD_XY; strcpy(Set_Vars_Coord_Label,"Radius (xy) [m]"); strcpy(Set_Vars_Coord_Var,"xy"); lmin = 0; lmax = xmax; }
        if (!strcmp(token, "yz"))
          { Set_Vars_Coord_Type = DEFS->COORD_YZ; strcpy(Set_Vars_Coord_Label,"Radius (yz) [m]"); strcpy(Set_Vars_Coord_Var,"yz"); lmin = 0; lmax = xmax; }
        if (!strcmp(token, "xz"))
          { Set_Vars_Coord_Type = DEFS->COORD_XZ; strcpy(Set_Vars_Coord_Label,"Radius (xz) [m]"); strcpy(Set_Vars_Coord_Var,"xz"); lmin = 0; lmax = xmax; }
        if (!strcmp(token, "vxy"))
          { Set_Vars_Coord_Type = DEFS->COORD_VXY; strcpy(Set_Vars_Coord_Label,"Radial Velocity (xy) [m]"); strcpy(Set_Vars_Coord_Var,"Vxy"); lmin = 0; lmax = 2000; }
        if (!strcmp(token, "kxy"))
          { Set_Vars_Coord_Type = DEFS->COORD_KXY; strcpy(Set_Vars_Coord_Label,"Radial Wavevector (xy) [Angs-1]"); strcpy(Set_Vars_Coord_Var,"Kxy"); lmin = 0; lmax = 2; }
        if (!strcmp(token, "vyz"))
          { Set_Vars_Coord_Type = DEFS->COORD_VYZ; strcpy(Set_Vars_Coord_Label,"Radial Velocity (yz) [m]"); strcpy(Set_Vars_Coord_Var,"Vyz"); lmin = 0; lmax = 2000; }
        if (!strcmp(token, "kyz"))
          { Set_Vars_Coord_Type = DEFS->COORD_KYZ; strcpy(Set_Vars_Coord_Label,"Radial Wavevector (yz) [Angs-1]"); strcpy(Set_Vars_Coord_Var,"Kyz"); lmin = 0; lmax = 2; }
        if (!strcmp(token, "vxz"))
          { Set_Vars_Coord_Type = DEFS->COORD_VXZ; strcpy(Set_Vars_Coord_Label,"Radial Velocity (xz) [m]"); strcpy(Set_Vars_Coord_Var,"Vxz"); lmin = 0; lmax = 2000; }
        if (!strcmp(token, "kxz"))
          { Set_Vars_Coord_Type = DEFS->COORD_KXZ; strcpy(Set_Vars_Coord_Label,"Radial Wavevector (xz) [Angs-1]"); strcpy(Set_Vars_Coord_Var,"Kxz"); lmin = 0; lmax = 2; }
        if (!strcmp(token, "angle") || !strcmp(token, "a"))
          { Set_Vars_Coord_Type = DEFS->COORD_ANGLE; strcpy(Set_Vars_Coord_Label,"Angle [deg]"); strcpy(Set_Vars_Coord_Var,"A"); lmin = -50; lmax = 50; }
        if (!strcmp(token, "hdiv")|| !strcmp(token, "divergence") || !strcmp(token, "xdiv") || !strcmp(token, "hd") || !strcmp(token, "dx"))
          { Set_Vars_Coord_Type = DEFS->COORD_HDIV; strcpy(Set_Vars_Coord_Label,"Hor. Divergence [deg]"); strcpy(Set_Vars_Coord_Var,"hd"); lmin = -5; lmax = 5; }
        if (!strcmp(token, "vdiv") || !strcmp(token, "ydiv") || !strcmp(token, "vd") || !strcmp(token, "dy"))
          { Set_Vars_Coord_Type = DEFS->COORD_VDIV; strcpy(Set_Vars_Coord_Label,"Vert. Divergence [deg]"); strcpy(Set_Vars_Coord_Var,"vd"); lmin = -5; lmax = 5; }
        if (!strcmp(token, "theta") || !strcmp(token, "longitude") || !strcmp(token, "th"))
          { Set_Vars_Coord_Type = DEFS->COORD_THETA; strcpy(Set_Vars_Coord_Label,"Longitude [deg]"); strcpy(Set_Vars_Coord_Var,"th"); lmin = -180; lmax = 180; }
        if (!strcmp(token, "phi") || !strcmp(token, "lattitude") || !strcmp(token, "ph"))
          { Set_Vars_Coord_Type = DEFS->COORD_PHI; strcpy(Set_Vars_Coord_Label,"Lattitude [deg]"); strcpy(Set_Vars_Coord_Var,"ph"); lmin = -180; lmax = 180; }
        if (!strcmp(token, "ncounts") || !strcmp(token, "n") || !strcmp(token, "neutron"))
          { Set_Vars_Coord_Type = DEFS->COORD_NCOUNT; strcpy(Set_Vars_Coord_Label,"Neutron ID [1]"); strcpy(Set_Vars_Coord_Var,"n"); lmin = 0; lmax = mcget_ncount(); if (Flag_auto>0) Flag_auto=0; }
        if (!strcmp(token, "id") || !strcmp(token, "pixel"))
          { Set_Vars_Coord_Type = DEFS->COORD_PIXELID; 
            strcpy(Set_Vars_Coord_Label,"Pixel ID [1]"); 
            strcpy(Set_Vars_Coord_Var,"id"); lmin = 0; lmax = FLT_MAX; 
            if (Flag_auto>0) Flag_auto=0;
            Vars->Flag_List = 1; }
        if (!strcmp(token, "user") || !strcmp(token, "user1") || !strcmp(token, "u1"))
          { Set_Vars_Coord_Type = DEFS->COORD_USER1; strncpy(Set_Vars_Coord_Label,Vars->UserName1,30); strcpy(Set_Vars_Coord_Var,"U1"); lmin = -1e10; lmax = 1e10; }
        if (!strcmp(token, "user2") || !strcmp(token, "u2"))
          { Set_Vars_Coord_Type = DEFS->COORD_USER2; strncpy(Set_Vars_Coord_Label,Vars->UserName2,30); strcpy(Set_Vars_Coord_Var,"U2"); lmin = -1e10; lmax = 1e10; }
        if (!strcmp(token, "user3") || !strcmp(token, "u3"))
          { Set_Vars_Coord_Type = DEFS->COORD_USER3; strncpy(Set_Vars_Coord_Label,Vars->UserName3,30); strcpy(Set_Vars_Coord_Var,"U3"); lmin = -1e10; lmax = 1e10; }

        /* now stores variable keywords detected, if any */
        if (Set_Vars_Coord_Type != DEFS->COORD_NONE)
        {
          int Coord_Number = Vars->Coord_Number;
          if (Vars->Flag_log) { Set_Vars_Coord_Type |= DEFS->COORD_LOG; Vars->Flag_log = 0; }
          if (Flag_abs) { Set_Vars_Coord_Type |= DEFS->COORD_ABS; Flag_abs = 0; }
          if (Flag_auto != 0) { Set_Vars_Coord_Type |= DEFS->COORD_AUTO; 
            if (Flag_auto > 0) Flag_auto = 0; }
          if (Set_Coord_Mode == DEFS->COORD_SIGNAL)
          {
            Coord_Number = 0;
            Vars->Flag_signal = Set_Vars_Coord_Type;
          }
          else
          {
            if (Coord_Number < MONnD_COORD_NMAX)
            { Coord_Number++;
              Vars->Coord_Number = Coord_Number; 
              if (Set_Vars_Coord_Type != DEFS->COORD_PIXELID)
                Vars->Coord_NumberNoPixel++;
            }
            else if (Vars->Flag_Verbose) printf("Monitor_nD: %s reached max number of variables (%i).\n", Vars->compcurname, MONnD_COORD_NMAX);
          }
          Vars->Coord_Type[Coord_Number] = Set_Vars_Coord_Type;
          strncpy(Vars->Coord_Label[Coord_Number], Set_Vars_Coord_Label,30);
          strncpy(Vars->Coord_Var[Coord_Number], Set_Vars_Coord_Var,30);
          if (lmin > lmax) { XY = lmin; lmin=lmax; lmax = XY; }
          Vars->Coord_Min[Coord_Number] = lmin;
          Vars->Coord_Max[Coord_Number] = lmax;
          if (Set_Vars_Coord_Type == DEFS->COORD_NCOUNT || Set_Vars_Coord_Type == DEFS->COORD_PIXELID || Set_Vars_Coord_Type == DEFS->COORD_SIGNAL)
            Vars->Coord_Bin[Coord_Number] = 1;
          else
            Vars->Coord_Bin[Coord_Number] = 20;
          Set_Coord_Mode = DEFS->COORD_VAR;
          Flag_All = 0;
          Flag_No  = 0;
        } else {
          /* no variable name could be read from options */
          if (!iskeyword) {
            if (strcmp(token, "cm2") && strcmp(token, "incoming")
             && strcmp(token, "outgoing") && strcmp(token, "cm2")
             && strcmp(token, "cm^2") && strcmp(token, "float")
             && strcmp(token, "double") && strcmp(token, "binary")
             && strcmp(token, "steradian") && Vars->Flag_Verbose)
              printf("Monitor_nD: %s: unknown '%s' keyword in 'options'. Ignoring.\n", Vars->compcurname, token);
          }
        }
      carg++;
      } /* end if token */
    } /* end while carg */
    free(option_copy);
    if (carg == 128) printf("Monitor_nD: %s reached max number of tokens (%i). Skipping.\n", Vars->compcurname, 128);

    if ((Vars->Flag_Shape == DEFS->SHAPE_BOX) && (fabs(Vars->mzmax - Vars->mzmin) == 0)) Vars->Flag_Shape = DEFS->SHAPE_SQUARE;

    if (Vars->Flag_log == 1) Vars->Coord_Type[0] |= DEFS->COORD_LOG;
    if (Vars->Coord_Number == 0)
    { Vars->Flag_Auto_Limits=0; Vars->Flag_Multiple=0; Vars->Flag_List=0; }

    /* now setting Monitor Name from variable labels */
    strcpy(Vars->Monitor_Label,"");
    XY = 1; /* will contain total bin number */
    for (i = 0; i <= Vars->Coord_Number; i++)
    {
      if (Flag_auto != 0) Vars->Coord_Type[i] |= DEFS->COORD_AUTO;
      Set_Vars_Coord_Type = (Vars->Coord_Type[i] & (DEFS->COORD_LOG-1));
      if ((Set_Vars_Coord_Type == DEFS->COORD_X)
       || (Set_Vars_Coord_Type == DEFS->COORD_Y)
       || (Set_Vars_Coord_Type == DEFS->COORD_Z))
       strcpy(Short_Label[i],"Position");
      else
      if ((Set_Vars_Coord_Type == DEFS->COORD_THETA)
       || (Set_Vars_Coord_Type == DEFS->COORD_PHI)
       || (Set_Vars_Coord_Type == DEFS->COORD_ANGLE))
       strcpy(Short_Label[i],"Angle");
      else
      if ((Set_Vars_Coord_Type == DEFS->COORD_XY)
       || (Set_Vars_Coord_Type == DEFS->COORD_XZ)
       || (Set_Vars_Coord_Type == DEFS->COORD_YZ)
       || (Set_Vars_Coord_Type == DEFS->COORD_RADIUS))
       strcpy(Short_Label[i],"Radius");
      else
      if ((Set_Vars_Coord_Type == DEFS->COORD_VX)
       || (Set_Vars_Coord_Type == DEFS->COORD_VY)
       || (Set_Vars_Coord_Type == DEFS->COORD_VZ)
       || (Set_Vars_Coord_Type == DEFS->COORD_V)
       || (Set_Vars_Coord_Type == DEFS->COORD_VXY)
       || (Set_Vars_Coord_Type == DEFS->COORD_VYZ)
       || (Set_Vars_Coord_Type == DEFS->COORD_VXZ))
       strcpy(Short_Label[i],"Velocity");
      else
      if ((Set_Vars_Coord_Type == DEFS->COORD_KX)
       || (Set_Vars_Coord_Type == DEFS->COORD_KY)
       || (Set_Vars_Coord_Type == DEFS->COORD_KZ)
       || (Set_Vars_Coord_Type == DEFS->COORD_KXY)
       || (Set_Vars_Coord_Type == DEFS->COORD_KYZ)
       || (Set_Vars_Coord_Type == DEFS->COORD_KXZ)
       || (Set_Vars_Coord_Type == DEFS->COORD_K))
       strcpy(Short_Label[i],"Wavevector");
      else
      if ((Set_Vars_Coord_Type == DEFS->COORD_SX)
       || (Set_Vars_Coord_Type == DEFS->COORD_SY)
       || (Set_Vars_Coord_Type == DEFS->COORD_SZ))
       strcpy(Short_Label[i],"Spin");
      else
      if ((Set_Vars_Coord_Type == DEFS->COORD_HDIV)
       || (Set_Vars_Coord_Type == DEFS->COORD_VDIV))
       strcpy(Short_Label[i],"Divergence");
      else
      if (Set_Vars_Coord_Type == DEFS->COORD_ENERGY)
       strcpy(Short_Label[i],"Energy");
      else
      if (Set_Vars_Coord_Type == DEFS->COORD_LAMBDA)
       strcpy(Short_Label[i],"Wavelength");
      else
      if (Set_Vars_Coord_Type == DEFS->COORD_NCOUNT)
       strcpy(Short_Label[i],"Neutron_ID");
      else
      if (Set_Vars_Coord_Type == DEFS->COORD_PIXELID)
       strcpy(Short_Label[i],"Pixel_ID");
      else
      if (Set_Vars_Coord_Type == DEFS->COORD_T)
          strcpy(Short_Label[i],"Time_Of_Flight");
      else
      if (Set_Vars_Coord_Type == DEFS->COORD_P)
          strcpy(Short_Label[i],"Intensity");
      else
      if (Set_Vars_Coord_Type == DEFS->COORD_USER1)
          strncpy(Short_Label[i],Vars->UserName1,30);
      else
      if (Set_Vars_Coord_Type == DEFS->COORD_USER2)
          strncpy(Short_Label[i],Vars->UserName2,30);
      else
      if (Set_Vars_Coord_Type == DEFS->COORD_USER3)
          strncpy(Short_Label[i],Vars->UserName3,30);
      else
          strcpy(Short_Label[i],"Unknown");

      if (Vars->Coord_Type[i] & DEFS->COORD_ABS)
      { strcat(Vars->Coord_Label[i]," (abs)"); }

      if (Vars->Coord_Type[i] & DEFS->COORD_LOG)
      { strcat(Vars->Coord_Label[i]," (log)"); }

      strcat(Vars->Monitor_Label, " ");
      strcat(Vars->Monitor_Label, Short_Label[i]);
      XY *= Vars->Coord_Bin[i];

    } /* end for Short_Label */

    if ((Vars->Coord_Type[0] & (DEFS->COORD_LOG-1)) == DEFS->COORD_P) {
      strncat(Vars->Coord_Label[0], " [n/s", 30);
      if (Vars->Flag_per_cm2) strncat(Vars->Coord_Label[0], "/cm2", 30);

      if (XY > 1 && Vars->Coord_Number)
        strncat(Vars->Coord_Label[0], "/bin", 30);
      strncat(Vars->Coord_Label[0], "]", 30);
    }

    /* update label 'signal per bin' if more than 1 bin */
    if (XY > 1 && Vars->Coord_Number) {
      if (Vars->Flag_capture)
        printf("Monitor_nD: %s: Using capture flux weightening on %ld bins.\n"
               "WARNING     Use binned data with caution, and prefer monitor integral value (I,Ierr).\n", Vars->compcurname, (long)XY);
    }

    strcat(Vars->Monitor_Label, " Monitor");
    if (Vars->Flag_Shape == DEFS->SHAPE_SQUARE) strcat(Vars->Monitor_Label, " (Square)");
    if (Vars->Flag_Shape == DEFS->SHAPE_DISK)   strcat(Vars->Monitor_Label, " (Disk)");
    if (Vars->Flag_Shape == DEFS->SHAPE_SPHERE) strcat(Vars->Monitor_Label, " (Sphere)");
    if (Vars->Flag_Shape == DEFS->SHAPE_CYLIND) strcat(Vars->Monitor_Label, " (Cylinder)");
    if (Vars->Flag_Shape == DEFS->SHAPE_BANANA) strcat(Vars->Monitor_Label, " (Banana)");
    if (Vars->Flag_Shape == DEFS->SHAPE_BOX)    strcat(Vars->Monitor_Label, " (Box)");
    if (Vars->Flag_Shape == DEFS->SHAPE_PREVIOUS) strcat(Vars->Monitor_Label, " (on PREVIOUS)");
    if (Vars->Flag_Shape == DEFS->SHAPE_OFF) strcat(Vars->Monitor_Label, " (OFF geometry)");
    if ((Vars->Flag_Shape == DEFS->SHAPE_CYLIND) || (Vars->Flag_Shape == DEFS->SHAPE_BANANA) || (Vars->Flag_Shape == DEFS->SHAPE_SPHERE) || (Vars->Flag_Shape == DEFS->SHAPE_BOX))
    {
      if (strstr(Vars->option, "incoming"))
      {
        Vars->Flag_Shape = abs(Vars->Flag_Shape);
        strcat(Vars->Monitor_Label, " [in]");
      }
      else /* if strstr(Vars->option, "outgoing")) */
      {
        Vars->Flag_Shape = -abs(Vars->Flag_Shape);
        strcat(Vars->Monitor_Label, " [out]");
      }
    }
    if (Vars->Flag_UsePreMonitor == 1)
    {
        strcat(Vars->Monitor_Label, " at ");
        strncat(Vars->Monitor_Label, Vars->UserName1,30);
    }
    if (Vars->Flag_log == 1) strcat(Vars->Monitor_Label, " [log] ");

    /* now allocate memory to store variables in TRACE */

    /* Vars->Coord_Number  0   : intensity or signal
     * Vars->Coord_Number  1:n : detector variables */

    if ((Vars->Coord_NumberNoPixel != 2) && !Vars->Flag_Multiple && !Vars->Flag_List)
    { Vars->Flag_Multiple = 1; /* default is n1D */
      if (Vars->Coord_Number != Vars->Coord_NumberNoPixel) Vars->Flag_List = 1; }

    /* list and auto limits case : Vars->Flag_List or Vars->Flag_Auto_Limits
     * -> Buffer to flush and suppress after Vars->Flag_Auto_Limits
     */
    if ((Vars->Flag_Auto_Limits || Vars->Flag_List) && Vars->Coord_Number)
    { /* Dim : (Vars->Coord_Number+1)*Vars->Buffer_Block matrix (for p, dp) */
      Vars->Mon2D_Buffer = (double *)malloc((Vars->Coord_Number+1)*Vars->Buffer_Block*sizeof(double));
      if (Vars->Mon2D_Buffer == NULL)
      { printf("Monitor_nD: %s cannot allocate Vars->Mon2D_Buffer (%li). No list and auto limits.\n", Vars->compcurname, Vars->Buffer_Block*(Vars->Coord_Number+1)*sizeof(double)); Vars->Flag_List = 0; Vars->Flag_Auto_Limits = 0; }
      else
      {
        for (i=0; i < (Vars->Coord_Number+1)*Vars->Buffer_Block; Vars->Mon2D_Buffer[i++] = (double)0);
      }
      Vars->Buffer_Size = Vars->Buffer_Block;
    }

    /* 1D and n1D case : Vars->Flag_Multiple */
    if (Vars->Flag_Multiple && Vars->Coord_NumberNoPixel)
    { /* Dim : Vars->Coord_Number*Vars->Coord_Bin[i] vectors */
      Vars->Mon2D_N  = (double **)malloc((Vars->Coord_Number)*sizeof(double *));
      Vars->Mon2D_p  = (double **)malloc((Vars->Coord_Number)*sizeof(double *));
      Vars->Mon2D_p2 = (double **)malloc((Vars->Coord_Number)*sizeof(double *));
      if ((Vars->Mon2D_N == NULL) || (Vars->Mon2D_p == NULL) || (Vars->Mon2D_p2 == NULL))
      { fprintf(stderr,"Monitor_nD: %s n1D cannot allocate Vars->Mon2D_N/p/p2 (%li). Fatal.\n", Vars->compcurname, (Vars->Coord_Number)*sizeof(double *)); exit(-1); }
      for (i= 1; i <= Vars->Coord_Number; i++)
      {
        Vars->Mon2D_N[i-1]  = (double *)malloc(Vars->Coord_Bin[i]*sizeof(double));
        Vars->Mon2D_p[i-1]  = (double *)malloc(Vars->Coord_Bin[i]*sizeof(double));
        Vars->Mon2D_p2[i-1] = (double *)malloc(Vars->Coord_Bin[i]*sizeof(double));
        if ((Vars->Mon2D_N == NULL) || (Vars->Mon2D_p == NULL) || (Vars->Mon2D_p2 == NULL))
        { fprintf(stderr,"Monitor_nD: %s n1D cannot allocate %s Vars->Mon2D_N/p/p2[%li] (%li). Fatal.\n", Vars->compcurname, Vars->Coord_Var[i], i, (Vars->Coord_Bin[i])*sizeof(double *)); exit(-1); }
        else
        {
          for (j=0; j < Vars->Coord_Bin[i]; j++ )
          { Vars->Mon2D_N[i-1][j] = (double)0; Vars->Mon2D_p[i-1][j] = (double)0; Vars->Mon2D_p2[i-1][j] = (double)0; }
        }
      }
    }
    else /* 2D case : Vars->Coord_Number==2 and !Vars->Flag_Multiple and !Vars->Flag_List */
    if ((Vars->Coord_NumberNoPixel == 2) && !Vars->Flag_Multiple)
    { /* Dim : Vars->Coord_Bin[1]*Vars->Coord_Bin[2] matrix */
      Vars->Mon2D_N  = (double **)malloc((Vars->Coord_Bin[1])*sizeof(double *));
      Vars->Mon2D_p  = (double **)malloc((Vars->Coord_Bin[1])*sizeof(double *));
      Vars->Mon2D_p2 = (double **)malloc((Vars->Coord_Bin[1])*sizeof(double *));
      if ((Vars->Mon2D_N == NULL) || (Vars->Mon2D_p == NULL) || (Vars->Mon2D_p2 == NULL))
      { fprintf(stderr,"Monitor_nD: %s 2D cannot allocate %s Vars->Mon2D_N/p/p2 (%li). Fatal.\n", Vars->compcurname, Vars->Coord_Var[1], (Vars->Coord_Bin[1])*sizeof(double *)); exit(-1); }
      for (i= 0; i < Vars->Coord_Bin[1]; i++)
      {
        Vars->Mon2D_N[i]  = (double *)malloc(Vars->Coord_Bin[2]*sizeof(double));
        Vars->Mon2D_p[i]  = (double *)malloc(Vars->Coord_Bin[2]*sizeof(double));
        Vars->Mon2D_p2[i] = (double *)malloc(Vars->Coord_Bin[2]*sizeof(double));
        if ((Vars->Mon2D_N == NULL) || (Vars->Mon2D_p == NULL) || (Vars->Mon2D_p2 == NULL))
        { fprintf(stderr,"Monitor_nD: %s 2D cannot allocate %s Vars->Mon2D_N/p/p2[%li] (%li). Fatal.\n", Vars->compcurname, Vars->Coord_Var[1], i, (Vars->Coord_Bin[2])*sizeof(double *)); exit(-1); }
        else
        {
          for (j=0; j < Vars->Coord_Bin[2]; j++ )
          { Vars->Mon2D_N[i][j] = (double)0; Vars->Mon2D_p[i][j] = (double)0; Vars->Mon2D_p2[i][j] = (double)0; }
        }
      }
    }
    else {
      Vars->Mon2D_N = Vars->Mon2D_p = Vars->Mon2D_p2 = NULL;
    }
      /* no Mon2D allocated for
       * (Vars->Coord_Number != 2) && !Vars->Flag_Multiple && Vars->Flag_List */

    Vars->psum  = 0;
    Vars->p2sum = 0;
    Vars->Nsum  = 0;

    Vars->area  = fabs(Vars->mxmax - Vars->mxmin)*fabs(Vars->mymax - Vars->mymin)*1E4; /* in cm**2 for square and box shapes */
    Vars->Sphere_Radius = fabs(Vars->mxmax - Vars->mxmin)/2;
    if ((abs(Vars->Flag_Shape) == DEFS->SHAPE_DISK) || (abs(Vars->Flag_Shape) == DEFS->SHAPE_SPHERE))
    {
      Vars->area = PI*Vars->Sphere_Radius*Vars->Sphere_Radius*1E4; /* disk shapes */
    }


    if (Vars->area == 0 && abs(Vars->Flag_Shape) != DEFS->SHAPE_PREVIOUS ) {
      if (abs(Vars->Flag_Shape) != DEFS->SHAPE_OFF) {  
	Vars->Coord_Number = 0;
      }
    }
    if (Vars->Coord_Number == 0 && Vars->Flag_Verbose)
      printf("Monitor_nD: %s is unactivated (0D)\n", Vars->compcurname);
    Vars->Cylinder_Height = fabs(Vars->mymax - Vars->mymin);

    if (Vars->Flag_Verbose)
    {
      printf("Monitor_nD: %s is a %s.\n", Vars->compcurname, Vars->Monitor_Label);
      printf("Monitor_nD: version %s with options=%s\n", MONITOR_ND_LIB_H, Vars->option);
    }
    
    /* compute the product of bin dimensions for PixelID */
    Vars->Coord_BinProd[0]=1;
    for (i = 1; i <= Vars->Coord_Number; i++)
      Vars->Coord_BinProd[i]=Vars->Coord_Bin[i]*Vars->Coord_BinProd[i-1];
  } /* end Monitor_nD_Init */

/* ========================================================================= */
/* Monitor_nD_Trace: this routine is used to monitor one propagating neutron */
/* return values: 0=neutron was absorbed, -1=neutron was outside bounds, 1=neutron was measured*/
/* ========================================================================= */
#pragma acc routine seq
int Monitor_nD_Trace(MonitornD_Defines_type *DEFS, MonitornD_Variables_type *Vars, _class_particle* _particle)
{

  double  XY=0, pp=0;
  int     retval;
  long    i =0, j =0;
  double  Coord[MONnD_COORD_NMAX];
  long    Coord_Index[MONnD_COORD_NMAX];
  char    While_End   =0;
  long    While_Buffer=0;
  char    Set_Vars_Coord_Type = DEFS->COORD_NONE;
  
  /* the logic below depends mainly on:
       Flag_List:        1=store 1 buffer, 2=list all, 3=re-use buffer 
       Flag_Auto_Limits: 0 (no auto limits/list), 1 (store events into Buffer), 2 (re-emit store events)
   */

  /* Vars->Flag_Auto_Limits=1: buffer full, we read the Buffer, and determine min and max bounds */
  if ((Vars->Buffer_Counter >= Vars->Buffer_Block) && (Vars->Flag_Auto_Limits == 1) && (Vars->Coord_Number > 0))
  {
    /* auto limits case : get limits in Buffer for each variable */
          /* Dim : (Vars->Coord_Number+1)*Vars->Buffer_Block matrix (for p, dp) */
    if (Vars->Flag_Verbose) printf("Monitor_nD: %s getting %li Auto Limits from List (%li events) in TRACE.\n", Vars->compcurname, Vars->Coord_Number, Vars->Buffer_Counter);
    for (i = 1; i <= Vars->Coord_Number; i++)
    {
      if (Vars->Coord_Type[i] & DEFS->COORD_AUTO)
      {
        Vars->Coord_Min[i] =  FLT_MAX;
        Vars->Coord_Max[i] = -FLT_MAX;
        for (j = 0; j < Vars->Buffer_Counter; j++)
        {
          XY = Vars->Mon2D_Buffer[i+j*(Vars->Coord_Number+1)];  /* scanning variables in Buffer */
          if (XY < Vars->Coord_Min[i]) Vars->Coord_Min[i] = XY;
          if (XY > Vars->Coord_Max[i]) Vars->Coord_Max[i] = XY;
        }
        if  (Vars->Flag_Verbose)  
          printf("  %s: min=%g max=%g\n", Vars->Coord_Var[i], Vars->Coord_Min[i], Vars->Coord_Max[i]);
      }
    }
    Vars->Flag_Auto_Limits = 2;  /* pass to 2nd auto limits step (read Buffer and generate new events to store in histograms) */
  } /* end if Flag_Auto_Limits == 1 */

#ifndef USE_PGI
  /* manage realloc for 'list all' if Buffer size exceeded: flush Buffer to file */
  if ((Vars->Buffer_Counter >= Vars->Buffer_Block) && (Vars->Flag_List >= 2))
  {
    if (Vars->Buffer_Size >= 1000000 || Vars->Flag_List == 3)
    { /* save current (possibly append) and re-use Buffer */

      Monitor_nD_Save(DEFS, Vars);
      Vars->Flag_List = 3;
      Vars->Buffer_Block = Vars->Buffer_Size;
      Vars->Buffer_Counter  = 0;
      Vars->Neutron_Counter = 0;

    }
    else
    {
      Vars->Mon2D_Buffer  = (double *)realloc(Vars->Mon2D_Buffer, (Vars->Coord_Number+1)*(Vars->Neutron_Counter+Vars->Buffer_Block)*sizeof(double));
      if (Vars->Mon2D_Buffer == NULL)
            { printf("Monitor_nD: %s cannot reallocate Vars->Mon2D_Buffer[%li] (%li). Skipping.\n", Vars->compcurname, i, (Vars->Neutron_Counter+Vars->Buffer_Block)*sizeof(double)); Vars->Flag_List = 1; }
      else { Vars->Buffer_Counter = 0; Vars->Buffer_Size = Vars->Neutron_Counter+Vars->Buffer_Block; }
    }
  } /* end if Buffer realloc */
#endif
 
  char    outsidebounds=0;
  while (!While_End)
  { /* we generate Coord[] and Coord_index[] from Buffer (auto limits) or passing neutron */
    if ((Vars->Flag_Auto_Limits == 2) && (Vars->Coord_Number > 0))
    { /* Vars->Flag_Auto_Limits == 2: read back from Buffer (Buffer is filled or auto limits have been computed) */
      if (While_Buffer < Vars->Buffer_Block)
      {
        /* first while loop (While_Buffer) */
        /* auto limits case : scan Buffer within limits and store in Mon2D */
        Coord[0] = pp = Vars->Mon2D_Buffer[While_Buffer*(Vars->Coord_Number+1)];

        for (i = 1; i <= Vars->Coord_Number; i++)
        {
          /* scanning variables in Buffer */
          if (Vars->Coord_Bin[i] <= 1) continue;
          XY = (Vars->Coord_Max[i]-Vars->Coord_Min[i]);

          Coord[i] = Vars->Mon2D_Buffer[i+While_Buffer*(Vars->Coord_Number+1)];
          if (XY > 0) Coord_Index[i] = floor((Coord[i]-Vars->Coord_Min[i])*Vars->Coord_Bin[i]/XY);
          else        Coord_Index[i] = 0;
          if (Vars->Flag_With_Borders)
          {
            if (Coord_Index[i] < 0)                   Coord_Index[i] = 0;
            if (Coord_Index[i] >= Vars->Coord_Bin[i]) Coord_Index[i] = Vars->Coord_Bin[i] - 1;
          }
        } /* end for */
        
        /* update the PixelID, we compute it from the previous variables index */
        if (Vars->Coord_NumberNoPixel < Vars->Coord_Number) /* there is a Pixel variable */
        for (i = 1; i <= Vars->Coord_Number; i++) {
          char Set_Vars_Coord_Type = (Vars->Coord_Type[i] & (DEFS->COORD_LOG-1));
          if (Set_Vars_Coord_Type == DEFS->COORD_PIXELID) {
            char flag_outside=0;
            Coord_Index[i] = Coord[i] = 0;
            for (j= 1; j < i; j++) {
              /* not for 1D variables with Bin=1 such as PixelID, NCOUNT, Intensity */
              if (Vars->Coord_Bin[j] == 1) continue; 
              if (0 > Coord_Index[j] || Coord_Index[j] >= Vars->Coord_Bin[j]) {
                flag_outside=1;
                Coord[i] = 0;
                break;
              }
              Coord[i] += Coord_Index[j]*Vars->Coord_BinProd[j-1];
            }
            if (!flag_outside) {
              Vars->Mon2D_Buffer[i+While_Buffer*(Vars->Coord_Number+1)] = Coord[i];
            }
          } /* end if PixelID */
        }
        While_Buffer++;
      } /* end if in Buffer */
      else /* (While_Buffer >= Vars->Buffer_Block) && (Vars->Flag_Auto_Limits == 2) */
      {
        Vars->Flag_Auto_Limits = 0;
        if (!Vars->Flag_List) /* free Buffer not needed anymore (no list to output) */
        { /* Dim : (Vars->Coord_Number+1)*Vars->Buffer_Block matrix (for p, p2) */
          free(Vars->Mon2D_Buffer); Vars->Mon2D_Buffer = NULL;
        }
        if (Vars->Flag_Verbose) printf("Monitor_nD: %s flushed %li Auto Limits from List (%li) in TRACE.\n", Vars->compcurname, Vars->Coord_Number, Vars->Buffer_Counter);
      }
    } /* if Vars->Flag_Auto_Limits == 2 */
    
    if (Vars->Flag_Auto_Limits != 2 || !Vars->Coord_Number) /* Vars->Flag_Auto_Limits == 0 (no auto limits/list) or 1 (store events into Buffer) */
    {
      /* automatically compute area and steradian solid angle when in AUTO mode */
      /* compute the steradian solid angle incoming on the monitor */
      double v;
      v=sqrt(Vars->cvx*Vars->cvx
            +Vars->cvy*Vars->cvy
            +Vars->cvz*Vars->cvz);
      if (Vars->min_x > Vars->cx) Vars->min_x = Vars->cx;
      if (Vars->max_x < Vars->cx) Vars->max_x = Vars->cx;
      if (Vars->min_y > Vars->cy) Vars->min_y = Vars->cy;
      if (Vars->max_y < Vars->cy) Vars->max_y = Vars->cy;
      Vars->mean_p  += Vars->cp;
      if (v) {
        Vars->mean_dx += Vars->cp*fabs(Vars->cvx/v);
        Vars->mean_dy += Vars->cp*fabs(Vars->cvy/v);
      }

      for (i = 0; i <= Vars->Coord_Number; i++)
      { /* handle current neutron : last while */
        XY = 0;
        Set_Vars_Coord_Type = (Vars->Coord_Type[i] & (DEFS->COORD_LOG-1));
        /* get values for variables to monitor */
        if (Set_Vars_Coord_Type == DEFS->COORD_X) XY = Vars->cx;
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_Y) XY = Vars->cy;
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_Z) XY = Vars->cz;
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_VX) XY = Vars->cvx;
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_VY) XY = Vars->cvy;
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_VZ) XY = Vars->cvz;
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_KX) XY = V2K*Vars->cvx;
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_KY) XY = V2K*Vars->cvy;
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_KZ) XY = V2K*Vars->cvz;
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_SX) XY = Vars->csx;
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_SY) XY = Vars->csy;
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_SZ) XY = Vars->csz;
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_T) XY = Vars->ct;
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_P) XY = Vars->cp;
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_HDIV) XY = RAD2DEG*atan2(Vars->cvx,Vars->cvz);
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_VDIV) XY = RAD2DEG*atan2(Vars->cvy,Vars->cvz);
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_V) XY = sqrt(Vars->cvx*Vars->cvx+Vars->cvy*Vars->cvy+Vars->cvz*Vars->cvz);
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_RADIUS)
          XY = sqrt(Vars->cx*Vars->cx+Vars->cy*Vars->cy+Vars->cz*Vars->cz);
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_XY)
          XY = sqrt(Vars->cx*Vars->cx+Vars->cy*Vars->cy)*(Vars->cx > 0 ? 1 : -1);
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_YZ) XY = sqrt(Vars->cy*Vars->cy+Vars->cz*Vars->cz);
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_XZ)
          XY = sqrt(Vars->cx*Vars->cx+Vars->cz*Vars->cz);
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_VXY) XY = sqrt(Vars->cvx*Vars->cvx+Vars->cvy*Vars->cvy);
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_VXZ) XY = sqrt(Vars->cvx*Vars->cvx+Vars->cvz*Vars->cvz);
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_VYZ) XY = sqrt(Vars->cvy*Vars->cvy+Vars->cvz*Vars->cvz);
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_K) { XY = sqrt(Vars->cvx*Vars->cvx+Vars->cvy*Vars->cvy+Vars->cvz*Vars->cvz);  XY *= V2K; }
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_KXY) { XY = sqrt(Vars->cvx*Vars->cvx+Vars->cvy*Vars->cvy);  XY *= V2K; }
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_KXZ) { XY = sqrt(Vars->cvx*Vars->cvx+Vars->cvz*Vars->cvz);  XY *= V2K; }
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_KYZ) { XY = sqrt(Vars->cvy*Vars->cvy+Vars->cvz*Vars->cvz);  XY *= V2K; }
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_ENERGY) { XY = Vars->cvx*Vars->cvx+Vars->cvy*Vars->cvy+Vars->cvz*Vars->cvz;  XY *= VS2E; }
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_LAMBDA) { XY = sqrt(Vars->cvx*Vars->cvx+Vars->cvy*Vars->cvy+Vars->cvz*Vars->cvz);  XY *= V2K; if (XY != 0) XY = 2*PI/XY; }
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_NCOUNT) XY = Vars->Neutron_Counter;
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_ANGLE)
        {  XY = sqrt(Vars->cvx*Vars->cvx+Vars->cvy*Vars->cvy);
           if (Vars->cvz != 0)
                XY = RAD2DEG*atan2(XY,Vars->cvz)*(Vars->cx > 0 ? 1 : -1);
           else XY = 0;
        }
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_THETA)  { if (Vars->cz != 0) XY = RAD2DEG*atan2(Vars->cx,Vars->cz); }
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_PHI) { if (Vars->cz != 0) XY = RAD2DEG*asin(Vars->cy/Vars->cz); }
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_USER1) XY = Vars->UserVariable1;
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_USER2) XY = Vars->UserVariable2;
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_USER3) XY = Vars->UserVariable3;
        else
        if (Set_Vars_Coord_Type == DEFS->COORD_PIXELID && !Vars->Flag_Auto_Limits) {
          /* compute the PixelID from previous coordinates 
             the PixelID is the product of Coord_Index[i] in the detector geometry 
             pixelID = sum( Coord_Index[j]*prod(Vars->Coord_Bin[1:(j-1)]) )
             
             this does not apply when we store events in the buffer as Coord_Index
             is not set. Then the pixelID will be re-computed during SAVE.
          */
          char flag_outside=0;
          for (j= 1; j < i; j++) {
            /* not for 1D variables with Bin=1 such as PixelID, NCOUNT, Intensity */
            if (Vars->Coord_Bin[j] <= 1) continue; 
            if (0 > Coord_Index[j] || Coord_Index[j] >= Vars->Coord_Bin[j]) { 
              flag_outside=1; XY=0; break;
            }
            XY += Coord_Index[j]*Vars->Coord_BinProd[j-1];
          }
	  if (Vars->Flag_mantid && Vars->Flag_OFF && Vars->OFF_polyidx >=0) XY=Vars->OFF_polyidx;
          if (!flag_outside) XY += Vars->Coord_Min[i];
        }
        
        /* handle 'abs' and 'log' keywords */
        if (Vars->Coord_Type[i] & DEFS->COORD_ABS) XY=fabs(XY);

        if (Vars->Coord_Type[i] & DEFS->COORD_LOG) /* compute log of variable if requested */
        {  if (XY > 0) XY = log(XY)/log(10);
           else        XY = -100; }

        Coord[i] = XY; Coord_Index[i] = 0;
        if (i == 0) { pp = XY; Coord_Index[i] = 0; }
        else {
        /* check bounds for variables which have no automatic limits */
          if ((!Vars->Flag_Auto_Limits || !(Vars->Coord_Type[i] & DEFS->COORD_AUTO)) && Vars->Coord_Bin[i]>1)
          { /* compute index in histograms for each variable to monitor */
            XY = (Vars->Coord_Max[i]-Vars->Coord_Min[i]);
            if (XY > 0) Coord_Index[i] = floor((Coord[i]-Vars->Coord_Min[i])*Vars->Coord_Bin[i]/XY);
            if (Vars->Flag_With_Borders)
            {
              if (Coord_Index[i] >= Vars->Coord_Bin[i]) Coord_Index[i] = Vars->Coord_Bin[i] - 1;
              if (Coord_Index[i] < 0) Coord_Index[i] = 0;
            }
            //if (0 > Coord_Index[i] || Coord_Index[i] >= Vars->Coord_Bin[i])
            //  outsidebounds=1;
          } /* else will get Index later from Buffer when Flag_Auto_Limits == 2 */
        }
        
      } /* end for i */
      While_End = 1;
    }/* end else if Vars->Flag_Auto_Limits == 2 */
    
    /* ====================================================================== */
    /* store n1d/2d neutron from Buffer (Auto_Limits == 2) or current neutron in while */
    if (Vars->Flag_Auto_Limits != 1) /* not when storing auto limits Buffer */
    {
      /* apply per cm2 */
      if (Vars->Flag_per_cm2 && Vars->area != 0)
        pp /= Vars->area;

      /* 2D case : Vars->Coord_Number==2 and !Vars->Flag_Multiple and !Vars->Flag_List */
      if ( Vars->Coord_NumberNoPixel == 2 && !Vars->Flag_Multiple)
      { /* Dim : Vars->Coord_Bin[1]*Vars->Coord_Bin[2] matrix */
        
        i = Coord_Index[1];
        j = Coord_Index[2];
        if (i >= 0 && i < Vars->Coord_Bin[1] && j >= 0 && j < Vars->Coord_Bin[2])
        {
          if (Vars->Mon2D_N) {
	    double p2 = pp*pp;
            #pragma acc atomic
	    Vars->Mon2D_N[i][j] = Vars->Mon2D_N[i][j]+1;
            #pragma acc atomic
	    Vars->Mon2D_p[i][j] = Vars->Mon2D_p[i][j]+pp;
            #pragma acc atomic
	    Vars->Mon2D_p2[i][j] = Vars->Mon2D_p2[i][j] + p2;
	  }
        } else {
          outsidebounds=1; 
        }
      } else {
        /* 1D and n1D case : Vars->Flag_Multiple */
        /* Dim : Vars->Coord_Number*Vars->Coord_Bin[i] vectors (intensity is not included) */
          
        for (i= 1; i <= Vars->Coord_Number; i++) {
          j = Coord_Index[i];
          if (j >= 0 && j < Vars->Coord_Bin[i]) {
            if  (Vars->Flag_Multiple && Vars->Mon2D_N) {
	      if (Vars->Mon2D_N) {
		double p2 = pp*pp;
                #pragma acc atomic
		Vars->Mon2D_N[i-1][j] = Vars->Mon2D_N[i-1][j]+1;
                #pragma acc atomic
		Vars->Mon2D_p[i-1][j] = Vars->Mon2D_p[i-1][j]+pp;
		#pragma acc atomic
		Vars->Mon2D_p2[i-1][j] = Vars->Mon2D_p2[i-1][j] + p2;
	      }
	    }
          } else { 
            outsidebounds=1;
            break;
          }
        }
      }
    } /* end (Vars->Flag_Auto_Limits != 1) */
    
    if (Vars->Flag_Auto_Limits != 2 && !outsidebounds) /* not when reading auto limits Buffer */
    { /* now store Coord into Buffer (no index needed) if necessary (list or auto limits) */
      if ((Vars->Buffer_Counter < Vars->Buffer_Block) && ((Vars->Flag_List) || (Vars->Flag_Auto_Limits == 1)))
      {
          
        for (i = 0; i <= Vars->Coord_Number; i++)
        {
          Vars->Mon2D_Buffer[i + Vars->Neutron_Counter*(Vars->Coord_Number+1)] = Coord[i];
        }
        Vars->Buffer_Counter++;
        if (Vars->Flag_Verbose && (Vars->Buffer_Counter >= Vars->Buffer_Block) && (Vars->Flag_List == 1)) 
          printf("Monitor_nD: %s %li neutrons stored in List.\n", Vars->compcurname, Vars->Buffer_Counter);
      }
      Vars->Neutron_Counter++;
    } /* end (Vars->Flag_Auto_Limits != 2) */
    
  } /* end while */
  Vars->Nsum++;
  Vars->psum  += pp;
  Vars->p2sum += pp*pp;

  /*determine return value: 1:neutron was in bounds and measured, -1: outside bounds, 0: outside bounds, should be absorbed.*/
  if(outsidebounds){
      if(Vars->Flag_Absorb){
          return 0;
      }else{
          return -1;
      }
  }
  return 1;
} /* end Monitor_nD_Trace */

/* ========================================================================= */
/* Monitor_nD_Save: this routine is used to save data files                  */
/* ========================================================================= */
MCDETECTOR Monitor_nD_Save(MonitornD_Defines_type *DEFS, MonitornD_Variables_type *Vars)
  {
    char   *fname;
    long    i,j;
    double *p0m = NULL;
    double *p1m = NULL;
    double *p2m = NULL;
    char    Coord_X_Label[CHAR_BUF_LENGTH];
    double  min1d, max1d;
    double  min2d, max2d;
    long    bin1d, bin2d;
    char    While_End = 0;
    long    While_Buffer = 0;
    double  XY=0, pp=0;
    double  Coord[MONnD_COORD_NMAX];
    long    Coord_Index[MONnD_COORD_NMAX];
    char    label[CHAR_BUF_LENGTH];
    double  ratio;

    MCDETECTOR detector;

    ratio = 100.0*mcget_run_num()/mcget_ncount();
    if (Vars->Flag_Verbose && Vars->Flag_per_cm2) {
      printf("Monitor_nD: %s: active flat detector area is %g [cm^2], total area is %g [cm^2]\n",
        Vars->compcurname, (Vars->max_x-Vars->min_x)
                          *(Vars->max_y-Vars->min_y)*1E4, Vars->area);
      printf("Monitor_nD: %s: beam solid angle is %g [st] (%g x %g [deg^2])\n",
        Vars->compcurname,
        2*fabs(2*atan(Vars->mean_dx/Vars->mean_p)
         *sin(2*atan(Vars->mean_dy/Vars->mean_p)/2)),
        atan(Vars->mean_dx/Vars->mean_p)*RAD2DEG,
        atan(Vars->mean_dy/Vars->mean_p)*RAD2DEG);
    }

    /* check Buffer flush when end of simulation reached */
    if ((Vars->Buffer_Counter <= Vars->Buffer_Block) && Vars->Flag_Auto_Limits && Vars->Mon2D_Buffer && Vars->Buffer_Counter)
    {
      /* Get Auto Limits */
      if (Vars->Flag_Verbose) printf("Monitor_nD: %s getting %li Auto Limits from List (%li events).\n", Vars->compcurname, Vars->Coord_Number, Vars->Buffer_Counter);

      for (i = 1; i <= Vars->Coord_Number; i++)
      {
        if ((Vars->Coord_Type[i] & DEFS->COORD_AUTO) && Vars->Coord_Bin[i] > 1)
        {
          Vars->Coord_Min[i] = FLT_MAX;
          Vars->Coord_Max[i] = -FLT_MAX;
          for (j = 0; j < Vars->Buffer_Counter; j++)
          {
            XY = Vars->Mon2D_Buffer[i+j*(Vars->Coord_Number+1)];  /* scanning variables in Buffer */
            if (XY < Vars->Coord_Min[i]) Vars->Coord_Min[i] = XY;
            if (XY > Vars->Coord_Max[i]) Vars->Coord_Max[i] = XY;
          }
          if  (Vars->Flag_Verbose)  
            printf("  %s: min=%g max=%g in %li bins\n", Vars->Coord_Var[i], Vars->Coord_Min[i], Vars->Coord_Max[i], Vars->Coord_Bin[i]);
        }
      }
      Vars->Flag_Auto_Limits = 2;  /* pass to 2nd auto limits step */
      Vars->Buffer_Block = Vars->Buffer_Counter;

      while (!While_End)
      { /* we generate Coord[] and Coord_index[] from Buffer (auto limits) */
        /* simulation ended before Buffer was filled. Limits have to be computed, and stored events must be sent into histograms */
        
        if (While_Buffer < Vars->Buffer_Block)
        {
          /* first while loops (While_Buffer) */
          Coord[0] = Vars->Mon2D_Buffer[While_Buffer*(Vars->Coord_Number+1)];

          /* auto limits case : scan Buffer within limits and store in Mon2D */
          for (i = 1; i <= Vars->Coord_Number; i++)
          {
            /* scanning variables in Buffer */
            if (Vars->Coord_Bin[i] <= 1) Coord_Index[i] = 0;
            else {
              XY = (Vars->Coord_Max[i]-Vars->Coord_Min[i]);
              Coord[i] = Vars->Mon2D_Buffer[i+While_Buffer*(Vars->Coord_Number+1)];
              if (XY > 0) Coord_Index[i] = floor((Coord[i]-Vars->Coord_Min[i])*Vars->Coord_Bin[i]/XY);
              else Coord_Index[i] = 0;
              if (Vars->Flag_With_Borders)
              {
                if (Coord_Index[i] < 0) Coord_Index[i] = 0;
                if (Coord_Index[i] >= Vars->Coord_Bin[i]) Coord_Index[i] = Vars->Coord_Bin[i] - 1;
              }
            }
          } /* end for */

          /* update the PixelID, we compute it from the previous variables index */
          for (i = 1; i <= Vars->Coord_Number; i++) {
            char Set_Vars_Coord_Type = (Vars->Coord_Type[i] & (DEFS->COORD_LOG-1));
            if (Set_Vars_Coord_Type == DEFS->COORD_PIXELID) {
              char outsidebounds=0;
              Coord_Index[i] = Coord[i] = 0;
              for (j= 1; j < i; j++) {
                /* not for 1D variables with Bin=1 such as PixelID, NCOUNT, Intensity */
                if (Vars->Coord_Bin[j] == 1) continue; 
                if (0 > Coord_Index[j] || Coord_Index[j] >= Vars->Coord_Bin[j]) {
                  outsidebounds=1;
                  Coord[i] = 0;
                  break;
                }
                Coord[i] += Coord_Index[j]*Vars->Coord_BinProd[j-1];
              }
              if (!outsidebounds) {
                Vars->Mon2D_Buffer[i+While_Buffer*(Vars->Coord_Number+1)] = Coord[i];
              }
            } /* end if PixelID */
          }
          While_Buffer++;
        } /* end if in Buffer */
        else /* (While_Buffer >= Vars->Buffer_Block) && (Vars->Flag_Auto_Limits == 2) */
        {
          Vars->Flag_Auto_Limits = 0;
          While_End = 1;
          if (Vars->Flag_Verbose) printf("Monitor_nD: %s flushed %li Auto Limits from List (%li).\n", Vars->compcurname, Vars->Coord_Number, Vars->Buffer_Counter);
        }

        /* store n1d/2d section from Buffer */

        pp = Coord[0];
        /* apply per cm2 or per st */
        if (Vars->Flag_per_cm2 && Vars->area      != 0)
          pp /= Vars->area;
        
        /* 2D case : Vars->Coord_Number==2 and !Vars->Flag_Multiple and !Vars->Flag_List */
        if (!Vars->Flag_Multiple && Vars->Coord_NumberNoPixel == 2)
        { /* Dim : Vars->Coord_Bin[1]*Vars->Coord_Bin[2] matrix */
          i = Coord_Index[1];
          j = Coord_Index[2];
          if (i >= 0 && i < Vars->Coord_Bin[1] && j >= 0 && j < Vars->Coord_Bin[2])
          {
            if (Vars->Mon2D_N) {
              Vars->Mon2D_N[i][j]++;
              Vars->Mon2D_p[i][j] += pp;
              Vars->Mon2D_p2[i][j] += pp*pp;
            }
          } else if (Vars->Flag_Absorb) pp=0;
        }
        else
        /* 1D and n1D case : Vars->Flag_Multiple */
        { /* Dim : Vars->Coord_Number*Vars->Coord_Bin[i] vectors (intensity is not included) */
          for (i= 1; i <= Vars->Coord_Number; i++)
          {
            j = Coord_Index[i];
            if (j >= 0 && j < Vars->Coord_Bin[i])
            {
              if (Vars->Flag_Multiple && Vars->Mon2D_N) {
                Vars->Mon2D_N[i-1][j]++;
                Vars->Mon2D_p[i-1][j] += pp;
                Vars->Mon2D_p2[i-1][j] += pp*pp;
              }
            } else if (Vars->Flag_Absorb) {
              pp=0; break;
            }
          }
        } /* end store 2D/1D */
        
      } /* end while */
    } /* end Force Get Limits */

    /* write output files (sent to file as p[i*n + j] vectors) */
    if (Vars->Coord_Number == 0)
    {
      double Nsum;
      double psum, p2sum;
      Nsum = Vars->Nsum;
      psum = Vars->psum;
      p2sum= Vars->p2sum;
      if (Vars->Flag_signal != DEFS->COORD_P && Nsum > 0)
      { psum /=Nsum; p2sum /= Nsum*Nsum; }
      /* DETECTOR_OUT_0D(Vars->Monitor_Label, Vars->Nsum, Vars->psum, Vars->p2sum); */
      detector = mcdetector_out_0D(Vars->Monitor_Label, Nsum, psum, p2sum, Vars->compcurname, Vars->compcurpos);
    }
    else
    if (strlen(Vars->Mon_File) > 0)
    {
      fname = (char*)malloc(strlen(Vars->Mon_File)+10*Vars->Coord_Number);
      if (Vars->Flag_List && Vars->Mon2D_Buffer) /* List: DETECTOR_OUT_2D */
      {
       
        if (Vars->Flag_List >= 2) Vars->Buffer_Size = Vars->Neutron_Counter;
        if (Vars->Buffer_Size >= Vars->Neutron_Counter)
          Vars->Buffer_Size = Vars->Neutron_Counter;
        strcpy(fname,Vars->Mon_File);
        if (strchr(Vars->Mon_File,'.') == NULL) strcat(fname, "_list");

        strcpy(Coord_X_Label,"");
        for (i= 0; i <= Vars->Coord_Number; i++)
        {
          strcat(Coord_X_Label, Vars->Coord_Var[i]);
          strcat(Coord_X_Label, " ");
          if (strchr(Vars->Mon_File,'.') == NULL)
          { strcat(fname, "."); strcat(fname, Vars->Coord_Var[i]); }
        }
        if (Vars->Flag_Verbose) printf("Monitor_nD: %s write monitor file %s List (%lix%li).\n", Vars->compcurname, fname,bin2d,bin1d);

        /* handle the type of list output */
        strcpy(label, Vars->Monitor_Label);
        
        detector = mcdetector_out_list(
              label, "List of neutron events", Coord_X_Label,
              -Vars->Buffer_Size, Vars->Coord_Number+1,
              Vars->Mon2D_Buffer,
              fname, Vars->compcurname, Vars->compcurpos);
      }
      if (Vars->Flag_Multiple) /* n1D: DETECTOR_OUT_1D */
      {
        for (i= 0; i < Vars->Coord_Number; i++)
        {

          strcpy(fname,Vars->Mon_File);
          if (strchr(Vars->Mon_File,'.') == NULL)
          { strcat(fname, "."); strcat(fname, Vars->Coord_Var[i+1]); }
          sprintf(Coord_X_Label, "%s monitor", Vars->Coord_Label[i+1]);
          strcpy(label, Coord_X_Label);
          if (Vars->Coord_Bin[i+1] > 0) { /* 1D monitor */
            if (Vars->Flag_Verbose) printf("Monitor_nD: %s write monitor file %s 1D (%li).\n", Vars->compcurname, fname, Vars->Coord_Bin[i+1]);
            min1d = Vars->Coord_Min[i+1];
            max1d = Vars->Coord_Max[i+1];
            if (min1d == max1d) max1d = min1d+1e-6;
            p1m = (double *)malloc(Vars->Coord_Bin[i+1]*sizeof(double));
            p2m = (double *)malloc(Vars->Coord_Bin[i+1]*sizeof(double));
            if (p2m == NULL) /* use Raw Buffer line output */
            {
              if (Vars->Flag_Verbose) printf("Monitor_nD: %s cannot allocate memory for output. Using raw data.\n", Vars->compcurname);
              if (p1m != NULL) free(p1m);
              detector = mcdetector_out_1D(
              label,
              Vars->Coord_Label[i+1],
              Vars->Coord_Label[0],
              Vars->Coord_Var[i+1],
              min1d, max1d,
              Vars->Coord_Bin[i+1],
              Vars->Mon2D_N[i],Vars->Mon2D_p[i],Vars->Mon2D_p2[i],
              fname, Vars->compcurname, Vars->compcurpos);
            } /* if (p2m == NULL) */
            else
            {
              if (Vars->Flag_log != 0)
              {
                XY = FLT_MAX;
                for (j=0; j < Vars->Coord_Bin[i+1]; j++) /* search min of signal */
                  if ((XY > Vars->Mon2D_p[i][j]) && (Vars->Mon2D_p[i][j] > 0)) XY = Vars->Mon2D_p[i][j];
                if (XY <= 0) XY = -log(FLT_MAX)/log(10); else XY = log(XY)/log(10)-1;
              } /* if */

              for (j=0; j < Vars->Coord_Bin[i+1]; j++)
              {
                p1m[j] = Vars->Mon2D_p[i][j];
                p2m[j] = Vars->Mon2D_p2[i][j];
                if (Vars->Flag_signal != DEFS->COORD_P && Vars->Mon2D_N[i][j] > 0)
                { /* normalize mean signal to the number of events */
                  p1m[j] /= Vars->Mon2D_N[i][j];
                  p2m[j] /= Vars->Mon2D_N[i][j]*Vars->Mon2D_N[i][j];
                }
                if (Vars->Flag_log != 0)
                {
                  if ((p1m[j] > 0) && (p2m[j] > 0))
                  {
                    p2m[j] /= p1m[j]*p1m[j];
                    p1m[j] = log(p1m[j])/log(10);
                  }
                  else
                  {
                    p1m[j] = XY;
                    p2m[j] = 0;
                  }
                }
              } /* for */
              detector = mcdetector_out_1D(
                label,
                Vars->Coord_Label[i+1],
                Vars->Coord_Label[0],
                Vars->Coord_Var[i+1],
                min1d, max1d,
                Vars->Coord_Bin[i+1],
                Vars->Mon2D_N[i],p1m,p2m,
                fname, Vars->compcurname, Vars->compcurpos);

            } /* else */
            /* comment out 'free memory' lines to avoid loosing arrays if
               'detector' structure is used by other instrument parts
            if (p1m != NULL) free(p1m); p1m=NULL;
            if (p2m != NULL) free(p2m); p2m=NULL;
            */
          } else { /* 0d monitor */
            detector = mcdetector_out_0D(label, Vars->Mon2D_p[i][0], Vars->Mon2D_p2[i][0], Vars->Mon2D_N[i][0], Vars->compcurname, Vars->compcurpos);
          }


        } /* for */
      } /* if 1D */
      else
      if (Vars->Coord_NumberNoPixel == 2)  /* 2D: DETECTOR_OUT_2D */
      {
        strcpy(fname,Vars->Mon_File);

        p0m = (double *)malloc(Vars->Coord_Bin[1]*Vars->Coord_Bin[2]*sizeof(double));
        p1m = (double *)malloc(Vars->Coord_Bin[1]*Vars->Coord_Bin[2]*sizeof(double));
        p2m = (double *)malloc(Vars->Coord_Bin[1]*Vars->Coord_Bin[2]*sizeof(double));
        if (p2m == NULL)
        {
          if (Vars->Flag_Verbose) printf("Monitor_nD: %s cannot allocate memory for 2D array (%li). Skipping.\n", Vars->compcurname, 3*Vars->Coord_Bin[1]*Vars->Coord_Bin[2]*sizeof(double));
          /* comment out 'free memory' lines to avoid loosing arrays if
               'detector' structure is used by other instrument parts
          if (p0m != NULL) free(p0m);
          if (p1m != NULL) free(p1m);
          */
        }
        else
        {
          if (Vars->Flag_log != 0)
          {
            XY = FLT_MAX;
            for (i= 0; i < Vars->Coord_Bin[1]; i++)
              for (j= 0; j < Vars->Coord_Bin[2]; j++) /* search min of signal */
                if ((XY > Vars->Mon2D_p[i][j]) && (Vars->Mon2D_p[i][j]>0)) XY = Vars->Mon2D_p[i][j];
            if (XY <= 0) XY = -log(FLT_MAX)/log(10); else XY = log(XY)/log(10)-1;
          }
          for (i= 0; i < Vars->Coord_Bin[1]; i++)
          {
            for (j= 0; j < Vars->Coord_Bin[2]; j++)
            {
              long index;
              index = j + i*Vars->Coord_Bin[2];
              p0m[index] = Vars->Mon2D_N[i][j];
              p1m[index] = Vars->Mon2D_p[i][j];
              p2m[index] = Vars->Mon2D_p2[i][j];
              if (Vars->Flag_signal != DEFS->COORD_P && p0m[index] > 0)
              {
                  p1m[index] /= p0m[index];
                  p2m[index] /= p0m[index]*p0m[index];
              }

              if (Vars->Flag_log != 0)
              {
                if ((p1m[index] > 0) && (p2m[index] > 0))
                {
                  p2m[index] /= (p1m[index]*p1m[index]);
                  p1m[index] = log(p1m[index])/log(10);

                }
                else
                {
                  p1m[index] = XY;
                  p2m[index] = 0;
                }
              }
            }
          }
          if (strchr(Vars->Mon_File,'.') == NULL)
          { strcat(fname, "."); strcat(fname, Vars->Coord_Var[1]);
              strcat(fname, "_"); strcat(fname, Vars->Coord_Var[2]); }
          if (Vars->Flag_Verbose) printf("Monitor_nD: %s write monitor file %s 2D (%lix%li).\n", Vars->compcurname, fname, Vars->Coord_Bin[1], Vars->Coord_Bin[2]);

          min1d = Vars->Coord_Min[1];
          max1d = Vars->Coord_Max[1];
          if (min1d == max1d) max1d = min1d+1e-6;
          min2d = Vars->Coord_Min[2];
          max2d = Vars->Coord_Max[2];
          if (min2d == max2d) max2d = min2d+1e-6;
          strcpy(label, Vars->Monitor_Label);
          if (Vars->Coord_Bin[1]*Vars->Coord_Bin[2] > 1
           && Vars->Flag_signal == DEFS->COORD_P)
            strcat(label, " per bin");

          detector = mcdetector_out_2D(
            label,
            Vars->Coord_Label[1],
            Vars->Coord_Label[2],
            min1d, max1d,
            min2d, max2d,
            Vars->Coord_Bin[1],
            Vars->Coord_Bin[2],
            p0m,p1m,p2m,
            fname, Vars->compcurname, Vars->compcurpos);

          /* comment out 'free memory' lines to avoid loosing arrays if
               'detector' structure is used by other instrument parts
          if (p0m != NULL) free(p0m);
          if (p1m != NULL) free(p1m);
          if (p2m != NULL) free(p2m);
          */
        }
      }
      free(fname);
    }
    return(detector);
  } /* end Monitor_nD_Save */

/* ========================================================================= */
/* Monitor_nD_Finally: this routine is used to free memory                   */
/* ========================================================================= */

void Monitor_nD_Finally(MonitornD_Defines_type *DEFS,
  MonitornD_Variables_type *Vars)
  {
    int i;

    /* Now Free memory Mon2D.. */
    if ((Vars->Flag_Auto_Limits || Vars->Flag_List) && Vars->Coord_Number)
    { /* Dim : (Vars->Coord_Number+1)*Vars->Buffer_Block matrix (for p, dp) */
      if (Vars->Mon2D_Buffer != NULL) free(Vars->Mon2D_Buffer);
    }

    /* 1D and n1D case : Vars->Flag_Multiple */
    if (Vars->Flag_Multiple && Vars->Coord_Number)
    { /* Dim : Vars->Coord_Number*Vars->Coord_Bin[i] vectors */
      for (i= 0; i < Vars->Coord_Number; i++)
      {
        free(Vars->Mon2D_N[i]);
        free(Vars->Mon2D_p[i]);
        free(Vars->Mon2D_p2[i]);
      }
      free(Vars->Mon2D_N);
      free(Vars->Mon2D_p);
      free(Vars->Mon2D_p2);
    }


    /* 2D case : Vars->Coord_Number==2 and !Vars->Flag_Multiple and !Vars->Flag_List */
    if ((Vars->Coord_NumberNoPixel == 2) && !Vars->Flag_Multiple)
    { /* Dim : Vars->Coord_Bin[1]*Vars->Coord_Bin[2] matrix */
      for (i= 0; i < Vars->Coord_Bin[1]; i++)
      {
        free(Vars->Mon2D_N[i]);
        free(Vars->Mon2D_p[i]);
        free(Vars->Mon2D_p2[i]);
      }
      free(Vars->Mon2D_N);
      free(Vars->Mon2D_p);
      free(Vars->Mon2D_p2);
    }
  } /* end Monitor_nD_Finally */

/* ========================================================================= */
/* Monitor_nD_McDisplay: this routine is used to display component           */
/* ========================================================================= */

void Monitor_nD_McDisplay(MonitornD_Defines_type *DEFS,
  MonitornD_Variables_type *Vars)
  {
    double radius, h;
    double xmin;
    double xmax;
    double ymin;
    double ymax;
    double zmin;
    double zmax;
    int    i;
    double hdiv_min=-180, hdiv_max=180, vdiv_min=-90, vdiv_max=90;
    char   restricted = 0;

    radius = Vars->Sphere_Radius;
    h = Vars->Cylinder_Height;
    xmin = Vars->mxmin;
    xmax = Vars->mxmax;
    ymin = Vars->mymin;
    ymax = Vars->mymax;
    zmin = Vars->mzmin;
    zmax = Vars->mzmax;

    /* determine if there are angular limits set at start (no auto) in coord_types
     * cylinder/banana: look for hdiv
     * sphere: look for angle, radius (->atan2(val,radius)), hdiv, vdiv
     * this activates a 'restricted' flag, to draw a region as blades on cylinder/sphere
     */
    for (i= 0; i <= Vars->Coord_Number; i++)
    {
      int Set_Vars_Coord_Type;
      Set_Vars_Coord_Type = (Vars->Coord_Type[i] & (DEFS->COORD_LOG-1));
      if (Set_Vars_Coord_Type == DEFS->COORD_HDIV || Set_Vars_Coord_Type == DEFS->COORD_THETA)
      { hdiv_min = Vars->Coord_Min[i]; hdiv_max = Vars->Coord_Max[i]; restricted = 1; }
      else if (Set_Vars_Coord_Type == DEFS->COORD_VDIV || Set_Vars_Coord_Type == DEFS->COORD_PHI)
      { vdiv_min = Vars->Coord_Min[i]; vdiv_max = Vars->Coord_Max[i];restricted = 1;  }
      else if (Set_Vars_Coord_Type == DEFS->COORD_ANGLE)
      { hdiv_min = vdiv_min = Vars->Coord_Min[i];
        hdiv_max = vdiv_max = Vars->Coord_Max[i];
        restricted = 1; }
      else if (Set_Vars_Coord_Type == DEFS->COORD_RADIUS)
      { double angle;
        angle = RAD2DEG*atan2(Vars->Coord_Max[i], radius);
        hdiv_min = vdiv_min = angle;
        hdiv_max = vdiv_max = angle;
        restricted = 1; }
      else if (Set_Vars_Coord_Type == DEFS->COORD_Y && abs(Vars->Flag_Shape) == DEFS->SHAPE_SPHERE)
      {
        vdiv_min = atan2(ymin,radius)*RAD2DEG;
        vdiv_max = atan2(ymax,radius)*RAD2DEG;
        restricted = 1;
      }
    }
    /* full sphere */
    if ((!restricted && (abs(Vars->Flag_Shape) == DEFS->SHAPE_SPHERE))
    || abs(Vars->Flag_Shape) == DEFS->SHAPE_PREVIOUS)
    {
      mcdis_magnify("");
      mcdis_circle("xy",0,0,0,radius);
      mcdis_circle("xz",0,0,0,radius);
      mcdis_circle("yz",0,0,0,radius);
    }
    /* banana/cylinder/sphere portion */
    else
    if (restricted && ((abs(Vars->Flag_Shape) == DEFS->SHAPE_CYLIND)
                    || (abs(Vars->Flag_Shape) == DEFS->SHAPE_BANANA)
                    || (abs(Vars->Flag_Shape) == DEFS->SHAPE_SPHERE)))
    {
      int NH=24, NV=24;
      int ih, iv;
      double width, height;
      int issphere;
      issphere = (abs(Vars->Flag_Shape) == DEFS->SHAPE_SPHERE);
      width = (hdiv_max-hdiv_min)/NH;
      if (!issphere) NV=1; /* cylinder has vertical axis */
      else height= (vdiv_max-vdiv_min)/NV;
      
      /* check width and height of elements (sphere) to make sure the nb
         of plates remains limited */
      if (width < 10  && NH > 1) { width = 10;  NH=(hdiv_max-hdiv_min)/width; width=(hdiv_max-hdiv_min)/NH; }
      if (height < 10 && NV > 1) { height = 10; NV=(vdiv_max-vdiv_min)/height; height= (vdiv_max-vdiv_min)/NV; }
      
      mcdis_magnify("xyz");
      for(ih = 0; ih < NH; ih++)
        for(iv = 0; iv < NV; iv++)
        {
          double theta0, phi0, theta1, phi1;          /* angles in spherical coordinates */
          double x0,y0,z0,x1,y1,z1,x2,y2,z2,x3,y3,z3; /* vertices at plate edges */
          phi0 = (hdiv_min+ width*ih-90)*DEG2RAD;        /* in xz plane */
          phi1 = (hdiv_min+ width*(ih+1)-90)*DEG2RAD;
          if (issphere)
          {
            theta0= (vdiv_min+height* iv + 90)   *DEG2RAD; /* in vertical plane */
            theta1= (vdiv_min+height*(iv+1) + 90)*DEG2RAD;
            if (y0 < ymin) y0=ymin; 
            if (y0 > ymax) y0=ymax;
            if (y1 < ymin) y1=ymin; 
            if (y1 > ymax) y1=ymax;
            
            y0 = -radius*cos(theta0);            /* z with Z vertical */
            y1 = -radius*cos(theta1);
            if (y0 < ymin) y0=ymin;
            if (y0 > ymax) y0=ymax;
            if (y1 < ymin) y1=ymin;
            if (y1 > ymax) y1=ymax;
          } else {
            y0 = ymin;
            y1 = ymax;
            theta0=theta1=90*DEG2RAD;
          }

          x0 = radius*sin(theta0)*cos(phi0); /* x with Z vertical */
          z0 =-radius*sin(theta0)*sin(phi0); /* y with Z vertical */
          x1 = radius*sin(theta1)*cos(phi0); 
          z1 =-radius*sin(theta1)*sin(phi0);
          x2 = radius*sin(theta1)*cos(phi1); 
          z2 =-radius*sin(theta1)*sin(phi1);
          x3 = radius*sin(theta0)*cos(phi1); 
          z3 =-radius*sin(theta0)*sin(phi1);
          y2 = y1; y3 = y0;

          mcdis_multiline(5,
            x0,y0,z0,
            x1,y1,z1,
            x2,y2,z2,
            x3,y3,z3,
            x0,y0,z0);
        }
      if (Vars->Flag_mantid) {
	/* First define the base pixel type */
	double dt, dy;
	dt = (Vars->Coord_Max[1]-Vars->Coord_Min[1])/Vars->Coord_Bin[1];
	dy = (Vars->Coord_Max[2]-Vars->Coord_Min[2])/Vars->Coord_Bin[2];
	printf("MANTID_BANANA_DET:  %g, %g, %g, %g, %g, %li, %li, %g\n", radius, 
	       Vars->Coord_Min[1],Vars->Coord_Max[1], Vars->Coord_Min[2],Vars->Coord_Max[2], Vars->Coord_Bin[1], Vars->Coord_Bin[2], Vars->Coord_Min[4]); 
      }
    }
    /* disk (circle) */
    else
    if (abs(Vars->Flag_Shape) == DEFS->SHAPE_DISK)
    {
      mcdis_magnify("");
      mcdis_circle("xy",0,0,0,radius);
    }
    /* rectangle (square) */
    else
    if (abs(Vars->Flag_Shape) == DEFS->SHAPE_SQUARE)
    {
      mcdis_magnify("xy");
      mcdis_multiline(5, (double)xmin, (double)ymin, 0.0,
             (double)xmax, (double)ymin, 0.0,
             (double)xmax, (double)ymax, 0.0,
             (double)xmin, (double)ymax, 0.0,
             (double)xmin, (double)ymin, 0.0);
      
      if (Vars->Flag_mantid) {
	/* First define the base pixel type */
	double dx, dy;
	dx = (Vars->Coord_Max[1]-Vars->Coord_Min[1])/Vars->Coord_Bin[1];
	dy = (Vars->Coord_Max[2]-Vars->Coord_Min[2])/Vars->Coord_Bin[2];
	printf("MANTID_RECTANGULAR_DET:  %g, %g, %g, %g, %li, %li, %g\n", 
	       Vars->Coord_Min[1],Vars->Coord_Max[1], Vars->Coord_Min[2],Vars->Coord_Max[2], Vars->Coord_Bin[1], Vars->Coord_Bin[2], Vars->Coord_Min[4]);
      }
    }
    /* full cylinder/banana */
    else
    if (!restricted && ((abs(Vars->Flag_Shape) == DEFS->SHAPE_CYLIND) || (abs(Vars->Flag_Shape) == DEFS->SHAPE_BANANA)))
    {
      mcdis_magnify("xyz");
      mcdis_circle("xz", 0,  h/2.0, 0, radius);
      mcdis_circle("xz", 0, -h/2.0, 0, radius);
      mcdis_line(-radius, -h/2.0, 0, -radius, +h/2.0, 0);
      mcdis_line(+radius, -h/2.0, 0, +radius, +h/2.0, 0);
      mcdis_line(0, -h/2.0, -radius, 0, +h/2.0, -radius);
      mcdis_line(0, -h/2.0, +radius, 0, +h/2.0, +radius);
    }
    else
    /* box */
    if (abs(Vars->Flag_Shape) == DEFS->SHAPE_BOX)
    {
      mcdis_magnify("xyz");
      mcdis_multiline(5, xmin, ymin, zmin,
                   xmax, ymin, zmin,
                   xmax, ymax, zmin,
                   xmin, ymax, zmin,
                   xmin, ymin, zmin);
      mcdis_multiline(5, xmin, ymin, zmax,
                   xmax, ymin, zmax,
                   xmax, ymax, zmax,
                   xmin, ymax, zmax,
                   xmin, ymin, zmax);
      mcdis_line(xmin, ymin, zmin, xmin, ymin, zmax);
      mcdis_line(xmax, ymin, zmin, xmax, ymin, zmax);
      mcdis_line(xmin, ymax, zmin, xmin, ymax, zmax);
      mcdis_line(xmax, ymax, zmin, xmax, ymax, zmax);
    }
  } /* end Monitor_nD_McDisplay */

/* end of monitor_nd-lib.c */






/* ************************************************************************** */
/*             End of SHARE user declarations for all components              */
/* ************************************************************************** */


/* ********************** component definition declarations. **************** */

/* component source_arm=Progress_bar() [1] DECLARE */
/* Parameter definition for component type 'Progress_bar' */
struct _struct_Progress_bar_parameters {
  /* Component type 'Progress_bar' setting parameters */
  char profile[16384];
  MCNUM percent;
  MCNUM flag_save;
  MCNUM minutes;
  /* Component type 'Progress_bar' private parameters */
  /* Component type 'Progress_bar' DECLARE code stored as structure members */
#ifndef PROGRESS_BAR
#define PROGRESS_BAR
#else
#error Only one Progress_bar component may be used in an instrument definition.
#endif

double IntermediateCnts;
time_t StartTime;
time_t EndTime;
time_t CurrentTime;
}; /* _struct_Progress_bar_parameters */
typedef struct _struct_Progress_bar_parameters _class_Progress_bar_parameters;

/* Parameters for component type 'Progress_bar' */
struct _struct_Progress_bar {
  char     _name[256]; /* e.g. source_arm */
  char     _type[256]; /* Progress_bar */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_Progress_bar_parameters _parameters;
};
typedef struct _struct_Progress_bar _class_Progress_bar;
_class_Progress_bar _source_arm_var;
#pragma acc declare create ( _source_arm_var )

/* component source=Source_Maxwell_3() [2] DECLARE */
/* Parameter definition for component type 'Source_Maxwell_3' */
struct _struct_Source_Maxwell_3_parameters {
  /* Component type 'Source_Maxwell_3' setting parameters */
  MCNUM size;
  MCNUM yheight;
  MCNUM xwidth;
  MCNUM Lmin;
  MCNUM Lmax;
  MCNUM dist;
  MCNUM focus_xw;
  MCNUM focus_yh;
  MCNUM T1;
  MCNUM T2;
  MCNUM T3;
  MCNUM I1;
  MCNUM I2;
  MCNUM I3;
  long target_index;
  MCNUM lambda0;
  MCNUM dlambda;
  /* Component type 'Source_Maxwell_3' private parameters */
  MCNUM M;
  /* Component type 'Source_Maxwell_3' DECLARE code stored as structure members */
  double l_range, w_mult;
  double w_source, h_source;
}; /* _struct_Source_Maxwell_3_parameters */
typedef struct _struct_Source_Maxwell_3_parameters _class_Source_Maxwell_3_parameters;

/* Parameters for component type 'Source_Maxwell_3' */
struct _struct_Source_Maxwell_3 {
  char     _name[256]; /* e.g. source */
  char     _type[256]; /* Source_Maxwell_3 */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_Source_Maxwell_3_parameters _parameters;
};
typedef struct _struct_Source_Maxwell_3 _class_Source_Maxwell_3;
_class_Source_Maxwell_3 _source_var;
#pragma acc declare create ( _source_var )

/* component PSDbefore_guides=PSD_monitor() [3] DECLARE */
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
  long nowritefile;
  /* Component type 'PSD_monitor' private parameters */
  /* Component type 'PSD_monitor' DECLARE code stored as structure members */
  DArray2d PSD_N;
  DArray2d PSD_p;
  DArray2d PSD_p2;
}; /* _struct_PSD_monitor_parameters */
typedef struct _struct_PSD_monitor_parameters _class_PSD_monitor_parameters;

/* Parameters for component type 'PSD_monitor' */
struct _struct_PSD_monitor {
  char     _name[256]; /* e.g. PSDbefore_guides */
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
_class_PSD_monitor _PSDbefore_guides_var;
#pragma acc declare create ( _PSDbefore_guides_var )

/* component l_mon_source=L_monitor() [4] DECLARE */
/* Parameter definition for component type 'L_monitor' */
struct _struct_L_monitor_parameters {
  /* Component type 'L_monitor' setting parameters */
  MCNUM nL;
  char filename[16384];
  MCNUM xmin;
  MCNUM xmax;
  MCNUM ymin;
  MCNUM ymax;
  MCNUM xwidth;
  MCNUM yheight;
  MCNUM Lmin;
  MCNUM Lmax;
  MCNUM restore_neutron;
  /* Component type 'L_monitor' private parameters */
  /* Component type 'L_monitor' DECLARE code stored as structure members */
  DArray1d L_N;
  DArray1d L_p;
  DArray1d L_p2;
}; /* _struct_L_monitor_parameters */
typedef struct _struct_L_monitor_parameters _class_L_monitor_parameters;

/* Parameters for component type 'L_monitor' */
struct _struct_L_monitor {
  char     _name[256]; /* e.g. l_mon_source */
  char     _type[256]; /* L_monitor */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_L_monitor_parameters _parameters;
};
typedef struct _struct_L_monitor _class_L_monitor;
_class_L_monitor _l_mon_source_var;
#pragma acc declare create ( _l_mon_source_var )

/* component guide1=Guide() [5] DECLARE */
/* Parameter definition for component type 'Guide' */
struct _struct_Guide_parameters {
  /* Component type 'Guide' setting parameters */
  char reflect[16384];
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
  /* Component type 'Guide' private parameters */
  /* Component type 'Guide' DECLARE code stored as structure members */
t_Table pTable;
int table_present;
}; /* _struct_Guide_parameters */
typedef struct _struct_Guide_parameters _class_Guide_parameters;

/* Parameters for component type 'Guide' */
struct _struct_Guide {
  char     _name[256]; /* e.g. guide1 */
  char     _type[256]; /* Guide */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_Guide_parameters _parameters;
};
typedef struct _struct_Guide _class_Guide;
_class_Guide _guide1_var;
#pragma acc declare create ( _guide1_var )

_class_PSD_monitor _PSDbefore_curve_var;
#pragma acc declare create ( _PSDbefore_curve_var )

/* component guide2=Bender() [7] DECLARE */
/* Parameter definition for component type 'Bender' */
struct _struct_Bender_parameters {
  /* Component type 'Bender' setting parameters */
  MCNUM w;
  MCNUM h;
  MCNUM r;
  MCNUM Win;
  MCNUM k;
  MCNUM d;
  MCNUM l;
  MCNUM R0a;
  MCNUM Qca;
  MCNUM alphaa;
  MCNUM ma;
  MCNUM Wa;
  MCNUM R0i;
  MCNUM Qci;
  MCNUM alphai;
  MCNUM mi;
  MCNUM Wi;
  MCNUM R0s;
  MCNUM Qcs;
  MCNUM alphas;
  MCNUM ms;
  MCNUM Ws;
  /* Component type 'Bender' private parameters */
  /* Component type 'Bender' DECLARE code stored as structure members */
double bk, mWin;
}; /* _struct_Bender_parameters */
typedef struct _struct_Bender_parameters _class_Bender_parameters;

/* Parameters for component type 'Bender' */
struct _struct_Bender {
  char     _name[256]; /* e.g. guide2 */
  char     _type[256]; /* Bender */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_Bender_parameters _parameters;
};
typedef struct _struct_Bender _class_Bender;
_class_Bender _guide2_var;
#pragma acc declare create ( _guide2_var )

_class_PSD_monitor _PSDafter_curve_var;
#pragma acc declare create ( _PSDafter_curve_var )

_class_Guide _bunker_var;
#pragma acc declare create ( _bunker_var )

_class_Guide _guide3_var;
#pragma acc declare create ( _guide3_var )

_class_Guide _guide4_var;
#pragma acc declare create ( _guide4_var )

/* component window1=Al_window() [12] DECLARE */
/* Parameter definition for component type 'Al_window' */
struct _struct_Al_window_parameters {
  /* Component type 'Al_window' setting parameters */
  MCNUM thickness;
}; /* _struct_Al_window_parameters */
typedef struct _struct_Al_window_parameters _class_Al_window_parameters;

/* Parameters for component type 'Al_window' */
struct _struct_Al_window {
  char     _name[256]; /* e.g. window1 */
  char     _type[256]; /* Al_window */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_Al_window_parameters _parameters;
};
typedef struct _struct_Al_window _class_Al_window;
_class_Al_window _window1_var;
#pragma acc declare create ( _window1_var )

/* component ydist_fluxpos=PSDlin_monitor() [13] DECLARE */
/* Parameter definition for component type 'PSDlin_monitor' */
struct _struct_PSDlin_monitor_parameters {
  /* Component type 'PSDlin_monitor' setting parameters */
  MCNUM nx;
  char filename[16384];
  MCNUM xmin;
  MCNUM xmax;
  MCNUM ymin;
  MCNUM ymax;
  MCNUM xwidth;
  MCNUM yheight;
  MCNUM restore_neutron;
  /* Component type 'PSDlin_monitor' private parameters */
  /* Component type 'PSDlin_monitor' DECLARE code stored as structure members */
  DArray1d PSDlin_N;
  DArray1d PSDlin_p;
  DArray1d PSDlin_p2;
}; /* _struct_PSDlin_monitor_parameters */
typedef struct _struct_PSDlin_monitor_parameters _class_PSDlin_monitor_parameters;

/* Parameters for component type 'PSDlin_monitor' */
struct _struct_PSDlin_monitor {
  char     _name[256]; /* e.g. ydist_fluxpos */
  char     _type[256]; /* PSDlin_monitor */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_PSDlin_monitor_parameters _parameters;
};
typedef struct _struct_PSDlin_monitor _class_PSDlin_monitor;
_class_PSDlin_monitor _ydist_fluxpos_var;
#pragma acc declare create ( _ydist_fluxpos_var )

_class_PSD_monitor _PSD_fluxpos_var;
#pragma acc declare create ( _PSD_fluxpos_var )

_class_PSDlin_monitor _xdist_flux_pos_var;
#pragma acc declare create ( _xdist_flux_pos_var )

_class_PSD_monitor _PSD_fluxposB_var;
#pragma acc declare create ( _PSD_fluxposB_var )

_class_Al_window _window2_var;
#pragma acc declare create ( _window2_var )

/* component in_slit=Slit() [18] DECLARE */
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
  char     _name[256]; /* e.g. in_slit */
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
_class_Slit _in_slit_var;
#pragma acc declare create ( _in_slit_var )

_class_L_monitor _lambda_in_var;
#pragma acc declare create ( _lambda_in_var )

/* component sma=Arm() [20] DECLARE */
/* Parameter definition for component type 'Arm' */
struct _struct_Arm_parameters {
  char Arm_has_no_parameters;
}; /* _struct_Arm_parameters */
typedef struct _struct_Arm_parameters _class_Arm_parameters;

/* Parameters for component type 'Arm' */
struct _struct_Arm {
  char     _name[256]; /* e.g. sma */
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
_class_Arm _sma_var;
#pragma acc declare create ( _sma_var )

/* component foc_mono=Monochromator_2foc() [21] DECLARE */
/* Parameter definition for component type 'Monochromator_2foc' */
struct _struct_Monochromator_2foc_parameters {
  /* Component type 'Monochromator_2foc' setting parameters */
  char reflect[16384];
  MCNUM zwidth;
  MCNUM yheight;
  MCNUM gap;
  MCNUM NH;
  MCNUM NV;
  MCNUM mosaich;
  MCNUM mosaicv;
  MCNUM r0;
  MCNUM Q;
  MCNUM RV;
  MCNUM RH;
  MCNUM DM;
  MCNUM mosaic;
  MCNUM width;
  MCNUM height;
  MCNUM verbose;
  /* Component type 'Monochromator_2foc' private parameters */
  /* Component type 'Monochromator_2foc' DECLARE code stored as structure members */
double mos_y;                         
double mos_z;
double mono_Q;
double SlabWidth;
double SlabHeight;
t_Table rTable;
}; /* _struct_Monochromator_2foc_parameters */
typedef struct _struct_Monochromator_2foc_parameters _class_Monochromator_2foc_parameters;

/* Parameters for component type 'Monochromator_2foc' */
struct _struct_Monochromator_2foc {
  char     _name[256]; /* e.g. foc_mono */
  char     _type[256]; /* Monochromator_2foc */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_Monochromator_2foc_parameters _parameters;
};
typedef struct _struct_Monochromator_2foc _class_Monochromator_2foc;
_class_Monochromator_2foc _foc_mono_var;
#pragma acc declare create ( _foc_mono_var )

_class_Arm _msa_var;
#pragma acc declare create ( _msa_var )

_class_Slit _out1_slit_var;
#pragma acc declare create ( _out1_slit_var )

_class_Slit _Amoin_slit_var;
#pragma acc declare create ( _Amoin_slit_var )

_class_Slit _Bmoin_slit_var;
#pragma acc declare create ( _Bmoin_slit_var )

_class_Slit _out2_slit_var;
#pragma acc declare create ( _out2_slit_var )

_class_PSD_monitor _PSD_sample_var;
#pragma acc declare create ( _PSD_sample_var )

_class_L_monitor _lambda_sample_var;
#pragma acc declare create ( _lambda_sample_var )

_class_Arm _sa_arm_var;
#pragma acc declare create ( _sa_arm_var )

/* component sample=PowderN() [30] DECLARE */
/* Parameter definition for component type 'PowderN' */
struct _struct_PowderN_parameters {
  /* Component type 'PowderN' setting parameters */
  char reflections[16384];
  char geometry[16384];
  MCNUM* format;
  MCNUM radius;
  MCNUM yheight;
  MCNUM xwidth;
  MCNUM zdepth;
  MCNUM thickness;
  MCNUM pack;
  MCNUM Vc;
  MCNUM sigma_abs;
  MCNUM sigma_inc;
  MCNUM delta_d_d;
  MCNUM p_inc;
  MCNUM p_transmit;
  MCNUM DW;
  MCNUM nb_atoms;
  MCNUM d_phi;
  MCNUM p_interact;
  MCNUM concentric;
  MCNUM density;
  MCNUM weight;
  MCNUM barns;
  MCNUM Strain;
  MCNUM focus_flip;
  /* Component type 'PowderN' private parameters */
  /* Component type 'PowderN' DECLARE code stored as structure members */
  struct line_info_struct line_info;
  double *columns;
#ifndef USE_PGI
  off_struct offdata;
#endif
}; /* _struct_PowderN_parameters */
typedef struct _struct_PowderN_parameters _class_PowderN_parameters;

/* Parameters for component type 'PowderN' */
struct _struct_PowderN {
  char     _name[256]; /* e.g. sample */
  char     _type[256]; /* PowderN */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_PowderN_parameters _parameters;
};
typedef struct _struct_PowderN _class_PowderN;
_class_PowderN _sample_var;
#pragma acc declare create ( _sample_var )

/* component STOP=Beamstop() [31] DECLARE */
/* Parameter definition for component type 'Beamstop' */
struct _struct_Beamstop_parameters {
  /* Component type 'Beamstop' setting parameters */
  MCNUM xmin;
  MCNUM xmax;
  MCNUM ymin;
  MCNUM ymax;
  MCNUM xwidth;
  MCNUM yheight;
  MCNUM radius;
}; /* _struct_Beamstop_parameters */
typedef struct _struct_Beamstop_parameters _class_Beamstop_parameters;

/* Parameters for component type 'Beamstop' */
struct _struct_Beamstop {
  char     _name[256]; /* e.g. STOP */
  char     _type[256]; /* Beamstop */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_Beamstop_parameters _parameters;
};
typedef struct _struct_Beamstop _class_Beamstop;
_class_Beamstop _STOP_var;
#pragma acc declare create ( _STOP_var )

/* component Detector=Monitor_nD() [32] DECLARE */
/* Parameter definition for component type 'Monitor_nD' */
struct _struct_Monitor_nD_parameters {
  /* Component type 'Monitor_nD' setting parameters */
  double user1;
  double user2;
  double user3;
  MCNUM xwidth;
  MCNUM yheight;
  MCNUM zdepth;
  MCNUM xmin;
  MCNUM xmax;
  MCNUM ymin;
  MCNUM ymax;
  MCNUM zmin;
  MCNUM zmax;
  MCNUM bins;
  MCNUM min;
  MCNUM max;
  MCNUM restore_neutron;
  MCNUM radius;
  char options[16384];
  char filename[16384];
  char geometry[16384];
  char username1[16384];
  char username2[16384];
  char username3[16384];
  /* Component type 'Monitor_nD' private parameters */
  /* Component type 'Monitor_nD' DECLARE code stored as structure members */
  MonitornD_Defines_type DEFS;
  MonitornD_Variables_type Vars;
  MCDETECTOR detector;
#ifndef USE_PGI
  off_struct offdata;
#endif
}; /* _struct_Monitor_nD_parameters */
typedef struct _struct_Monitor_nD_parameters _class_Monitor_nD_parameters;

/* Parameters for component type 'Monitor_nD' */
struct _struct_Monitor_nD {
  char     _name[256]; /* e.g. Detector */
  char     _type[256]; /* Monitor_nD */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_Monitor_nD_parameters _parameters;
};
typedef struct _struct_Monitor_nD _class_Monitor_nD;
_class_Monitor_nD _Detector_var;
#pragma acc declare create ( _Detector_var )

int mcNUMCOMP = 32;

/* User declarations from instrument definition. Can define functions. */
  double mono_q = 1.8734;
  double OMA;  
  double RV;
  double y_mono = 0.025;
  double NV = 5;
  double d_phi_0;
  double TTM;
  double sample_radius = 0.008/2;
  double sample_height = 0.03;
  double can_radius = 0.0083/2;
  double can_height = 0.0303;
  double can_thick = 0.00015;
  
  /******Mirrorvalues*****/
  
  double alpha;
  double Qc=0.0217;
  double R0=0.995;
  double Mvalue=1.9;
  double W=1.0/250.0;
  
  double alpha_curve;
  double Qc_curve=0.0217;
  double R0_curve= 0.995;
  double Mvalue_curve=2.1;
  double W_curve=1.0/250.0;
  
  double ldiff=0.05;
  /* Curved guide element angle*/
  double angleGuideCurved;


#undef compcurname
#undef compcurtype
#undef compcurindex
/* end of instrument 'PSI_DMC' and components DECLARE */

/* *****************************************************************************
* instrument 'PSI_DMC' and components INITIALISE
***************************************************************************** */

/* component source_arm=Progress_bar() SETTING, POSITION/ROTATION */
int _source_arm_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_source_arm_setpos] component source_arm=Progress_bar() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/misc/Progress_bar.comp:57]");
  stracpy(_source_arm_var._name, "source_arm", 16384);
  stracpy(_source_arm_var._type, "Progress_bar", 16384);
  _source_arm_var._index=1;
  if("NULL" && strlen("NULL"))
    stracpy(_source_arm_var._parameters.profile, "NULL" ? "NULL" : "", 16384);
  else 
  _source_arm_var._parameters.profile[0]='\0';
  _source_arm_var._parameters.percent = 10;
  _source_arm_var._parameters.flag_save = 0;
  _source_arm_var._parameters.minutes = 0;


  /* component source_arm=Progress_bar() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(_source_arm_var._rotation_absolute,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_copy(_source_arm_var._rotation_relative, _source_arm_var._rotation_absolute);
    _source_arm_var._rotation_is_identity =  rot_test_identity(_source_arm_var._rotation_relative);
    _source_arm_var._position_absolute = coords_set(
      0, 0, 0);
    tc1 = coords_neg(_source_arm_var._position_absolute);
    _source_arm_var._position_relative = rot_apply(_source_arm_var._rotation_absolute, tc1);
  } /* source_arm=Progress_bar() AT ROTATED */
  DEBUG_COMPONENT("source_arm", _source_arm_var._position_absolute, _source_arm_var._rotation_absolute);
  instrument->_position_absolute[1] = _source_arm_var._position_absolute;
  instrument->_position_relative[1] = _source_arm_var._position_relative;
  instrument->counter_N[1]  = instrument->counter_P[1] = instrument->counter_P2[1] = 0;
  instrument->counter_AbsorbProp[1]= 0;
  return(0);
} /* _source_arm_setpos */

/* component source=Source_Maxwell_3() SETTING, POSITION/ROTATION */
int _source_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_source_setpos] component source=Source_Maxwell_3() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/sources/Source_Maxwell_3.comp:86]");
  stracpy(_source_var._name, "source", 16384);
  stracpy(_source_var._type, "Source_Maxwell_3", 16384);
  _source_var._index=2;
  _source_var._parameters.size = 0;
  _source_var._parameters.yheight = 0.156;
  _source_var._parameters.xwidth = 0.126;
  _source_var._parameters.Lmin = instrument->_parameters.lambda - ldiff / 2;
  _source_var._parameters.Lmax = instrument->_parameters.lambda + ldiff / 2;
  _source_var._parameters.dist = 1.5;
  _source_var._parameters.focus_xw = 0.02;
  _source_var._parameters.focus_yh = 0.12;
  _source_var._parameters.T1 = 296.16;
  _source_var._parameters.T2 = 40.68;
  _source_var._parameters.T3 = 300;
  _source_var._parameters.I1 = 8.5E11;
  _source_var._parameters.I2 = 5.2E11;
  _source_var._parameters.I3 = 0;
  _source_var._parameters.target_index = + 1;
  _source_var._parameters.lambda0 = 0;
  _source_var._parameters.dlambda = 0;


  /* component source=Source_Maxwell_3() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (0)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _source_arm_var._rotation_absolute, _source_var._rotation_absolute);
    rot_transpose(_source_arm_var._rotation_absolute, tr1);
    rot_mul(_source_var._rotation_absolute, tr1, _source_var._rotation_relative);
    _source_var._rotation_is_identity =  rot_test_identity(_source_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0);
    rot_transpose(_source_arm_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _source_var._position_absolute = coords_add(_source_arm_var._position_absolute, tc2);
    tc1 = coords_sub(_source_arm_var._position_absolute, _source_var._position_absolute);
    _source_var._position_relative = rot_apply(_source_var._rotation_absolute, tc1);
  } /* source=Source_Maxwell_3() AT ROTATED */
  DEBUG_COMPONENT("source", _source_var._position_absolute, _source_var._rotation_absolute);
  instrument->_position_absolute[2] = _source_var._position_absolute;
  instrument->_position_relative[2] = _source_var._position_relative;
  instrument->counter_N[2]  = instrument->counter_P[2] = instrument->counter_P2[2] = 0;
  instrument->counter_AbsorbProp[2]= 0;
  return(0);
} /* _source_setpos */

/* component PSDbefore_guides=PSD_monitor() SETTING, POSITION/ROTATION */
int _PSDbefore_guides_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_PSDbefore_guides_setpos] component PSDbefore_guides=PSD_monitor() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/PSD_monitor.comp:62]");
  stracpy(_PSDbefore_guides_var._name, "PSDbefore_guides", 16384);
  stracpy(_PSDbefore_guides_var._type, "PSD_monitor", 16384);
  _PSDbefore_guides_var._index=3;
  _PSDbefore_guides_var._parameters.nx = 128;
  _PSDbefore_guides_var._parameters.ny = 128;
  if("PSDbefore_guides" && strlen("PSDbefore_guides"))
    stracpy(_PSDbefore_guides_var._parameters.filename, "PSDbefore_guides" ? "PSDbefore_guides" : "", 16384);
  else 
  _PSDbefore_guides_var._parameters.filename[0]='\0';
  _PSDbefore_guides_var._parameters.xmin = -0.05;
  _PSDbefore_guides_var._parameters.xmax = 0.05;
  _PSDbefore_guides_var._parameters.ymin = -0.05;
  _PSDbefore_guides_var._parameters.ymax = 0.05;
  _PSDbefore_guides_var._parameters.xwidth = 0.02;
  _PSDbefore_guides_var._parameters.yheight = 0.12;
  _PSDbefore_guides_var._parameters.restore_neutron = 0;
  _PSDbefore_guides_var._parameters.nowritefile = 0;


  /* component PSDbefore_guides=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _source_arm_var._rotation_absolute, _PSDbefore_guides_var._rotation_absolute);
    rot_transpose(_source_var._rotation_absolute, tr1);
    rot_mul(_PSDbefore_guides_var._rotation_absolute, tr1, _PSDbefore_guides_var._rotation_relative);
    _PSDbefore_guides_var._rotation_is_identity =  rot_test_identity(_PSDbefore_guides_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 1.49999);
    rot_transpose(_source_arm_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _PSDbefore_guides_var._position_absolute = coords_add(_source_arm_var._position_absolute, tc2);
    tc1 = coords_sub(_source_var._position_absolute, _PSDbefore_guides_var._position_absolute);
    _PSDbefore_guides_var._position_relative = rot_apply(_PSDbefore_guides_var._rotation_absolute, tc1);
  } /* PSDbefore_guides=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("PSDbefore_guides", _PSDbefore_guides_var._position_absolute, _PSDbefore_guides_var._rotation_absolute);
  instrument->_position_absolute[3] = _PSDbefore_guides_var._position_absolute;
  instrument->_position_relative[3] = _PSDbefore_guides_var._position_relative;
  instrument->counter_N[3]  = instrument->counter_P[3] = instrument->counter_P2[3] = 0;
  instrument->counter_AbsorbProp[3]= 0;
  return(0);
} /* _PSDbefore_guides_setpos */

/* component l_mon_source=L_monitor() SETTING, POSITION/ROTATION */
int _l_mon_source_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_l_mon_source_setpos] component l_mon_source=L_monitor() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/L_monitor.comp:65]");
  stracpy(_l_mon_source_var._name, "l_mon_source", 16384);
  stracpy(_l_mon_source_var._type, "L_monitor", 16384);
  _l_mon_source_var._index=4;
  _l_mon_source_var._parameters.nL = 101;
  if("lmonsource.dat" && strlen("lmonsource.dat"))
    stracpy(_l_mon_source_var._parameters.filename, "lmonsource.dat" ? "lmonsource.dat" : "", 16384);
  else 
  _l_mon_source_var._parameters.filename[0]='\0';
  _l_mon_source_var._parameters.xmin = -0.05;
  _l_mon_source_var._parameters.xmax = 0.05;
  _l_mon_source_var._parameters.ymin = -0.05;
  _l_mon_source_var._parameters.ymax = 0.05;
  _l_mon_source_var._parameters.xwidth = 0.02;
  _l_mon_source_var._parameters.yheight = 0.12;
  _l_mon_source_var._parameters.Lmin = 0;
  _l_mon_source_var._parameters.Lmax = 20;
  _l_mon_source_var._parameters.restore_neutron = 0;


  /* component l_mon_source=L_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _PSDbefore_guides_var._rotation_absolute, _l_mon_source_var._rotation_absolute);
    rot_transpose(_PSDbefore_guides_var._rotation_absolute, tr1);
    rot_mul(_l_mon_source_var._rotation_absolute, tr1, _l_mon_source_var._rotation_relative);
    _l_mon_source_var._rotation_is_identity =  rot_test_identity(_l_mon_source_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 1e-9);
    rot_transpose(_PSDbefore_guides_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _l_mon_source_var._position_absolute = coords_add(_PSDbefore_guides_var._position_absolute, tc2);
    tc1 = coords_sub(_PSDbefore_guides_var._position_absolute, _l_mon_source_var._position_absolute);
    _l_mon_source_var._position_relative = rot_apply(_l_mon_source_var._rotation_absolute, tc1);
  } /* l_mon_source=L_monitor() AT ROTATED */
  DEBUG_COMPONENT("l_mon_source", _l_mon_source_var._position_absolute, _l_mon_source_var._rotation_absolute);
  instrument->_position_absolute[4] = _l_mon_source_var._position_absolute;
  instrument->_position_relative[4] = _l_mon_source_var._position_relative;
  instrument->counter_N[4]  = instrument->counter_P[4] = instrument->counter_P2[4] = 0;
  instrument->counter_AbsorbProp[4]= 0;
  return(0);
} /* _l_mon_source_setpos */

/* component guide1=Guide() SETTING, POSITION/ROTATION */
int _guide1_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_guide1_setpos] component guide1=Guide() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Guide.comp:74]");
  stracpy(_guide1_var._name, "guide1", 16384);
  stracpy(_guide1_var._type, "Guide", 16384);
  _guide1_var._index=5;
  _guide1_var._parameters.reflect[0]='\0';
  _guide1_var._parameters.w1 = 0.02;
  _guide1_var._parameters.h1 = 0.12;
  _guide1_var._parameters.w2 = 0.02;
  _guide1_var._parameters.h2 = 0.12;
  _guide1_var._parameters.l = 4.66;
  _guide1_var._parameters.R0 = R0;
  _guide1_var._parameters.Qc = Qc;
  _guide1_var._parameters.alpha = alpha;
  _guide1_var._parameters.m = 1.8;
  _guide1_var._parameters.W = W;


  /* component guide1=Guide() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (0)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _source_arm_var._rotation_absolute, _guide1_var._rotation_absolute);
    rot_transpose(_l_mon_source_var._rotation_absolute, tr1);
    rot_mul(_guide1_var._rotation_absolute, tr1, _guide1_var._rotation_relative);
    _guide1_var._rotation_is_identity =  rot_test_identity(_guide1_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 1.50);
    rot_transpose(_source_arm_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _guide1_var._position_absolute = coords_add(_source_arm_var._position_absolute, tc2);
    tc1 = coords_sub(_l_mon_source_var._position_absolute, _guide1_var._position_absolute);
    _guide1_var._position_relative = rot_apply(_guide1_var._rotation_absolute, tc1);
  } /* guide1=Guide() AT ROTATED */
  DEBUG_COMPONENT("guide1", _guide1_var._position_absolute, _guide1_var._rotation_absolute);
  instrument->_position_absolute[5] = _guide1_var._position_absolute;
  instrument->_position_relative[5] = _guide1_var._position_relative;
  instrument->counter_N[5]  = instrument->counter_P[5] = instrument->counter_P2[5] = 0;
  instrument->counter_AbsorbProp[5]= 0;
  return(0);
} /* _guide1_setpos */

/* component PSDbefore_curve=PSD_monitor() SETTING, POSITION/ROTATION */
int _PSDbefore_curve_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_PSDbefore_curve_setpos] component PSDbefore_curve=PSD_monitor() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/PSD_monitor.comp:62]");
  stracpy(_PSDbefore_curve_var._name, "PSDbefore_curve", 16384);
  stracpy(_PSDbefore_curve_var._type, "PSD_monitor", 16384);
  _PSDbefore_curve_var._index=6;
  _PSDbefore_curve_var._parameters.nx = 128;
  _PSDbefore_curve_var._parameters.ny = 128;
  if("PSDbefore_curve" && strlen("PSDbefore_curve"))
    stracpy(_PSDbefore_curve_var._parameters.filename, "PSDbefore_curve" ? "PSDbefore_curve" : "", 16384);
  else 
  _PSDbefore_curve_var._parameters.filename[0]='\0';
  _PSDbefore_curve_var._parameters.xmin = -0.05;
  _PSDbefore_curve_var._parameters.xmax = 0.05;
  _PSDbefore_curve_var._parameters.ymin = -0.05;
  _PSDbefore_curve_var._parameters.ymax = 0.05;
  _PSDbefore_curve_var._parameters.xwidth = 0.02;
  _PSDbefore_curve_var._parameters.yheight = 0.12;
  _PSDbefore_curve_var._parameters.restore_neutron = 0;
  _PSDbefore_curve_var._parameters.nowritefile = 0;


  /* component PSDbefore_curve=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _guide1_var._rotation_absolute, _PSDbefore_curve_var._rotation_absolute);
    rot_transpose(_guide1_var._rotation_absolute, tr1);
    rot_mul(_PSDbefore_curve_var._rotation_absolute, tr1, _PSDbefore_curve_var._rotation_relative);
    _PSDbefore_curve_var._rotation_is_identity =  rot_test_identity(_PSDbefore_curve_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 4.664);
    rot_transpose(_guide1_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _PSDbefore_curve_var._position_absolute = coords_add(_guide1_var._position_absolute, tc2);
    tc1 = coords_sub(_guide1_var._position_absolute, _PSDbefore_curve_var._position_absolute);
    _PSDbefore_curve_var._position_relative = rot_apply(_PSDbefore_curve_var._rotation_absolute, tc1);
  } /* PSDbefore_curve=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("PSDbefore_curve", _PSDbefore_curve_var._position_absolute, _PSDbefore_curve_var._rotation_absolute);
  instrument->_position_absolute[6] = _PSDbefore_curve_var._position_absolute;
  instrument->_position_relative[6] = _PSDbefore_curve_var._position_relative;
  instrument->counter_N[6]  = instrument->counter_P[6] = instrument->counter_P2[6] = 0;
  instrument->counter_AbsorbProp[6]= 0;
  return(0);
} /* _PSDbefore_curve_setpos */

/* component guide2=Bender() SETTING, POSITION/ROTATION */
int _guide2_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_guide2_setpos] component guide2=Bender() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Bender.comp:112]");
  stracpy(_guide2_var._name, "guide2", 16384);
  stracpy(_guide2_var._type, "Bender", 16384);
  _guide2_var._index=7;
  _guide2_var._parameters.w = 0.02;
  _guide2_var._parameters.h = 0.12;
  _guide2_var._parameters.r = 3612;
  _guide2_var._parameters.Win = 0.04;
  _guide2_var._parameters.k = 1;
  _guide2_var._parameters.d = 0.001;
  _guide2_var._parameters.l = 20;
  _guide2_var._parameters.R0a = R0_curve;
  _guide2_var._parameters.Qca = Qc_curve;
  _guide2_var._parameters.alphaa = alpha_curve;
  _guide2_var._parameters.ma = Mvalue_curve;
  _guide2_var._parameters.Wa = W_curve;
  _guide2_var._parameters.R0i = R0_curve;
  _guide2_var._parameters.Qci = Qc_curve;
  _guide2_var._parameters.alphai = alpha_curve;
  _guide2_var._parameters.mi = 1;
  _guide2_var._parameters.Wi = W_curve;
  _guide2_var._parameters.R0s = R0_curve;
  _guide2_var._parameters.Qcs = Qc_curve;
  _guide2_var._parameters.alphas = alpha_curve;
  _guide2_var._parameters.ms = Mvalue_curve;
  _guide2_var._parameters.Ws = W_curve;


  /* component guide2=Bender() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _guide1_var._rotation_absolute, _guide2_var._rotation_absolute);
    rot_transpose(_PSDbefore_curve_var._rotation_absolute, tr1);
    rot_mul(_guide2_var._rotation_absolute, tr1, _guide2_var._rotation_relative);
    _guide2_var._rotation_is_identity =  rot_test_identity(_guide2_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 4.69);
    rot_transpose(_guide1_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _guide2_var._position_absolute = coords_add(_guide1_var._position_absolute, tc2);
    tc1 = coords_sub(_PSDbefore_curve_var._position_absolute, _guide2_var._position_absolute);
    _guide2_var._position_relative = rot_apply(_guide2_var._rotation_absolute, tc1);
  } /* guide2=Bender() AT ROTATED */
  DEBUG_COMPONENT("guide2", _guide2_var._position_absolute, _guide2_var._rotation_absolute);
  instrument->_position_absolute[7] = _guide2_var._position_absolute;
  instrument->_position_relative[7] = _guide2_var._position_relative;
  instrument->counter_N[7]  = instrument->counter_P[7] = instrument->counter_P2[7] = 0;
  instrument->counter_AbsorbProp[7]= 0;
  return(0);
} /* _guide2_setpos */

/* component PSDafter_curve=PSD_monitor() SETTING, POSITION/ROTATION */
int _PSDafter_curve_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_PSDafter_curve_setpos] component PSDafter_curve=PSD_monitor() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/PSD_monitor.comp:62]");
  stracpy(_PSDafter_curve_var._name, "PSDafter_curve", 16384);
  stracpy(_PSDafter_curve_var._type, "PSD_monitor", 16384);
  _PSDafter_curve_var._index=8;
  _PSDafter_curve_var._parameters.nx = 128;
  _PSDafter_curve_var._parameters.ny = 128;
  if("PSDafter_curve" && strlen("PSDafter_curve"))
    stracpy(_PSDafter_curve_var._parameters.filename, "PSDafter_curve" ? "PSDafter_curve" : "", 16384);
  else 
  _PSDafter_curve_var._parameters.filename[0]='\0';
  _PSDafter_curve_var._parameters.xmin = -0.05;
  _PSDafter_curve_var._parameters.xmax = 0.05;
  _PSDafter_curve_var._parameters.ymin = -0.05;
  _PSDafter_curve_var._parameters.ymax = 0.05;
  _PSDafter_curve_var._parameters.xwidth = 0.02;
  _PSDafter_curve_var._parameters.yheight = 0.12;
  _PSDafter_curve_var._parameters.restore_neutron = 0;
  _PSDafter_curve_var._parameters.nowritefile = 0;


  /* component PSDafter_curve=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _guide2_var._rotation_absolute, _PSDafter_curve_var._rotation_absolute);
    rot_transpose(_guide2_var._rotation_absolute, tr1);
    rot_mul(_PSDafter_curve_var._rotation_absolute, tr1, _PSDafter_curve_var._rotation_relative);
    _PSDafter_curve_var._rotation_is_identity =  rot_test_identity(_PSDafter_curve_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 20.0001);
    rot_transpose(_guide2_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _PSDafter_curve_var._position_absolute = coords_add(_guide2_var._position_absolute, tc2);
    tc1 = coords_sub(_guide2_var._position_absolute, _PSDafter_curve_var._position_absolute);
    _PSDafter_curve_var._position_relative = rot_apply(_PSDafter_curve_var._rotation_absolute, tc1);
  } /* PSDafter_curve=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("PSDafter_curve", _PSDafter_curve_var._position_absolute, _PSDafter_curve_var._rotation_absolute);
  instrument->_position_absolute[8] = _PSDafter_curve_var._position_absolute;
  instrument->_position_relative[8] = _PSDafter_curve_var._position_relative;
  instrument->counter_N[8]  = instrument->counter_P[8] = instrument->counter_P2[8] = 0;
  instrument->counter_AbsorbProp[8]= 0;
  return(0);
} /* _PSDafter_curve_setpos */

/* component bunker=Guide() SETTING, POSITION/ROTATION */
int _bunker_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_bunker_setpos] component bunker=Guide() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Guide.comp:74]");
  stracpy(_bunker_var._name, "bunker", 16384);
  stracpy(_bunker_var._type, "Guide", 16384);
  _bunker_var._index=9;
  _bunker_var._parameters.reflect[0]='\0';
  _bunker_var._parameters.w1 = 0.02;
  _bunker_var._parameters.h1 = .12;
  _bunker_var._parameters.w2 = 0.02;
  _bunker_var._parameters.h2 = .12;
  _bunker_var._parameters.l = 3.43;
  _bunker_var._parameters.R0 = R0;
  _bunker_var._parameters.Qc = Qc;
  _bunker_var._parameters.alpha = alpha;
  _bunker_var._parameters.m = 1.6;
  _bunker_var._parameters.W = W;


  /* component bunker=Guide() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (0)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _guide2_var._rotation_absolute, _bunker_var._rotation_absolute);
    rot_transpose(_PSDafter_curve_var._rotation_absolute, tr1);
    rot_mul(_bunker_var._rotation_absolute, tr1, _bunker_var._rotation_relative);
    _bunker_var._rotation_is_identity =  rot_test_identity(_bunker_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 20.1502);
    rot_transpose(_guide2_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _bunker_var._position_absolute = coords_add(_guide2_var._position_absolute, tc2);
    tc1 = coords_sub(_PSDafter_curve_var._position_absolute, _bunker_var._position_absolute);
    _bunker_var._position_relative = rot_apply(_bunker_var._rotation_absolute, tc1);
  } /* bunker=Guide() AT ROTATED */
  DEBUG_COMPONENT("bunker", _bunker_var._position_absolute, _bunker_var._rotation_absolute);
  instrument->_position_absolute[9] = _bunker_var._position_absolute;
  instrument->_position_relative[9] = _bunker_var._position_relative;
  instrument->counter_N[9]  = instrument->counter_P[9] = instrument->counter_P2[9] = 0;
  instrument->counter_AbsorbProp[9]= 0;
  return(0);
} /* _bunker_setpos */

/* component guide3=Guide() SETTING, POSITION/ROTATION */
int _guide3_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_guide3_setpos] component guide3=Guide() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Guide.comp:74]");
  stracpy(_guide3_var._name, "guide3", 16384);
  stracpy(_guide3_var._type, "Guide", 16384);
  _guide3_var._index=10;
  _guide3_var._parameters.reflect[0]='\0';
  _guide3_var._parameters.w1 = 0.02;
  _guide3_var._parameters.h1 = .12;
  _guide3_var._parameters.w2 = 0.02;
  _guide3_var._parameters.h2 = .12;
  _guide3_var._parameters.l = 12.275;
  _guide3_var._parameters.R0 = R0;
  _guide3_var._parameters.Qc = Qc;
  _guide3_var._parameters.alpha = alpha;
  _guide3_var._parameters.m = 1.6;
  _guide3_var._parameters.W = W;


  /* component guide3=Guide() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (0)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _bunker_var._rotation_absolute, _guide3_var._rotation_absolute);
    rot_transpose(_bunker_var._rotation_absolute, tr1);
    rot_mul(_guide3_var._rotation_absolute, tr1, _guide3_var._rotation_relative);
    _guide3_var._rotation_is_identity =  rot_test_identity(_guide3_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 3.56);
    rot_transpose(_bunker_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _guide3_var._position_absolute = coords_add(_bunker_var._position_absolute, tc2);
    tc1 = coords_sub(_bunker_var._position_absolute, _guide3_var._position_absolute);
    _guide3_var._position_relative = rot_apply(_guide3_var._rotation_absolute, tc1);
  } /* guide3=Guide() AT ROTATED */
  DEBUG_COMPONENT("guide3", _guide3_var._position_absolute, _guide3_var._rotation_absolute);
  instrument->_position_absolute[10] = _guide3_var._position_absolute;
  instrument->_position_relative[10] = _guide3_var._position_relative;
  instrument->counter_N[10]  = instrument->counter_P[10] = instrument->counter_P2[10] = 0;
  instrument->counter_AbsorbProp[10]= 0;
  return(0);
} /* _guide3_setpos */

/* component guide4=Guide() SETTING, POSITION/ROTATION */
int _guide4_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_guide4_setpos] component guide4=Guide() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Guide.comp:74]");
  stracpy(_guide4_var._name, "guide4", 16384);
  stracpy(_guide4_var._type, "Guide", 16384);
  _guide4_var._index=11;
  _guide4_var._parameters.reflect[0]='\0';
  _guide4_var._parameters.w1 = 0.02;
  _guide4_var._parameters.h1 = .12;
  _guide4_var._parameters.w2 = 0.02;
  _guide4_var._parameters.h2 = .12;
  _guide4_var._parameters.l = 5.66;
  _guide4_var._parameters.R0 = R0;
  _guide4_var._parameters.Qc = Qc;
  _guide4_var._parameters.alpha = alpha;
  _guide4_var._parameters.m = 1.6;
  _guide4_var._parameters.W = W;


  /* component guide4=Guide() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (0)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _guide3_var._rotation_absolute, _guide4_var._rotation_absolute);
    rot_transpose(_guide3_var._rotation_absolute, tr1);
    rot_mul(_guide4_var._rotation_absolute, tr1, _guide4_var._rotation_relative);
    _guide4_var._rotation_is_identity =  rot_test_identity(_guide4_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 15.8555);
    rot_transpose(_bunker_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _guide4_var._position_absolute = coords_add(_bunker_var._position_absolute, tc2);
    tc1 = coords_sub(_guide3_var._position_absolute, _guide4_var._position_absolute);
    _guide4_var._position_relative = rot_apply(_guide4_var._rotation_absolute, tc1);
  } /* guide4=Guide() AT ROTATED */
  DEBUG_COMPONENT("guide4", _guide4_var._position_absolute, _guide4_var._rotation_absolute);
  instrument->_position_absolute[11] = _guide4_var._position_absolute;
  instrument->_position_relative[11] = _guide4_var._position_relative;
  instrument->counter_N[11]  = instrument->counter_P[11] = instrument->counter_P2[11] = 0;
  instrument->counter_AbsorbProp[11]= 0;
  return(0);
} /* _guide4_setpos */

/* component window1=Al_window() SETTING, POSITION/ROTATION */
int _window1_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_window1_setpos] component window1=Al_window() SETTING [Al_window:0]");
  stracpy(_window1_var._name, "window1", 16384);
  stracpy(_window1_var._type, "Al_window", 16384);
  _window1_var._index=12;
  _window1_var._parameters.thickness = 0.002;

  /* component window1=Al_window() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _guide4_var._rotation_absolute, _window1_var._rotation_absolute);
    rot_transpose(_guide4_var._rotation_absolute, tr1);
    rot_mul(_window1_var._rotation_absolute, tr1, _window1_var._rotation_relative);
    _window1_var._rotation_is_identity =  rot_test_identity(_window1_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 5.66 + 1e-9);
    rot_transpose(_guide4_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _window1_var._position_absolute = coords_add(_guide4_var._position_absolute, tc2);
    tc1 = coords_sub(_guide4_var._position_absolute, _window1_var._position_absolute);
    _window1_var._position_relative = rot_apply(_window1_var._rotation_absolute, tc1);
  } /* window1=Al_window() AT ROTATED */
  DEBUG_COMPONENT("window1", _window1_var._position_absolute, _window1_var._rotation_absolute);
  instrument->_position_absolute[12] = _window1_var._position_absolute;
  instrument->_position_relative[12] = _window1_var._position_relative;
  instrument->counter_N[12]  = instrument->counter_P[12] = instrument->counter_P2[12] = 0;
  instrument->counter_AbsorbProp[12]= 0;
  return(0);
} /* _window1_setpos */

/* component ydist_fluxpos=PSDlin_monitor() SETTING, POSITION/ROTATION */
int _ydist_fluxpos_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_ydist_fluxpos_setpos] component ydist_fluxpos=PSDlin_monitor() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/PSDlin_monitor.comp:58]");
  stracpy(_ydist_fluxpos_var._name, "ydist_fluxpos", 16384);
  stracpy(_ydist_fluxpos_var._type, "PSDlin_monitor", 16384);
  _ydist_fluxpos_var._index=13;
  _ydist_fluxpos_var._parameters.nx = 11;
  if("ydist_fluxpos.dat" && strlen("ydist_fluxpos.dat"))
    stracpy(_ydist_fluxpos_var._parameters.filename, "ydist_fluxpos.dat" ? "ydist_fluxpos.dat" : "", 16384);
  else 
  _ydist_fluxpos_var._parameters.filename[0]='\0';
  _ydist_fluxpos_var._parameters.xmin = -0.05;
  _ydist_fluxpos_var._parameters.xmax = 0.05;
  _ydist_fluxpos_var._parameters.ymin = -0.05;
  _ydist_fluxpos_var._parameters.ymax = 0.05;
  _ydist_fluxpos_var._parameters.xwidth = 0.120;
  _ydist_fluxpos_var._parameters.yheight = 0.02;
  _ydist_fluxpos_var._parameters.restore_neutron = 0;


  /* component ydist_fluxpos=PSDlin_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (0)*DEG2RAD, (90)*DEG2RAD);
    rot_mul(tr1, _window1_var._rotation_absolute, _ydist_fluxpos_var._rotation_absolute);
    rot_transpose(_window1_var._rotation_absolute, tr1);
    rot_mul(_ydist_fluxpos_var._rotation_absolute, tr1, _ydist_fluxpos_var._rotation_relative);
    _ydist_fluxpos_var._rotation_is_identity =  rot_test_identity(_ydist_fluxpos_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 5.66 + 1e-8 + 0.01);
    rot_transpose(_guide4_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _ydist_fluxpos_var._position_absolute = coords_add(_guide4_var._position_absolute, tc2);
    tc1 = coords_sub(_window1_var._position_absolute, _ydist_fluxpos_var._position_absolute);
    _ydist_fluxpos_var._position_relative = rot_apply(_ydist_fluxpos_var._rotation_absolute, tc1);
  } /* ydist_fluxpos=PSDlin_monitor() AT ROTATED */
  DEBUG_COMPONENT("ydist_fluxpos", _ydist_fluxpos_var._position_absolute, _ydist_fluxpos_var._rotation_absolute);
  instrument->_position_absolute[13] = _ydist_fluxpos_var._position_absolute;
  instrument->_position_relative[13] = _ydist_fluxpos_var._position_relative;
  instrument->counter_N[13]  = instrument->counter_P[13] = instrument->counter_P2[13] = 0;
  instrument->counter_AbsorbProp[13]= 0;
  return(0);
} /* _ydist_fluxpos_setpos */

/* component PSD_fluxpos=PSD_monitor() SETTING, POSITION/ROTATION */
int _PSD_fluxpos_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_PSD_fluxpos_setpos] component PSD_fluxpos=PSD_monitor() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/PSD_monitor.comp:62]");
  stracpy(_PSD_fluxpos_var._name, "PSD_fluxpos", 16384);
  stracpy(_PSD_fluxpos_var._type, "PSD_monitor", 16384);
  _PSD_fluxpos_var._index=14;
  _PSD_fluxpos_var._parameters.nx = 100;
  _PSD_fluxpos_var._parameters.ny = 100;
  if("xdist_fluxposy.dat" && strlen("xdist_fluxposy.dat"))
    stracpy(_PSD_fluxpos_var._parameters.filename, "xdist_fluxposy.dat" ? "xdist_fluxposy.dat" : "", 16384);
  else 
  _PSD_fluxpos_var._parameters.filename[0]='\0';
  _PSD_fluxpos_var._parameters.xmin = -0.05;
  _PSD_fluxpos_var._parameters.xmax = 0.05;
  _PSD_fluxpos_var._parameters.ymin = -0.05;
  _PSD_fluxpos_var._parameters.ymax = 0.05;
  _PSD_fluxpos_var._parameters.xwidth = 0.02;
  _PSD_fluxpos_var._parameters.yheight = 0.12;
  _PSD_fluxpos_var._parameters.restore_neutron = 0;
  _PSD_fluxpos_var._parameters.nowritefile = 0;


  /* component PSD_fluxpos=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _guide4_var._rotation_absolute, _PSD_fluxpos_var._rotation_absolute);
    rot_transpose(_ydist_fluxpos_var._rotation_absolute, tr1);
    rot_mul(_PSD_fluxpos_var._rotation_absolute, tr1, _PSD_fluxpos_var._rotation_relative);
    _PSD_fluxpos_var._rotation_is_identity =  rot_test_identity(_PSD_fluxpos_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 5.66 + 1e-7 + 0.01);
    rot_transpose(_guide4_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _PSD_fluxpos_var._position_absolute = coords_add(_guide4_var._position_absolute, tc2);
    tc1 = coords_sub(_ydist_fluxpos_var._position_absolute, _PSD_fluxpos_var._position_absolute);
    _PSD_fluxpos_var._position_relative = rot_apply(_PSD_fluxpos_var._rotation_absolute, tc1);
  } /* PSD_fluxpos=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("PSD_fluxpos", _PSD_fluxpos_var._position_absolute, _PSD_fluxpos_var._rotation_absolute);
  instrument->_position_absolute[14] = _PSD_fluxpos_var._position_absolute;
  instrument->_position_relative[14] = _PSD_fluxpos_var._position_relative;
  instrument->counter_N[14]  = instrument->counter_P[14] = instrument->counter_P2[14] = 0;
  instrument->counter_AbsorbProp[14]= 0;
  return(0);
} /* _PSD_fluxpos_setpos */

/* component xdist_flux_pos=PSDlin_monitor() SETTING, POSITION/ROTATION */
int _xdist_flux_pos_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_xdist_flux_pos_setpos] component xdist_flux_pos=PSDlin_monitor() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/PSDlin_monitor.comp:58]");
  stracpy(_xdist_flux_pos_var._name, "xdist_flux_pos", 16384);
  stracpy(_xdist_flux_pos_var._type, "PSDlin_monitor", 16384);
  _xdist_flux_pos_var._index=15;
  _xdist_flux_pos_var._parameters.nx = 11;
  if("xdist_fluxpos.dat" && strlen("xdist_fluxpos.dat"))
    stracpy(_xdist_flux_pos_var._parameters.filename, "xdist_fluxpos.dat" ? "xdist_fluxpos.dat" : "", 16384);
  else 
  _xdist_flux_pos_var._parameters.filename[0]='\0';
  _xdist_flux_pos_var._parameters.xmin = -0.05;
  _xdist_flux_pos_var._parameters.xmax = 0.05;
  _xdist_flux_pos_var._parameters.ymin = -0.05;
  _xdist_flux_pos_var._parameters.ymax = 0.05;
  _xdist_flux_pos_var._parameters.xwidth = 0.020;
  _xdist_flux_pos_var._parameters.yheight = 0.12;
  _xdist_flux_pos_var._parameters.restore_neutron = 0;


  /* component xdist_flux_pos=PSDlin_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _PSD_fluxpos_var._rotation_absolute, _xdist_flux_pos_var._rotation_absolute);
    rot_transpose(_PSD_fluxpos_var._rotation_absolute, tr1);
    rot_mul(_xdist_flux_pos_var._rotation_absolute, tr1, _xdist_flux_pos_var._rotation_relative);
    _xdist_flux_pos_var._rotation_is_identity =  rot_test_identity(_xdist_flux_pos_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 1e-9);
    rot_transpose(_PSD_fluxpos_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _xdist_flux_pos_var._position_absolute = coords_add(_PSD_fluxpos_var._position_absolute, tc2);
    tc1 = coords_sub(_PSD_fluxpos_var._position_absolute, _xdist_flux_pos_var._position_absolute);
    _xdist_flux_pos_var._position_relative = rot_apply(_xdist_flux_pos_var._rotation_absolute, tc1);
  } /* xdist_flux_pos=PSDlin_monitor() AT ROTATED */
  DEBUG_COMPONENT("xdist_flux_pos", _xdist_flux_pos_var._position_absolute, _xdist_flux_pos_var._rotation_absolute);
  instrument->_position_absolute[15] = _xdist_flux_pos_var._position_absolute;
  instrument->_position_relative[15] = _xdist_flux_pos_var._position_relative;
  instrument->counter_N[15]  = instrument->counter_P[15] = instrument->counter_P2[15] = 0;
  instrument->counter_AbsorbProp[15]= 0;
  return(0);
} /* _xdist_flux_pos_setpos */

/* component PSD_fluxposB=PSD_monitor() SETTING, POSITION/ROTATION */
int _PSD_fluxposB_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_PSD_fluxposB_setpos] component PSD_fluxposB=PSD_monitor() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/PSD_monitor.comp:62]");
  stracpy(_PSD_fluxposB_var._name, "PSD_fluxposB", 16384);
  stracpy(_PSD_fluxposB_var._type, "PSD_monitor", 16384);
  _PSD_fluxposB_var._index=16;
  _PSD_fluxposB_var._parameters.nx = 100;
  _PSD_fluxposB_var._parameters.ny = 100;
  if("PSD_fluxposB.dat" && strlen("PSD_fluxposB.dat"))
    stracpy(_PSD_fluxposB_var._parameters.filename, "PSD_fluxposB.dat" ? "PSD_fluxposB.dat" : "", 16384);
  else 
  _PSD_fluxposB_var._parameters.filename[0]='\0';
  _PSD_fluxposB_var._parameters.xmin = -0.05;
  _PSD_fluxposB_var._parameters.xmax = 0.05;
  _PSD_fluxposB_var._parameters.ymin = -0.05;
  _PSD_fluxposB_var._parameters.ymax = 0.05;
  _PSD_fluxposB_var._parameters.xwidth = 0.02;
  _PSD_fluxposB_var._parameters.yheight = 0.12;
  _PSD_fluxposB_var._parameters.restore_neutron = 0;
  _PSD_fluxposB_var._parameters.nowritefile = 0;


  /* component PSD_fluxposB=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _guide4_var._rotation_absolute, _PSD_fluxposB_var._rotation_absolute);
    rot_transpose(_xdist_flux_pos_var._rotation_absolute, tr1);
    rot_mul(_PSD_fluxposB_var._rotation_absolute, tr1, _PSD_fluxposB_var._rotation_relative);
    _PSD_fluxposB_var._rotation_is_identity =  rot_test_identity(_PSD_fluxposB_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 6.24 -1e-7 -0.01);
    rot_transpose(_guide4_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _PSD_fluxposB_var._position_absolute = coords_add(_guide4_var._position_absolute, tc2);
    tc1 = coords_sub(_xdist_flux_pos_var._position_absolute, _PSD_fluxposB_var._position_absolute);
    _PSD_fluxposB_var._position_relative = rot_apply(_PSD_fluxposB_var._rotation_absolute, tc1);
  } /* PSD_fluxposB=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("PSD_fluxposB", _PSD_fluxposB_var._position_absolute, _PSD_fluxposB_var._rotation_absolute);
  instrument->_position_absolute[16] = _PSD_fluxposB_var._position_absolute;
  instrument->_position_relative[16] = _PSD_fluxposB_var._position_relative;
  instrument->counter_N[16]  = instrument->counter_P[16] = instrument->counter_P2[16] = 0;
  instrument->counter_AbsorbProp[16]= 0;
  return(0);
} /* _PSD_fluxposB_setpos */

/* component window2=Al_window() SETTING, POSITION/ROTATION */
int _window2_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_window2_setpos] component window2=Al_window() SETTING [Al_window:0]");
  stracpy(_window2_var._name, "window2", 16384);
  stracpy(_window2_var._type, "Al_window", 16384);
  _window2_var._index=17;
  _window2_var._parameters.thickness = 0.002;

  /* component window2=Al_window() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _PSD_fluxposB_var._rotation_absolute, _window2_var._rotation_absolute);
    rot_transpose(_PSD_fluxposB_var._rotation_absolute, tr1);
    rot_mul(_window2_var._rotation_absolute, tr1, _window2_var._rotation_relative);
    _window2_var._rotation_is_identity =  rot_test_identity(_window2_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 1e-9);
    rot_transpose(_PSD_fluxposB_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _window2_var._position_absolute = coords_add(_PSD_fluxposB_var._position_absolute, tc2);
    tc1 = coords_sub(_PSD_fluxposB_var._position_absolute, _window2_var._position_absolute);
    _window2_var._position_relative = rot_apply(_window2_var._rotation_absolute, tc1);
  } /* window2=Al_window() AT ROTATED */
  DEBUG_COMPONENT("window2", _window2_var._position_absolute, _window2_var._rotation_absolute);
  instrument->_position_absolute[17] = _window2_var._position_absolute;
  instrument->_position_relative[17] = _window2_var._position_relative;
  instrument->counter_N[17]  = instrument->counter_P[17] = instrument->counter_P2[17] = 0;
  instrument->counter_AbsorbProp[17]= 0;
  return(0);
} /* _window2_setpos */

/* component in_slit=Slit() SETTING, POSITION/ROTATION */
int _in_slit_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_in_slit_setpos] component in_slit=Slit() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Slit.comp:50]");
  stracpy(_in_slit_var._name, "in_slit", 16384);
  stracpy(_in_slit_var._type, "Slit", 16384);
  _in_slit_var._index=18;
  _in_slit_var._parameters.xmin = -0.01;
  _in_slit_var._parameters.xmax = 0.01;
  _in_slit_var._parameters.ymin = -0.06;
  _in_slit_var._parameters.ymax = 0.06;
  _in_slit_var._parameters.radius = 0;
  _in_slit_var._parameters.xwidth = 0;
  _in_slit_var._parameters.yheight = 0;

  /* component in_slit=Slit() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _window2_var._rotation_absolute, _in_slit_var._rotation_absolute);
    rot_transpose(_window2_var._rotation_absolute, tr1);
    rot_mul(_in_slit_var._rotation_absolute, tr1, _in_slit_var._rotation_relative);
    _in_slit_var._rotation_is_identity =  rot_test_identity(_in_slit_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.0021);
    rot_transpose(_window2_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _in_slit_var._position_absolute = coords_add(_window2_var._position_absolute, tc2);
    tc1 = coords_sub(_window2_var._position_absolute, _in_slit_var._position_absolute);
    _in_slit_var._position_relative = rot_apply(_in_slit_var._rotation_absolute, tc1);
  } /* in_slit=Slit() AT ROTATED */
  DEBUG_COMPONENT("in_slit", _in_slit_var._position_absolute, _in_slit_var._rotation_absolute);
  instrument->_position_absolute[18] = _in_slit_var._position_absolute;
  instrument->_position_relative[18] = _in_slit_var._position_relative;
  instrument->counter_N[18]  = instrument->counter_P[18] = instrument->counter_P2[18] = 0;
  instrument->counter_AbsorbProp[18]= 0;
  return(0);
} /* _in_slit_setpos */

/* component lambda_in=L_monitor() SETTING, POSITION/ROTATION */
int _lambda_in_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_lambda_in_setpos] component lambda_in=L_monitor() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/L_monitor.comp:65]");
  stracpy(_lambda_in_var._name, "lambda_in", 16384);
  stracpy(_lambda_in_var._type, "L_monitor", 16384);
  _lambda_in_var._index=19;
  _lambda_in_var._parameters.nL = 128;
  if("L_in.dat" && strlen("L_in.dat"))
    stracpy(_lambda_in_var._parameters.filename, "L_in.dat" ? "L_in.dat" : "", 16384);
  else 
  _lambda_in_var._parameters.filename[0]='\0';
  _lambda_in_var._parameters.xmin = -0.011;
  _lambda_in_var._parameters.xmax = 0.011;
  _lambda_in_var._parameters.ymin = -0.061;
  _lambda_in_var._parameters.ymax = 0.061;
  _lambda_in_var._parameters.xwidth = 0;
  _lambda_in_var._parameters.yheight = 0;
  _lambda_in_var._parameters.Lmin = 0;
  _lambda_in_var._parameters.Lmax = 2 * instrument->_parameters.lambda;
  _lambda_in_var._parameters.restore_neutron = 0;


  /* component lambda_in=L_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _in_slit_var._rotation_absolute, _lambda_in_var._rotation_absolute);
    rot_transpose(_in_slit_var._rotation_absolute, tr1);
    rot_mul(_lambda_in_var._rotation_absolute, tr1, _lambda_in_var._rotation_relative);
    _lambda_in_var._rotation_is_identity =  rot_test_identity(_lambda_in_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.001);
    rot_transpose(_in_slit_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _lambda_in_var._position_absolute = coords_add(_in_slit_var._position_absolute, tc2);
    tc1 = coords_sub(_in_slit_var._position_absolute, _lambda_in_var._position_absolute);
    _lambda_in_var._position_relative = rot_apply(_lambda_in_var._rotation_absolute, tc1);
  } /* lambda_in=L_monitor() AT ROTATED */
  DEBUG_COMPONENT("lambda_in", _lambda_in_var._position_absolute, _lambda_in_var._rotation_absolute);
  instrument->_position_absolute[19] = _lambda_in_var._position_absolute;
  instrument->_position_relative[19] = _lambda_in_var._position_relative;
  instrument->counter_N[19]  = instrument->counter_P[19] = instrument->counter_P2[19] = 0;
  instrument->counter_AbsorbProp[19]= 0;
  return(0);
} /* _lambda_in_setpos */

/* component sma=Arm() SETTING, POSITION/ROTATION */
int _sma_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_sma_setpos] component sma=Arm() SETTING [Arm:0]");
  stracpy(_sma_var._name, "sma", 16384);
  stracpy(_sma_var._type, "Arm", 16384);
  _sma_var._index=20;
  /* component sma=Arm() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (OMA)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _in_slit_var._rotation_absolute, _sma_var._rotation_absolute);
    rot_transpose(_lambda_in_var._rotation_absolute, tr1);
    rot_mul(_sma_var._rotation_absolute, tr1, _sma_var._rotation_relative);
    _sma_var._rotation_is_identity =  rot_test_identity(_sma_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.65);
    rot_transpose(_in_slit_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _sma_var._position_absolute = coords_add(_in_slit_var._position_absolute, tc2);
    tc1 = coords_sub(_lambda_in_var._position_absolute, _sma_var._position_absolute);
    _sma_var._position_relative = rot_apply(_sma_var._rotation_absolute, tc1);
  } /* sma=Arm() AT ROTATED */
  DEBUG_COMPONENT("sma", _sma_var._position_absolute, _sma_var._rotation_absolute);
  instrument->_position_absolute[20] = _sma_var._position_absolute;
  instrument->_position_relative[20] = _sma_var._position_relative;
  instrument->counter_N[20]  = instrument->counter_P[20] = instrument->counter_P2[20] = 0;
  instrument->counter_AbsorbProp[20]= 0;
  return(0);
} /* _sma_setpos */

/* component foc_mono=Monochromator_2foc() SETTING, POSITION/ROTATION */
int _foc_mono_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_foc_mono_setpos] component foc_mono=Monochromator_2foc() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/contrib/Monochromator_2foc.comp:100]");
  stracpy(_foc_mono_var._name, "foc_mono", 16384);
  stracpy(_foc_mono_var._type, "Monochromator_2foc", 16384);
  _foc_mono_var._index=21;
  _foc_mono_var._parameters.reflect[0]='\0';
  _foc_mono_var._parameters.zwidth = 0.05;
  _foc_mono_var._parameters.yheight = 0.025;
  _foc_mono_var._parameters.gap = 0.0005;
  _foc_mono_var._parameters.NH = 1;
  _foc_mono_var._parameters.NV = 5;
  _foc_mono_var._parameters.mosaich = 38;
  _foc_mono_var._parameters.mosaicv = 38;
  _foc_mono_var._parameters.r0 = 0.7;
  _foc_mono_var._parameters.Q = mono_q;
  _foc_mono_var._parameters.RV = RV;
  _foc_mono_var._parameters.RH = 0;
  _foc_mono_var._parameters.DM = 0;
  _foc_mono_var._parameters.mosaic = 0;
  _foc_mono_var._parameters.width = 0;
  _foc_mono_var._parameters.height = 0;
  _foc_mono_var._parameters.verbose = 0;


  /* component foc_mono=Monochromator_2foc() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _sma_var._rotation_absolute, _foc_mono_var._rotation_absolute);
    rot_transpose(_sma_var._rotation_absolute, tr1);
    rot_mul(_foc_mono_var._rotation_absolute, tr1, _foc_mono_var._rotation_relative);
    _foc_mono_var._rotation_is_identity =  rot_test_identity(_foc_mono_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0);
    rot_transpose(_sma_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _foc_mono_var._position_absolute = coords_add(_sma_var._position_absolute, tc2);
    tc1 = coords_sub(_sma_var._position_absolute, _foc_mono_var._position_absolute);
    _foc_mono_var._position_relative = rot_apply(_foc_mono_var._rotation_absolute, tc1);
  } /* foc_mono=Monochromator_2foc() AT ROTATED */
  DEBUG_COMPONENT("foc_mono", _foc_mono_var._position_absolute, _foc_mono_var._rotation_absolute);
  instrument->_position_absolute[21] = _foc_mono_var._position_absolute;
  instrument->_position_relative[21] = _foc_mono_var._position_relative;
  instrument->counter_N[21]  = instrument->counter_P[21] = instrument->counter_P2[21] = 0;
  instrument->counter_AbsorbProp[21]= 0;
  return(0);
} /* _foc_mono_setpos */

/* component msa=Arm() SETTING, POSITION/ROTATION */
int _msa_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_msa_setpos] component msa=Arm() SETTING [Arm:0]");
  stracpy(_msa_var._name, "msa", 16384);
  stracpy(_msa_var._type, "Arm", 16384);
  _msa_var._index=22;
  /* component msa=Arm() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (TTM)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _in_slit_var._rotation_absolute, _msa_var._rotation_absolute);
    rot_transpose(_foc_mono_var._rotation_absolute, tr1);
    rot_mul(_msa_var._rotation_absolute, tr1, _msa_var._rotation_relative);
    _msa_var._rotation_is_identity =  rot_test_identity(_msa_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0);
    rot_transpose(_sma_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _msa_var._position_absolute = coords_add(_sma_var._position_absolute, tc2);
    tc1 = coords_sub(_foc_mono_var._position_absolute, _msa_var._position_absolute);
    _msa_var._position_relative = rot_apply(_msa_var._rotation_absolute, tc1);
  } /* msa=Arm() AT ROTATED */
  DEBUG_COMPONENT("msa", _msa_var._position_absolute, _msa_var._rotation_absolute);
  instrument->_position_absolute[22] = _msa_var._position_absolute;
  instrument->_position_relative[22] = _msa_var._position_relative;
  instrument->counter_N[22]  = instrument->counter_P[22] = instrument->counter_P2[22] = 0;
  instrument->counter_AbsorbProp[22]= 0;
  return(0);
} /* _msa_setpos */

/* component out1_slit=Slit() SETTING, POSITION/ROTATION */
int _out1_slit_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_out1_slit_setpos] component out1_slit=Slit() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Slit.comp:50]");
  stracpy(_out1_slit_var._name, "out1_slit", 16384);
  stracpy(_out1_slit_var._type, "Slit", 16384);
  _out1_slit_var._index=23;
  _out1_slit_var._parameters.xmin = -0.01;
  _out1_slit_var._parameters.xmax = 0.01;
  _out1_slit_var._parameters.ymin = -0.06;
  _out1_slit_var._parameters.ymax = 0.06;
  _out1_slit_var._parameters.radius = 0;
  _out1_slit_var._parameters.xwidth = 0;
  _out1_slit_var._parameters.yheight = 0;

  /* component out1_slit=Slit() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (0)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _msa_var._rotation_absolute, _out1_slit_var._rotation_absolute);
    rot_transpose(_msa_var._rotation_absolute, tr1);
    rot_mul(_out1_slit_var._rotation_absolute, tr1, _out1_slit_var._rotation_relative);
    _out1_slit_var._rotation_is_identity =  rot_test_identity(_out1_slit_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.2);
    rot_transpose(_msa_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _out1_slit_var._position_absolute = coords_add(_msa_var._position_absolute, tc2);
    tc1 = coords_sub(_msa_var._position_absolute, _out1_slit_var._position_absolute);
    _out1_slit_var._position_relative = rot_apply(_out1_slit_var._rotation_absolute, tc1);
  } /* out1_slit=Slit() AT ROTATED */
  DEBUG_COMPONENT("out1_slit", _out1_slit_var._position_absolute, _out1_slit_var._rotation_absolute);
  instrument->_position_absolute[23] = _out1_slit_var._position_absolute;
  instrument->_position_relative[23] = _out1_slit_var._position_relative;
  instrument->counter_N[23]  = instrument->counter_P[23] = instrument->counter_P2[23] = 0;
  instrument->counter_AbsorbProp[23]= 0;
  return(0);
} /* _out1_slit_setpos */

/* component Amoin_slit=Slit() SETTING, POSITION/ROTATION */
int _Amoin_slit_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Amoin_slit_setpos] component Amoin_slit=Slit() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Slit.comp:50]");
  stracpy(_Amoin_slit_var._name, "Amoin_slit", 16384);
  stracpy(_Amoin_slit_var._type, "Slit", 16384);
  _Amoin_slit_var._index=24;
  _Amoin_slit_var._parameters.xmin = -0.01;
  _Amoin_slit_var._parameters.xmax = 0.01;
  _Amoin_slit_var._parameters.ymin = -0.06;
  _Amoin_slit_var._parameters.ymax = 0.06;
  _Amoin_slit_var._parameters.radius = 0;
  _Amoin_slit_var._parameters.xwidth = 0;
  _Amoin_slit_var._parameters.yheight = 0;

  /* component Amoin_slit=Slit() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (0)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _msa_var._rotation_absolute, _Amoin_slit_var._rotation_absolute);
    rot_transpose(_out1_slit_var._rotation_absolute, tr1);
    rot_mul(_Amoin_slit_var._rotation_absolute, tr1, _Amoin_slit_var._rotation_relative);
    _Amoin_slit_var._rotation_is_identity =  rot_test_identity(_Amoin_slit_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.325);
    rot_transpose(_msa_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _Amoin_slit_var._position_absolute = coords_add(_msa_var._position_absolute, tc2);
    tc1 = coords_sub(_out1_slit_var._position_absolute, _Amoin_slit_var._position_absolute);
    _Amoin_slit_var._position_relative = rot_apply(_Amoin_slit_var._rotation_absolute, tc1);
  } /* Amoin_slit=Slit() AT ROTATED */
  DEBUG_COMPONENT("Amoin_slit", _Amoin_slit_var._position_absolute, _Amoin_slit_var._rotation_absolute);
  instrument->_position_absolute[24] = _Amoin_slit_var._position_absolute;
  instrument->_position_relative[24] = _Amoin_slit_var._position_relative;
  instrument->counter_N[24]  = instrument->counter_P[24] = instrument->counter_P2[24] = 0;
  instrument->counter_AbsorbProp[24]= 0;
  return(0);
} /* _Amoin_slit_setpos */

/* component Bmoin_slit=Slit() SETTING, POSITION/ROTATION */
int _Bmoin_slit_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Bmoin_slit_setpos] component Bmoin_slit=Slit() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Slit.comp:50]");
  stracpy(_Bmoin_slit_var._name, "Bmoin_slit", 16384);
  stracpy(_Bmoin_slit_var._type, "Slit", 16384);
  _Bmoin_slit_var._index=25;
  _Bmoin_slit_var._parameters.xmin = -0.01;
  _Bmoin_slit_var._parameters.xmax = 0.01;
  _Bmoin_slit_var._parameters.ymin = -0.06;
  _Bmoin_slit_var._parameters.ymax = 0.06;
  _Bmoin_slit_var._parameters.radius = 0;
  _Bmoin_slit_var._parameters.xwidth = 0;
  _Bmoin_slit_var._parameters.yheight = 0;

  /* component Bmoin_slit=Slit() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (0)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _msa_var._rotation_absolute, _Bmoin_slit_var._rotation_absolute);
    rot_transpose(_Amoin_slit_var._rotation_absolute, tr1);
    rot_mul(_Bmoin_slit_var._rotation_absolute, tr1, _Bmoin_slit_var._rotation_relative);
    _Bmoin_slit_var._rotation_is_identity =  rot_test_identity(_Bmoin_slit_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.525);
    rot_transpose(_msa_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _Bmoin_slit_var._position_absolute = coords_add(_msa_var._position_absolute, tc2);
    tc1 = coords_sub(_Amoin_slit_var._position_absolute, _Bmoin_slit_var._position_absolute);
    _Bmoin_slit_var._position_relative = rot_apply(_Bmoin_slit_var._rotation_absolute, tc1);
  } /* Bmoin_slit=Slit() AT ROTATED */
  DEBUG_COMPONENT("Bmoin_slit", _Bmoin_slit_var._position_absolute, _Bmoin_slit_var._rotation_absolute);
  instrument->_position_absolute[25] = _Bmoin_slit_var._position_absolute;
  instrument->_position_relative[25] = _Bmoin_slit_var._position_relative;
  instrument->counter_N[25]  = instrument->counter_P[25] = instrument->counter_P2[25] = 0;
  instrument->counter_AbsorbProp[25]= 0;
  return(0);
} /* _Bmoin_slit_setpos */

/* component out2_slit=Slit() SETTING, POSITION/ROTATION */
int _out2_slit_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_out2_slit_setpos] component out2_slit=Slit() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Slit.comp:50]");
  stracpy(_out2_slit_var._name, "out2_slit", 16384);
  stracpy(_out2_slit_var._type, "Slit", 16384);
  _out2_slit_var._index=26;
  _out2_slit_var._parameters.xmin = -0.01;
  _out2_slit_var._parameters.xmax = 0.01;
  _out2_slit_var._parameters.ymin = -0.06;
  _out2_slit_var._parameters.ymax = 0.06;
  _out2_slit_var._parameters.radius = 0;
  _out2_slit_var._parameters.xwidth = 0;
  _out2_slit_var._parameters.yheight = 0;

  /* component out2_slit=Slit() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (0)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _msa_var._rotation_absolute, _out2_slit_var._rotation_absolute);
    rot_transpose(_Bmoin_slit_var._rotation_absolute, tr1);
    rot_mul(_out2_slit_var._rotation_absolute, tr1, _out2_slit_var._rotation_relative);
    _out2_slit_var._rotation_is_identity =  rot_test_identity(_out2_slit_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.65);
    rot_transpose(_msa_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _out2_slit_var._position_absolute = coords_add(_msa_var._position_absolute, tc2);
    tc1 = coords_sub(_Bmoin_slit_var._position_absolute, _out2_slit_var._position_absolute);
    _out2_slit_var._position_relative = rot_apply(_out2_slit_var._rotation_absolute, tc1);
  } /* out2_slit=Slit() AT ROTATED */
  DEBUG_COMPONENT("out2_slit", _out2_slit_var._position_absolute, _out2_slit_var._rotation_absolute);
  instrument->_position_absolute[26] = _out2_slit_var._position_absolute;
  instrument->_position_relative[26] = _out2_slit_var._position_relative;
  instrument->counter_N[26]  = instrument->counter_P[26] = instrument->counter_P2[26] = 0;
  instrument->counter_AbsorbProp[26]= 0;
  return(0);
} /* _out2_slit_setpos */

/* component PSD_sample=PSD_monitor() SETTING, POSITION/ROTATION */
int _PSD_sample_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_PSD_sample_setpos] component PSD_sample=PSD_monitor() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/PSD_monitor.comp:62]");
  stracpy(_PSD_sample_var._name, "PSD_sample", 16384);
  stracpy(_PSD_sample_var._type, "PSD_monitor", 16384);
  _PSD_sample_var._index=27;
  _PSD_sample_var._parameters.nx = 80;
  _PSD_sample_var._parameters.ny = 80;
  if("PSD_sample.dat" && strlen("PSD_sample.dat"))
    stracpy(_PSD_sample_var._parameters.filename, "PSD_sample.dat" ? "PSD_sample.dat" : "", 16384);
  else 
  _PSD_sample_var._parameters.filename[0]='\0';
  _PSD_sample_var._parameters.xmin = -0.05;
  _PSD_sample_var._parameters.xmax = 0.05;
  _PSD_sample_var._parameters.ymin = -0.07;
  _PSD_sample_var._parameters.ymax = 0.07;
  _PSD_sample_var._parameters.xwidth = 0;
  _PSD_sample_var._parameters.yheight = 0;
  _PSD_sample_var._parameters.restore_neutron = 0;
  _PSD_sample_var._parameters.nowritefile = 0;


  /* component PSD_sample=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _msa_var._rotation_absolute, _PSD_sample_var._rotation_absolute);
    rot_transpose(_out2_slit_var._rotation_absolute, tr1);
    rot_mul(_PSD_sample_var._rotation_absolute, tr1, _PSD_sample_var._rotation_relative);
    _PSD_sample_var._rotation_is_identity =  rot_test_identity(_PSD_sample_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 2.77);
    rot_transpose(_msa_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _PSD_sample_var._position_absolute = coords_add(_msa_var._position_absolute, tc2);
    tc1 = coords_sub(_out2_slit_var._position_absolute, _PSD_sample_var._position_absolute);
    _PSD_sample_var._position_relative = rot_apply(_PSD_sample_var._rotation_absolute, tc1);
  } /* PSD_sample=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("PSD_sample", _PSD_sample_var._position_absolute, _PSD_sample_var._rotation_absolute);
  instrument->_position_absolute[27] = _PSD_sample_var._position_absolute;
  instrument->_position_relative[27] = _PSD_sample_var._position_relative;
  instrument->counter_N[27]  = instrument->counter_P[27] = instrument->counter_P2[27] = 0;
  instrument->counter_AbsorbProp[27]= 0;
  return(0);
} /* _PSD_sample_setpos */

/* component lambda_sample=L_monitor() SETTING, POSITION/ROTATION */
int _lambda_sample_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_lambda_sample_setpos] component lambda_sample=L_monitor() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/L_monitor.comp:65]");
  stracpy(_lambda_sample_var._name, "lambda_sample", 16384);
  stracpy(_lambda_sample_var._type, "L_monitor", 16384);
  _lambda_sample_var._index=28;
  _lambda_sample_var._parameters.nL = 128;
  if("L_sample.dat" && strlen("L_sample.dat"))
    stracpy(_lambda_sample_var._parameters.filename, "L_sample.dat" ? "L_sample.dat" : "", 16384);
  else 
  _lambda_sample_var._parameters.filename[0]='\0';
  _lambda_sample_var._parameters.xmin = - sample_radius;
  _lambda_sample_var._parameters.xmax = sample_radius;
  _lambda_sample_var._parameters.ymin = - sample_height / 2;
  _lambda_sample_var._parameters.ymax = sample_height / 2;
  _lambda_sample_var._parameters.xwidth = 0;
  _lambda_sample_var._parameters.yheight = 0;
  _lambda_sample_var._parameters.Lmin = instrument->_parameters.lambda -0.2;
  _lambda_sample_var._parameters.Lmax = instrument->_parameters.lambda + 0.2;
  _lambda_sample_var._parameters.restore_neutron = 0;


  /* component lambda_sample=L_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _msa_var._rotation_absolute, _lambda_sample_var._rotation_absolute);
    rot_transpose(_PSD_sample_var._rotation_absolute, tr1);
    rot_mul(_lambda_sample_var._rotation_absolute, tr1, _lambda_sample_var._rotation_relative);
    _lambda_sample_var._rotation_is_identity =  rot_test_identity(_lambda_sample_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 2.81);
    rot_transpose(_msa_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _lambda_sample_var._position_absolute = coords_add(_msa_var._position_absolute, tc2);
    tc1 = coords_sub(_PSD_sample_var._position_absolute, _lambda_sample_var._position_absolute);
    _lambda_sample_var._position_relative = rot_apply(_lambda_sample_var._rotation_absolute, tc1);
  } /* lambda_sample=L_monitor() AT ROTATED */
  DEBUG_COMPONENT("lambda_sample", _lambda_sample_var._position_absolute, _lambda_sample_var._rotation_absolute);
  instrument->_position_absolute[28] = _lambda_sample_var._position_absolute;
  instrument->_position_relative[28] = _lambda_sample_var._position_relative;
  instrument->counter_N[28]  = instrument->counter_P[28] = instrument->counter_P2[28] = 0;
  instrument->counter_AbsorbProp[28]= 0;
  return(0);
} /* _lambda_sample_setpos */

/* component sa_arm=Arm() SETTING, POSITION/ROTATION */
int _sa_arm_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_sa_arm_setpos] component sa_arm=Arm() SETTING [Arm:0]");
  stracpy(_sa_arm_var._name, "sa_arm", 16384);
  stracpy(_sa_arm_var._type, "Arm", 16384);
  _sa_arm_var._index=29;
  /* component sa_arm=Arm() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (0)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _msa_var._rotation_absolute, _sa_arm_var._rotation_absolute);
    rot_transpose(_lambda_sample_var._rotation_absolute, tr1);
    rot_mul(_sa_arm_var._rotation_absolute, tr1, _sa_arm_var._rotation_relative);
    _sa_arm_var._rotation_is_identity =  rot_test_identity(_sa_arm_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 2.82);
    rot_transpose(_msa_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _sa_arm_var._position_absolute = coords_add(_msa_var._position_absolute, tc2);
    tc1 = coords_sub(_lambda_sample_var._position_absolute, _sa_arm_var._position_absolute);
    _sa_arm_var._position_relative = rot_apply(_sa_arm_var._rotation_absolute, tc1);
  } /* sa_arm=Arm() AT ROTATED */
  DEBUG_COMPONENT("sa_arm", _sa_arm_var._position_absolute, _sa_arm_var._rotation_absolute);
  instrument->_position_absolute[29] = _sa_arm_var._position_absolute;
  instrument->_position_relative[29] = _sa_arm_var._position_relative;
  instrument->counter_N[29]  = instrument->counter_P[29] = instrument->counter_P2[29] = 0;
  instrument->counter_AbsorbProp[29]= 0;
  return(0);
} /* _sa_arm_setpos */

/* component sample=PowderN() SETTING, POSITION/ROTATION */
int _sample_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_sample_setpos] component sample=PowderN() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/samples/PowderN.comp:552]");
  stracpy(_sample_var._name, "sample", 16384);
  stracpy(_sample_var._type, "PowderN", 16384);
  _sample_var._index=30;
  if(instrument->_parameters.filename && strlen(instrument->_parameters.filename))
    stracpy(_sample_var._parameters.reflections, instrument->_parameters.filename ? instrument->_parameters.filename : "", 16384);
  else 
  _sample_var._parameters.reflections[0]='\0';
  if("NULL" && strlen("NULL"))
    stracpy(_sample_var._parameters.geometry, "NULL" ? "NULL" : "", 16384);
  else 
  _sample_var._parameters.geometry[0]='\0';
  _sample_var._parameters.format = (double []){ 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 }; // static pointer allocation
  _sample_var._parameters.radius = sample_radius;
  _sample_var._parameters.yheight = sample_height;
  _sample_var._parameters.xwidth = 0;
  _sample_var._parameters.zdepth = 0;
  _sample_var._parameters.thickness = 0;
  _sample_var._parameters.pack = instrument->_parameters.PACK;
  _sample_var._parameters.Vc = 0;
  _sample_var._parameters.sigma_abs = 0;
  _sample_var._parameters.sigma_inc = 0;
  _sample_var._parameters.delta_d_d = 0;
  _sample_var._parameters.p_inc = 0;
  _sample_var._parameters.p_transmit = 0;
  _sample_var._parameters.DW = instrument->_parameters.Dw;
  _sample_var._parameters.nb_atoms = 1;
  _sample_var._parameters.d_phi = instrument->_parameters.D_PHI;
  _sample_var._parameters.p_interact = 0;
  _sample_var._parameters.concentric = 0;
  _sample_var._parameters.density = 0;
  _sample_var._parameters.weight = 0;
  _sample_var._parameters.barns = instrument->_parameters.BARNS;
  _sample_var._parameters.Strain = 0;
  _sample_var._parameters.focus_flip = 0;


  /* component sample=PowderN() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _sa_arm_var._rotation_absolute, _sample_var._rotation_absolute);
    rot_transpose(_sa_arm_var._rotation_absolute, tr1);
    rot_mul(_sample_var._rotation_absolute, tr1, _sample_var._rotation_relative);
    _sample_var._rotation_is_identity =  rot_test_identity(_sample_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0);
    rot_transpose(_sa_arm_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _sample_var._position_absolute = coords_add(_sa_arm_var._position_absolute, tc2);
    tc1 = coords_sub(_sa_arm_var._position_absolute, _sample_var._position_absolute);
    _sample_var._position_relative = rot_apply(_sample_var._rotation_absolute, tc1);
  } /* sample=PowderN() AT ROTATED */
  DEBUG_COMPONENT("sample", _sample_var._position_absolute, _sample_var._rotation_absolute);
  instrument->_position_absolute[30] = _sample_var._position_absolute;
  instrument->_position_relative[30] = _sample_var._position_relative;
  instrument->counter_N[30]  = instrument->counter_P[30] = instrument->counter_P2[30] = 0;
  instrument->counter_AbsorbProp[30]= 0;
  return(0);
} /* _sample_setpos */

/* component STOP=Beamstop() SETTING, POSITION/ROTATION */
int _STOP_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_STOP_setpos] component STOP=Beamstop() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Beamstop.comp:50]");
  stracpy(_STOP_var._name, "STOP", 16384);
  stracpy(_STOP_var._type, "Beamstop", 16384);
  _STOP_var._index=31;
  _STOP_var._parameters.xmin = -0.05;
  _STOP_var._parameters.xmax = 0.05;
  _STOP_var._parameters.ymin = -0.05;
  _STOP_var._parameters.ymax = 0.05;
  _STOP_var._parameters.xwidth = 0;
  _STOP_var._parameters.yheight = 0;
  _STOP_var._parameters.radius = 0.3;

  /* component STOP=Beamstop() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (0)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _sa_arm_var._rotation_absolute, _STOP_var._rotation_absolute);
    rot_transpose(_sample_var._rotation_absolute, tr1);
    rot_mul(_STOP_var._rotation_absolute, tr1, _STOP_var._rotation_relative);
    _STOP_var._rotation_is_identity =  rot_test_identity(_STOP_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 1.4);
    rot_transpose(_sa_arm_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _STOP_var._position_absolute = coords_add(_sa_arm_var._position_absolute, tc2);
    tc1 = coords_sub(_sample_var._position_absolute, _STOP_var._position_absolute);
    _STOP_var._position_relative = rot_apply(_STOP_var._rotation_absolute, tc1);
  } /* STOP=Beamstop() AT ROTATED */
  DEBUG_COMPONENT("STOP", _STOP_var._position_absolute, _STOP_var._rotation_absolute);
  instrument->_position_absolute[31] = _STOP_var._position_absolute;
  instrument->_position_relative[31] = _STOP_var._position_relative;
  instrument->counter_N[31]  = instrument->counter_P[31] = instrument->counter_P2[31] = 0;
  instrument->counter_AbsorbProp[31]= 0;
  return(0);
} /* _STOP_setpos */

/* component Detector=Monitor_nD() SETTING, POSITION/ROTATION */
int _Detector_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Detector_setpos] component Detector=Monitor_nD() SETTING [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/Monitor_nD.comp:231]");
  stracpy(_Detector_var._name, "Detector", 16384);
  stracpy(_Detector_var._type, "Monitor_nD", 16384);
  _Detector_var._index=32;
  _Detector_var._parameters.user1 = '\0';
  _Detector_var._parameters.user2 = '\0';
  _Detector_var._parameters.user3 = '\0';
  _Detector_var._parameters.xwidth = 3.0;
  _Detector_var._parameters.yheight = 0.09;
  _Detector_var._parameters.zdepth = 0;
  _Detector_var._parameters.xmin = 0;
  _Detector_var._parameters.xmax = 0;
  _Detector_var._parameters.ymin = 0;
  _Detector_var._parameters.ymax = 0;
  _Detector_var._parameters.zmin = 0;
  _Detector_var._parameters.zmax = 0;
  _Detector_var._parameters.bins = 400;
  _Detector_var._parameters.min = 19.9 + instrument->_parameters.SHIFT;
  _Detector_var._parameters.max = 99.9 + instrument->_parameters.SHIFT;
  _Detector_var._parameters.restore_neutron = 0;
  _Detector_var._parameters.radius = 0;
  if("banana, theta" && strlen("banana, theta"))
    stracpy(_Detector_var._parameters.options, "banana, theta" ? "banana, theta" : "", 16384);
  else 
  _Detector_var._parameters.options[0]='\0';
  if("detector.dat" && strlen("detector.dat"))
    stracpy(_Detector_var._parameters.filename, "detector.dat" ? "detector.dat" : "", 16384);
  else 
  _Detector_var._parameters.filename[0]='\0';
  if("NULL" && strlen("NULL"))
    stracpy(_Detector_var._parameters.geometry, "NULL" ? "NULL" : "", 16384);
  else 
  _Detector_var._parameters.geometry[0]='\0';
  if("NULL" && strlen("NULL"))
    stracpy(_Detector_var._parameters.username1, "NULL" ? "NULL" : "", 16384);
  else 
  _Detector_var._parameters.username1[0]='\0';
  if("NULL" && strlen("NULL"))
    stracpy(_Detector_var._parameters.username2, "NULL" ? "NULL" : "", 16384);
  else 
  _Detector_var._parameters.username2[0]='\0';
  if("NULL" && strlen("NULL"))
    stracpy(_Detector_var._parameters.username3, "NULL" ? "NULL" : "", 16384);
  else 
  _Detector_var._parameters.username3[0]='\0';


  /* component Detector=Monitor_nD() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (0)*DEG2RAD, (180)*DEG2RAD);
    rot_mul(tr1, _sa_arm_var._rotation_absolute, _Detector_var._rotation_absolute);
    rot_transpose(_STOP_var._rotation_absolute, tr1);
    rot_mul(_Detector_var._rotation_absolute, tr1, _Detector_var._rotation_relative);
    _Detector_var._rotation_is_identity =  rot_test_identity(_Detector_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0);
    rot_transpose(_sa_arm_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _Detector_var._position_absolute = coords_add(_sa_arm_var._position_absolute, tc2);
    tc1 = coords_sub(_STOP_var._position_absolute, _Detector_var._position_absolute);
    _Detector_var._position_relative = rot_apply(_Detector_var._rotation_absolute, tc1);
  } /* Detector=Monitor_nD() AT ROTATED */
  DEBUG_COMPONENT("Detector", _Detector_var._position_absolute, _Detector_var._rotation_absolute);
  instrument->_position_absolute[32] = _Detector_var._position_absolute;
  instrument->_position_relative[32] = _Detector_var._position_relative;
  instrument->counter_N[32]  = instrument->counter_P[32] = instrument->counter_P2[32] = 0;
  instrument->counter_AbsorbProp[32]= 0;
  return(0);
} /* _Detector_setpos */

_class_Progress_bar *class_Progress_bar_init(_class_Progress_bar *_comp
) {
  #define profile (_comp->_parameters.profile)
  #define percent (_comp->_parameters.percent)
  #define flag_save (_comp->_parameters.flag_save)
  #define minutes (_comp->_parameters.minutes)
  #define IntermediateCnts (_comp->_parameters.IntermediateCnts)
  #define StartTime (_comp->_parameters.StartTime)
  #define EndTime (_comp->_parameters.EndTime)
  #define CurrentTime (_comp->_parameters.CurrentTime)
  SIG_MESSAGE("[_source_arm_init] component source_arm=Progress_bar() INITIALISE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/misc/Progress_bar.comp:57]");

IntermediateCnts=0;
StartTime=0;
EndTime=0;
CurrentTime=0;

fprintf(stdout, "[%s] Initialize\n", instrument_name);
  if (percent*mcget_ncount()/100 < 1e5) {
    percent=1e5*100.0/mcget_ncount();
  }
  #undef profile
  #undef percent
  #undef flag_save
  #undef minutes
  #undef IntermediateCnts
  #undef StartTime
  #undef EndTime
  #undef CurrentTime
  return(_comp);
} /* class_Progress_bar_init */

_class_Source_Maxwell_3 *class_Source_Maxwell_3_init(_class_Source_Maxwell_3 *_comp
) {
  #define size (_comp->_parameters.size)
  #define yheight (_comp->_parameters.yheight)
  #define xwidth (_comp->_parameters.xwidth)
  #define Lmin (_comp->_parameters.Lmin)
  #define Lmax (_comp->_parameters.Lmax)
  #define dist (_comp->_parameters.dist)
  #define focus_xw (_comp->_parameters.focus_xw)
  #define focus_yh (_comp->_parameters.focus_yh)
  #define T1 (_comp->_parameters.T1)
  #define T2 (_comp->_parameters.T2)
  #define T3 (_comp->_parameters.T3)
  #define I1 (_comp->_parameters.I1)
  #define I2 (_comp->_parameters.I2)
  #define I3 (_comp->_parameters.I3)
  #define target_index (_comp->_parameters.target_index)
  #define lambda0 (_comp->_parameters.lambda0)
  #define dlambda (_comp->_parameters.dlambda)
  #define M (_comp->_parameters.M)
  #define l_range (_comp->_parameters.l_range)
  #define w_mult (_comp->_parameters.w_mult)
  #define w_source (_comp->_parameters.w_source)
  #define h_source (_comp->_parameters.h_source)
  SIG_MESSAGE("[_source_init] component source=Source_Maxwell_3() INITIALISE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/sources/Source_Maxwell_3.comp:86]");

  if (target_index && !dist)
  {
    Coords ToTarget;
    double tx,ty,tz;
    ToTarget = coords_sub(POS_A_COMP_INDEX(INDEX_CURRENT_COMP+target_index),POS_A_CURRENT_COMP);
    ToTarget = rot_apply(ROT_A_CURRENT_COMP, ToTarget);
    coords_get(ToTarget, &tx, &ty, &tz);
    dist=sqrt(tx*tx+ty*ty+tz*tz);
  }

  if (size>0) {
    w_source = h_source = size;
  } else {
    w_source = xwidth;
    h_source = yheight;
  }
  if (lambda0) {
    Lmin=lambda0-dlambda;
    Lmax=lambda0+dlambda;
  }
  l_range = Lmax-Lmin;
  w_mult = w_source*h_source*1.0e4;     /* source area correction */
  w_mult *= l_range;            /* wavelength range correction */
  w_mult *= 1.0/mcget_ncount();   /* correct for # neutron rays */

  if (w_source <0 || h_source < 0 || Lmin <= 0 || Lmax <= 0 || dist <= 0 || T1 <= 0 || T2 <= 0|| T3 <= 0 || Lmax<=Lmin) {
      printf("Source_Maxwell_3: %s: Error in input parameter values!\n"
             "ERROR          Exiting\n",
           NAME_CURRENT_COMP);
      exit(0);
  }

  #undef size
  #undef yheight
  #undef xwidth
  #undef Lmin
  #undef Lmax
  #undef dist
  #undef focus_xw
  #undef focus_yh
  #undef T1
  #undef T2
  #undef T3
  #undef I1
  #undef I2
  #undef I3
  #undef target_index
  #undef lambda0
  #undef dlambda
  #undef M
  #undef l_range
  #undef w_mult
  #undef w_source
  #undef h_source
  return(_comp);
} /* class_Source_Maxwell_3_init */

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
  #define nowritefile (_comp->_parameters.nowritefile)
  #define PSD_N (_comp->_parameters.PSD_N)
  #define PSD_p (_comp->_parameters.PSD_p)
  #define PSD_p2 (_comp->_parameters.PSD_p2)
  SIG_MESSAGE("[_PSDbefore_guides_init] component PSDbefore_guides=PSD_monitor() INITIALISE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/PSD_monitor.comp:62]");

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
  #undef nowritefile
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  return(_comp);
} /* class_PSD_monitor_init */

_class_L_monitor *class_L_monitor_init(_class_L_monitor *_comp
) {
  #define nL (_comp->_parameters.nL)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define Lmin (_comp->_parameters.Lmin)
  #define Lmax (_comp->_parameters.Lmax)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define L_N (_comp->_parameters.L_N)
  #define L_p (_comp->_parameters.L_p)
  #define L_p2 (_comp->_parameters.L_p2)
  SIG_MESSAGE("[_l_mon_source_init] component l_mon_source=L_monitor() INITIALISE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/L_monitor.comp:65]");

  if (xwidth  > 0) { xmax = xwidth/2;  xmin = -xmax; }
  if (yheight > 0) { ymax = yheight/2; ymin = -ymax; }

  if ((xmin >= xmax) || (ymin >= ymax)) {
    printf("L_monitor: %s: Null detection area !\n"
      "ERROR      (xwidth,yheight,xmin,xmax,ymin,ymax). Exiting",
      NAME_CURRENT_COMP);
    exit(0);
  }

  L_N = create_darr1d(nL);
  L_p = create_darr1d(nL);
  L_p2 = create_darr1d(nL);

  int i;
  for (i=0; i<nL; i++)
  {
    L_N[i] = 0;
    L_p[i] = 0;
    L_p2[i] = 0;
  }
  #undef nL
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef Lmin
  #undef Lmax
  #undef restore_neutron
  #undef L_N
  #undef L_p
  #undef L_p2
  return(_comp);
} /* class_L_monitor_init */

_class_Guide *class_Guide_init(_class_Guide *_comp
) {
  #define reflect (_comp->_parameters.reflect)
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
  #define pTable (_comp->_parameters.pTable)
  #define table_present (_comp->_parameters.table_present)
  SIG_MESSAGE("[_guide1_init] component guide1=Guide() INITIALISE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Guide.comp:74]");

if (mcgravitation) fprintf(stderr,"WARNING: Guide: %s: "
    "This component produces wrong results with gravitation !\n"
    "Use Guide_gravity.\n",
    NAME_CURRENT_COMP);

  if (!w2) w2=w1;
  if (!h2) h2=h1;

  if (reflect && strlen(reflect) && strcmp(reflect,"NULL") && strcmp(reflect,"0")) {
    if (Table_Read(&pTable, reflect, 1) <= 0) /* read 1st block data from file into pTable */
      exit(fprintf(stderr,"Guide: %s: can not read file %s\n", NAME_CURRENT_COMP, reflect));
    table_present=1;
  } else {
    table_present=0;
    if (W < 0 || R0 < 0 || Qc < 0 || m < 0)
    { fprintf(stderr,"Guide: %s: W R0 Qc must be >0.\n", NAME_CURRENT_COMP);
      exit(-1); }
  }
  #undef reflect
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
  #undef pTable
  #undef table_present
  return(_comp);
} /* class_Guide_init */

_class_Bender *class_Bender_init(_class_Bender *_comp
) {
  #define w (_comp->_parameters.w)
  #define h (_comp->_parameters.h)
  #define r (_comp->_parameters.r)
  #define Win (_comp->_parameters.Win)
  #define k (_comp->_parameters.k)
  #define d (_comp->_parameters.d)
  #define l (_comp->_parameters.l)
  #define R0a (_comp->_parameters.R0a)
  #define Qca (_comp->_parameters.Qca)
  #define alphaa (_comp->_parameters.alphaa)
  #define ma (_comp->_parameters.ma)
  #define Wa (_comp->_parameters.Wa)
  #define R0i (_comp->_parameters.R0i)
  #define Qci (_comp->_parameters.Qci)
  #define alphai (_comp->_parameters.alphai)
  #define mi (_comp->_parameters.mi)
  #define Wi (_comp->_parameters.Wi)
  #define R0s (_comp->_parameters.R0s)
  #define Qcs (_comp->_parameters.Qcs)
  #define alphas (_comp->_parameters.alphas)
  #define ms (_comp->_parameters.ms)
  #define Ws (_comp->_parameters.Ws)
  #define bk (_comp->_parameters.bk)
  #define mWin (_comp->_parameters.mWin)
  SIG_MESSAGE("[_guide2_init] component guide2=Bender() INITIALISE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Bender.comp:112]");

if (r <0)
      { fprintf(stderr,"Bender: error: %s: to bend in the other direction\n", NAME_CURRENT_COMP);
        fprintf(stderr,"        rotate comp on z-axis by 180 deg.\n"); exit(-1); }

      if (k*d > w)
      { fprintf(stderr,"Bender: error: %s has (k*d > w).\n", NAME_CURRENT_COMP);
        exit(-1); }
      if (w*h*r*Win*k == 0)
      { fprintf(stderr,"Bender: error: %s has one of w,h,r,Win,k null.\n", NAME_CURRENT_COMP);
        exit(-1); }
      /* width of one channel + thickness d of partition */
      mWin = Win;
      if (l!= 0 && r != 0) mWin = (double)l/(double)r;
      bk=(w+d)/k;
      if (mcgravitation) fprintf(stderr,"WARNING: Bender: %s: "
        "This component produces wrong results with gravitation !\n",
        NAME_CURRENT_COMP);
  #undef w
  #undef h
  #undef r
  #undef Win
  #undef k
  #undef d
  #undef l
  #undef R0a
  #undef Qca
  #undef alphaa
  #undef ma
  #undef Wa
  #undef R0i
  #undef Qci
  #undef alphai
  #undef mi
  #undef Wi
  #undef R0s
  #undef Qcs
  #undef alphas
  #undef ms
  #undef Ws
  #undef bk
  #undef mWin
  return(_comp);
} /* class_Bender_init */

_class_PSDlin_monitor *class_PSDlin_monitor_init(_class_PSDlin_monitor *_comp
) {
  #define nx (_comp->_parameters.nx)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define PSDlin_N (_comp->_parameters.PSDlin_N)
  #define PSDlin_p (_comp->_parameters.PSDlin_p)
  #define PSDlin_p2 (_comp->_parameters.PSDlin_p2)
  SIG_MESSAGE("[_ydist_fluxpos_init] component ydist_fluxpos=PSDlin_monitor() INITIALISE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/PSDlin_monitor.comp:58]");

  int i;

  if (xwidth  > 0) { xmax = xwidth/2;  xmin = -xmax; }
  if (yheight > 0) { ymax = yheight/2; ymin = -ymax; }

  if ((xmin >= xmax) || (ymin >= ymax)) {
    printf("PSDlin_monitor: %s: Null detection area !\n"
           "ERROR           (xwidth,yheight,xmin,xmax,ymin,ymax). Exiting",
           NAME_CURRENT_COMP);
    exit(0);
  }

  PSDlin_N = create_darr1d(nx);
  PSDlin_p = create_darr1d(nx);
  PSDlin_p2 = create_darr1d(nx);

  for (i=0; i<nx; i++)
  {
    PSDlin_N[i] = 0;
    PSDlin_p[i] = 0;
    PSDlin_p2[i] = 0;
  }
  #undef nx
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef PSDlin_N
  #undef PSDlin_p
  #undef PSDlin_p2
  return(_comp);
} /* class_PSDlin_monitor_init */

_class_Slit *class_Slit_init(_class_Slit *_comp
) {
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define radius (_comp->_parameters.radius)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  SIG_MESSAGE("[_in_slit_init] component in_slit=Slit() INITIALISE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Slit.comp:50]");

if (xwidth > 0)  { 
  if (!xmin && !xmax) {
    xmax=xwidth/2;  xmin=-xmax;
  } else {
    fprintf(stderr,"Slit: %s: Error: please specify EITHER xmin & xmax or xwidth\n", NAME_CURRENT_COMP); exit(-1);
  }
 }
 if (yheight > 0) { 
   if (!ymin && !ymax) {
     ymax=yheight/2; ymin=-ymax; 
   } else {
     fprintf(stderr,"Slit: %s: Error: please specify EITHER ymin & ymax or ywidth\n", NAME_CURRENT_COMP); exit(-1);
   }
 }
 if (xmin == 0 && xmax == 0 && ymin == 0 && ymax == 0 && radius == 0)
    { fprintf(stderr,"Slit: %s: Warning: Running with CLOSED slit - is this intentional?? \n", NAME_CURRENT_COMP); }

  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef radius
  #undef xwidth
  #undef yheight
  return(_comp);
} /* class_Slit_init */

_class_Monochromator_2foc *class_Monochromator_2foc_init(_class_Monochromator_2foc *_comp
) {
  #define reflect (_comp->_parameters.reflect)
  #define zwidth (_comp->_parameters.zwidth)
  #define yheight (_comp->_parameters.yheight)
  #define gap (_comp->_parameters.gap)
  #define NH (_comp->_parameters.NH)
  #define NV (_comp->_parameters.NV)
  #define mosaich (_comp->_parameters.mosaich)
  #define mosaicv (_comp->_parameters.mosaicv)
  #define r0 (_comp->_parameters.r0)
  #define Q (_comp->_parameters.Q)
  #define RV (_comp->_parameters.RV)
  #define RH (_comp->_parameters.RH)
  #define DM (_comp->_parameters.DM)
  #define mosaic (_comp->_parameters.mosaic)
  #define width (_comp->_parameters.width)
  #define height (_comp->_parameters.height)
  #define verbose (_comp->_parameters.verbose)
  #define mos_y (_comp->_parameters.mos_y)
  #define mos_z (_comp->_parameters.mos_z)
  #define mono_Q (_comp->_parameters.mono_Q)
  #define SlabWidth (_comp->_parameters.SlabWidth)
  #define SlabHeight (_comp->_parameters.SlabHeight)
  #define rTable (_comp->_parameters.rTable)
  SIG_MESSAGE("[_foc_mono_init] component foc_mono=Monochromator_2foc() INITIALISE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/contrib/Monochromator_2foc.comp:100]");

if (mosaic != 0) {
    mos_y = mosaic;
    mos_z = mos_y; }
  else {
    mos_y = mosaich;
    mos_z = mosaicv; }

  mono_Q = Q;
  if (DM != 0) mono_Q = 2*PI/DM;

  if (mono_Q == 0) { fprintf(stderr,"Monochromator_2foc: %s: Error scattering vector Q = 0\n", NAME_CURRENT_COMP); exit(-1); }
  if (r0 == 0) { fprintf(stderr,"Monochromator_2foc: %s: Error reflectivity r0 is null\n", NAME_CURRENT_COMP); exit(-1); }
  if (NH*NV == 0) { fprintf(stderr,"Monochromator_2foc: %s: no slabs ??? (NH or NV=0)\n", NAME_CURRENT_COMP); exit(-1); }

  if (verbose)
  {
    printf("Monochromator_2foc: component %s Q=%.3g Angs-1 (DM=%.4g Angs)\n", NAME_CURRENT_COMP, mono_Q, 2*PI/mono_Q);
    if (NH*NV == 1) printf("            flat.\n");
    else
    { if (NH > 1)
      { printf("            horizontal: %i blades", (int)NH);
        if (RH != 0) printf(" focusing with RH=%.3g [m]", RH);
        printf("\n");
      }
      if (NV > 1)
      { printf("            vertical:   %i blades", (int)NV);
        if (RV != 0) printf(" focusing with RV=%.3g [m]", RV);
        printf("\n");
      }
    }
  }

  if (reflect != NULL)
  {
    if (verbose) fprintf(stdout, "Monochromator_2foc: %s : Reflectivity data (k, R)\n", NAME_CURRENT_COMP);
    Table_Read(&rTable, reflect, 1); /* read 1st block data from file into rTable */
    Table_Rebin(&rTable);         /* rebin as evenly, increasing array */
    if (rTable.rows < 2) Table_Free(&rTable);
    Table_Info(rTable);
  } else rTable.data = NULL;

  if (width == 0) SlabWidth = zwidth;
  else SlabWidth = (width+gap)/NH - gap;
  if (height == 0) SlabHeight = yheight;
  else SlabHeight = (height+gap)/NV - gap;
  #undef reflect
  #undef zwidth
  #undef yheight
  #undef gap
  #undef NH
  #undef NV
  #undef mosaich
  #undef mosaicv
  #undef r0
  #undef Q
  #undef RV
  #undef RH
  #undef DM
  #undef mosaic
  #undef width
  #undef height
  #undef verbose
  #undef mos_y
  #undef mos_z
  #undef mono_Q
  #undef SlabWidth
  #undef SlabHeight
  #undef rTable
  return(_comp);
} /* class_Monochromator_2foc_init */

_class_PowderN *class_PowderN_init(_class_PowderN *_comp
) {
  #define reflections (_comp->_parameters.reflections)
  #define geometry (_comp->_parameters.geometry)
  #define format (_comp->_parameters.format)
  #define radius (_comp->_parameters.radius)
  #define yheight (_comp->_parameters.yheight)
  #define xwidth (_comp->_parameters.xwidth)
  #define zdepth (_comp->_parameters.zdepth)
  #define thickness (_comp->_parameters.thickness)
  #define pack (_comp->_parameters.pack)
  #define Vc (_comp->_parameters.Vc)
  #define sigma_abs (_comp->_parameters.sigma_abs)
  #define sigma_inc (_comp->_parameters.sigma_inc)
  #define delta_d_d (_comp->_parameters.delta_d_d)
  #define p_inc (_comp->_parameters.p_inc)
  #define p_transmit (_comp->_parameters.p_transmit)
  #define DW (_comp->_parameters.DW)
  #define nb_atoms (_comp->_parameters.nb_atoms)
  #define d_phi (_comp->_parameters.d_phi)
  #define p_interact (_comp->_parameters.p_interact)
  #define concentric (_comp->_parameters.concentric)
  #define density (_comp->_parameters.density)
  #define weight (_comp->_parameters.weight)
  #define barns (_comp->_parameters.barns)
  #define Strain (_comp->_parameters.Strain)
  #define focus_flip (_comp->_parameters.focus_flip)
  #define line_info (_comp->_parameters.line_info)
  #define columns (_comp->_parameters.columns)
  #define offdata (_comp->_parameters.offdata)
  SIG_MESSAGE("[_sample_init] component sample=PowderN() INITIALISE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/samples/PowderN.comp:552]");

  /* We ought to clean up the columns variable as format is now a proper vector/array */
  columns = format;

  int i=0;
  struct line_data *L;
  line_info.Dd       = delta_d_d;
  line_info.DWfactor = DW;
  line_info.V_0      = Vc;
  line_info.rho      = density;
  line_info.at_weight= weight;
  line_info.at_nb    = nb_atoms;
  line_info.sigma_a  = sigma_abs;
  line_info.sigma_i  = sigma_inc;
  line_info.flag_barns=barns;
  line_info.shape    = 0;
  line_info.flag_warning=0;
  line_info.Epsilon  = Strain;
  line_info.radius_i =line_info.xwidth_i=line_info.yheight_i=line_info.zdepth_i=0;
  line_info.v  = 0;
  line_info.Nq = 0;
  line_info.v_min = FLT_MAX; line_info.v_max = 0;
  line_info.neutron_passed=0;
  line_info.nb_reuses = line_info.nb_refl = line_info.nb_refl_count = 0;
  line_info.xs_compute= line_info.xs_reuse= line_info.xs_calls =0;
  for (i=0; i< 9; i++) line_info.column_order[i] = (int)columns[i];
  strncpy(line_info.compname, NAME_CURRENT_COMP, 256);

  line_info.shape=-1; /* -1:no shape, 0:cyl, 1:box, 2:sphere, 3:any-shape  */
#ifndef USE_PGI
  if (geometry && strlen(geometry) && strcmp(geometry, "NULL") && strcmp(geometry, "0")) {
	  if (off_init(geometry, xwidth, yheight, zdepth, 0, &offdata)) {
      line_info.shape=3; thickness=0; concentric=0;
    }
  }
  else
#endif
    if (xwidth && yheight && zdepth)  line_info.shape=1; /* box */
  else if (radius > 0 && yheight)        line_info.shape=0; /* cylinder */
  else if (radius > 0 && !yheight)       line_info.shape=2; /* sphere */

  if (line_info.shape < 0)
    exit(fprintf(stderr,"PowderN: %s: sample has invalid dimensions.\n"
                        "ERROR    Please check parameter values (xwidth, yheight, zdepth, radius).\n", NAME_CURRENT_COMP));
  if (thickness) {
    if (radius && (radius < fabs(thickness))) {
      MPI_MASTER(
      printf("PowderN: %s: hollow sample thickness is larger than its volume (sphere/cylinder).\n"
                     "WARNING  Please check parameter values. Using bulk sample (thickness=0).\n", NAME_CURRENT_COMP);
      );
      thickness=0;
    }
    else if (!radius && (xwidth < 2*fabs(thickness) || yheight < 2*fabs(thickness) || zdepth < 2*fabs(thickness))) {
      MPI_MASTER(
      printf("PowderN: %s: hollow sample thickness is larger than its volume (box).\n"
                     "WARNING  Please check parameter values.\n", NAME_CURRENT_COMP);
      );
    }
  }

  if (concentric && thickness==0) {
    MPI_MASTER(
    printf("PowderN: %s:Can not use concentric mode\n"
           "WARNING     on non hollow shape. Ignoring.\n",
           NAME_CURRENT_COMP);
    );
    concentric=0;
  }

  if (thickness>0) {
    if (radius>thickness) {
      line_info.radius_i=radius-thickness;
    } else {
      if (xwidth>2*thickness)  line_info.xwidth_i =xwidth -2*thickness;
      if (yheight>2*thickness) line_info.yheight_i=yheight-2*thickness;
      if (zdepth>2*thickness)  line_info.zdepth_i =zdepth -2*thickness;
    }
  } else if (thickness<0) {
    thickness = fabs(thickness);
    if (radius) {
      line_info.radius_i=radius;
      radius=line_info.radius_i+thickness;
    } else {
      line_info.xwidth_i =xwidth;
      line_info.yheight_i=yheight;
      line_info.zdepth_i =zdepth;
      xwidth   =xwidth +2*thickness;
      yheight  =yheight+2*thickness;
      zdepth   =zdepth +2*thickness;
    }
  }

  if (!line_info.yheight_i) {
    line_info.yheight_i = yheight;
  }
  if (p_interact) {
    if (p_interact < p_inc) { double tmp=p_interact; p_interact=p_inc; p_inc=tmp; }
    p_transmit = 1-p_interact-p_inc;
  }

  if (p_inc + p_transmit > 1) {
    MPI_MASTER(
    printf("PowderN: %s: You have requested an unmeaningful choice of the 'p_inc' and 'p_transmit' parameters (sum is %g, exeeding 1). Fixing.\n",
                 NAME_CURRENT_COMP, p_inc+p_transmit);
    );
    if (p_inc > p_transmit) p_transmit=1-2*p_inc;
    else p_transmit=1-2*p_inc;
  } else if (p_inc + p_transmit == 1) {
    MPI_MASTER(
    printf("PowderN: %s: You have requested all neutrons be attenuated\n"
           "WARNING  or incoherently scattered!\n", NAME_CURRENT_COMP);
    );
  }

  if (concentric) {
    MPI_MASTER(
    printf("PowderN: %s: Concentric mode - remember to include the 'opposite' copy of this component !\n"
           "WARNING  The equivalent, 'opposite' comp should have concentric=0\n", NAME_CURRENT_COMP);
    );
    if (p_transmit == 0) {
      MPI_MASTER(
      printf("PowderN: %s: Concentric mode and p_transmit==0 !?\n"
             "WARNING  Don't you want any transmitted neutrons?\n", NAME_CURRENT_COMP);
      );
    }
  }

  if (reflections && strlen(reflections) && strcmp(reflections, "NULL") && strcmp(reflections, "0")) {
    i = read_line_data(reflections, &line_info);
    if (i == 0)
      exit(fprintf(stderr,"PowderN: %s: reflection file %s is not valid.\n"
                          "ERROR    Please check file format (laz or lau).\n", NAME_CURRENT_COMP, reflections));
  }

  /* compute the scattering unit density from material weight and density */
  /* the weight of the scattering element is the chemical formula molecular weight
   * times the nb of chemical formulae in the scattering element (nb_atoms) */
  if (!line_info.V_0 && line_info.at_nb > 0
    && line_info.at_weight > 0 && line_info.rho > 0) {
    /* molar volume [cm^3/mol] = weight [g/mol] / density [g/cm^3] */
    /* atom density per Angs^3 = [mol/cm^3] * N_Avogadro *(1e-8)^3 */
    line_info.V_0 = line_info.at_nb
      /(line_info.rho/line_info.at_weight/1e24*6.02214199e23);
  }

  /* the scattering unit cross sections are the chemical formula onces
   * times the nb of chemical formulae in the scattering element */
  if (line_info.at_nb > 0) {
    line_info.sigma_a *= line_info.at_nb; line_info.sigma_i *= line_info.at_nb;
  }

  if (line_info.sigma_a<0) line_info.sigma_a=0;
  if (line_info.sigma_i<0) line_info.sigma_i=0;

  if (line_info.V_0 <= 0)
  MPI_MASTER(
    printf("PowderN: %s: density/unit cell volume is NULL (Vc). Unactivating component.\n", NAME_CURRENT_COMP);
  );

  if (line_info.V_0 > 0 && p_inc && !line_info.sigma_i) {
  MPI_MASTER(
    printf("PowderN: %s: WARNING: You have requested statistics for incoherent scattering but not defined sigma_inc!\n", NAME_CURRENT_COMP);
  );
  }

  if (line_info.flag_barns) { /* Factor 100 to convert from barns to fm^2 */
    line_info.XsectionFactor = 100;
  } else {
    line_info.XsectionFactor = 1;
  }

  if (line_info.V_0 > 0 && i) {
    L = line_info.list;

    line_info.q_v = malloc(line_info.count*sizeof(double));
    line_info.w_v = malloc(line_info.count*sizeof(double));
    line_info.my_s_v2 = malloc(line_info.count*sizeof(double));
    if (!line_info.q_v || !line_info.w_v || !line_info.my_s_v2)
      exit(fprintf(stderr,"PowderN: %s: ERROR allocating memory (init)\n", NAME_CURRENT_COMP));
    for(i=0; i<line_info.count; i++)
    {
      line_info.my_s_v2[i] = 4*PI*PI*PI*pack*(L[i].DWfactor ? L[i].DWfactor : 1)
                 /(line_info.V_0*line_info.V_0*V2K*V2K)
                 *(L[i].j * L[i].F2 / L[i].q)*line_info.XsectionFactor;
      /* Is not yet divided by v^2 */
      /* Squires [3.103] */
      line_info.q_v[i] = L[i].q*K2V;
      line_info.w_v[i] = L[i].w;
    }
  }
  if (line_info.V_0 > 0) {
    /* Is not yet divided by v */
    line_info.my_a_v = pack*line_info.sigma_a/line_info.V_0*2200*100;   // Factor 100 to convert from barns to fm^2
    line_info.my_inc = pack*line_info.sigma_i/line_info.V_0*100;   // Factor 100 to convert from barns to fm^2
    MPI_MASTER(
    printf("PowderN: %s: Vc=%g [Angs] sigma_abs=%g [barn] sigma_inc=%g [barn] reflections=%s\n",
      NAME_CURRENT_COMP, line_info.V_0, line_info.sigma_a, line_info.sigma_i, reflections && strlen(reflections) ? reflections : "NULL");
    );
  }

  #undef reflections
  #undef geometry
  #undef format
  #undef radius
  #undef yheight
  #undef xwidth
  #undef zdepth
  #undef thickness
  #undef pack
  #undef Vc
  #undef sigma_abs
  #undef sigma_inc
  #undef delta_d_d
  #undef p_inc
  #undef p_transmit
  #undef DW
  #undef nb_atoms
  #undef d_phi
  #undef p_interact
  #undef concentric
  #undef density
  #undef weight
  #undef barns
  #undef Strain
  #undef focus_flip
  #undef line_info
  #undef columns
  #undef offdata
  return(_comp);
} /* class_PowderN_init */

_class_Beamstop *class_Beamstop_init(_class_Beamstop *_comp
) {
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define radius (_comp->_parameters.radius)
  SIG_MESSAGE("[_STOP_init] component STOP=Beamstop() INITIALISE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Beamstop.comp:50]");

if (xwidth  > 0) { xmax = xwidth/2;  xmin = -xmax; }
  if (yheight > 0) { ymax = yheight/2; ymin = -ymax; }

  if (xmin == 0 && xmax == 0 && ymin == 0 & ymax == 0 && radius == 0)
  { fprintf(stderr,"Beamstop: %s: Error: give geometry\n", NAME_CURRENT_COMP); exit(-1); }
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef radius
  return(_comp);
} /* class_Beamstop_init */

_class_Monitor_nD *class_Monitor_nD_init(_class_Monitor_nD *_comp
) {
  #define user1 (_comp->_parameters.user1)
  #define user2 (_comp->_parameters.user2)
  #define user3 (_comp->_parameters.user3)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define zdepth (_comp->_parameters.zdepth)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define zmin (_comp->_parameters.zmin)
  #define zmax (_comp->_parameters.zmax)
  #define bins (_comp->_parameters.bins)
  #define min (_comp->_parameters.min)
  #define max (_comp->_parameters.max)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define radius (_comp->_parameters.radius)
  #define options (_comp->_parameters.options)
  #define filename (_comp->_parameters.filename)
  #define geometry (_comp->_parameters.geometry)
  #define username1 (_comp->_parameters.username1)
  #define username2 (_comp->_parameters.username2)
  #define username3 (_comp->_parameters.username3)
  #define DEFS (_comp->_parameters.DEFS)
  #define Vars (_comp->_parameters.Vars)
  #define detector (_comp->_parameters.detector)
  #define offdata (_comp->_parameters.offdata)
  SIG_MESSAGE("[_Detector_init] component Detector=Monitor_nD() INITIALISE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/Monitor_nD.comp:231]");

  char tmp[CHAR_BUF_LENGTH];
  strcpy(Vars.compcurname, NAME_CURRENT_COMP);
  if (options != NULL)
    strncpy(Vars.option, options, CHAR_BUF_LENGTH);
  else {
    strcpy(Vars.option, "x y");
    printf("Monitor_nD: %s has no option specified. Setting to PSD ('x y') monitor.\n", NAME_CURRENT_COMP);
  }
  Vars.compcurpos = POS_A_CURRENT_COMP;

  if (strstr(Vars.option, "source"))
    strcat(Vars.option, " list, x y z vx vy vz t sx sy sz ");

  if (bins) { sprintf(tmp, " all bins=%ld ", (long)bins); strcat(Vars.option, tmp); }
  if (min > -FLT_MAX && max < FLT_MAX) { sprintf(tmp, " all limits=[%g %g]", min, max); strcat(Vars.option, tmp); }
  else if (min > -FLT_MAX) { sprintf(tmp, " all min=%g", min); strcat(Vars.option, tmp); }
  else if (max <  FLT_MAX) { sprintf(tmp, " all max=%g", max); strcat(Vars.option, tmp); }

  strncpy(Vars.UserName1,
    username1 && strlen(username1) && strcmp(username1, "0") && strcmp(username1, "NULL") ?
    username1 : "", 128);
  strncpy(Vars.UserName2,
    username2 && strlen(username2) && strcmp(username2, "0") && strcmp(username2, "NULL") ?
    username2 : "", 128);
  strncpy(Vars.UserName3,
    username3 && strlen(username3) && strcmp(username3, "0") && strcmp(username3, "NULL") ?
    username3 : "", 128);
  if (radius) {
    xwidth = zdepth = 2*radius;
    if (yheight && !strstr(Vars.option, "cylinder") && !strstr(Vars.option, "banana") && !strstr(Vars.option, "sphere"))
      strcat(Vars.option, " banana");
    else if (!yheight && !strstr(Vars.option ,"sphere")) {
      strcat(Vars.option, " sphere");
      yheight=2*radius;
    }
  }
  int offflag=0;
#ifndef USE_PGI
  if (geometry && strlen(geometry) && strcmp(geometry,"0") && strcmp(geometry, "NULL"))
    if (!off_init(  geometry, xwidth, yheight, zdepth, 1, &offdata )) {
      printf("Monitor_nD: %s could not initiate the OFF geometry %s. \n"
             "            Defaulting to normal Monitor dimensions.\n",
             NAME_CURRENT_COMP, geometry);
      strcpy(geometry, "");
    } else {
      offflag=1;
    }
#endif
  if (!radius && !xwidth && !yheight && !zdepth && !xmin && !xmax && !ymin && !ymax &&
    !strstr(Vars.option, "previous") && (!geometry || !strlen(geometry)))
    exit(printf("Monitor_nD: %s has no dimension specified. Aborting (radius, xwidth, yheight, zdepth, previous, geometry).\n", NAME_CURRENT_COMP));

  Monitor_nD_Init(&DEFS, &Vars, xwidth, yheight, zdepth, xmin,xmax,ymin,ymax,zmin,zmax,offflag);

#ifndef USE_PGI
  if (Vars.Flag_OFF) {
    offdata.mantidflag=Vars.Flag_mantid;
    offdata.mantidoffset=Vars.Coord_Min[Vars.Coord_Number-1];
  }
#endif

  if (filename && strlen(filename) && strcmp(filename,"NULL") && strcmp(filename,"0"))
    strncpy(Vars.Mon_File, filename, 128);

  /* check if user given filename with ext will be used more than once */
  if ( ((Vars.Flag_Multiple && Vars.Coord_Number > 1) || Vars.Flag_List) && strchr(Vars.Mon_File,'.') )
  { char *XY; XY = strrchr(Vars.Mon_File,'.'); *XY='_'; }

  if (restore_neutron) Vars.Flag_parallel=1;
  detector.m = 0;

#ifdef USE_MPI
MPI_MASTER(
  if (strstr(Vars.option, "auto") && mpi_node_count > 1)
    printf("Monitor_nD: %s is using automatic limits option 'auto' together with MPI.\n"
           "WARNING     this may create incorrect distributions (but integrated flux will be right).\n", NAME_CURRENT_COMP);
);
#else
#ifdef USE_PGI
  if (strstr(Vars.option, "auto"))
    printf("Monitor_nD: %s is using automatic limits option 'auto' together with GPU.\n"
           "WARNING     this may create incorrect distributions (but integrated flux will be right).\n", NAME_CURRENT_COMP);
#endif
#endif
  #undef user1
  #undef user2
  #undef user3
  #undef xwidth
  #undef yheight
  #undef zdepth
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef zmin
  #undef zmax
  #undef bins
  #undef min
  #undef max
  #undef restore_neutron
  #undef radius
  #undef options
  #undef filename
  #undef geometry
  #undef username1
  #undef username2
  #undef username3
  #undef DEFS
  #undef Vars
  #undef detector
  #undef offdata
  return(_comp);
} /* class_Monitor_nD_init */



int init(void) { /* called by mccode_main for PSI_DMC:INITIALISE */
  DEBUG_INSTR();

  /* code_main/parseoptions/readparams sets instrument parameters value */
  stracpy(instrument->_name, "PSI_DMC", 256);

  /* Instrument 'PSI_DMC' INITIALISE */
  SIG_MESSAGE("[PSI_DMC] INITIALISE [PSI_DMC.instr:80]");
  #define lambda (instrument->_parameters.lambda)
  #define R (instrument->_parameters.R)
  #define R_curve (instrument->_parameters.R_curve)
  #define filename (instrument->_parameters.filename)
  #define D_PHI (instrument->_parameters.D_PHI)
  #define SHIFT (instrument->_parameters.SHIFT)
  #define PACK (instrument->_parameters.PACK)
  #define Dw (instrument->_parameters.Dw)
  #define BARNS (instrument->_parameters.BARNS)
{
  TTM = 2*asin(mono_q*lambda/(4*PI))*RAD2DEG;
  OMA = TTM/2;
  RV = fabs(2*2.82*sin(DEG2RAD*OMA));
  
  angleGuideCurved=-2.0*asin(0.4995 /2.0/3612)/PI*180;
  alpha=(R0-R)/Qc/(Mvalue-1);
  alpha_curve=(R0_curve-R_curve)/Qc_curve/(Mvalue_curve-1);
  
}
  #undef lambda
  #undef R
  #undef R_curve
  #undef filename
  #undef D_PHI
  #undef SHIFT
  #undef PACK
  #undef Dw
  #undef BARNS
  _source_arm_setpos(); /* type Progress_bar */
  _source_setpos(); /* type Source_Maxwell_3 */
  _PSDbefore_guides_setpos(); /* type PSD_monitor */
  _l_mon_source_setpos(); /* type L_monitor */
  _guide1_setpos(); /* type Guide */
  _PSDbefore_curve_setpos(); /* type PSD_monitor */
  _guide2_setpos(); /* type Bender */
  _PSDafter_curve_setpos(); /* type PSD_monitor */
  _bunker_setpos(); /* type Guide */
  _guide3_setpos(); /* type Guide */
  _guide4_setpos(); /* type Guide */
  _window1_setpos(); /* type Al_window */
  _ydist_fluxpos_setpos(); /* type PSDlin_monitor */
  _PSD_fluxpos_setpos(); /* type PSD_monitor */
  _xdist_flux_pos_setpos(); /* type PSDlin_monitor */
  _PSD_fluxposB_setpos(); /* type PSD_monitor */
  _window2_setpos(); /* type Al_window */
  _in_slit_setpos(); /* type Slit */
  _lambda_in_setpos(); /* type L_monitor */
  _sma_setpos(); /* type Arm */
  _foc_mono_setpos(); /* type Monochromator_2foc */
  _msa_setpos(); /* type Arm */
  _out1_slit_setpos(); /* type Slit */
  _Amoin_slit_setpos(); /* type Slit */
  _Bmoin_slit_setpos(); /* type Slit */
  _out2_slit_setpos(); /* type Slit */
  _PSD_sample_setpos(); /* type PSD_monitor */
  _lambda_sample_setpos(); /* type L_monitor */
  _sa_arm_setpos(); /* type Arm */
  _sample_setpos(); /* type PowderN */
  _STOP_setpos(); /* type Beamstop */
  _Detector_setpos(); /* type Monitor_nD */

  /* call iteratively all components INITIALISE */
  class_Progress_bar_init(&_source_arm_var);

  class_Source_Maxwell_3_init(&_source_var);

  class_PSD_monitor_init(&_PSDbefore_guides_var);

  class_L_monitor_init(&_l_mon_source_var);

  class_Guide_init(&_guide1_var);

  class_PSD_monitor_init(&_PSDbefore_curve_var);

  class_Bender_init(&_guide2_var);

  class_PSD_monitor_init(&_PSDafter_curve_var);

  class_Guide_init(&_bunker_var);

  class_Guide_init(&_guide3_var);

  class_Guide_init(&_guide4_var);


  class_PSDlin_monitor_init(&_ydist_fluxpos_var);

  class_PSD_monitor_init(&_PSD_fluxpos_var);

  class_PSDlin_monitor_init(&_xdist_flux_pos_var);

  class_PSD_monitor_init(&_PSD_fluxposB_var);


  class_Slit_init(&_in_slit_var);

  class_L_monitor_init(&_lambda_in_var);


  class_Monochromator_2foc_init(&_foc_mono_var);


  class_Slit_init(&_out1_slit_var);

  class_Slit_init(&_Amoin_slit_var);

  class_Slit_init(&_Bmoin_slit_var);

  class_Slit_init(&_out2_slit_var);

  class_PSD_monitor_init(&_PSD_sample_var);

  class_L_monitor_init(&_lambda_sample_var);


  class_PowderN_init(&_sample_var);

  class_Beamstop_init(&_STOP_var);

  class_Monitor_nD_init(&_Detector_var);

  if (mcdotrace) display();
  DEBUG_INSTR_END();

#ifdef USE_PGI
#  include <openacc.h>
acc_attach( (void*)&_source_arm_var );
acc_attach( (void*)&_source_var );
acc_attach( (void*)&_PSDbefore_guides_var );
acc_attach( (void*)&_l_mon_source_var );
acc_attach( (void*)&_guide1_var );
acc_attach( (void*)&_PSDbefore_curve_var );
acc_attach( (void*)&_guide2_var );
acc_attach( (void*)&_PSDafter_curve_var );
acc_attach( (void*)&_bunker_var );
acc_attach( (void*)&_guide3_var );
acc_attach( (void*)&_guide4_var );
acc_attach( (void*)&_window1_var );
acc_attach( (void*)&_ydist_fluxpos_var );
acc_attach( (void*)&_PSD_fluxpos_var );
acc_attach( (void*)&_xdist_flux_pos_var );
acc_attach( (void*)&_PSD_fluxposB_var );
acc_attach( (void*)&_window2_var );
acc_attach( (void*)&_in_slit_var );
acc_attach( (void*)&_lambda_in_var );
acc_attach( (void*)&_sma_var );
acc_attach( (void*)&_foc_mono_var );
acc_attach( (void*)&_msa_var );
acc_attach( (void*)&_out1_slit_var );
acc_attach( (void*)&_Amoin_slit_var );
acc_attach( (void*)&_Bmoin_slit_var );
acc_attach( (void*)&_out2_slit_var );
acc_attach( (void*)&_PSD_sample_var );
acc_attach( (void*)&_lambda_sample_var );
acc_attach( (void*)&_sa_arm_var );
acc_attach( (void*)&_sample_var );
acc_attach( (void*)&_STOP_var );
acc_attach( (void*)&_Detector_var );
#pragma acc update device(_source_arm_var)
#pragma acc update device(_source_var)
#pragma acc update device(_PSDbefore_guides_var)
#pragma acc update device(_l_mon_source_var)
#pragma acc update device(_guide1_var)
#pragma acc update device(_PSDbefore_curve_var)
#pragma acc update device(_guide2_var)
#pragma acc update device(_PSDafter_curve_var)
#pragma acc update device(_bunker_var)
#pragma acc update device(_guide3_var)
#pragma acc update device(_guide4_var)
#pragma acc update device(_window1_var)
#pragma acc update device(_ydist_fluxpos_var)
#pragma acc update device(_PSD_fluxpos_var)
#pragma acc update device(_xdist_flux_pos_var)
#pragma acc update device(_PSD_fluxposB_var)
#pragma acc update device(_window2_var)
#pragma acc update device(_in_slit_var)
#pragma acc update device(_lambda_in_var)
#pragma acc update device(_sma_var)
#pragma acc update device(_foc_mono_var)
#pragma acc update device(_msa_var)
#pragma acc update device(_out1_slit_var)
#pragma acc update device(_Amoin_slit_var)
#pragma acc update device(_Bmoin_slit_var)
#pragma acc update device(_out2_slit_var)
#pragma acc update device(_PSD_sample_var)
#pragma acc update device(_lambda_sample_var)
#pragma acc update device(_sa_arm_var)
#pragma acc update device(_sample_var)
#pragma acc update device(_STOP_var)
#pragma acc update device(_Detector_var)
acc_attach( (void*)&_instrument_var );
#pragma acc update device(_instrument_var)
#endif

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
/* if on GPU, globally nullify sprintf,fprintf,printfs   */
/* (Similar defines are available in each comp trace but */
/*  those are not enough to handle external libs etc. )  */
#ifdef USE_PGI
#define fprintf(stderr,...) printf(__VA_ARGS__)
#define sprintf(string,...) printf(__VA_ARGS__)
#define printf(...) noprintf()
#define exit(...) noprintf()
#define strcmp(a,b) str_comp(a,b)
#define strlen(a) str_len(a)
#endif
#define SCATTERED (_particle->_scattered)
#define RESTORE (_particle->_restore)
#define RESTORE_NEUTRON(_index, ...) _particle->_restore = _index;
#define ABSORBED (_particle->_absorbed)
#define ABSORB0 do { DEBUG_STATE(); DEBUG_ABSORB(); MAGNET_OFF; ABSORBED++; return(_comp); } while(0)
#define ABSORB ABSORB0
#pragma acc routine seq
_class_Progress_bar *class_Progress_bar_trace(_class_Progress_bar *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;
  #define profile (_comp->_parameters.profile)
  #define percent (_comp->_parameters.percent)
  #define flag_save (_comp->_parameters.flag_save)
  #define minutes (_comp->_parameters.minutes)
  #define IntermediateCnts (_comp->_parameters.IntermediateCnts)
  #define StartTime (_comp->_parameters.StartTime)
  #define EndTime (_comp->_parameters.EndTime)
  #define CurrentTime (_comp->_parameters.CurrentTime)
  SIG_MESSAGE("[_source_arm_trace] component source_arm=Progress_bar() TRACE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/misc/Progress_bar.comp:70]");

{
#ifndef USE_PGI
  double ncount;
  ncount = mcget_run_num();
  if (!StartTime) {
    time(&StartTime); /* compute starting time */
    IntermediateCnts = 1e3;
  }
  time_t NowTime;
  time(&NowTime);
  /* compute initial estimate of computation duration */
  if (!EndTime && ncount >= IntermediateCnts) {
    CurrentTime = NowTime;
    if (difftime(NowTime,StartTime) > 10 && ncount) { /* wait 10 sec before writing ETA */
      EndTime = StartTime + (time_t)(difftime(NowTime,StartTime)
				     *(double)mcget_ncount()/ncount);
      IntermediateCnts = 0;
      fprintf(stdout, "\nTrace ETA ");
      if (difftime(EndTime,StartTime) < 60.0)
        fprintf(stdout, "%g [s] %% ", difftime(EndTime,StartTime));
      else if (difftime(EndTime,StartTime) > 3600.0)
        fprintf(stdout, "%g [h] %% ", difftime(EndTime,StartTime)/3600.0);
      else
        fprintf(stdout, "%g [min] %% ", difftime(EndTime,StartTime)/60.0);
    } else IntermediateCnts += 1e3;
    fflush(stdout);
  }

  /* display percentage when percent or minutes have reached step */
  if (EndTime && mcget_ncount() &&
    (    (minutes && difftime(NowTime,CurrentTime) > minutes*60)
      || (percent && !minutes && ncount >= IntermediateCnts))   )
  {
    fprintf(stdout, "%d ", (int)(ncount*100.0/mcget_ncount())); fflush(stdout);
    CurrentTime = NowTime;

    IntermediateCnts = ncount + percent*mcget_ncount()/100;
    /* check that next intermediate ncount check is a multiple of the desired percentage */
    IntermediateCnts = floor(IntermediateCnts*100/percent/mcget_ncount())*percent*mcget_ncount()/100;
    /* raise flag to indicate that we did something */
    SCATTER;
    if (flag_save) save(NULL);
  }
#endif
}
  #undef profile
  #undef percent
  #undef flag_save
  #undef minutes
  #undef IntermediateCnts
  #undef StartTime
  #undef EndTime
  #undef CurrentTime
  return(_comp);
} /* class_Progress_bar_trace */

#pragma acc routine seq
_class_Source_Maxwell_3 *class_Source_Maxwell_3_trace(_class_Source_Maxwell_3 *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;
  #define size (_comp->_parameters.size)
  #define yheight (_comp->_parameters.yheight)
  #define xwidth (_comp->_parameters.xwidth)
  #define Lmin (_comp->_parameters.Lmin)
  #define Lmax (_comp->_parameters.Lmax)
  #define dist (_comp->_parameters.dist)
  #define focus_xw (_comp->_parameters.focus_xw)
  #define focus_yh (_comp->_parameters.focus_yh)
  #define T1 (_comp->_parameters.T1)
  #define T2 (_comp->_parameters.T2)
  #define T3 (_comp->_parameters.T3)
  #define I1 (_comp->_parameters.I1)
  #define I2 (_comp->_parameters.I2)
  #define I3 (_comp->_parameters.I3)
  #define target_index (_comp->_parameters.target_index)
  #define lambda0 (_comp->_parameters.lambda0)
  #define dlambda (_comp->_parameters.dlambda)
  #define M (_comp->_parameters.M)
  #define l_range (_comp->_parameters.l_range)
  #define w_mult (_comp->_parameters.w_mult)
  #define w_source (_comp->_parameters.w_source)
  #define h_source (_comp->_parameters.h_source)
  SIG_MESSAGE("[_source_trace] component source=Source_Maxwell_3() TRACE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/sources/Source_Maxwell_3.comp:122]");

{
  double v,tau_l,E,lambda,k,r,xf,yf,dx,dy,w_focus;

  t=0;
  z=0;
  x = 0.5*w_source*randpm1();
  y = 0.5*h_source*randpm1();         /* Choose initial position */

  randvec_target_rect_real(&xf, &yf, &r, &w_focus,
		      0, 0, dist, focus_xw, focus_yh, ROT_A_CURRENT_COMP, x, y, z, 2);

  dx = xf-x;
  dy = yf-y;
  r = sqrt(dx*dx+dy*dy+dist*dist);

  lambda = Lmin+l_range*rand01();    /* Choose from uniform distribution */
  k = 2*PI/lambda;
  v = K2V*k;

  vz = v*dist/r;
  vy = v*dy/r;
  vx = v*dx/r;


/*  printf("pos0 (%g %g %g), pos1 (%g %g %g), r: %g, v (%g %g %g), v %g\n",
  x,y,z,xf,yf,dist,r,vx,vy,vz, v);
  printf("l %g, w_focus %g \n", lambda, w_focus);  */

  p *= w_mult*w_focus;                /* Correct for target focusing etc */
  p *= I1*SM3_Maxwell(lambda,T1)+I2*SM3_Maxwell(lambda,T2)+I3*SM3_Maxwell(lambda,T3);
                                        /* Calculate true intensity */
}
  #undef size
  #undef yheight
  #undef xwidth
  #undef Lmin
  #undef Lmax
  #undef dist
  #undef focus_xw
  #undef focus_yh
  #undef T1
  #undef T2
  #undef T3
  #undef I1
  #undef I2
  #undef I3
  #undef target_index
  #undef lambda0
  #undef dlambda
  #undef M
  #undef l_range
  #undef w_mult
  #undef w_source
  #undef h_source
  return(_comp);
} /* class_Source_Maxwell_3_trace */

#pragma acc routine seq
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
  #define nowritefile (_comp->_parameters.nowritefile)
  #define PSD_N (_comp->_parameters.PSD_N)
  #define PSD_p (_comp->_parameters.PSD_p)
  #define PSD_p2 (_comp->_parameters.PSD_p2)
  SIG_MESSAGE("[_PSDbefore_guides_trace] component PSDbefore_guides=PSD_monitor() TRACE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/PSD_monitor.comp:88]");

{
  PROP_Z0;
  if (x>xmin && x<xmax && y>ymin && y<ymax){
    int i = floor((x - xmin)*nx/(xmax - xmin));
    int j = floor((y - ymin)*ny/(ymax - ymin));

    double p2 = p*p;
    #pragma acc atomic
    PSD_N[i][j] = PSD_N[i][j]+1;

    #pragma acc atomic
    PSD_p[i][j] = PSD_p[i][j]+p;
    
    #pragma acc atomic
    PSD_p2[i][j] = PSD_p2[i][j] + p2;
    
    SCATTER;
  }
  if (restore_neutron) {
    RESTORE_NEUTRON(INDEX_CURRENT_COMP, x, y, z, vx, vy, vz, t, sx, sy, sz, p);
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
  #undef nowritefile
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  return(_comp);
} /* class_PSD_monitor_trace */

#pragma acc routine seq
_class_L_monitor *class_L_monitor_trace(_class_L_monitor *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;
  #define nL (_comp->_parameters.nL)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define Lmin (_comp->_parameters.Lmin)
  #define Lmax (_comp->_parameters.Lmax)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define L_N (_comp->_parameters.L_N)
  #define L_p (_comp->_parameters.L_p)
  #define L_p2 (_comp->_parameters.L_p2)
  SIG_MESSAGE("[_l_mon_source_trace] component l_mon_source=L_monitor() TRACE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/L_monitor.comp:90]");

{
  PROP_Z0;
  if (x>xmin && x<xmax && y>ymin && y<ymax)
  {
    double L = (2*PI/V2K)/sqrt(vx*vx + vy*vy + vz*vz);
    int i = floor((L-Lmin)*nL/(Lmax-Lmin));
    if(i >= 0 && i < nL)
    {
      double p2 = p*p;
      #pragma acc atomic
      L_N[i] = L_N[i] +1;
      #pragma acc atomic
      L_p[i] = L_p[i] + p;
      #pragma acc atomic
      L_p2[i] = L_p2[i] + p2;
      SCATTER;
    }
  }
  if (restore_neutron) {
    RESTORE_NEUTRON(INDEX_CURRENT_COMP, x, y, z, vx, vy, vz, t, sx, sy, sz, p);
  }
}
  #undef nL
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef Lmin
  #undef Lmax
  #undef restore_neutron
  #undef L_N
  #undef L_p
  #undef L_p2
  return(_comp);
} /* class_L_monitor_trace */

#pragma acc routine seq
_class_Guide *class_Guide_trace(_class_Guide *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;
  #define reflect (_comp->_parameters.reflect)
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
  #define pTable (_comp->_parameters.pTable)
  #define table_present (_comp->_parameters.table_present)
  SIG_MESSAGE("[_guide1_trace] component guide1=Guide() TRACE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Guide.comp:96]");

{
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
    if (reflect && table_present==1)
       TableReflecFunc(q, &pTable, &weight);
    else {
      double par[] = {R0, Qc, alpha, m, W};
      StdReflecFunc(q, par, &weight);
    }
    if (weight > 0)
      p *= weight;
    else ABSORB;
    SCATTER;
  }
}
  #undef reflect
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
  #undef pTable
  #undef table_present
  return(_comp);
} /* class_Guide_trace */

#pragma acc routine seq
_class_Bender *class_Bender_trace(_class_Bender *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;
  #define w (_comp->_parameters.w)
  #define h (_comp->_parameters.h)
  #define r (_comp->_parameters.r)
  #define Win (_comp->_parameters.Win)
  #define k (_comp->_parameters.k)
  #define d (_comp->_parameters.d)
  #define l (_comp->_parameters.l)
  #define R0a (_comp->_parameters.R0a)
  #define Qca (_comp->_parameters.Qca)
  #define alphaa (_comp->_parameters.alphaa)
  #define ma (_comp->_parameters.ma)
  #define Wa (_comp->_parameters.Wa)
  #define R0i (_comp->_parameters.R0i)
  #define Qci (_comp->_parameters.Qci)
  #define alphai (_comp->_parameters.alphai)
  #define mi (_comp->_parameters.mi)
  #define Wi (_comp->_parameters.Wi)
  #define R0s (_comp->_parameters.R0s)
  #define Qcs (_comp->_parameters.Qcs)
  #define alphas (_comp->_parameters.alphas)
  #define ms (_comp->_parameters.ms)
  #define Ws (_comp->_parameters.Ws)
  #define bk (_comp->_parameters.bk)
  #define mWin (_comp->_parameters.mWin)
  SIG_MESSAGE("[_guide2_trace] component guide2=Bender() TRACE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Bender.comp:133]");

{
    int i,num,numa,numi;
    double dru,ab,dab,R,Q,Ta,vpl;
    double einmWin,ausmWin,zykmWin,aeumWin,innmWin,ref,innref,aeuref;
    double einzei,auszei,zykzei;

    /* does the neutron hit the bender at the entrance? */
    PROP_Z0;
    if ((fabs(x)<w/2) && (fabs(y)<h/2))
    {
      /*** reflections in the XZ-plane ***/

      /* distance between neutron and concave side of the channel at the entrance */
      dru=floor((w/2-x)/bk)*bk;
      ab=w/2.0-x-dru;

      /* radius of the channel */
      R=r-dru;

      /* does the neutron hit the partition at the entrance? */
      if (ab<bk-d)
      {
        double aeu[] = {R0a, Qca, alphaa, ma, Wa};
        /* velocity in the XZ-plane */
        vpl=sqrt(vx*vx+vz*vz);

        /* divergence of the neutron at the entrance */
        einmWin=atan(vx/vz);

        /* maximal distance between neutron and concave side of the channel */
        dab=R-cos(einmWin)*(R-ab);

        /* reflection angle at the concave side */
        aeumWin=acos((R-dab)/R);

        /* reflection coefficient at the concave side */
        Q=2.0*V2K*vpl*sin(aeumWin);
        StdReflecFunc(Q, aeu, &aeuref);

        /* does the neutron hit the convex side of the channel? */
        innmWin=0.0;
        innref=1.0;
        if (dab>bk-d)
        {
           double inn[] = {R0i, Qci, alphai, mi, Wi};
           /* reflection coefficient at the convex side */
           innmWin=acos((R-dab)/(R-bk+d));
           Q=2.0*V2K*vpl*sin(innmWin);
           StdReflecFunc(Q, inn, &innref);
        }

        /* divergence of the neutron at the exit */
        zykmWin=2.0*(aeumWin-innmWin);
        ausmWin=fmod(mWin+einmWin+aeumWin-innmWin
          *(1.0+SIGN(einmWin)),zykmWin)-zykmWin/2.0;
        ausmWin+=innmWin*SIGN(ausmWin);

        /* number of reflections at the concave side */
        numa=(mWin+einmWin+aeumWin-innmWin*(1.0+SIGN(einmWin)))/zykmWin;

        /* number of reflections at the convex side */
        numi=numa;
        if (ausmWin*einmWin<0)
        {
           if (ausmWin-einmWin>0)
              numi++;
           else
              numi--;
        }

        /* is the reflection coefficient too small? */
        if (((numa>0) && (aeuref<=0)) || ((numi>0) && (innref<=0)))
           ABSORB;

        /* calculation of the neutron probability weight p */
        for (i=1;i<=numa;i++)
            p*=aeuref;
        for (i=1;i<=numi;i++)
            p*=innref;

        /* time to cross the bender */
        Ta=(2*numa*(tan(aeumWin)-tan(innmWin))
          +tan(ausmWin)-tan(einmWin)
          -tan(innmWin)*(SIGN(ausmWin)-SIGN(einmWin)))
          *(R-dab)/vpl;
        t+=Ta;

        /* distance between neutron and concave side of channel at the exit */
        ab=R-(R-dab)/cos(ausmWin);

        /* calculation of the exit coordinates in the XZ-plane */
        x=w/2.0-ab-dru;
        z=r*mWin;
        vx=sin(ausmWin)*vpl;
        vz=cos(ausmWin)*vpl;

        /*** reflections at top and bottom side (Y axis) ***/

        if (vy!=0.0)
        {
          double s[] = {R0s, Qcs, alphas, ms, Ws};
          /* reflection coefficent at the top and bottom side */
          Q=2.0*V2K*fabs(vy);
          StdReflecFunc(Q, s, &ref);

          /* number of reflections at top and bottom */
          einzei=h/2.0/fabs(vy)+y/vy;
          zykzei=h/fabs(vy);
          num=(Ta+einzei)/zykzei;

          /* time between the last reflection and the exit */
          auszei=fmod(Ta+einzei,zykzei);

          /* is the reflection coefficient too small? */
          if ((num>0) && (ref<=0))
             ABSORB;

          /* calculation of the probability weight p */
          for (i=1;i<=num;i++) {
               p*=ref;
               vy*=-1.0; }

          /* calculation of the exit coordinate */
          y=auszei*vy-vy*h/fabs(vy)/2.0;
        } /* if (vy!=0.0) */
        SCATTER;
      } /* if (dab>bk-d)  */
      else
        ABSORB; /* hit separating walls */
    }
    else /* if ((fabs(x)<w/2) && (fabs(y)<h/2))   */
      ABSORB; /* miss entry window */

}
  #undef w
  #undef h
  #undef r
  #undef Win
  #undef k
  #undef d
  #undef l
  #undef R0a
  #undef Qca
  #undef alphaa
  #undef ma
  #undef Wa
  #undef R0i
  #undef Qci
  #undef alphai
  #undef mi
  #undef Wi
  #undef R0s
  #undef Qcs
  #undef alphas
  #undef ms
  #undef Ws
  #undef bk
  #undef mWin
  return(_comp);
} /* class_Bender_trace */

#pragma acc routine seq
_class_Al_window *class_Al_window_trace(_class_Al_window *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;
  #define thickness (_comp->_parameters.thickness)
  SIG_MESSAGE("[_window1_trace] component window1=Al_window() TRACE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/contrib/Al_window.comp:68]");

{
  double v;                     /* Neutron velocity */
  double dt0;     /* Flight times through sample */
  double dist;
  double Al_s_tot_lambda,Al_my_tot,Al_my_a ; /* total XS (barn), total scattering length (m-1), absorption scat. length */
  double lambda; /* neutrons wavelength */

  PROP_Z0;

  dt0=thickness/vz;
  v=sqrt(vx*vx+vy*vy+vz*vz);
  PROP_DT(dt0);
  dist=v*dt0;

  lambda=sqrt(81.81/(VS2E*v*v));
  Al_s_tot_lambda= Al_pf_A+Al_pf_B1*lambda+ Al_pf_B2*lambda*lambda+ Al_pf_B3*lambda*lambda*lambda;
  Al_s_tot_lambda+=Al_pf_B4*lambda*lambda*lambda*lambda;
  Al_my_tot=Al_rho / Al_mmol * Al_s_tot_lambda * avogadro * 10;
  Al_my_a = Al_my_a_v/v;

  p *=exp(-Al_my_a*dist);/* neutron passes window without any interaction */

  /* TODO: scatter in Debye-Scherrer cone */

}
  #undef thickness
  return(_comp);
} /* class_Al_window_trace */

#pragma acc routine seq
_class_PSDlin_monitor *class_PSDlin_monitor_trace(_class_PSDlin_monitor *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;
  #define nx (_comp->_parameters.nx)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define PSDlin_N (_comp->_parameters.PSDlin_N)
  #define PSDlin_p (_comp->_parameters.PSDlin_p)
  #define PSDlin_p2 (_comp->_parameters.PSDlin_p2)
  SIG_MESSAGE("[_ydist_fluxpos_trace] component ydist_fluxpos=PSDlin_monitor() TRACE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/PSDlin_monitor.comp:84]");

{
  int i;

  PROP_Z0;
  if (x>xmin && x<xmax && y>ymin && y<ymax)
  {
    i = floor(nx*(x-xmin)/(xmax-xmin));              /* Bin number */
    if((i >= nx) || (i<0))
    {
      printf("FATAL ERROR: wrong positioning in linear PSD. i= %i \n",i);
      exit(1);
    }
    double p2 = p*p;
    #pragma acc atomic
    PSDlin_N[i] = PSDlin_N[i] +1;
    #pragma acc atomic
    PSDlin_p[i] = PSDlin_p[i] + p;
    #pragma acc atomic
    PSDlin_p2[i] = PSDlin_p2[i] + p2;
    SCATTER;
  }
  if (restore_neutron) {
    RESTORE_NEUTRON(INDEX_CURRENT_COMP, x, y, z, vx, vy, vz, t, sx, sy, sz, p);
  }
}
  #undef nx
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef PSDlin_N
  #undef PSDlin_p
  #undef PSDlin_p2
  return(_comp);
} /* class_PSDlin_monitor_trace */

#pragma acc routine seq
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
  SIG_MESSAGE("[_in_slit_trace] component in_slit=Slit() TRACE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Slit.comp:71]");

{
    PROP_Z0;
    if (((radius == 0) && (x<xmin || x>xmax || y<ymin || y>ymax))
    || ((radius != 0) && (x*x + y*y > radius*radius)))
      ABSORB;
    else
        SCATTER;
}
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef radius
  #undef xwidth
  #undef yheight
  return(_comp);
} /* class_Slit_trace */

#pragma acc routine seq
_class_Monochromator_2foc *class_Monochromator_2foc_trace(_class_Monochromator_2foc *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;
  #define reflect (_comp->_parameters.reflect)
  #define zwidth (_comp->_parameters.zwidth)
  #define yheight (_comp->_parameters.yheight)
  #define gap (_comp->_parameters.gap)
  #define NH (_comp->_parameters.NH)
  #define NV (_comp->_parameters.NV)
  #define mosaich (_comp->_parameters.mosaich)
  #define mosaicv (_comp->_parameters.mosaicv)
  #define r0 (_comp->_parameters.r0)
  #define Q (_comp->_parameters.Q)
  #define RV (_comp->_parameters.RV)
  #define RH (_comp->_parameters.RH)
  #define DM (_comp->_parameters.DM)
  #define mosaic (_comp->_parameters.mosaic)
  #define width (_comp->_parameters.width)
  #define height (_comp->_parameters.height)
  #define verbose (_comp->_parameters.verbose)
  #define mos_y (_comp->_parameters.mos_y)
  #define mos_z (_comp->_parameters.mos_z)
  #define mono_Q (_comp->_parameters.mono_Q)
  #define SlabWidth (_comp->_parameters.SlabWidth)
  #define SlabHeight (_comp->_parameters.SlabHeight)
  #define rTable (_comp->_parameters.rTable)
  SIG_MESSAGE("[_foc_mono_trace] component foc_mono=Monochromator_2foc() TRACE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/contrib/Monochromator_2foc.comp:149]");

{
  double dt;

  if(vx != 0.0 && (dt = -x/vx) >= 0.0)
  {
    double zmin,zmax, ymin,ymax, zp,yp, y1,z1,t1;

    zmax = ((NH*(SlabWidth+gap))-gap)/2;
    zmin = -1*zmax;
    ymax = ((NV*(SlabHeight+gap))-gap)/2;
    ymin = -1*ymax;
    y1 = y + vy*dt;             /* Propagate to crystal plane */
    z1 = z + vz*dt;
    t1 = t + dt;
    zp = fmod ( (z1-zmin),(SlabWidth+gap) );
    yp = fmod ( (y1-ymin),(SlabHeight+gap) );


    /* hit a slab or a gap ? */

    if (z1>zmin && z1<zmax && y1>ymin && y1<ymax && zp<SlabWidth && yp< SlabHeight)
    {
      double row,col, sna,snb,csa,csb,vxp,vyp,vzp;
      double v, theta0, theta, tmp3;
      double tilth,tiltv;         /* used to calculate tilt angle of slab */

      col = ceil ( (z1-zmin)/(SlabWidth+gap));  /* which slab hit ? */
      row = ceil ( (y1-ymin)/(SlabHeight+gap));
      if (RH != 0) tilth = asin((col-(NH+1)/2)*(SlabWidth+gap)/RH);
      else tilth=0;
      if (RV != 0) tiltv = -asin((row-(NV+1)/2)*(SlabHeight+gap)/RV);
      else tiltv=0;

      /* rotate with tilth and tiltv */

      sna = sin(tilth);
      snb = sin(tiltv);
      csa = cos(tilth);
      csb = cos(tiltv);
      vxp = vx*csa*csb+vy*snb-vz*sna*csb;
      vyp = -vx*csa*snb+vy*csb+vz*sna*snb;
      vzp = vx*sna+vz*csa;

      /* First: scattering in plane */
      /* theta0 = atan2(vx,vz);  neutron angle to slab Risoe version */

      v = sqrt(vxp*vxp+vyp*vyp+vzp*vzp);
      theta0 = asin(vxp/v);                /* correct neutron angle to slab */

      theta = asin(Q2V*mono_Q/(2.0*v));               /* Bragg's law */
      if (theta0 < 0)
              theta = -theta;
      tmp3 = (theta-theta0)/(MIN2RAD*mos_y);
      if (tmp3 < DIV_CUTOFF)
      {
        double my_r0, k;
        double dphi,tmp1,tmp2,tmp4,vratio,phi,cs,sn;

        k = V2K*v;

#ifndef USE_PGI
        if (rTable.data != NULL)
        {
          my_r0 = r0*Table_Value(rTable, k, 1); /* 2nd column */
        }
        else
#endif
	  my_r0 = r0;

	if (my_r0 >= 1)
        {
#ifndef USE_PGI
          if (verbose) fprintf(stdout, "Warning: Monochromator_2foc: %s: lowered reflectivity from %f to 0.99 (k=%f)\n", 
            NAME_CURRENT_COMP, my_r0, k);
#endif
          my_r0=0.99;
        }
        if (my_r0 < 0)
        {
#ifndef USE_PGI
          if (verbose) fprintf(stdout, "Warning: Monochromator_2foc: %s: raised reflectivity from %f to 0 (k=%f)\n", 
          NAME_CURRENT_COMP, my_r0, k);
#endif
          my_r0=0;
        }
        x = 0.0;
        y = y1;
        z = z1;
        t = t1;
        
        /* reflectivity */
        t1 = fabs(my_r0)*exp(-tmp3*tmp3*4*log(2));
        if (t1 <= 0) ABSORB;
        if (t1 > 1)  t1 = 1;
        p *= t1; /* Use mosaics */
        
        tmp1 = 2*theta;
        cs = cos(tmp1);
        sn = sin(tmp1);
        tmp2 = cs*vxp - sn*vzp;
        vyp = vyp;
        /* vz = cs*vz + sn*vx; diese Zeile wurde durch die folgende ersetzt */
        tmp4 = vyp/vzp;  /* korrigiert den schr�en Einfall aufs Pl�tchen  */
        vzp = cs*(-vyp*sin(tmp4)+vzp*cos(tmp4)) + sn*vxp;
        vxp = tmp2;

        /* Second: scatering out of plane.
           Approximation is that Debye-Scherrer cone is a plane */

        phi = atan2(vyp,vzp);  /* out-of plane angle */
        dphi = (MIN2RAD*mos_z)/(2*sqrt(2*log(2)))*randnorm();  /* MC choice: */
        /* Vertical angle of the crystallite */
        vyp = vzp*tan(phi+2*dphi*sin(theta));
        vratio = v/sqrt(vxp*vxp+vyp*vyp+vzp*vzp);
        vzp = vzp*vratio;
        vyp = vyp*vratio;                             /* Renormalize v */
        vxp = vxp*vratio;

        /* rotate v coords back */
        vx = vxp*csb*csa-vyp*snb*csa+vzp*sna;
        vy = vxp*snb+vyp*csb;
        vz = -vxp*csb*sna+vyp*snb*sna+vzp*csa;
        /* v=sqrt(vx*vx+vy*vy+vz*vz);  */
        SCATTER;
      } /* end if Bragg ok */
    } /* End intersect the crystal (if z1) */
  } /* End neutron moving towards crystal (if vx)*/
}
  #undef reflect
  #undef zwidth
  #undef yheight
  #undef gap
  #undef NH
  #undef NV
  #undef mosaich
  #undef mosaicv
  #undef r0
  #undef Q
  #undef RV
  #undef RH
  #undef DM
  #undef mosaic
  #undef width
  #undef height
  #undef verbose
  #undef mos_y
  #undef mos_z
  #undef mono_Q
  #undef SlabWidth
  #undef SlabHeight
  #undef rTable
  return(_comp);
} /* class_Monochromator_2foc_trace */

#pragma acc routine seq
_class_PowderN *class_PowderN_trace(_class_PowderN *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;
  #define reflections (_comp->_parameters.reflections)
  #define geometry (_comp->_parameters.geometry)
  #define format (_comp->_parameters.format)
  #define radius (_comp->_parameters.radius)
  #define yheight (_comp->_parameters.yheight)
  #define xwidth (_comp->_parameters.xwidth)
  #define zdepth (_comp->_parameters.zdepth)
  #define thickness (_comp->_parameters.thickness)
  #define pack (_comp->_parameters.pack)
  #define Vc (_comp->_parameters.Vc)
  #define sigma_abs (_comp->_parameters.sigma_abs)
  #define sigma_inc (_comp->_parameters.sigma_inc)
  #define delta_d_d (_comp->_parameters.delta_d_d)
  #define p_inc (_comp->_parameters.p_inc)
  #define p_transmit (_comp->_parameters.p_transmit)
  #define DW (_comp->_parameters.DW)
  #define nb_atoms (_comp->_parameters.nb_atoms)
  #define d_phi (_comp->_parameters.d_phi)
  #define p_interact (_comp->_parameters.p_interact)
  #define concentric (_comp->_parameters.concentric)
  #define density (_comp->_parameters.density)
  #define weight (_comp->_parameters.weight)
  #define barns (_comp->_parameters.barns)
  #define Strain (_comp->_parameters.Strain)
  #define focus_flip (_comp->_parameters.focus_flip)
  #define line_info (_comp->_parameters.line_info)
  #define columns (_comp->_parameters.columns)
  #define offdata (_comp->_parameters.offdata)
  SIG_MESSAGE("[_sample_trace] component sample=PowderN() TRACE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/samples/PowderN.comp:754]");

{
  double t0, t1, t2, t3, v, v1,l_full, l, l_1, dt, alpha0, alpha, theta, my_s, my_s_n;
  double solid_angle, neutrontype;
  double arg, tmp_vx, tmp_vy, tmp_vz, vout_x, vout_y, vout_z, nx, ny, nz, pmul=1;
  int    line;
  char   intersect=0;
  char   intersecti=0;

  line_info.type = '\0';

  if (line_info.V_0 > 0 && (line_info.count || line_info.my_inc)) {
    if (line_info.shape == 1) {
      intersect  = box_intersect(&t0, &t3, x, y, z, vx, vy, vz, xwidth, yheight, zdepth);
      intersecti = box_intersect(&t1, &t2, x, y, z, vx, vy, vz, line_info.xwidth_i, line_info.yheight_i, line_info.zdepth_i);
    } else if (line_info.shape == 0) {
      intersect  = cylinder_intersect(&t0, &t3, x, y, z, vx, vy, vz, radius, yheight);
      intersecti = cylinder_intersect(&t1, &t2, x, y, z, vx, vy, vz, line_info.radius_i, line_info.yheight_i);
    } else if (line_info.shape == 2) {
      intersect  = sphere_intersect  (&t0, &t3, x,y,z, vx,vy,vz, radius);
      intersecti = sphere_intersect  (&t1, &t2, x,y,z, vx,vy,vz, line_info.radius_i);
    } else if (line_info.shape == 3) {
#ifndef USE_PGI
      intersect  = off_intersect  (&t0, &t3, NULL, NULL, x,y,z, vx,vy,vz, offdata);
      intersecti = 0;
#endif
    }
  }

  if(intersect && t3 >0) {

    if (concentric) {
      /* Set up for concentric case */
      /* 'Remove' the backside of this comp */
      if (!intersecti) {
        t1 = (t3 + t0) /2;
      }
      t2 = t1;
      t3 = t1;
      dt = -1.0*rand01(); /* In case of scattering we will scatter on 'forward' part of sample */
    } else {
      if (!intersecti) {
        t1 = (t3 + t0) /2;
        t2 = t1;
      }
      dt = randpm1(); /* Possibility to scatter at all points in line of sight */
    }

    /* Neutron enters at t=t0. */
    if(t0 < 0) t0=0; /* already in sample */
    if(t1 < 0) t1=0; /* already in inner hollow */
    if(t2 < 0) t2=0; /* already past inner hollow */
    v = sqrt(vx*vx + vy*vy + vz*vz);
    l_full = v * (t3 - t2 + t1 - t0);

    if (line_info.neutron_passed < CHAR_BUF_LENGTH) {
      if (v < line_info.v_min) line_info.v_min = v;
      if (v > line_info.v_max) line_info.v_max = v;
      line_info.neutron_passed++;
    }

    /* Calculate total scattering cross section at relevant velocity */
    if ( fabs(v - line_info.v) < 1e-6) {
        line_info.nb_reuses++;
      } else {
        line_info.Nq = calc_xsect(v, line_info.q_v, line_info.my_s_v2, line_info.count, &line_info.my_s_v2_sum, &line_info);
        line_info.v = v;
        line_info.nb_refl += line_info.Nq;
        line_info.nb_refl_count++;
      }

    if (t3 < 0) {
      t3=0; /* Already past sample?! */
      if (line_info.flag_warning < 100)
      printf("PowderN: %s: Warning: Neutron has already passed us? (Skipped).\n"
             "         In concentric geometry, this may be caused by a missing concentric=0 option in 2nd enclosing instance.\n", NAME_CURRENT_COMP);
      line_info.flag_warning++;
    } else {
      if (dt<0) { /* Calculate scattering point position */
        dt = fabs(dt)*(t1 - t0); /* 'Forward' part */
      } else {
        dt = dt * (t3 - t2) + (t2-t0) ; /* Possibly also 'backside' part */
      }

      my_s = line_info.my_s_v2_sum/(v*v)+line_info.my_inc;
      /* Total attenuation from scattering */

      neutrontype = rand01();
      /* How to handle this one? Transmit (1) / Incoherent (2) / Coherent (3) ? */
      if (neutrontype < p_transmit) {
        neutrontype = 1;
        l = l_full; /* Passing through, full length */
        PROP_DT(t3);
      } else if (neutrontype >= p_transmit && neutrontype < (p_transmit + p_inc)) {
        neutrontype = 2;
        l = v*dt;       /* Penetration in sample */
        PROP_DT(dt+t0); /* Point of scattering */
        SCATTER;
      } else if (neutrontype >= p_transmit + p_inc) {
        neutrontype = 3;
        l = v*dt;       /* Penetration in sample */
        PROP_DT(dt+t0); /* Point of scattering */
        SCATTER;
      } else {
        exit(fprintf(stderr,"PowderN %s: DEAD - this shouldn't happen!\n", NAME_CURRENT_COMP));
      }

      if (neutrontype == 3) { /* Make coherent scattering event */
        if (line_info.count > 0) {
          /* choose line */
          if (line_info.Nq > 1) line=floor(line_info.Nq*rand01());  /* Select between Nq powder lines */
          else line = 0;
          if (line_info.w_v[line])
            arg = line_info.q_v[line]*(1+line_info.w_v[line]*randnorm())/(2.0*v);
          else
            arg = line_info.q_v[line]/(2.0*v);
          my_s_n = line_info.my_s_v2[line]/(v*v);
          if(fabs(arg) > 1)
            ABSORB;                   /* No bragg scattering possible*/
          theta = asin(arg);          /* Bragg scattering law */

          /* Choose point on Debye-Scherrer cone */
          if (d_phi)
          { /* relate height of detector to the height on DS cone */
            arg = sin(d_phi*DEG2RAD/2)/sin(2*theta);
            /* If full Debye-Scherrer cone is within d_phi, don't focus */
            if (arg < -1 || arg > 1) d_phi = 0;
            /* Otherwise, determine alpha to rotate from scattering plane
               into d_phi focusing area*/
            else alpha = 2*asin(arg);
          }
          if (d_phi) {
            /* Focusing */
            alpha = fabs(alpha);
            /* Trick to get scattering for pos/neg theta's */
            alpha0= 2*rand01()*alpha;
            if (alpha0 > alpha) {
              alpha0=PI+(alpha0-1.5*alpha);
            } else {
              alpha0=alpha0-0.5*alpha;
            }
            if(focus_flip){
                alpha0+=M_PI_2;
            }
          }
          else
            alpha0 = PI*randpm1();

          /* now find a nearly vertical rotation axis:
           * Either
           *  (v along Z) x (X axis) -> nearly Y axis
           * Or
           *  (v along X) x (Z axis) -> nearly Y axis
           */
          if (fabs(scalar_prod(1,0,0,vx/v,vy/v,vz/v)) < fabs(scalar_prod(0,0,1,vx/v,vy/v,vz/v))) {
            nx = 1; ny = 0; nz = 0;
          } else {
            nx = 0; ny = 0; nz = 1;
          }
          vec_prod(tmp_vx,tmp_vy,tmp_vz, vx,vy,vz, nx,ny,nz);

          /* v_out = rotate 'v' by 2*theta around tmp_v: Bragg angle */
          rotate(vout_x,vout_y,vout_z, vx,vy,vz, 2*theta, tmp_vx,tmp_vy,tmp_vz);

          /* tmp_v = rotate v_out by alpha0 around 'v' (Debye-Scherrer cone) */
          rotate(tmp_vx,tmp_vy,tmp_vz, vout_x,vout_y,vout_z, alpha0, vx, vy, vz);
          vx = tmp_vx;
          vy = tmp_vy;
          vz = tmp_vz;

          /* Since now scattered and new direction given, calculate path to exit */
          if (line_info.shape == 1) {
            intersect  = box_intersect(&t0, &t3, x, y, z, vx, vy, vz, xwidth, yheight, zdepth);
            intersecti = box_intersect(&t1, &t2, x, y, z, vx, vy, vz, line_info.xwidth_i, line_info.yheight_i, line_info.zdepth_i);
          } else if (line_info.shape == 0) {
            intersect  = cylinder_intersect(&t0, &t3, x, y, z, vx, vy, vz, radius, yheight);
            intersecti = cylinder_intersect(&t1, &t2, x, y, z, vx, vy, vz, line_info.radius_i, line_info.yheight_i);
          } else if (line_info.shape == 2) {
            intersect  = sphere_intersect  (&t0, &t3, x,y,z, vx,vy,vz, radius);
            intersecti = sphere_intersect  (&t1, &t2, x,y,z, vx,vy,vz, line_info.radius_i);
          } else if (line_info.shape == 3) {
#ifndef USE_PGI
            intersect  = off_intersect  (&t0, &t3, NULL, NULL, x,y,z, vx,vy,vz, offdata);
            intersecti = 0;
#endif
          }

          if (!intersect) {
            /* Strange error: did not hit cylinder */
            if (line_info.flag_warning < 100)
              printf("PowderN: %s: WARNING: Did not hit sample from inside (coh). ABSORB.\n", NAME_CURRENT_COMP);
            line_info.flag_warning++;
            ABSORB;
          }

          if (!intersecti) {
            t1 = (t3 + t0) /2;
            t2 = t1;
          }

          if (concentric && intersecti) {
            /* In case of concentricity, 'remove' backward wall of sample */
            t2 = t1;
            t3 = t1;
          }

          if(t0 < 0) t0=0; /* already in sample */
          if(t1 < 0) t1=0; /* already in inner hollow */
          if(t2 < 0) t2=0; /* already past inner hollow */


          l_1 = v*(t3 - t2 + t1 - t0); /* Length to exit */

          pmul  = line_info.Nq*l_full*my_s_n*exp(-(line_info.my_a_v/v+my_s)*(l+l_1))
                                  /(1-(p_inc+p_transmit));
          /* Correction in case of d_phi focusing - BUT only when d_phi != 0 */
          if (d_phi) pmul *= alpha/PI;

          line_info.type = 'c';
          line_info.dq = line_info.q_v[line]*V2K;
        } /* else transmit <-- No powder lines in file */
      }  /* Coherent scattering event */
      else if (neutrontype == 2) {  /* Make incoherent scattering event */
        if(d_phi) {
          randvec_target_rect_angular(&vx, &vy, &vz, &solid_angle,
                                      0, 0, 1,
                                      2*PI, d_phi*DEG2RAD, ROT_A_CURRENT_COMP);
        } else {
          randvec_target_circle(&vx, &vy, &vz,
                                &solid_angle, 0, 0, 1, 0);
        }
        v1 = sqrt(vx*vx+vy*vy+vz*vz);
        vx *= v/v1;
        vy *= v/v1;
        vz *= v/v1;

        /* Since now scattered and new direction given, calculate path to exit */
        if (line_info.shape == 1) {
          intersect  = box_intersect(&t0, &t3, x, y, z, vx, vy, vz, xwidth, yheight, zdepth);
          intersecti = box_intersect(&t1, &t2, x, y, z, vx, vy, vz, line_info.xwidth_i, line_info.yheight_i, line_info.zdepth_i);
        } else if (line_info.shape == 0) {
          intersect  = cylinder_intersect(&t0, &t3, x, y, z, vx, vy, vz, radius, yheight);
          intersecti = cylinder_intersect(&t1, &t2, x, y, z, vx, vy, vz, line_info.radius_i, line_info.yheight_i);
        } else if (line_info.shape == 2) {
          intersect  = sphere_intersect  (&t0, &t3, x,y,z, vx,vy,vz, radius);
          intersecti = sphere_intersect  (&t1, &t2, x,y,z, vx,vy,vz, line_info.radius_i);
        } else if (line_info.shape == 3) {
#ifndef USE_PGI
          intersect  = off_intersect  (&t0, &t3, NULL, NULL, x,y,z, vx,vy,vz, offdata);
          intersecti = 0;
#endif
        }

        if (!intersect) {
          /* Strange error: did not hit cylinder */
          if (line_info.flag_warning < 100)
            printf("PowderN: %s: WARNING: Did not hit sample from inside (inc). ABSORB.\n", NAME_CURRENT_COMP);
          line_info.flag_warning++;
          ABSORB;
        }

        if (!intersecti) {
          t1 = (t3 + t0) /2;
          t2 = t1;
        }

        if (concentric && intersecti) {
          /* In case of concentricity, 'remove' backward wall of sample */
          t2 = t1;
          t3 = t1;
        }

        if(t0 < 0) t0=0; /* already in sample */
        if(t1 < 0) t1=0; /* already in inner hollow */
        if(t2 < 0) t2=0; /* already past inner hollow */


        l_1 = v*(t3 - t2 + t1 - t0); /* Length to exit */

        pmul = l_full*line_info.my_inc*exp(-(line_info.my_a_v/v+my_s)*(l+l_1))/(p_inc);
        pmul *= solid_angle/(4*PI);

        line_info.type = 'i';

      }  /* Incoherent scattering event */
      else if (neutrontype == 1) {
        /* Make transmitted (absorption-corrected) event */
        /* No coordinate changes here, simply change neutron weight */
        pmul = exp(-(line_info.my_a_v/v+my_s)*(l))/(p_transmit);

        line_info.type = 't';
      }
      p *= pmul;
    } /* Neutron leaving since it has passed already */
  } /* else transmit non interacting neutrons */

}
  #undef reflections
  #undef geometry
  #undef format
  #undef radius
  #undef yheight
  #undef xwidth
  #undef zdepth
  #undef thickness
  #undef pack
  #undef Vc
  #undef sigma_abs
  #undef sigma_inc
  #undef delta_d_d
  #undef p_inc
  #undef p_transmit
  #undef DW
  #undef nb_atoms
  #undef d_phi
  #undef p_interact
  #undef concentric
  #undef density
  #undef weight
  #undef barns
  #undef Strain
  #undef focus_flip
  #undef line_info
  #undef columns
  #undef offdata
  return(_comp);
} /* class_PowderN_trace */

#pragma acc routine seq
_class_Beamstop *class_Beamstop_trace(_class_Beamstop *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define radius (_comp->_parameters.radius)
  SIG_MESSAGE("[_STOP_trace] component STOP=Beamstop() TRACE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Beamstop.comp:59]");

{
  /*    double Time = t;
    ALLOW_BACKPROP;
    PROP_Z0;
    Time = t - Time;
    if ((Time>=0) && ((radius!=0) && (x*x + y*y <= radius*radius))
    || ((Time>=0) && (radius==0) && (x>xmin && x<xmax && y>ymin && y<ymax)))
      ABSORB;
    else
      RESTORE_NEUTRON(INDEX_CURRENT_COMP, x, y, z, vx, vy, vz, t, sx, sy, sz, p);
  */
}
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef radius
  return(_comp);
} /* class_Beamstop_trace */

#pragma acc routine seq
_class_Monitor_nD *class_Monitor_nD_trace(_class_Monitor_nD *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;
  #define user1 (_comp->_parameters.user1)
  #define user2 (_comp->_parameters.user2)
  #define user3 (_comp->_parameters.user3)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define zdepth (_comp->_parameters.zdepth)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define zmin (_comp->_parameters.zmin)
  #define zmax (_comp->_parameters.zmax)
  #define bins (_comp->_parameters.bins)
  #define min (_comp->_parameters.min)
  #define max (_comp->_parameters.max)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define radius (_comp->_parameters.radius)
  #define options (_comp->_parameters.options)
  #define filename (_comp->_parameters.filename)
  #define geometry (_comp->_parameters.geometry)
  #define username1 (_comp->_parameters.username1)
  #define username2 (_comp->_parameters.username2)
  #define username3 (_comp->_parameters.username3)
  #define DEFS (_comp->_parameters.DEFS)
  #define Vars (_comp->_parameters.Vars)
  #define detector (_comp->_parameters.detector)
  #define offdata (_comp->_parameters.offdata)
  SIG_MESSAGE("[_Detector_trace] component Detector=Monitor_nD() TRACE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/Monitor_nD.comp:319]");

  /* Check if this is component 'Detector' */ 
  if(_comp->_index == 32) { 
    // HEST!
    user1 = FLT_MAX;
    // HEST!
    user2 = FLT_MAX;
    // HEST!
    user3 = FLT_MAX;
  }

{
  double  XY=0;
  double  t0 = 0;
  double  t1 = 0;
  double  pp;
  int     intersect   = 0;
  char    Flag_Restore = 0;

  Vars.UserVariable1 = user1;
  Vars.UserVariable2 = user2;
  Vars.UserVariable3 = user3;

  /* this is done automatically
    STORE_NEUTRON(INDEX_CURRENT_COMP, x, y, z, vx, vy, vz, t, sx, sy, sz, p);
  */

 #ifndef USE_PGI
  if (geometry && strlen(geometry) && strcmp(geometry,"0") && strcmp(geometry, "NULL"))
  {
    /* determine intersections with object */
    intersect = off_intersect_all(&t0, &t1, NULL, NULL,
       x,y,z, vx, vy, vz, &offdata );
    if (Vars.Flag_mantid) {
      if(intersect) {
        Vars.OFF_polyidx=(offdata.intersects[offdata.nextintersect]).index;
      } else {
        Vars.OFF_polyidx=-1;
      }
    }
  }
  else
#endif
    if ( (abs(Vars.Flag_Shape) == DEFS.SHAPE_SQUARE)
            || (abs(Vars.Flag_Shape) == DEFS.SHAPE_DISK) ) /* square xy or disk xy */
  {
    // propagate to xy plane and find intersection
    // make sure the event is recoverable afterwards
    t0 = t;
    ALLOW_BACKPROP;
    PROP_Z0;
    if ( (t>=t0) && (z==0.0) ) // forward propagation to xy plane was successful
    {
      if (abs(Vars.Flag_Shape) == DEFS.SHAPE_SQUARE)
      {
        // square xy
        intersect = (x>=Vars.mxmin && x<=Vars.mxmax && y>=Vars.mymin && y<=Vars.mymax);
      }
      else
      {
        // disk xy
        intersect = (SQR(x) + SQR(y)) <= SQR(Vars.Sphere_Radius);
      }
    }
    else
    {
      intersect=0;
    }
  }
  else if (abs(Vars.Flag_Shape) == DEFS.SHAPE_SPHERE) /* sphere */
  {
    intersect = sphere_intersect(&t0, &t1, x, y, z, vx, vy, vz, Vars.Sphere_Radius);
  /*      intersect = (intersect && t0 > 0); */
  }
  else if ((abs(Vars.Flag_Shape) == DEFS.SHAPE_CYLIND) || (abs(Vars.Flag_Shape) == DEFS.SHAPE_BANANA)) /* cylinder */
  {
    intersect = cylinder_intersect(&t0, &t1, x, y, z, vx, vy, vz, Vars.Sphere_Radius, Vars.Cylinder_Height);
  }
  else if (abs(Vars.Flag_Shape) == DEFS.SHAPE_BOX) /* box */
  {
    intersect = box_intersect(&t0, &t1, x, y, z, vx, vy, vz,
                              fabs(Vars.mxmax-Vars.mxmin), fabs(Vars.mymax-Vars.mymin), fabs(Vars.mzmax-Vars.mzmin));
  }
  else if (abs(Vars.Flag_Shape) == DEFS.SHAPE_PREVIOUS) /* previous comp */
  { intersect = 1; }

  if (intersect)
  {
    if ((abs(Vars.Flag_Shape) == DEFS.SHAPE_SPHERE) || (abs(Vars.Flag_Shape) == DEFS.SHAPE_CYLIND)
     || (abs(Vars.Flag_Shape) == DEFS.SHAPE_BOX) || (abs(Vars.Flag_Shape) == DEFS.SHAPE_BANANA)
#ifndef USE_PGI
	|| (geometry && strlen(geometry) && strcmp(geometry,"0") && strcmp(geometry, "NULL")) )
#else
      )
#endif
    {
      /* check if we have to remove the top/bottom with BANANA shape */
      if ((abs(Vars.Flag_Shape) == DEFS.SHAPE_BANANA) && (intersect != 1)) {
        double y0,y1;
        /* propagate to intersection point as temporary variable to check top/bottom */
        y0 = y+t0*vy;
        y1 = y+t1*vy;
        if (fabs(y0) >= Vars.Cylinder_Height/2*0.99) t0 = t1;
        if (fabs(y1) >= Vars.Cylinder_Height/2*0.99) t1 = t0;
      }
      if (t0 < 0 && t1 > 0)
        t0 = t;  /* neutron was already inside ! */
      if (t1 < 0 && t0 > 0) /* neutron exit before entering !! */
        t1 = t;
      /* t0 is now time of incoming intersection with the detection area */
      if ((Vars.Flag_Shape < 0) && (t1 > 0))
        PROP_DT(t1); /* t1 outgoing beam */
      else
        PROP_DT(t0); /* t0 incoming beam */
      /* Final test if we are on lid / bottom of banana/sphere */
      if (abs(Vars.Flag_Shape) == DEFS.SHAPE_BANANA || abs(Vars.Flag_Shape) == DEFS.SHAPE_SPHERE) {
        if (fabs(y) >= Vars.Cylinder_Height/2*0.99) {
          intersect=0;
          Flag_Restore=1;
        }
      }
    }
  }

  if (intersect)
  {
    /* Now get the data to monitor: current or keep from PreMonitor */
    if (Vars.Flag_UsePreMonitor != 1)
    {
      Vars.cp  = p;
      Vars.cx  = x;
      Vars.cvx = vx;
      Vars.csx = sx;
      Vars.cy  = y;
      Vars.cvy = vy;
      Vars.csy = sy;
      Vars.cz  = z;
      Vars.cvz = vz;
      Vars.csz = sz;
      Vars.ct  = t;
    }

    if ((Vars.He3_pressure > 0) && (t1 != t0) && ((abs(Vars.Flag_Shape) == DEFS.SHAPE_SPHERE) || (abs(Vars.Flag_Shape) == DEFS.SHAPE_CYLIND) || (abs(Vars.Flag_Shape) == DEFS.SHAPE_BOX)))
    {
      XY = exp(-7.417*Vars.He3_pressure*fabs(t1-t0)*2*PI*K2V);
      /* will monitor the absorbed part */
      Vars.cp *= 1-XY;
      /* and modify the neutron weight after monitor, only remains 1-p_detect */
      p *= XY;
    }

    if (Vars.Flag_capture)
    {
      XY = sqrt(Vars.cvx*Vars.cvx+Vars.cvy*Vars.cvy+Vars.cvz*Vars.cvz);
      XY *= V2K;
      if (XY != 0) XY = 2*PI/XY; /* lambda. lambda(2200 m/2) = 1.7985 Angs  */
      Vars.cp *= XY/1.7985;
    }

    pp = Monitor_nD_Trace(&DEFS, &Vars, _particle);
    if (pp==0.0)
    { ABSORB;
    }
    else if(pp==1)
    {
      SCATTER;
    }

    if (Vars.Flag_parallel) /* back to neutron state before detection */
      Flag_Restore = 1;
  } /* end if intersection */
  else {
    if (Vars.Flag_Absorb && !Vars.Flag_parallel)
    {
      // restore neutron ray before absorbing for correct mcdisplay
      RESTORE_NEUTRON(INDEX_CURRENT_COMP, x, y, z, vx, vy, vz, t, sx, sy, sz, p);
      ABSORB;
    }
    else Flag_Restore = 1;  /* no intersection, back to previous state */
  }

  if (Flag_Restore)
  {
    RESTORE_NEUTRON(INDEX_CURRENT_COMP, x, y, z, vx, vy, vz, t, sx, sy, sz, p);
  }
}
  #undef user1
  #undef user2
  #undef user3
  #undef xwidth
  #undef yheight
  #undef zdepth
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef zmin
  #undef zmax
  #undef bins
  #undef min
  #undef max
  #undef restore_neutron
  #undef radius
  #undef options
  #undef filename
  #undef geometry
  #undef username1
  #undef username2
  #undef username3
  #undef DEFS
  #undef Vars
  #undef detector
  #undef offdata
  return(_comp);
} /* class_Monitor_nD_trace */

/* *****************************************************************************
* instrument 'PSI_DMC' TRACE
***************************************************************************** */

#pragma acc routine seq
int raytrace(_class_particle* _particle) { /* single event propagation, called by mccode_main for PSI_DMC:TRACE */

  /* init variables and counters for TRACE */
  #undef ABSORB0
  #undef ABSORB
  #define ABSORB0 do { DEBUG_ABSORB(); MAGNET_OFF; ABSORBED++; return(ABSORBED);} while(0)
  #define ABSORB ABSORB0
  DEBUG_ENTER();
  DEBUG_STATE();
  /* the main iteration loop for one incoming event */
  while (!ABSORBED) { /* iterate event until absorbed */
    _class_particle _particle_save;
    /* send particle event to component instance, one after the other */
    char flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 1) {
      /* begin component source_arm=Progress_bar() [1] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_source_arm_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _source_arm_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_source_arm_var._position_relative, _source_arm_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_source_arm_var._name);
      DEBUG_STATE();
      class_Progress_bar_trace(&_source_arm_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component source_arm [1] */
    if (!ABSORBED && _particle->_index == 2) {
      /* begin component source=Source_Maxwell_3() [2] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_source_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _source_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_source_var._position_relative, _source_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_source_var._name);
      DEBUG_STATE();
      class_Source_Maxwell_3_trace(&_source_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component source [2] */
    if (!ABSORBED && _particle->_index == 3) {
      /* begin component PSDbefore_guides=PSD_monitor() [3] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_PSDbefore_guides_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _PSDbefore_guides_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_PSDbefore_guides_var._position_relative, _PSDbefore_guides_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_PSDbefore_guides_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_PSDbefore_guides_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component PSDbefore_guides [3] */
    if (!ABSORBED && _particle->_index == 4) {
      /* begin component l_mon_source=L_monitor() [4] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_l_mon_source_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _l_mon_source_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_l_mon_source_var._position_relative, _l_mon_source_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_l_mon_source_var._name);
      DEBUG_STATE();
      class_L_monitor_trace(&_l_mon_source_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component l_mon_source [4] */
    if (!ABSORBED && _particle->_index == 5) {
      /* begin component guide1=Guide() [5] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_guide1_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _guide1_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_guide1_var._position_relative, _guide1_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_guide1_var._name);
      DEBUG_STATE();
      class_Guide_trace(&_guide1_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component guide1 [5] */
    if (!ABSORBED && _particle->_index == 6) {
      /* begin component PSDbefore_curve=PSD_monitor() [6] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_PSDbefore_curve_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _PSDbefore_curve_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_PSDbefore_curve_var._position_relative, _PSDbefore_curve_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_PSDbefore_curve_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_PSDbefore_curve_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component PSDbefore_curve [6] */
    if (!ABSORBED && _particle->_index == 7) {
      /* begin component guide2=Bender() [7] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_guide2_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _guide2_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_guide2_var._position_relative, _guide2_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_guide2_var._name);
      DEBUG_STATE();
      class_Bender_trace(&_guide2_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component guide2 [7] */
    if (!ABSORBED && _particle->_index == 8) {
      /* begin component PSDafter_curve=PSD_monitor() [8] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_PSDafter_curve_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _PSDafter_curve_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_PSDafter_curve_var._position_relative, _PSDafter_curve_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_PSDafter_curve_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_PSDafter_curve_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component PSDafter_curve [8] */
    if (!ABSORBED && _particle->_index == 9) {
      /* begin component bunker=Guide() [9] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_bunker_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _bunker_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_bunker_var._position_relative, _bunker_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_bunker_var._name);
      DEBUG_STATE();
      class_Guide_trace(&_bunker_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component bunker [9] */
    if (!ABSORBED && _particle->_index == 10) {
      /* begin component guide3=Guide() [10] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_guide3_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _guide3_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_guide3_var._position_relative, _guide3_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_guide3_var._name);
      DEBUG_STATE();
      class_Guide_trace(&_guide3_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component guide3 [10] */
    if (!ABSORBED && _particle->_index == 11) {
      /* begin component guide4=Guide() [11] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_guide4_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _guide4_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_guide4_var._position_relative, _guide4_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_guide4_var._name);
      DEBUG_STATE();
      class_Guide_trace(&_guide4_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component guide4 [11] */
    if (!ABSORBED && _particle->_index == 12) {
      /* begin component window1=Al_window() [12] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_window1_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _window1_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_window1_var._position_relative, _window1_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_window1_var._name);
      DEBUG_STATE();
      class_Al_window_trace(&_window1_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component window1 [12] */
    if (!ABSORBED && _particle->_index == 13) {
      /* begin component ydist_fluxpos=PSDlin_monitor() [13] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_ydist_fluxpos_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _ydist_fluxpos_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_ydist_fluxpos_var._position_relative, _ydist_fluxpos_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_ydist_fluxpos_var._name);
      DEBUG_STATE();
      class_PSDlin_monitor_trace(&_ydist_fluxpos_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component ydist_fluxpos [13] */
    if (!ABSORBED && _particle->_index == 14) {
      /* begin component PSD_fluxpos=PSD_monitor() [14] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_PSD_fluxpos_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _PSD_fluxpos_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_PSD_fluxpos_var._position_relative, _PSD_fluxpos_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_PSD_fluxpos_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_PSD_fluxpos_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component PSD_fluxpos [14] */
    if (!ABSORBED && _particle->_index == 15) {
      /* begin component xdist_flux_pos=PSDlin_monitor() [15] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_xdist_flux_pos_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _xdist_flux_pos_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_xdist_flux_pos_var._position_relative, _xdist_flux_pos_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_xdist_flux_pos_var._name);
      DEBUG_STATE();
      class_PSDlin_monitor_trace(&_xdist_flux_pos_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component xdist_flux_pos [15] */
    if (!ABSORBED && _particle->_index == 16) {
      /* begin component PSD_fluxposB=PSD_monitor() [16] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_PSD_fluxposB_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _PSD_fluxposB_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_PSD_fluxposB_var._position_relative, _PSD_fluxposB_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_PSD_fluxposB_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_PSD_fluxposB_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component PSD_fluxposB [16] */
    if (!ABSORBED && _particle->_index == 17) {
      /* begin component window2=Al_window() [17] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_window2_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _window2_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_window2_var._position_relative, _window2_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_window2_var._name);
      DEBUG_STATE();
      class_Al_window_trace(&_window2_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component window2 [17] */
    if (!ABSORBED && _particle->_index == 18) {
      /* begin component in_slit=Slit() [18] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_in_slit_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _in_slit_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_in_slit_var._position_relative, _in_slit_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_in_slit_var._name);
      DEBUG_STATE();
      class_Slit_trace(&_in_slit_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component in_slit [18] */
    if (!ABSORBED && _particle->_index == 19) {
      /* begin component lambda_in=L_monitor() [19] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_lambda_in_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _lambda_in_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_lambda_in_var._position_relative, _lambda_in_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_lambda_in_var._name);
      DEBUG_STATE();
      class_L_monitor_trace(&_lambda_in_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component lambda_in [19] */
    if (!ABSORBED && _particle->_index == 20) {
      /* begin component sma=Arm() [20] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_sma_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _sma_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_sma_var._position_relative, _sma_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_sma_var._name);
      DEBUG_STATE();
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component sma [20] */
#ifndef NOSPLIT
    /* start SPLIT at foc_mono */
    _class_particle Split_foc_mono_party[10];
    int  Split_foc_mono_counter;
    for (Split_foc_mono_counter = 0; Split_foc_mono_counter < 10; Split_foc_mono_counter++) {
      Split_foc_mono_party[Split_foc_mono_counter]=*_particle;
    }
    for (Split_foc_mono_counter = 0; Split_foc_mono_counter < 10; Split_foc_mono_counter++) {
      *_particle=Split_foc_mono_party[Split_foc_mono_counter];
      p /= 10 > 0 ? 10 : 1;
#endif
      if (!ABSORBED && _particle->_index == 21) {
	/* begin component foc_mono=Monochromator_2foc() [21] */
	if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
	  if (_foc_mono_var._rotation_is_identity) {
	    coords_get(coords_add(coords_set(x,y,z), _foc_mono_var._position_relative),&x, &y, &z);
	  } else
	    mccoordschange(_foc_mono_var._position_relative, _foc_mono_var._rotation_relative, _particle);
	} else flag_nocoordschange=0;
	_particle_save = *_particle;
	DEBUG_COMP(_foc_mono_var._name);
	DEBUG_STATE();
	class_Monochromator_2foc_trace(&_foc_mono_var, _particle);
	if (_particle->_restore)
	  *_particle = _particle_save;
	_particle->_index++;
	if (!ABSORBED) DEBUG_STATE();
      } /* end component foc_mono [21] */
      if (!ABSORBED && _particle->_index == 22) {
	/* begin component msa=Arm() [22] */
	if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
	  if (_msa_var._rotation_is_identity) {
	    coords_get(coords_add(coords_set(x,y,z), _msa_var._position_relative),&x, &y, &z);
	  } else
	    mccoordschange(_msa_var._position_relative, _msa_var._rotation_relative, _particle);
	} else flag_nocoordschange=0;
	_particle_save = *_particle;
	DEBUG_COMP(_msa_var._name);
	DEBUG_STATE();
	_particle->_index++;
	if (!ABSORBED) DEBUG_STATE();
      } /* end component msa [22] */
      if (!ABSORBED && _particle->_index == 23) {
	/* begin component out1_slit=Slit() [23] */
	if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
	  if (_out1_slit_var._rotation_is_identity) {
	    coords_get(coords_add(coords_set(x,y,z), _out1_slit_var._position_relative),&x, &y, &z);
	  } else
	    mccoordschange(_out1_slit_var._position_relative, _out1_slit_var._rotation_relative, _particle);
	} else flag_nocoordschange=0;
	_particle_save = *_particle;
	DEBUG_COMP(_out1_slit_var._name);
	DEBUG_STATE();
	class_Slit_trace(&_out1_slit_var, _particle);
	if (_particle->_restore)
	  *_particle = _particle_save;
	_particle->_index++;
	if (!ABSORBED) DEBUG_STATE();
      } /* end component out1_slit [23] */
      if (!ABSORBED && _particle->_index == 24) {
	/* begin component Amoin_slit=Slit() [24] */
	if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
	  if (_Amoin_slit_var._rotation_is_identity) {
	    coords_get(coords_add(coords_set(x,y,z), _Amoin_slit_var._position_relative),&x, &y, &z);
	  } else
	    mccoordschange(_Amoin_slit_var._position_relative, _Amoin_slit_var._rotation_relative, _particle);
	} else flag_nocoordschange=0;
	_particle_save = *_particle;
	DEBUG_COMP(_Amoin_slit_var._name);
	DEBUG_STATE();
	class_Slit_trace(&_Amoin_slit_var, _particle);
	if (_particle->_restore)
	  *_particle = _particle_save;
	_particle->_index++;
	if (!ABSORBED) DEBUG_STATE();
      } /* end component Amoin_slit [24] */
      if (!ABSORBED && _particle->_index == 25) {
	/* begin component Bmoin_slit=Slit() [25] */
	if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
	  if (_Bmoin_slit_var._rotation_is_identity) {
	    coords_get(coords_add(coords_set(x,y,z), _Bmoin_slit_var._position_relative),&x, &y, &z);
	  } else
	    mccoordschange(_Bmoin_slit_var._position_relative, _Bmoin_slit_var._rotation_relative, _particle);
	} else flag_nocoordschange=0;
	_particle_save = *_particle;
	DEBUG_COMP(_Bmoin_slit_var._name);
	DEBUG_STATE();
	class_Slit_trace(&_Bmoin_slit_var, _particle);
	if (_particle->_restore)
	  *_particle = _particle_save;
	_particle->_index++;
	if (!ABSORBED) DEBUG_STATE();
      } /* end component Bmoin_slit [25] */
      if (!ABSORBED && _particle->_index == 26) {
	/* begin component out2_slit=Slit() [26] */
	if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
	  if (_out2_slit_var._rotation_is_identity) {
	    coords_get(coords_add(coords_set(x,y,z), _out2_slit_var._position_relative),&x, &y, &z);
	  } else
	    mccoordschange(_out2_slit_var._position_relative, _out2_slit_var._rotation_relative, _particle);
	} else flag_nocoordschange=0;
	_particle_save = *_particle;
	DEBUG_COMP(_out2_slit_var._name);
	DEBUG_STATE();
	class_Slit_trace(&_out2_slit_var, _particle);
	if (_particle->_restore)
	  *_particle = _particle_save;
	_particle->_index++;
	if (!ABSORBED) DEBUG_STATE();
      } /* end component out2_slit [26] */
      if (!ABSORBED && _particle->_index == 27) {
	/* begin component PSD_sample=PSD_monitor() [27] */
	if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
	  if (_PSD_sample_var._rotation_is_identity) {
	    coords_get(coords_add(coords_set(x,y,z), _PSD_sample_var._position_relative),&x, &y, &z);
	  } else
	    mccoordschange(_PSD_sample_var._position_relative, _PSD_sample_var._rotation_relative, _particle);
	} else flag_nocoordschange=0;
	_particle_save = *_particle;
	DEBUG_COMP(_PSD_sample_var._name);
	DEBUG_STATE();
	class_PSD_monitor_trace(&_PSD_sample_var, _particle);
	if (_particle->_restore)
	  *_particle = _particle_save;
	_particle->_index++;
	if (!ABSORBED) DEBUG_STATE();
      } /* end component PSD_sample [27] */
      if (!ABSORBED && _particle->_index == 28) {
	/* begin component lambda_sample=L_monitor() [28] */
	if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
	  if (_lambda_sample_var._rotation_is_identity) {
	    coords_get(coords_add(coords_set(x,y,z), _lambda_sample_var._position_relative),&x, &y, &z);
	  } else
	    mccoordschange(_lambda_sample_var._position_relative, _lambda_sample_var._rotation_relative, _particle);
	} else flag_nocoordschange=0;
	_particle_save = *_particle;
	DEBUG_COMP(_lambda_sample_var._name);
	DEBUG_STATE();
	class_L_monitor_trace(&_lambda_sample_var, _particle);
	if (_particle->_restore)
	  *_particle = _particle_save;
	_particle->_index++;
	if (!ABSORBED) DEBUG_STATE();
      } /* end component lambda_sample [28] */
      if (!ABSORBED && _particle->_index == 29) {
	/* begin component sa_arm=Arm() [29] */
	if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
	  if (_sa_arm_var._rotation_is_identity) {
	    coords_get(coords_add(coords_set(x,y,z), _sa_arm_var._position_relative),&x, &y, &z);
	  } else
	    mccoordschange(_sa_arm_var._position_relative, _sa_arm_var._rotation_relative, _particle);
	} else flag_nocoordschange=0;
	_particle_save = *_particle;
	DEBUG_COMP(_sa_arm_var._name);
	DEBUG_STATE();
	_particle->_index++;
	if (!ABSORBED) DEBUG_STATE();
      } /* end component sa_arm [29] */
#ifndef NOSPLIT
      /* start SPLIT at sample */
      _class_particle Split_sample_party[10];
      int Split_sample_counter;
      for (Split_sample_counter = 0; Split_sample_counter < 10; Split_sample_counter++) {
	Split_sample_party[Split_sample_counter]=*_particle;
      }
      for (Split_sample_counter = 0; Split_sample_counter < 10; Split_sample_counter++) {
	*_particle=Split_sample_party[Split_sample_counter];
	p /= 10 > 0 ? 10 : 1;
#endif
	if (!ABSORBED && _particle->_index == 30) {
	  /* begin component sample=PowderN() [30] */
	  if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
	    if (_sample_var._rotation_is_identity) {
	      coords_get(coords_add(coords_set(x,y,z), _sample_var._position_relative),&x, &y, &z);
	    } else
	      mccoordschange(_sample_var._position_relative, _sample_var._rotation_relative, _particle);
	  } else flag_nocoordschange=0;
	  _particle_save = *_particle;
	  DEBUG_COMP(_sample_var._name);
	  DEBUG_STATE();
	  class_PowderN_trace(&_sample_var, _particle);
	  if (_particle->_restore)
	    *_particle = _particle_save;
	  _particle->_index++;
	  if (!ABSORBED) DEBUG_STATE();
	} /* end component sample [30] */
	if (!ABSORBED && _particle->_index == 31) {
	  /* begin component STOP=Beamstop() [31] */
	  if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
	    if (_STOP_var._rotation_is_identity) {
	      coords_get(coords_add(coords_set(x,y,z), _STOP_var._position_relative),&x, &y, &z);
	    } else
	      mccoordschange(_STOP_var._position_relative, _STOP_var._rotation_relative, _particle);
	  } else flag_nocoordschange=0;
	  _particle_save = *_particle;
	  DEBUG_COMP(_STOP_var._name);
	  DEBUG_STATE();
	  class_Beamstop_trace(&_STOP_var, _particle);
	  if (_particle->_restore)
	    *_particle = _particle_save;
	  _particle->_index++;
	  if (!ABSORBED) DEBUG_STATE();
	} /* end component STOP [31] */
	if (!ABSORBED && _particle->_index == 32) {
	  /* begin component Detector=Monitor_nD() [32] */
	  if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
	    if (_Detector_var._rotation_is_identity) {
	      coords_get(coords_add(coords_set(x,y,z), _Detector_var._position_relative),&x, &y, &z);
	    } else
	      mccoordschange(_Detector_var._position_relative, _Detector_var._rotation_relative, _particle);
	  } else flag_nocoordschange=0;
	  _particle_save = *_particle;
	  DEBUG_COMP(_Detector_var._name);
	  DEBUG_STATE();
	  class_Monitor_nD_trace(&_Detector_var, _particle);
	  if (_particle->_restore)
	    *_particle = _particle_save;
	  _particle->_index++;
	  if (!ABSORBED) DEBUG_STATE();
	} /* end component Detector [32] */
#ifndef NOSPLIT
      } /* end SPLIT at sample */
#endif
#ifndef NOSPLIT
    } /* end SPLIT at foc_mono */
#endif
    if (_particle->_index > 32)
      ABSORBED++; /* absorbed when passed all components */
  } /* while !ABSORBED */
  
  DEBUG_LEAVE()
  DEBUG_STATE()
    
    return(_particle->_index);
} /* raytrace */

/* loop to generate events and call raytrace() propagate them */
void raytrace_all(unsigned long long ncount, unsigned long seed) {

  /* CPU-loop */
  unsigned long long loops;
  long innerloop=2147483647;
  loops = ceil((double)ncount/innerloop);
  if (ncount>innerloop) {
    printf("Defining %llu CPU loops around kernel and adjusting ncount\n",loops);
    mcset_ncount(loops*2147483647);
  } else {
    loops=1;
    innerloop = ncount;
  }

  for (unsigned long long cloop=0; cloop<loops; cloop++) {
    if (loops>1) fprintf(stdout, "%d..", (int)cloop); fflush(stdout);

    #pragma acc parallel loop
    for (unsigned long pidx=0 ; pidx < innerloop ; pidx++) {
      _class_particle particleN = mcgenstate(); // initial particle
      _class_particle* _particle = &particleN;
      particleN._uid = pidx;

      long seq = pidx + seed;
      srandom(_hash(pidx + seed));

      raytrace(_particle);
    } /* inner for */
    seed = seed+innerloop;
  } /* CPU for */
  printf("\n");
} /* raytrace_all */


// global variable and declare create required for acc
_class_particle* particles;
#pragma acc declare device_resident(particles)

// Alternative raytrace algorithm which "funnels" particles through
// the instrument, enabling a higher degree of parallelization.
void raytrace_all_funnel(unsigned long long ncount, unsigned long seed) {

  // set up outer (CPU) loop / particle batches
  unsigned long long loops;
  long innerloop=1024*1024; // <-- tune by memory capacity here (max threads: 2147483647);
  loops = ceil((double)ncount/innerloop);
  if (ncount>innerloop) {
    printf("Defining %llu CPU loops around kernel and adjusting ncount\n",loops);
    mcset_ncount(loops*2147483647);
  } else {
    loops=1;
    innerloop = ncount;
  }

  // outer loop / particle batches
  for (unsigned long long cloop=0; cloop<loops; cloop++) {
    if (loops>1) fprintf(stdout, "%d..", (int)cloop); fflush(stdout);

    long livebatchsize = innerloop;

    // create particles memory block and pointer array (buffer and sorted)
    #ifdef USE_PGI
    _class_particle* particles = acc_malloc(livebatchsize*sizeof(_class_particle));
    #pragma acc enter data create(particles[0:livebatchsize])
    #else
    _class_particle* particles = malloc(livebatchsize*sizeof(_class_particle));
    #endif
    _class_particle** psorted = malloc(livebatchsize*sizeof(_class_particle*));
    _class_particle** pbuffer = malloc(livebatchsize*sizeof(_class_particle*));

    // TODO: do we need a GPU data section for the above buffers here?

    // set up, generate particles
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      // generate particle state, set loop index and seed
      particles[pidx] = mcgenstate();
      psorted[pidx] = &(particles[pidx]);
      _class_particle* _particle = particles + pidx;
      _particle->_uid = pidx;
      srandom(_hash(pidx + seed)); // _particle->state usage built into srandom macro
    }

    // iterate components

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // source_arm
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_source_arm_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _source_arm_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_source_arm_var._position_relative, _source_arm_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Progress_bar_trace(&_source_arm_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // source
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_source_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _source_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_source_var._position_relative, _source_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Source_Maxwell_3_trace(&_source_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // PSDbefore_guides
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_PSDbefore_guides_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _PSDbefore_guides_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_PSDbefore_guides_var._position_relative, _PSDbefore_guides_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_PSD_monitor_trace(&_PSDbefore_guides_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // l_mon_source
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_l_mon_source_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _l_mon_source_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_l_mon_source_var._position_relative, _l_mon_source_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_L_monitor_trace(&_l_mon_source_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // guide1
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_guide1_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _guide1_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_guide1_var._position_relative, _guide1_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Guide_trace(&_guide1_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // PSDbefore_curve
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_PSDbefore_curve_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _PSDbefore_curve_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_PSDbefore_curve_var._position_relative, _PSDbefore_curve_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_PSD_monitor_trace(&_PSDbefore_curve_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // guide2
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_guide2_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _guide2_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_guide2_var._position_relative, _guide2_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Bender_trace(&_guide2_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // PSDafter_curve
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_PSDafter_curve_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _PSDafter_curve_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_PSDafter_curve_var._position_relative, _PSDafter_curve_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_PSD_monitor_trace(&_PSDafter_curve_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // bunker
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_bunker_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _bunker_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_bunker_var._position_relative, _bunker_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Guide_trace(&_bunker_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // guide3
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_guide3_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _guide3_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_guide3_var._position_relative, _guide3_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Guide_trace(&_guide3_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // guide4
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_guide4_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _guide4_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_guide4_var._position_relative, _guide4_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Guide_trace(&_guide4_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // window1
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_window1_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _window1_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_window1_var._position_relative, _window1_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Al_window_trace(&_window1_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // ydist_fluxpos
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_ydist_fluxpos_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _ydist_fluxpos_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_ydist_fluxpos_var._position_relative, _ydist_fluxpos_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_PSDlin_monitor_trace(&_ydist_fluxpos_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // PSD_fluxpos
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_PSD_fluxpos_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _PSD_fluxpos_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_PSD_fluxpos_var._position_relative, _PSD_fluxpos_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_PSD_monitor_trace(&_PSD_fluxpos_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // xdist_flux_pos
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_xdist_flux_pos_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _xdist_flux_pos_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_xdist_flux_pos_var._position_relative, _xdist_flux_pos_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_PSDlin_monitor_trace(&_xdist_flux_pos_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // PSD_fluxposB
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_PSD_fluxposB_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _PSD_fluxposB_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_PSD_fluxposB_var._position_relative, _PSD_fluxposB_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_PSD_monitor_trace(&_PSD_fluxposB_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // window2
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_window2_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _window2_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_window2_var._position_relative, _window2_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Al_window_trace(&_window2_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // in_slit
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_in_slit_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _in_slit_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_in_slit_var._position_relative, _in_slit_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Slit_trace(&_in_slit_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // lambda_in
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_lambda_in_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _lambda_in_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_lambda_in_var._position_relative, _lambda_in_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_L_monitor_trace(&_lambda_in_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // sma
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_sma_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _sma_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_sma_var._position_relative, _sma_var._rotation_relative, _particle);
        _particle_save = *_particle;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // foc_mono
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_foc_mono_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _foc_mono_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_foc_mono_var._position_relative, _foc_mono_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Monochromator_2foc_trace(&_foc_mono_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // msa
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_msa_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _msa_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_msa_var._position_relative, _msa_var._rotation_relative, _particle);
        _particle_save = *_particle;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // out1_slit
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_out1_slit_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _out1_slit_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_out1_slit_var._position_relative, _out1_slit_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Slit_trace(&_out1_slit_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // Amoin_slit
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_Amoin_slit_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _Amoin_slit_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_Amoin_slit_var._position_relative, _Amoin_slit_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Slit_trace(&_Amoin_slit_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // Bmoin_slit
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_Bmoin_slit_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _Bmoin_slit_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_Bmoin_slit_var._position_relative, _Bmoin_slit_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Slit_trace(&_Bmoin_slit_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // out2_slit
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_out2_slit_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _out2_slit_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_out2_slit_var._position_relative, _out2_slit_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Slit_trace(&_out2_slit_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // PSD_sample
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_PSD_sample_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _PSD_sample_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_PSD_sample_var._position_relative, _PSD_sample_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_PSD_monitor_trace(&_PSD_sample_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // lambda_sample
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_lambda_sample_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _lambda_sample_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_lambda_sample_var._position_relative, _lambda_sample_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_L_monitor_trace(&_lambda_sample_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // sa_arm
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_sa_arm_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _sa_arm_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_sa_arm_var._position_relative, _sa_arm_var._rotation_relative, _particle);
        _particle_save = *_particle;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // sample
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_sample_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _sample_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_sample_var._position_relative, _sample_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_PowderN_trace(&_sample_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // STOP
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_STOP_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _STOP_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_STOP_var._position_relative, _STOP_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Beamstop_trace(&_STOP_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //printf("livebatchsize: percent ld\n", livebatchsize);
    livebatchsize = sort_absorb_last(psorted, pbuffer, innerloop);

    // Detector
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_Detector_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _Detector_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_Detector_var._position_relative, _Detector_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Monitor_nD_trace(&_Detector_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    // finalize particle batch
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = *(psorted + pidx);
      if (_particle->_index > 4)
        ABSORBED++; /* absorbed when passed all components */
    }
    // jump to next viable seed
    seed = seed + innerloop;
    #ifdef USE_PGI
    acc_free(particles);
    #else
    free(particles);
    #endif
  } // outer loop / particle batches

  printf("\n");
} /* raytrace_all_funnel */

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
#ifdef USE_PGI
#undef strlen
#undef strcmp
#undef exit
#undef printf
#undef sprintf
#undef fprintf
#endif
#undef SCATTERED
#undef RESTORE
#undef RESTORE_NEUTRON
#undef STORE_NEUTRON
#undef ABSORBED
#undef ABSORB
#undef ABSORB0
/* *****************************************************************************
* instrument 'PSI_DMC' and components SAVE
***************************************************************************** */

_class_Progress_bar *class_Progress_bar_save(_class_Progress_bar *_comp
) {
  #define profile (_comp->_parameters.profile)
  #define percent (_comp->_parameters.percent)
  #define flag_save (_comp->_parameters.flag_save)
  #define minutes (_comp->_parameters.minutes)
  #define IntermediateCnts (_comp->_parameters.IntermediateCnts)
  #define StartTime (_comp->_parameters.StartTime)
  #define EndTime (_comp->_parameters.EndTime)
  #define CurrentTime (_comp->_parameters.CurrentTime)
  SIG_MESSAGE("[_source_arm_save] component source_arm=Progress_bar() SAVE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/misc/Progress_bar.comp:117]");

  MPI_MASTER(fprintf(stdout, "\nSave [%s]\n", instrument_name););
  if (profile && strlen(profile) && strcmp(profile,"NULL") && strcmp(profile,"0")) {
    char filename[256];
    if (!strlen(profile) || !strcmp(profile,"NULL") || !strcmp(profile,"0")) strcpy(filename, instrument_name);
    else strcpy(filename, profile);
    DETECTOR_OUT_1D(
        "Intensity profiler",
        "Component index [1]",
        "Intensity",
        "prof", 1, mcNUMCOMP, mcNUMCOMP-1,
        &(instrument->counter_N[1]),&(instrument->counter_P[1]),&(instrument->counter_P2[1]),
        filename);

  }
  #undef profile
  #undef percent
  #undef flag_save
  #undef minutes
  #undef IntermediateCnts
  #undef StartTime
  #undef EndTime
  #undef CurrentTime
  return(_comp);
} /* class_Progress_bar_save */

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
  #define nowritefile (_comp->_parameters.nowritefile)
  #define PSD_N (_comp->_parameters.PSD_N)
  #define PSD_p (_comp->_parameters.PSD_p)
  #define PSD_p2 (_comp->_parameters.PSD_p2)
  SIG_MESSAGE("[_PSDbefore_guides_save] component PSDbefore_guides=PSD_monitor() SAVE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/PSD_monitor.comp:112]");

    if (!nowritefile) {
      DETECTOR_OUT_2D(
          "PSD monitor",
          "X position [cm]",
          "Y position [cm]",
          xmin*100.0, xmax*100.0, ymin*100.0, ymax*100.0,
          nx, ny,
          &PSD_N[0][0],&PSD_p[0][0],&PSD_p2[0][0],
          filename);
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
  #undef nowritefile
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  return(_comp);
} /* class_PSD_monitor_save */

_class_L_monitor *class_L_monitor_save(_class_L_monitor *_comp
) {
  #define nL (_comp->_parameters.nL)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define Lmin (_comp->_parameters.Lmin)
  #define Lmax (_comp->_parameters.Lmax)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define L_N (_comp->_parameters.L_N)
  #define L_p (_comp->_parameters.L_p)
  #define L_p2 (_comp->_parameters.L_p2)
  SIG_MESSAGE("[_l_mon_source_save] component l_mon_source=L_monitor() SAVE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/L_monitor.comp:114]");

  DETECTOR_OUT_1D(
    "Wavelength monitor",
    "Wavelength [AA]",
    "Intensity",
    "L", Lmin, Lmax, nL,
    &L_N[0],&L_p[0],&L_p2[0],
    filename);
  #undef nL
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef Lmin
  #undef Lmax
  #undef restore_neutron
  #undef L_N
  #undef L_p
  #undef L_p2
  return(_comp);
} /* class_L_monitor_save */

_class_PSDlin_monitor *class_PSDlin_monitor_save(_class_PSDlin_monitor *_comp
) {
  #define nx (_comp->_parameters.nx)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define PSDlin_N (_comp->_parameters.PSDlin_N)
  #define PSDlin_p (_comp->_parameters.PSDlin_p)
  #define PSDlin_p2 (_comp->_parameters.PSDlin_p2)
  SIG_MESSAGE("[_ydist_fluxpos_save] component ydist_fluxpos=PSDlin_monitor() SAVE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/PSDlin_monitor.comp:111]");

  DETECTOR_OUT_1D(
      "Linear PSD monitor",
      "x-Position [m]",
      "Intensity",
      "x", xmin, xmax, nx,
      &PSDlin_N[0],&PSDlin_p[0],&PSDlin_p2[0],
      filename);
  #undef nx
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef PSDlin_N
  #undef PSDlin_p
  #undef PSDlin_p2
  return(_comp);
} /* class_PSDlin_monitor_save */

_class_Monitor_nD *class_Monitor_nD_save(_class_Monitor_nD *_comp
) {
  #define user1 (_comp->_parameters.user1)
  #define user2 (_comp->_parameters.user2)
  #define user3 (_comp->_parameters.user3)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define zdepth (_comp->_parameters.zdepth)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define zmin (_comp->_parameters.zmin)
  #define zmax (_comp->_parameters.zmax)
  #define bins (_comp->_parameters.bins)
  #define min (_comp->_parameters.min)
  #define max (_comp->_parameters.max)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define radius (_comp->_parameters.radius)
  #define options (_comp->_parameters.options)
  #define filename (_comp->_parameters.filename)
  #define geometry (_comp->_parameters.geometry)
  #define username1 (_comp->_parameters.username1)
  #define username2 (_comp->_parameters.username2)
  #define username3 (_comp->_parameters.username3)
  #define DEFS (_comp->_parameters.DEFS)
  #define Vars (_comp->_parameters.Vars)
  #define detector (_comp->_parameters.detector)
  #define offdata (_comp->_parameters.offdata)
  SIG_MESSAGE("[_Detector_save] component Detector=Monitor_nD() SAVE [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/Monitor_nD.comp:496]");

  /* save results, but do not free pointers */
  detector = Monitor_nD_Save(&DEFS, &Vars);
  #undef user1
  #undef user2
  #undef user3
  #undef xwidth
  #undef yheight
  #undef zdepth
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef zmin
  #undef zmax
  #undef bins
  #undef min
  #undef max
  #undef restore_neutron
  #undef radius
  #undef options
  #undef filename
  #undef geometry
  #undef username1
  #undef username2
  #undef username3
  #undef DEFS
  #undef Vars
  #undef detector
  #undef offdata
  return(_comp);
} /* class_Monitor_nD_save */



int save(FILE *handle) { /* called by mccode_main for PSI_DMC:SAVE */
  if (!handle) siminfo_init(NULL);

  /* call iteratively all components SAVE */
  class_Progress_bar_save(&_source_arm_var);


  class_PSD_monitor_save(&_PSDbefore_guides_var);

  class_L_monitor_save(&_l_mon_source_var);


  class_PSD_monitor_save(&_PSDbefore_curve_var);


  class_PSD_monitor_save(&_PSDafter_curve_var);





  class_PSDlin_monitor_save(&_ydist_fluxpos_var);

  class_PSD_monitor_save(&_PSD_fluxpos_var);

  class_PSDlin_monitor_save(&_xdist_flux_pos_var);

  class_PSD_monitor_save(&_PSD_fluxposB_var);



  class_L_monitor_save(&_lambda_in_var);








  class_PSD_monitor_save(&_PSD_sample_var);

  class_L_monitor_save(&_lambda_sample_var);




  class_Monitor_nD_save(&_Detector_var);

  if (!handle) siminfo_close(); 

  return(0);
} /* save */

/* *****************************************************************************
* instrument 'PSI_DMC' and components FINALLY
***************************************************************************** */

_class_Progress_bar *class_Progress_bar_finally(_class_Progress_bar *_comp
) {
  #define profile (_comp->_parameters.profile)
  #define percent (_comp->_parameters.percent)
  #define flag_save (_comp->_parameters.flag_save)
  #define minutes (_comp->_parameters.minutes)
  #define IntermediateCnts (_comp->_parameters.IntermediateCnts)
  #define StartTime (_comp->_parameters.StartTime)
  #define EndTime (_comp->_parameters.EndTime)
  #define CurrentTime (_comp->_parameters.CurrentTime)
  SIG_MESSAGE("[_source_arm_finally] component source_arm=Progress_bar() FINALLY [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/misc/Progress_bar.comp:135]");

  time_t NowTime;
  time(&NowTime);
  fprintf(stdout, "\nFinally [%s: %s]. Time: ", instrument_name, dirname ? dirname : ".");
  if (difftime(NowTime,StartTime) < 60.0)
    fprintf(stdout, "%g [s] ", difftime(NowTime,StartTime));
  else if (difftime(NowTime,StartTime) > 3600.0)
    fprintf(stdout, "%g [h] ", difftime(NowTime,StartTime)/3660.0);
  else
    fprintf(stdout, "%g [min] ", difftime(NowTime,StartTime)/60.0);
  fprintf(stdout, "\n");
  #undef profile
  #undef percent
  #undef flag_save
  #undef minutes
  #undef IntermediateCnts
  #undef StartTime
  #undef EndTime
  #undef CurrentTime
  return(_comp);
} /* class_Progress_bar_finally */

_class_L_monitor *class_L_monitor_finally(_class_L_monitor *_comp
) {
  #define nL (_comp->_parameters.nL)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define Lmin (_comp->_parameters.Lmin)
  #define Lmax (_comp->_parameters.Lmax)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define L_N (_comp->_parameters.L_N)
  #define L_p (_comp->_parameters.L_p)
  #define L_p2 (_comp->_parameters.L_p2)
  SIG_MESSAGE("[_l_mon_source_finally] component l_mon_source=L_monitor() FINALLY [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/L_monitor.comp:125]");

  destroy_darr1d(L_N);
  destroy_darr1d(L_p);
  destroy_darr1d(L_p2);
  #undef nL
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef Lmin
  #undef Lmax
  #undef restore_neutron
  #undef L_N
  #undef L_p
  #undef L_p2
  return(_comp);
} /* class_L_monitor_finally */

_class_PSDlin_monitor *class_PSDlin_monitor_finally(_class_PSDlin_monitor *_comp
) {
  #define nx (_comp->_parameters.nx)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define PSDlin_N (_comp->_parameters.PSDlin_N)
  #define PSDlin_p (_comp->_parameters.PSDlin_p)
  #define PSDlin_p2 (_comp->_parameters.PSDlin_p2)
  SIG_MESSAGE("[_ydist_fluxpos_finally] component ydist_fluxpos=PSDlin_monitor() FINALLY [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/PSDlin_monitor.comp:122]");

  destroy_darr1d(PSDlin_N);
  destroy_darr1d(PSDlin_p);
  destroy_darr1d(PSDlin_p2);
  #undef nx
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef PSDlin_N
  #undef PSDlin_p
  #undef PSDlin_p2
  return(_comp);
} /* class_PSDlin_monitor_finally */

_class_PowderN *class_PowderN_finally(_class_PowderN *_comp
) {
  #define reflections (_comp->_parameters.reflections)
  #define geometry (_comp->_parameters.geometry)
  #define format (_comp->_parameters.format)
  #define radius (_comp->_parameters.radius)
  #define yheight (_comp->_parameters.yheight)
  #define xwidth (_comp->_parameters.xwidth)
  #define zdepth (_comp->_parameters.zdepth)
  #define thickness (_comp->_parameters.thickness)
  #define pack (_comp->_parameters.pack)
  #define Vc (_comp->_parameters.Vc)
  #define sigma_abs (_comp->_parameters.sigma_abs)
  #define sigma_inc (_comp->_parameters.sigma_inc)
  #define delta_d_d (_comp->_parameters.delta_d_d)
  #define p_inc (_comp->_parameters.p_inc)
  #define p_transmit (_comp->_parameters.p_transmit)
  #define DW (_comp->_parameters.DW)
  #define nb_atoms (_comp->_parameters.nb_atoms)
  #define d_phi (_comp->_parameters.d_phi)
  #define p_interact (_comp->_parameters.p_interact)
  #define concentric (_comp->_parameters.concentric)
  #define density (_comp->_parameters.density)
  #define weight (_comp->_parameters.weight)
  #define barns (_comp->_parameters.barns)
  #define Strain (_comp->_parameters.Strain)
  #define focus_flip (_comp->_parameters.focus_flip)
  #define line_info (_comp->_parameters.line_info)
  #define columns (_comp->_parameters.columns)
  #define offdata (_comp->_parameters.offdata)
  SIG_MESSAGE("[_sample_finally] component sample=PowderN() FINALLY [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/samples/PowderN.comp:1052]");

  free(line_info.list);
  free(line_info.q_v);
  free(line_info.w_v);
  free(line_info.my_s_v2);
  MPI_MASTER(
  if (line_info.flag_warning)
    printf("PowderN: %s: Error messages were repeated %i times with absorbed neutrons.\n",
      NAME_CURRENT_COMP, line_info.flag_warning);

  /* in case this instance is used in a SPLIT, we can recommend the
     optimal iteration value */
  if (line_info.nb_refl_count) {
    double split_iterations = (double)line_info.nb_reuses/line_info.nb_refl_count + 1;
    double split_optimal    = (double)line_info.nb_refl/line_info.nb_refl_count;
    if (split_optimal > split_iterations + 5)
      printf("PowderN: %s: Info: you may highly improve the computation efficiency by using\n"
        "    SPLIT %i COMPONENT %s=PowderN(...)\n"
        "  in the instrument description %s.\n",
        NAME_CURRENT_COMP, (int)split_optimal, NAME_CURRENT_COMP, instrument_source);
  }
  );

  #undef reflections
  #undef geometry
  #undef format
  #undef radius
  #undef yheight
  #undef xwidth
  #undef zdepth
  #undef thickness
  #undef pack
  #undef Vc
  #undef sigma_abs
  #undef sigma_inc
  #undef delta_d_d
  #undef p_inc
  #undef p_transmit
  #undef DW
  #undef nb_atoms
  #undef d_phi
  #undef p_interact
  #undef concentric
  #undef density
  #undef weight
  #undef barns
  #undef Strain
  #undef focus_flip
  #undef line_info
  #undef columns
  #undef offdata
  return(_comp);
} /* class_PowderN_finally */

_class_Monitor_nD *class_Monitor_nD_finally(_class_Monitor_nD *_comp
) {
  #define user1 (_comp->_parameters.user1)
  #define user2 (_comp->_parameters.user2)
  #define user3 (_comp->_parameters.user3)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define zdepth (_comp->_parameters.zdepth)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define zmin (_comp->_parameters.zmin)
  #define zmax (_comp->_parameters.zmax)
  #define bins (_comp->_parameters.bins)
  #define min (_comp->_parameters.min)
  #define max (_comp->_parameters.max)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define radius (_comp->_parameters.radius)
  #define options (_comp->_parameters.options)
  #define filename (_comp->_parameters.filename)
  #define geometry (_comp->_parameters.geometry)
  #define username1 (_comp->_parameters.username1)
  #define username2 (_comp->_parameters.username2)
  #define username3 (_comp->_parameters.username3)
  #define DEFS (_comp->_parameters.DEFS)
  #define Vars (_comp->_parameters.Vars)
  #define detector (_comp->_parameters.detector)
  #define offdata (_comp->_parameters.offdata)
  SIG_MESSAGE("[_Detector_finally] component Detector=Monitor_nD() FINALLY [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/Monitor_nD.comp:502]");

  /* free pointers */
  Monitor_nD_Finally(&DEFS, &Vars);
  #undef user1
  #undef user2
  #undef user3
  #undef xwidth
  #undef yheight
  #undef zdepth
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef zmin
  #undef zmax
  #undef bins
  #undef min
  #undef max
  #undef restore_neutron
  #undef radius
  #undef options
  #undef filename
  #undef geometry
  #undef username1
  #undef username2
  #undef username3
  #undef DEFS
  #undef Vars
  #undef detector
  #undef offdata
  return(_comp);
} /* class_Monitor_nD_finally */



int finally(void) { /* called by mccode_main for PSI_DMC:FINALLY */
#pragma acc update host(_source_arm_var)
#pragma acc update host(_source_var)
#pragma acc update host(_PSDbefore_guides_var)
#pragma acc update host(_l_mon_source_var)
#pragma acc update host(_guide1_var)
#pragma acc update host(_PSDbefore_curve_var)
#pragma acc update host(_guide2_var)
#pragma acc update host(_PSDafter_curve_var)
#pragma acc update host(_bunker_var)
#pragma acc update host(_guide3_var)
#pragma acc update host(_guide4_var)
#pragma acc update host(_window1_var)
#pragma acc update host(_ydist_fluxpos_var)
#pragma acc update host(_PSD_fluxpos_var)
#pragma acc update host(_xdist_flux_pos_var)
#pragma acc update host(_PSD_fluxposB_var)
#pragma acc update host(_window2_var)
#pragma acc update host(_in_slit_var)
#pragma acc update host(_lambda_in_var)
#pragma acc update host(_sma_var)
#pragma acc update host(_foc_mono_var)
#pragma acc update host(_msa_var)
#pragma acc update host(_out1_slit_var)
#pragma acc update host(_Amoin_slit_var)
#pragma acc update host(_Bmoin_slit_var)
#pragma acc update host(_out2_slit_var)
#pragma acc update host(_PSD_sample_var)
#pragma acc update host(_lambda_sample_var)
#pragma acc update host(_sa_arm_var)
#pragma acc update host(_sample_var)
#pragma acc update host(_STOP_var)
#pragma acc update host(_Detector_var)
#pragma acc update host(_instrument_var)

  siminfo_init(NULL);
  save(siminfo_file); /* save data when simulation ends */

  /* call iteratively all components FINALLY */
  class_Progress_bar_finally(&_source_arm_var);



  class_L_monitor_finally(&_l_mon_source_var);









  class_PSDlin_monitor_finally(&_ydist_fluxpos_var);


  class_PSDlin_monitor_finally(&_xdist_flux_pos_var);




  class_L_monitor_finally(&_lambda_in_var);









  class_L_monitor_finally(&_lambda_sample_var);


  class_PowderN_finally(&_sample_var);


  class_Monitor_nD_finally(&_Detector_var);

  siminfo_close(); 

  return(0);
} /* finally */

/* *****************************************************************************
* instrument 'PSI_DMC' and components DISPLAY
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
_class_Progress_bar *class_Progress_bar_display(_class_Progress_bar *_comp
) {
  #define profile (_comp->_parameters.profile)
  #define percent (_comp->_parameters.percent)
  #define flag_save (_comp->_parameters.flag_save)
  #define minutes (_comp->_parameters.minutes)
  #define IntermediateCnts (_comp->_parameters.IntermediateCnts)
  #define StartTime (_comp->_parameters.StartTime)
  #define EndTime (_comp->_parameters.EndTime)
  #define CurrentTime (_comp->_parameters.CurrentTime)
  SIG_MESSAGE("[_source_arm_display] component source_arm=Progress_bar() DISPLAY [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/misc/Progress_bar.comp:149]");

  printf("MCDISPLAY: component %s\n", _comp->_name);

  #undef profile
  #undef percent
  #undef flag_save
  #undef minutes
  #undef IntermediateCnts
  #undef StartTime
  #undef EndTime
  #undef CurrentTime
  return(_comp);
} /* class_Progress_bar_display */

_class_Source_Maxwell_3 *class_Source_Maxwell_3_display(_class_Source_Maxwell_3 *_comp
) {
  #define size (_comp->_parameters.size)
  #define yheight (_comp->_parameters.yheight)
  #define xwidth (_comp->_parameters.xwidth)
  #define Lmin (_comp->_parameters.Lmin)
  #define Lmax (_comp->_parameters.Lmax)
  #define dist (_comp->_parameters.dist)
  #define focus_xw (_comp->_parameters.focus_xw)
  #define focus_yh (_comp->_parameters.focus_yh)
  #define T1 (_comp->_parameters.T1)
  #define T2 (_comp->_parameters.T2)
  #define T3 (_comp->_parameters.T3)
  #define I1 (_comp->_parameters.I1)
  #define I2 (_comp->_parameters.I2)
  #define I3 (_comp->_parameters.I3)
  #define target_index (_comp->_parameters.target_index)
  #define lambda0 (_comp->_parameters.lambda0)
  #define dlambda (_comp->_parameters.dlambda)
  #define M (_comp->_parameters.M)
  #define l_range (_comp->_parameters.l_range)
  #define w_mult (_comp->_parameters.w_mult)
  #define w_source (_comp->_parameters.w_source)
  #define h_source (_comp->_parameters.h_source)
  SIG_MESSAGE("[_source_display] component source=Source_Maxwell_3() DISPLAY [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/sources/Source_Maxwell_3.comp:156]");

  printf("MCDISPLAY: component %s\n", _comp->_name);
  
  multiline(5, -(double)focus_xw/2.0, -(double)focus_yh/2.0, 0.0,
                (double)focus_xw/2.0, -(double)focus_yh/2.0, 0.0,
                (double)focus_xw/2.0,  (double)focus_yh/2.0, 0.0,
               -(double)focus_xw/2.0,  (double)focus_yh/2.0, 0.0,
               -(double)focus_xw/2.0, -(double)focus_yh/2.0, 0.0);
  if (dist) {
    dashed_line(0,0,0, -focus_xw/2,-focus_yh/2,dist, 4);
    dashed_line(0,0,0,  focus_xw/2,-focus_yh/2,dist, 4);
    dashed_line(0,0,0,  focus_xw/2, focus_yh/2,dist, 4);
    dashed_line(0,0,0, -focus_xw/2, focus_yh/2,dist, 4);
  }
  #undef size
  #undef yheight
  #undef xwidth
  #undef Lmin
  #undef Lmax
  #undef dist
  #undef focus_xw
  #undef focus_yh
  #undef T1
  #undef T2
  #undef T3
  #undef I1
  #undef I2
  #undef I3
  #undef target_index
  #undef lambda0
  #undef dlambda
  #undef M
  #undef l_range
  #undef w_mult
  #undef w_source
  #undef h_source
  return(_comp);
} /* class_Source_Maxwell_3_display */

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
  #define nowritefile (_comp->_parameters.nowritefile)
  #define PSD_N (_comp->_parameters.PSD_N)
  #define PSD_p (_comp->_parameters.PSD_p)
  #define PSD_p2 (_comp->_parameters.PSD_p2)
  SIG_MESSAGE("[_PSDbefore_guides_display] component PSDbefore_guides=PSD_monitor() DISPLAY [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/PSD_monitor.comp:126]");

  printf("MCDISPLAY: component %s\n", _comp->_name);
  
  multiline(5, (double)xmin, (double)ymin, 0.0,
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
  #undef nowritefile
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  return(_comp);
} /* class_PSD_monitor_display */

_class_L_monitor *class_L_monitor_display(_class_L_monitor *_comp
) {
  #define nL (_comp->_parameters.nL)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define Lmin (_comp->_parameters.Lmin)
  #define Lmax (_comp->_parameters.Lmax)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define L_N (_comp->_parameters.L_N)
  #define L_p (_comp->_parameters.L_p)
  #define L_p2 (_comp->_parameters.L_p2)
  SIG_MESSAGE("[_l_mon_source_display] component l_mon_source=L_monitor() DISPLAY [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/L_monitor.comp:132]");

  printf("MCDISPLAY: component %s\n", _comp->_name);
  multiline(5, (double)xmin, (double)ymin, 0.0,
               (double)xmax, (double)ymin, 0.0,
               (double)xmax, (double)ymax, 0.0,
               (double)xmin, (double)ymax, 0.0,
               (double)xmin, (double)ymin, 0.0);
  #undef nL
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef Lmin
  #undef Lmax
  #undef restore_neutron
  #undef L_N
  #undef L_p
  #undef L_p2
  return(_comp);
} /* class_L_monitor_display */

_class_Guide *class_Guide_display(_class_Guide *_comp
) {
  #define reflect (_comp->_parameters.reflect)
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
  #define pTable (_comp->_parameters.pTable)
  #define table_present (_comp->_parameters.table_present)
  SIG_MESSAGE("[_guide1_display] component guide1=Guide() DISPLAY [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Guide.comp:204]");

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
  #undef reflect
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
  #undef pTable
  #undef table_present
  return(_comp);
} /* class_Guide_display */

_class_Bender *class_Bender_display(_class_Bender *_comp
) {
  #define w (_comp->_parameters.w)
  #define h (_comp->_parameters.h)
  #define r (_comp->_parameters.r)
  #define Win (_comp->_parameters.Win)
  #define k (_comp->_parameters.k)
  #define d (_comp->_parameters.d)
  #define l (_comp->_parameters.l)
  #define R0a (_comp->_parameters.R0a)
  #define Qca (_comp->_parameters.Qca)
  #define alphaa (_comp->_parameters.alphaa)
  #define ma (_comp->_parameters.ma)
  #define Wa (_comp->_parameters.Wa)
  #define R0i (_comp->_parameters.R0i)
  #define Qci (_comp->_parameters.Qci)
  #define alphai (_comp->_parameters.alphai)
  #define mi (_comp->_parameters.mi)
  #define Wi (_comp->_parameters.Wi)
  #define R0s (_comp->_parameters.R0s)
  #define Qcs (_comp->_parameters.Qcs)
  #define alphas (_comp->_parameters.alphas)
  #define ms (_comp->_parameters.ms)
  #define Ws (_comp->_parameters.Ws)
  #define bk (_comp->_parameters.bk)
  #define mWin (_comp->_parameters.mWin)
  SIG_MESSAGE("[_guide2_display] component guide2=Bender() DISPLAY [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Bender.comp:269]");

  printf("MCDISPLAY: component %s\n", _comp->_name);
  int i;
  double w1c, w2c, h1, h2, L, w1, w2;

  w1c = (w + d)/(double)k;
  w2c = w1c; h1 = h; h2 = h;
  L = r*mWin; w1 = w; w2 = w;

  
  for(i = 0; i < k; i++)
  {
    multiline(5,
              i*w1c - w1/2.0, -h1/2.0, 0.0,
              i*w2c - w2/2.0, -h2/2.0, (double)L,
              i*w2c - w2/2.0,  h2/2.0, (double)L,
              i*w1c - w1/2.0,  h1/2.0, 0.0,
              i*w1c - w1/2.0, -h1/2.0, 0.0);
    multiline(5,
              (i+1)*w1c - d - w1/2.0, -h1/2.0, 0.0,
              (i+1)*w2c - d - w2/2.0, -h2/2.0, (double)L,
              (i+1)*w2c - d - w2/2.0,  h2/2.0, (double)L,
              (i+1)*w1c - d - w1/2.0,  h1/2.0, 0.0,
              (i+1)*w1c - d - w1/2.0, -h1/2.0, 0.0);
  }
  line(-w1/2.0, -h1/2.0, 0.0, w1/2.0, -h1/2.0, 0.0);
  line(-w2/2.0, -h2/2.0, (double)L, w2/2.0, -h2/2.0, (double)L);
  #undef w
  #undef h
  #undef r
  #undef Win
  #undef k
  #undef d
  #undef l
  #undef R0a
  #undef Qca
  #undef alphaa
  #undef ma
  #undef Wa
  #undef R0i
  #undef Qci
  #undef alphai
  #undef mi
  #undef Wi
  #undef R0s
  #undef Qcs
  #undef alphas
  #undef ms
  #undef Ws
  #undef bk
  #undef mWin
  return(_comp);
} /* class_Bender_display */

_class_Al_window *class_Al_window_display(_class_Al_window *_comp
) {
  #define thickness (_comp->_parameters.thickness)
  SIG_MESSAGE("[_window1_display] component window1=Al_window() DISPLAY [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/contrib/Al_window.comp:95]");

  printf("MCDISPLAY: component %s\n", _comp->_name);
  /* A bit ugly; hard-coded dimensions. */
  
  line(0,0,0,0.2,0,0);
  line(0,0,0,0,0.2,0);
  line(0,0,0,0,0,0.2);
  #undef thickness
  return(_comp);
} /* class_Al_window_display */

_class_PSDlin_monitor *class_PSDlin_monitor_display(_class_PSDlin_monitor *_comp
) {
  #define nx (_comp->_parameters.nx)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define PSDlin_N (_comp->_parameters.PSDlin_N)
  #define PSDlin_p (_comp->_parameters.PSDlin_p)
  #define PSDlin_p2 (_comp->_parameters.PSDlin_p2)
  SIG_MESSAGE("[_ydist_fluxpos_display] component ydist_fluxpos=PSDlin_monitor() DISPLAY [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/PSDlin_monitor.comp:129]");

  printf("MCDISPLAY: component %s\n", _comp->_name);
  multiline(5, (double)xmin, (double)ymin, 0.0,
               (double)xmax, (double)ymin, 0.0,
               (double)xmax, (double)ymax, 0.0,
               (double)xmin, (double)ymax, 0.0,
               (double)xmin, (double)ymin, 0.0);
  #undef nx
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef PSDlin_N
  #undef PSDlin_p
  #undef PSDlin_p2
  return(_comp);
} /* class_PSDlin_monitor_display */

_class_Slit *class_Slit_display(_class_Slit *_comp
) {
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define radius (_comp->_parameters.radius)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  SIG_MESSAGE("[_in_slit_display] component in_slit=Slit() DISPLAY [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Slit.comp:81]");

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

_class_Arm *class_Arm_display(_class_Arm *_comp
) {
  SIG_MESSAGE("[_sma_display] component sma=Arm() DISPLAY [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Arm.comp:40]");

  printf("MCDISPLAY: component %s\n", _comp->_name);
  /* A bit ugly; hard-coded dimensions. */
  
  line(0,0,0,0.2,0,0);
  line(0,0,0,0,0.2,0);
  line(0,0,0,0,0,0.2);
  return(_comp);
} /* class_Arm_display */

_class_Monochromator_2foc *class_Monochromator_2foc_display(_class_Monochromator_2foc *_comp
) {
  #define reflect (_comp->_parameters.reflect)
  #define zwidth (_comp->_parameters.zwidth)
  #define yheight (_comp->_parameters.yheight)
  #define gap (_comp->_parameters.gap)
  #define NH (_comp->_parameters.NH)
  #define NV (_comp->_parameters.NV)
  #define mosaich (_comp->_parameters.mosaich)
  #define mosaicv (_comp->_parameters.mosaicv)
  #define r0 (_comp->_parameters.r0)
  #define Q (_comp->_parameters.Q)
  #define RV (_comp->_parameters.RV)
  #define RH (_comp->_parameters.RH)
  #define DM (_comp->_parameters.DM)
  #define mosaic (_comp->_parameters.mosaic)
  #define width (_comp->_parameters.width)
  #define height (_comp->_parameters.height)
  #define verbose (_comp->_parameters.verbose)
  #define mos_y (_comp->_parameters.mos_y)
  #define mos_z (_comp->_parameters.mos_z)
  #define mono_Q (_comp->_parameters.mono_Q)
  #define SlabWidth (_comp->_parameters.SlabWidth)
  #define SlabHeight (_comp->_parameters.SlabHeight)
  #define rTable (_comp->_parameters.rTable)
  SIG_MESSAGE("[_foc_mono_display] component foc_mono=Monochromator_2foc() DISPLAY [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/contrib/Monochromator_2foc.comp:279]");

  printf("MCDISPLAY: component %s\n", _comp->_name);
  int ih;

  
  for(ih = 0; ih < NH; ih++)
  {
    int iv;
    for(iv = 0; iv < NV; iv++)
    {
      double zmin,zmax,ymin,ymax;
      double xt, xt1, yt, yt1;

      zmin = (SlabWidth+gap)*(ih-NH/2.0)+gap/2;
      zmax = zmin+SlabWidth;
      ymin = (SlabHeight+gap)*(iv-NV/2.0)+gap/2;
      ymax = ymin+SlabHeight;

      if (RH)
      { xt = zmin*zmin/RH;
        xt1 = zmax*zmax/RH; }
      else { xt = 0; xt1 = 0; }

      if (RV)
      { yt = ymin*ymin/RV;
        yt1 = ymax*ymax/RV; }
      else { yt = 0; yt1 = 0; }
      multiline(5, xt+yt, (double)ymin, (double)zmin,
                   xt+yt1, (double)ymax, (double)zmin,
                   xt1+yt1, (double)ymax, (double)zmax,
                   xt1+yt, (double)ymin, (double)zmax,
                   xt+yt, (double)ymin, (double)zmin);
     }
   }
  #undef reflect
  #undef zwidth
  #undef yheight
  #undef gap
  #undef NH
  #undef NV
  #undef mosaich
  #undef mosaicv
  #undef r0
  #undef Q
  #undef RV
  #undef RH
  #undef DM
  #undef mosaic
  #undef width
  #undef height
  #undef verbose
  #undef mos_y
  #undef mos_z
  #undef mono_Q
  #undef SlabWidth
  #undef SlabHeight
  #undef rTable
  return(_comp);
} /* class_Monochromator_2foc_display */

_class_PowderN *class_PowderN_display(_class_PowderN *_comp
) {
  #define reflections (_comp->_parameters.reflections)
  #define geometry (_comp->_parameters.geometry)
  #define format (_comp->_parameters.format)
  #define radius (_comp->_parameters.radius)
  #define yheight (_comp->_parameters.yheight)
  #define xwidth (_comp->_parameters.xwidth)
  #define zdepth (_comp->_parameters.zdepth)
  #define thickness (_comp->_parameters.thickness)
  #define pack (_comp->_parameters.pack)
  #define Vc (_comp->_parameters.Vc)
  #define sigma_abs (_comp->_parameters.sigma_abs)
  #define sigma_inc (_comp->_parameters.sigma_inc)
  #define delta_d_d (_comp->_parameters.delta_d_d)
  #define p_inc (_comp->_parameters.p_inc)
  #define p_transmit (_comp->_parameters.p_transmit)
  #define DW (_comp->_parameters.DW)
  #define nb_atoms (_comp->_parameters.nb_atoms)
  #define d_phi (_comp->_parameters.d_phi)
  #define p_interact (_comp->_parameters.p_interact)
  #define concentric (_comp->_parameters.concentric)
  #define density (_comp->_parameters.density)
  #define weight (_comp->_parameters.weight)
  #define barns (_comp->_parameters.barns)
  #define Strain (_comp->_parameters.Strain)
  #define focus_flip (_comp->_parameters.focus_flip)
  #define line_info (_comp->_parameters.line_info)
  #define columns (_comp->_parameters.columns)
  #define offdata (_comp->_parameters.offdata)
  SIG_MESSAGE("[_sample_display] component sample=PowderN() DISPLAY [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/samples/PowderN.comp:1078]");

  printf("MCDISPLAY: component %s\n", _comp->_name);
#ifndef USE_PGI
  if (line_info.V_0) {

    if (line_info.shape == 0) { /* cyl */
      circle("xz", 0,  yheight/2.0, 0, radius);
      circle("xz", 0, -yheight/2.0, 0, radius);
      line(-radius, -yheight/2.0, 0, -radius, +yheight/2.0, 0);
      line(+radius, -yheight/2.0, 0, +radius, +yheight/2.0, 0);
      line(0, -yheight/2.0, -radius, 0, +yheight/2.0, -radius);
      line(0, -yheight/2.0, +radius, 0, +yheight/2.0, +radius);
      if (thickness) {
        double radius_i=radius-thickness;
        circle("xz", 0,  yheight/2.0, 0, radius_i);
        circle("xz", 0, -yheight/2.0, 0, radius_i);
        line(-radius_i, -yheight/2.0, 0, -radius_i, +yheight/2.0, 0);
        line(+radius_i, -yheight/2.0, 0, +radius_i, +yheight/2.0, 0);
        line(0, -yheight/2.0, -radius_i, 0, +yheight/2.0, -radius_i);
        line(0, -yheight/2.0, +radius_i, 0, +yheight/2.0, +radius_i);
      }
    } else if (line_info.shape == 1) {  /* box */
      double xmin = -0.5*xwidth;
      double xmax =  0.5*xwidth;
      double ymin = -0.5*yheight;
      double ymax =  0.5*yheight;
      double zmin = -0.5*zdepth;
      double zmax =  0.5*zdepth;
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
      if (line_info.zdepth_i) {
        xmin = -0.5*line_info.xwidth_i;
        xmax =  0.5*line_info.xwidth_i;
        ymin = -0.5*line_info.yheight_i;
        ymax =  0.5*line_info.yheight_i;
        zmin = -0.5*line_info.zdepth_i;
        zmax =  0.5*line_info.zdepth_i;
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
    } if (line_info.shape == 2) { /* sphere */
      if (line_info.radius_i) {
        circle("xy",0,0,0,line_info.radius_i);
        circle("xz",0,0,0,line_info.radius_i);
        circle("yz",0,0,0,line_info.radius_i);
      }
      circle("xy",0,0,0,radius);
      circle("xz",0,0,0,radius);
      circle("yz",0,0,0,radius);
    } else if (line_info.shape == 3) {	/* OFF file */
      off_display(offdata);
    }
  }
#endif
  #undef reflections
  #undef geometry
  #undef format
  #undef radius
  #undef yheight
  #undef xwidth
  #undef zdepth
  #undef thickness
  #undef pack
  #undef Vc
  #undef sigma_abs
  #undef sigma_inc
  #undef delta_d_d
  #undef p_inc
  #undef p_transmit
  #undef DW
  #undef nb_atoms
  #undef d_phi
  #undef p_interact
  #undef concentric
  #undef density
  #undef weight
  #undef barns
  #undef Strain
  #undef focus_flip
  #undef line_info
  #undef columns
  #undef offdata
  return(_comp);
} /* class_PowderN_display */

_class_Beamstop *class_Beamstop_display(_class_Beamstop *_comp
) {
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define radius (_comp->_parameters.radius)
  SIG_MESSAGE("[_STOP_display] component STOP=Beamstop() DISPLAY [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/optics/Beamstop.comp:72]");

  printf("MCDISPLAY: component %s\n", _comp->_name);
  
  if (radius != 0)
    circle("xy", 0, 0, 0, radius);
  else
    multiline(5, (double)xmin, (double)ymin, 0.0,
               (double)xmax, (double)ymin, 0.0,
               (double)xmax, (double)ymax, 0.0,
               (double)xmin, (double)ymax, 0.0,
               (double)xmin, (double)ymin, 0.0);
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef radius
  return(_comp);
} /* class_Beamstop_display */

_class_Monitor_nD *class_Monitor_nD_display(_class_Monitor_nD *_comp
) {
  #define user1 (_comp->_parameters.user1)
  #define user2 (_comp->_parameters.user2)
  #define user3 (_comp->_parameters.user3)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define zdepth (_comp->_parameters.zdepth)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define zmin (_comp->_parameters.zmin)
  #define zmax (_comp->_parameters.zmax)
  #define bins (_comp->_parameters.bins)
  #define min (_comp->_parameters.min)
  #define max (_comp->_parameters.max)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define radius (_comp->_parameters.radius)
  #define options (_comp->_parameters.options)
  #define filename (_comp->_parameters.filename)
  #define geometry (_comp->_parameters.geometry)
  #define username1 (_comp->_parameters.username1)
  #define username2 (_comp->_parameters.username2)
  #define username3 (_comp->_parameters.username3)
  #define DEFS (_comp->_parameters.DEFS)
  #define Vars (_comp->_parameters.Vars)
  #define detector (_comp->_parameters.detector)
  #define offdata (_comp->_parameters.offdata)
  SIG_MESSAGE("[_Detector_display] component Detector=Monitor_nD() DISPLAY [/zhome/89/0/38697/SPLIT/../McStas/mcstas/3.0-dev/monitors/Monitor_nD.comp:508]");

  printf("MCDISPLAY: component %s\n", _comp->_name);
#ifndef USE_PGI
  if (geometry && strlen(geometry) && strcmp(geometry,"0") && strcmp(geometry, "NULL"))
  {
    off_display(offdata);
  } else {
    Monitor_nD_McDisplay(&DEFS, &Vars);
  }
#endif
  #undef user1
  #undef user2
  #undef user3
  #undef xwidth
  #undef yheight
  #undef zdepth
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef zmin
  #undef zmax
  #undef bins
  #undef min
  #undef max
  #undef restore_neutron
  #undef radius
  #undef options
  #undef filename
  #undef geometry
  #undef username1
  #undef username2
  #undef username3
  #undef DEFS
  #undef Vars
  #undef detector
  #undef offdata
  return(_comp);
} /* class_Monitor_nD_display */


  #undef magnify
  #undef line
  #undef dashed_line
  #undef multiline
  #undef rectangle
  #undef box
  #undef circle
  #undef cylinder
  #undef sphere

int display(void) { /* called by mccode_main for PSI_DMC:DISPLAY */
  printf("MCDISPLAY: start\n");

  /* call iteratively all components DISPLAY */
  class_Progress_bar_display(&_source_arm_var);

  class_Source_Maxwell_3_display(&_source_var);

  class_PSD_monitor_display(&_PSDbefore_guides_var);

  class_L_monitor_display(&_l_mon_source_var);

  class_Guide_display(&_guide1_var);

  class_PSD_monitor_display(&_PSDbefore_curve_var);

  class_Bender_display(&_guide2_var);

  class_PSD_monitor_display(&_PSDafter_curve_var);

  class_Guide_display(&_bunker_var);

  class_Guide_display(&_guide3_var);

  class_Guide_display(&_guide4_var);

  class_Al_window_display(&_window1_var);

  class_PSDlin_monitor_display(&_ydist_fluxpos_var);

  class_PSD_monitor_display(&_PSD_fluxpos_var);

  class_PSDlin_monitor_display(&_xdist_flux_pos_var);

  class_PSD_monitor_display(&_PSD_fluxposB_var);

  class_Al_window_display(&_window2_var);

  class_Slit_display(&_in_slit_var);

  class_L_monitor_display(&_lambda_in_var);

  class_Arm_display(&_sma_var);

  class_Monochromator_2foc_display(&_foc_mono_var);

  class_Arm_display(&_msa_var);

  class_Slit_display(&_out1_slit_var);

  class_Slit_display(&_Amoin_slit_var);

  class_Slit_display(&_Bmoin_slit_var);

  class_Slit_display(&_out2_slit_var);

  class_PSD_monitor_display(&_PSD_sample_var);

  class_L_monitor_display(&_lambda_sample_var);

  class_Arm_display(&_sa_arm_var);

  class_PowderN_display(&_sample_var);

  class_Beamstop_display(&_STOP_var);

  class_Monitor_nD_display(&_Detector_var);

  printf("MCDISPLAY: end\n");

  return(0);
} /* display */

void* _getvar_parameters(char* compname)
/* enables settings parameters based use of the GETPAR macro */
{
  if (!strcmp(compname, "source_arm")) return (void *) &(_source_arm_var._parameters);
  if (!strcmp(compname, "source")) return (void *) &(_source_var._parameters);
  if (!strcmp(compname, "PSDbefore_guides")) return (void *) &(_PSDbefore_guides_var._parameters);
  if (!strcmp(compname, "l_mon_source")) return (void *) &(_l_mon_source_var._parameters);
  if (!strcmp(compname, "guide1")) return (void *) &(_guide1_var._parameters);
  if (!strcmp(compname, "PSDbefore_curve")) return (void *) &(_PSDbefore_curve_var._parameters);
  if (!strcmp(compname, "guide2")) return (void *) &(_guide2_var._parameters);
  if (!strcmp(compname, "PSDafter_curve")) return (void *) &(_PSDafter_curve_var._parameters);
  if (!strcmp(compname, "bunker")) return (void *) &(_bunker_var._parameters);
  if (!strcmp(compname, "guide3")) return (void *) &(_guide3_var._parameters);
  if (!strcmp(compname, "guide4")) return (void *) &(_guide4_var._parameters);
  if (!strcmp(compname, "window1")) return (void *) &(_window1_var._parameters);
  if (!strcmp(compname, "ydist_fluxpos")) return (void *) &(_ydist_fluxpos_var._parameters);
  if (!strcmp(compname, "PSD_fluxpos")) return (void *) &(_PSD_fluxpos_var._parameters);
  if (!strcmp(compname, "xdist_flux_pos")) return (void *) &(_xdist_flux_pos_var._parameters);
  if (!strcmp(compname, "PSD_fluxposB")) return (void *) &(_PSD_fluxposB_var._parameters);
  if (!strcmp(compname, "window2")) return (void *) &(_window2_var._parameters);
  if (!strcmp(compname, "in_slit")) return (void *) &(_in_slit_var._parameters);
  if (!strcmp(compname, "lambda_in")) return (void *) &(_lambda_in_var._parameters);
  if (!strcmp(compname, "sma")) return (void *) &(_sma_var._parameters);
  if (!strcmp(compname, "foc_mono")) return (void *) &(_foc_mono_var._parameters);
  if (!strcmp(compname, "msa")) return (void *) &(_msa_var._parameters);
  if (!strcmp(compname, "out1_slit")) return (void *) &(_out1_slit_var._parameters);
  if (!strcmp(compname, "Amoin_slit")) return (void *) &(_Amoin_slit_var._parameters);
  if (!strcmp(compname, "Bmoin_slit")) return (void *) &(_Bmoin_slit_var._parameters);
  if (!strcmp(compname, "out2_slit")) return (void *) &(_out2_slit_var._parameters);
  if (!strcmp(compname, "PSD_sample")) return (void *) &(_PSD_sample_var._parameters);
  if (!strcmp(compname, "lambda_sample")) return (void *) &(_lambda_sample_var._parameters);
  if (!strcmp(compname, "sa_arm")) return (void *) &(_sa_arm_var._parameters);
  if (!strcmp(compname, "sample")) return (void *) &(_sample_var._parameters);
  if (!strcmp(compname, "STOP")) return (void *) &(_STOP_var._parameters);
  if (!strcmp(compname, "Detector")) return (void *) &(_Detector_var._parameters);
}

void* _get_particle_var(char *token, _class_particle *p)
/* enables setpars based use of GET_PARTICLE_DVAR macro and similar */
{
  return 0;
}

#include "_mccode_main.c"
/* End of file "mccode_main.c". */

/* end of generated C code PSI_DMC.c */
