/*******************************************************************************
* Main header file containing declarations of external functions and
* variables. This file is included by all modules.
*
* 	Project: Monte Carlo Simulation of Triple Axis Spectrometers
* 	File name: mcstas.h
*
* 	Author: K.N.			Jul  1, 1997
*
* 	$Id: mcstas.h,v 1.27 2003-01-21 07:50:44 pkwi Exp $
*
*
* Copyright (C) Risoe National Laboratory, 1997-1998, All rights reserved
*******************************************************************************/


#include <stdlib.h>
#include <stdio.h>

#include "port.h"

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif


/* Functions defined in memory.c */

typedef struct Pool_header *Pool;

void *mem(size_t);		/* Allocate memory. */
void memfree(void *);		/* Free memory. */
char *str_dup(char *);		/* Allocate new copy of string. */
char *str_dup_n(char *string, int n); /* Copies only first N chars. */
char *str_cat(char *first, ...);/* Concatenate strings to allocated string. */
char *str_quote(char *string);	/* Quote string for inclusion in C code */
void str_free(char *);		/* Free memory for string. */

Pool pool_create(void);		/* Create pool. */
void pool_free(Pool p);		/* Free pool and associated memory. */
void *pool_mem(Pool p, size_t size); /* Allocate memory in pool. */


/* Allocate memory to a pointer. If p is a pointer to type t, palloc(p) will
   make p point to dynamically allocated memory for one element of type
   t. Used to dynamicaaly allocate structures, eg.
   `struct S *p; palloc(p);'. */
#define palloc(p) ((p) = mem(sizeof(*(p))))

/* Allocate an array to a pointer. If p is a pointer to type t, nalloc(p, n)
   will make p point to a dynamically allocated array with n elements of type
   t. */
#define nalloc(p, n) ((p) = mem((n)*sizeof(*(p))))



/* Functions defined in symtab.c */

/* Structure for symbol table entries, returned by symtab_lookup() and the
   like.  */
struct Symtab_entry
  {
    char *name;
    void *val;
  };

/* Symbol table abstract data type. */
typedef struct Symbol_table *Symtab;
/* Abstract handle for symbol table traversals. */
typedef struct Symtab_position *Symtab_handle;

/* Create symbol table. */
Symtab symtab_create(void);
/* Lookup name in symbol table. */
struct Symtab_entry *symtab_lookup(Symtab, char *);
/* Add name to symbol table. */
struct Symtab_entry *symtab_add(Symtab, char *, void *);
/* Free memory for symbol table. */
void symtab_free(Symtab, void (*)(void *));
/* Prepare to traverse table (in no particular order). */
Symtab_handle symtab_iterate(Symtab s);
/* Get next entry in a traversal. */
struct Symtab_entry *symtab_next(Symtab_handle sh);
/* End a traversal. */
void symtab_iterate_end(Symtab_handle sh);


/* Definitions for list.c */

/* Abstract data type for lists. */
typedef struct List_header *List;
typedef struct List_position *List_handle;

List list_create(void);		/* Create list. */
void list_add(List, void *);	/* Add element at end. */
void list_free(List, void (*)(void *));	/* Deallocate a list. */
int list_len(List l);		/* Get list length. */
List_handle list_iterate(List);	/* Prepare to traverse list. */
void *list_next(List_handle);	/* Get next element in list. */
void list_iterate_end(List_handle); /* End list traversal. */


/*******************************************************************************
* Definitions for cexp.c
*******************************************************************************/

/* Type for expressions. The implementation is private and values of this type
   must only be accessed through the proper function calls. */
typedef struct cexp *CExp;

/* Extern functions defined in cexp.c */
CExp exp_id(char *id);		/* Make normal identifier. */
CExp exp_extern_id(char *id);	/* Make extern identifier. */
CExp exp_number(char *n);	/* Make expression from number. */
CExp exp_string(char *s);	/* Make expression from string. */
CExp exp_ctoken(char *s);	/* Make expression from generic C token */
CExp exp_compound(int n, ...);	/* Make compound expression */
void exp_free(CExp e);		/* Free memory for expression */
char *exp_tostring(CExp e);	/* Convert expression to string. */
void exp_fprint(FILE *f, CExp e); /* Output an expression to file. */
int exp_isvalue(CExp e);	/* Ask if expression is a value. */
void exp_setlineno(CExp e, int n); /* Set line number of expression */
int exp_getlineno(CExp e);	/* Get line number of expression, or zero */

/*******************************************************************************
* Definitions in coords.c
*******************************************************************************/

/* Type for coordinates. Public. */
struct coords
  {
    double x,y,z;
  };
typedef struct coords Coords;
struct coords_exp
  {
    CExp x,y,z;
  };
typedef struct coords_exp Coords_exp;

/* Get all-zero coordinate. */
Coords coords_origo(void);
Coords_exp coords_exp_origo(void);
/* Add two coordinates. */
Coords coords_add(Coords a, Coords b);


/*******************************************************************************
* Definitions for rotation.c
*******************************************************************************/

/* Rotation transformations. */
typedef double Rotation[3][3];

/* Get the unit rotation (no transformation). */
void rot_set_unit(Rotation t);
/* Rotate first around x, then around y, then around z axis. */
void rot_set_rotation(Rotation t, double phx, double phy, double phz);
/* Rotate around x axis. */
void rot_set_rotation_x(Rotation t, double ph);
/* Rotate around y axis. */
void rot_set_rotation_y(Rotation t, double ph);
/* Rotate around z axis. */
void rot_set_rotation_z(Rotation t, double ph);
/* Combine rotation (using a matrix multiply). */
void rot_mul(Rotation t1, Rotation t2, Rotation t3);
/* Copy rotation transformation. */
void rot_copy(Rotation dest, Rotation src);


/*******************************************************************************
* Definitions for position.c
*******************************************************************************/

/*******************************************************************************
* A component position consists in a place and an orientation. Place is the
* location in 3D space of the origo of the components local coordinate
* system, and orientation is the rotation transformation that transforms the
* global coordinate system into the component local one.
*
* At runtime, place is a 3-vector and orientation is a 3-by-3
* matrix. However, at compile time the actual values are not known. Instead,
* code is generated to compute the actual values for the position at
* runtime.
*******************************************************************************/

struct comp_position
  {
    Coords_exp place;		       /* (x,y,z) coordinate. */
    struct comp_inst *place_rel;       /* Instance relative to, or NULL. */
    Coords_exp orientation;	       /* X/Y/Z rotation. */
    struct comp_inst *orientation_rel;
  };

/* During parsing, individual structures are used for place and orientation. */
struct comp_place
  {
    Coords_exp place;
    struct comp_inst *place_rel;
  };
struct comp_orientation
  {
    Coords_exp orientation;
    struct comp_inst *orientation_rel;
    int isdefault;	/* True if this is a default orientation, generated
			   when no ROTATED modifier is given. */
  };

/*******************************************************************************
* Definitions in instrument.y
*******************************************************************************/

/* Name of the file currently being parsed. */
extern char *instr_current_filename;
/* Line number currently being scanned. */
extern int instr_current_line;
/* Result from parsing instrument definition. */
extern struct instr_def *instrument_definition;
/* Map from names to component instances. */
extern Symtab comp_instances;
/* List of component instances in declaration order. */
extern List comp_instances_list;
/* Map from names to component group instances. */ /* ADD: E. Farhi Sep 24th, 2001 group instances */
extern Symtab group_instances;
/* List of component group instances in declaration order. */ /* ADD: E. Farhi Sep 24th, 2001 group instances */
extern List group_instances_list;
/* Flag set to TRUE when scanning autoloaded component definitions. */
extern int parse_restricted;
/* Map of already-read components. */
extern Symtab read_components;

/* Check that component definition and setting parameters are unique. */
void check_comp_formals(List deflist, List setlist, char *compname);
/* Check that instrument parameters are unique. */
void check_instrument_formals(List formallist, char *instrname);
/* Check that the parameters of NXDICT declarations are valid. */
void check_nxdict(struct instr_def *instr);
/* Handle assignment of actual to formal component parameters. */
void comp_formals_actuals(struct comp_inst *comp, Symtab actuals);

/* Get component definition, reading from file if necessary. */
struct comp_def *read_component(char *name);


/*******************************************************************************
* Definitions in instrument.l
*******************************************************************************/

/* Prepare to run lexical analysis on new file. */
void lex_new_file(FILE *file);
/* Handle a new autoincluded file (uses recursive parser call). */
void push_autoload(FILE *file);



/*******************************************************************************
* Definitions for file.c
*******************************************************************************/


extern char *component_pathname;

/* Open file, searching the full search path. */
FILE *open_file_search(char *name);
/* Open component definition, searching the full search path. */
FILE *open_component_search(char *name);
/* Open file, searching only the system directory. */
FILE *open_file_search_sys(char *name);
/* Add a directory to the search path. */
void add_search_dir(char *name);


/*******************************************************************************
* Definitions for cogen.c
*******************************************************************************/

#define ID_PRE "mc"

/* Allocate a new, empty codeblock. */
struct code_block *codeblock_new(void);
/* Generate code for instrument definition. */
void cogen(char *output_name, struct instr_def *instr);


/*******************************************************************************
* Functions and variables defined in debug.c
*******************************************************************************/

extern int error_encountered;	/* Set to 1 when print_error called. */

void print_error(char *, ...);	/* Normal error messages. */
void print_warn(int *flag, char *format, ...); /* Warning. */
void fatal_error(char *, ...);	/* Report a fatal error and exit the program. */

#ifdef DEBUG

void debug_printf(char *, ...);	/* Internal; use debug macro instead. */
void debugn_printf(int, char *, ...); /* Internal; use debugn macro instead. */

/*******************************************************************************
* Debugging information. When the preprosessor flag DEBUG is defined,
* debugging messages are printed to stderr. This uses the 'debug' macro. A
* statement of the form debug((format, ...)) (note the double parenthesis)
* does nothing when debugging is disabled, and outputs debugging information
* printf-style when debigging is enabled. The macro 'debugn' takes an
* additional argument LEVEL; a compile-time option can be used to select
* output only up to a certain level.
*******************************************************************************/

#define debug(msg) debug_printf msg
#define debugn(msg) debugn_printf msg

/* 'Standard' debugging levels. */
#define DEBUG_ALWAYS  0		/* Always shown (if debugging enabled). */
#define DEBUG_HIGH   10
#define DEBUG_MEDIUM 20
#define DEBUG_LOW    30		/* Only shown at high debugging level. */

/*******************************************************************************
* Macro used to change the current debugging level. Useful to enable
* high-volume debugging output in a specific part of the program.
*******************************************************************************/
extern int debug_current_level;
#define debug_level(n) (debug_current_level = (n))

#else  /* !defined(DEBUG) */

#define debug(msg)
#define debugn(msg)
#define DEBUG_ALWAYS
#define DEBUG_HIGH
#define DEBUG_MEDIUM
#define DEBUG_LOW
#define debug_level(n)

#endif /* !defined(DEBUG) */



/* Common structure definitions. */

/* Code blocks. */
struct code_block
  {
    char *filename;		/* Name of origin source file. */
    char *quoted_filename;	/* Same, quoted for inclusion in C code. */
    int linenum;		/* Line number of first line. */
    List lines;			/* List of lines (strings with \n at end). */
  };


/* Component formal input parameters. */
struct comp_iformal
  {
    char *id;			/* Parameter name */
    int isoptional;		/* True if default value is available */
    CExp default_value;		/* Default value if isoptional is true */
  };

/* Component definitions. */
struct comp_def
  {
    char *name;			/* Component name. */
    int  comp_inst_number; /* ADD: E. Farhi Sep 20th, 2001 Number of this comp in the instrument  */
    List def_par, set_par, out_par, state_par; /* Formal parameters. */
    char **polarisation_par;	/* Polarisation state formal parameters. */
    struct code_block *share_code; /* Unique Declaration code (shared). */
    struct code_block *decl_code; /* Declaration code. */
    struct code_block *init_code; /* Initializeation code. */
    struct code_block *trace_code; /* Ray-trace simulation code. */
    struct code_block *finally_code; /* Code for simulation end. */
    struct code_block *mcdisplay_code; /* Code for drawing components. */
  };
  
/* ADD: E. Farhi Sep 24th, 2001 Component group instances */
struct group_inst
  {
    char *name;
    int  index;
  };

/* Component instance. */
struct comp_inst
  {
    char *name;			/* Instance name. */
    struct comp_def *def;	/* Pointer to definition. */
    struct comp_position *pos;	/* Component position (place & orientation). */
    struct code_block *extend; /* ADD: E. Farhi Sep 20th, 2001 code following comp instance */ 
    int    index;  /* ADD: E. Farhi Sep 20th, 2001 index of comp instance */
    struct group_inst *group;       /* ADD: E. Farhi Sep 24th, 2001 group name in which comp is */
    Symtab defpar, setpar;	/* Parameter values. */
  };

/* Instrument formal parameters. */
/* Note: the enum instr_formal_types definition MUST be kept
   synchronized with the one in mcstas-r.h. */
enum instr_formal_types
  {
    instr_type_double, instr_type_int, instr_type_string
  };
struct instr_formal
  {
    enum instr_formal_types type; /* Type (string, int, double) */
    char *id;			  /* Parameter name */
  };

/* NeXus dictionary information. NeXus is supported through the NXDICT
* API, and only if at least one NXDICT or NXDICTFILE declaration is
* present in the instrument. 
*/
struct NXDinfo
  {
    char *nxdfile;		/* NeXus dictionary file, or NULL */
    List nxdentries;		/* struct NXDentry list NXDICT declarations */
    int any;			/* True only if any NXDICT/NXDICTFILE decls. */
  };
/* Information from a NXDICT declaration. */
struct NXDentry
  {
    char *compname;		/* Component name (1st part of NXD alias) */
    char *param;		/* Parameter name (2nd part of NXD alias) */
    CExp spec;			/* NXDict specification string */
  };

/* Instrument definition. */
struct instr_def
  {
    char *name;			/* Instrument name */
    char *source;		/* Name of source file for definition */
    char *quoted_source;	/* File name quoted for inclusion in C */
    struct code_block *decls;	/* Code for declarations */
    struct code_block *inits;	/* Code for initializations */
    struct code_block *finals;	/* Code for simulation end */
    List formals;		/* List of formal parameters */
    Symtab compmap;		/* Map of component names to instances */
    Symtab groupmap;  /* Map of component group names */
    List complist;		/* List of components in declaration order */
    List grouplist;		/* List of component groups in declaration order */
    struct NXDinfo *nxdinfo;	/* NeXus dictionary declarations */
    int use_default_main;	/* If set, output a main() function */
    int include_runtime;	/* If set, include runtime in output */
    int enable_trace;		/* If set, enable output of neutron traces */
    int portable;		/* If set, emit strictly portable ANSI C */
    int polarised;		/* If set, handle neutron polarisation */
  };
