/*******************************************************************************
* Bison parser for instrument definition files.
*
*	Project: Monte Carlo Simulation of Triple Axis Spectrometers
*	File name: instrument.y
*
*	Author: K.N.			Jul  1, 1997
*
* Copyright (C) Risoe National Laboratory, 1997-1998, All rights reserved
*******************************************************************************/


%{
#include <math.h>
#include <string.h>
#include <stdio.h>

#include "mcstas.h"

#define YYERROR_VERBOSE 1
#define YYDEBUG 1

/* When a bison parser needs to extend the parser stack, by default it uses
* the alloca() function. This causes portability problems (eg. for Win32 and
* HPUX). To avoid that, we use our own method for extending the stack. This
* is a bit tricky and reliant on bison internals, but important for portability. 
*/

#define yyoverflow mc_yyoverflow
int mc_yyoverflow();
%}

/* Need a pure parser to allow for recursive calls when autoloading component
   definitions. */
%pure_parser

/*******************************************************************************
* Type definition for semantic values.
*******************************************************************************/

%union {
  double number;
  char *string;
  struct code_block *ccode;	/* User-supplied C code block. */
  CExp exp;			/* Expression datatype (for arguments). */
  int linenum;			/* Starting line number for code block. */
  Coords_exp coords;		/* Coordinates for location or rotation. */
  List formals;			/* List of formal parameters. */
  Symtab actuals;		/* Values for formal parameters. */
  struct {List def, set, out, state;} parms;	/* Parameter lists. */
  struct instr_def *instrument;	/* Instrument definition. */
  struct comp_inst *instance;	/* Component instance. */
  struct comp_place place;	/* Component place. */
  struct comp_orientation ori;	/* Component orientation. */
}

%token TOK_RESTRICTED TOK_GENERAL

%token TOK_ABSOLUTE	"ABSOLUTE"
%token TOK_AT		"AT"
%token TOK_COMPONENT	"COMPONENT"
%token TOK_DECLARE	"DECLARE"
%token TOK_DEFINE	"DEFINE"
%token TOK_DEFINITION	"DEFINITION"
%token TOK_END		"END"
%token TOK_FINALLY	"FINALLY"
%token TOK_EXTERN	"EXTERN"
%token TOK_INITIALIZE	"INITIALIZE"
%token TOK_INSTRUMENT	"INSTRUMENT"
%token TOK_MCDISPLAY	"MCDISPLAY"
%token TOK_OUTPUT	"OUTPUT"
%token TOK_PARAMETERS	"PARAMETERS"
%token TOK_RELATIVE	"RELATIVE"
%token TOK_ROTATED	"ROTATED"
%token TOK_SETTING	"SETTING"
%token TOK_STATE	"STATE"
%token TOK_TRACE	"TRACE"

/*******************************************************************************
* Declarations of terminals and nonterminals.
*******************************************************************************/

%token <string> TOK_ID		/* Note: returns new str_dup()'ed copy each time. */
%token <string> TOK_STRING
%token <number> TOK_NUMBER
%token <linenum> TOK_CODE_START
%token TOK_CODE_END
%token <string> TOK_CODE_LINE
%token TOK_INVALID

%type <instance> component compref reference
%type <ccode> code codeblock declare initialize trace finally mcdisplay
%type <coords>  coords
%type <exp> exp
%type <actuals> actuallist actuals actuals1
%type <formals> formallist formals formals1 def_par set_par out_par state_par
%type <parms> parameters
%type <place> place
%type <ori> orientation
%%

main:		  TOK_GENERAL compdefs instrument
		| TOK_RESTRICTED compdef
;

compdefs:	  /* empty */
		| compdefs compdef
;

compdef:	  "DEFINE" "COMPONENT" TOK_ID parameters declare initialize trace finally mcdisplay "END"
		  {
		    struct comp_def *c;
		    palloc(c);
		    c->name = $3;
		    c->def_par = $4.def;
		    c->set_par = $4.set;
		    c->out_par = $4.out;
		    c->state_par = $4.state;
		    c->decl_code = $5;
		    c->init_code = $6;
		    c->trace_code = $7;
		    c->finally_code = $8;
		    c->mcdisplay_code = $9;

		    /* Put component definition in table. */
		    symtab_add(read_components, c->name, c);
		  }
;

parameters:	  def_par set_par out_par state_par
		  {
		    $$.def = $1;
		    $$.set = $2;
		    $$.out = $3;
		    $$.state = $4;
		  }
;


def_par:	  "DEFINITION" "PARAMETERS" formallist
		  {
		    $$ = $3;
		  }
;

set_par:	  "SETTING" "PARAMETERS" formallist
		  {
		    $$ = $3;
		  }
;

out_par:	  /* empty */
		  {
		    $$ = list_create();
		  }
		| "OUTPUT" "PARAMETERS" formallist
		  {
		    $$ = $3;
		  }
;

state_par:	  "STATE" "PARAMETERS" formallist
		  {
		    $$ = $3;
		  }
;

instrument:	  "DEFINE" "INSTRUMENT" TOK_ID formallist
			{ instrument_definition->formals = $4; }
		  declare initialize instr_trace finally "END"
		  {
		    instrument_definition->name = $3;
		    instrument_definition->decls = $6;
		    instrument_definition->inits = $7;
		    instrument_definition->finals = $9;
		    instrument_definition->compmap = comp_instances;
		    instrument_definition->complist = comp_instances_list;
		  }
;

declare:	  /* empty */
		  {
		    $$ = codeblock_new();
		  }
		| "DECLARE" codeblock
		  {
		    $$ = $2;
		  }
;

initialize:	  /* empty */
		  {
		    $$ = codeblock_new();
		  }
		| "INITIALIZE" codeblock
		  {
		    $$ = $2;
		  }
;

trace:		  "TRACE" codeblock
		  {
		    $$ = $2;
		  }
;

finally:	  /* empty */
		  {
		    $$ = codeblock_new();
		  }
		| "FINALLY" codeblock
		  {
		    $$ = $2;
		  }
;

mcdisplay:	  /* empty */
		  {
		    $$ = codeblock_new();
		  }
		| "MCDISPLAY" codeblock
		  {
		    $$ = $2;
		  }
;

instr_trace:	  "TRACE" complist
		  {
		    instrument_definition->rotations_in_radians = 0;
		  }
		| complist
		  {
		    /* Backward compatibility mode, rotations in radians. */
		    static did_warn = 0;
		    print_warn(&did_warn,
	"Missing TRACE keyword in instrument definition. This is only\n"
	"supported for backwards compatibility, and should not be used.\n"
	"NOTE: All rotation angles will be interpreted as radians!\n");
		    instrument_definition->rotations_in_radians = 1;
		  }

complist:	  /* empty */
		  {
		    comp_instances = symtab_create();
		    comp_instances_list = list_create();
		  }
		| complist component
		  {
		    /* Check that the component instance name has not
                       been used before. */
		    if(symtab_lookup(comp_instances, $2->name))
		    {
		      print_error("Multiple use of component instance name "
				  "'%s'.\n", $2->name);
		      /* Since this is an error condition, we do not
		         worry about freeing the memory allocated for
			 the component instance. */
		    }
		    else
		    {
		      symtab_add(comp_instances, $2->name, $2);
		      list_add(comp_instances_list, $2);
		    }
		  }
;

component:	  "COMPONENT" TOK_ID '=' TOK_ID actuallist place orientation
		  {
		    struct comp_def *def;
		    struct comp_inst *comp;

		    def = read_component($4);
		    palloc(comp); /* Allocate new instance. */
		    comp->name = $2;
		    comp->def = def;
		    palloc(comp->pos);
		    comp->pos->place = $6.place;
		    comp->pos->place_rel = $6.place_rel;
		    comp->pos->orientation = $7.orientation;
		    comp->pos->orientation_rel =
		      $7.isdefault ? $6.place_rel : $7.orientation_rel;
		    if(def != NULL)
		    {
		      /* Check actual parameters against definition and
                         setting parameters. */
		      comp_formals_actuals(comp, $5);
		    }
		    str_free($4);
		    debugn((DEBUG_HIGH, "Component: %s.\n", $2));
		    $$ = comp;
		  }
;

formallist:	  '(' formals ')'
		  {
		    $$ = $2;
		  }
;


formals:	  /* empty */
		  {
		    $$ = list_create();
		  }
		| formals1
		  {
		    $$ = $1;
		  }
;

formals1:	  TOK_ID
		  {
		    $$ = list_create();
		    list_add($$, $1);
		  }
		| formals1 ',' TOK_ID
		  {
		    list_add($1, $3);
		    $$ = $1;
		  }
;

actuallist:	  '(' actuals ')'
		  {
		    $$ = $2;
		  }
;

actuals:	  /* empty */
		  {
		    $$ = symtab_create();
		  }
		| actuals1
		  {
		    $$ = $1;
		  }
;

actuals1:	  TOK_ID '=' exp
		  {
		    $$ = symtab_create();
		    symtab_add($$, $1, $3);
		    str_free($1);
		  }
		| actuals1 ',' TOK_ID '=' exp
		  {
		    symtab_add($1, $3, $5);
		    str_free($3);
		    $$ = $1;
		  }
;

place:		  "AT" coords reference
		  {
		    $$.place = $2;
		    $$.place_rel = $3;
		  }
;

orientation:	  /* empty */
		  {
		    $$.orientation = coords_exp_origo(); /* Default to (0,0,0). */
		    $$.isdefault = 1; /* No ROTATED modifier was present */
		  }
		| "ROTATED" coords reference
		  {
		    $$.orientation = $2;
		    $$.orientation_rel = $3;
		    $$.isdefault = 0;
		  }
;


reference:	  "ABSOLUTE"
		  {
		    $$ = NULL;
		  }
		| "RELATIVE" compref
		  {
		    $$ = $2;
		  }
;


compref:	  TOK_ID
		  {
		    struct comp_inst *comp;
		    struct Symtab_entry *ent;

		    ent = symtab_lookup(comp_instances, $1);
		    comp = NULL;
		    if(ent == NULL)
		      print_error("Reference to undefined component %s at line %d.\n",
				  $1, instr_current_line);
		    else
		      comp = ent->val;
		    str_free($1);
		    $$ = comp;
		  }
;

coords:		  '(' exp ',' exp ',' exp ')'
		  {
		    $$.x = $2;
		    $$.y = $4;
		    $$.z = $6;
		  }
;

exp:		  TOK_ID
		  {
		    List_handle liter;
		    char *formal;
		    /* Check if this is an instrument parameter or not. */
		    /* ToDo: This will be inefficient if the number of
                       instrument parameters is really huge. */
		    liter = list_iterate(instrument_definition->formals);
		    while(formal = list_next(liter))
		    {
		      if(!strcasecmp($1, formal))
		      {
			/* It was an instrument parameter */
			$$ = exp_id($1);
			goto found;
		      }
		    }
		    /* It was an external id. */
		    $$ = exp_extern_id($1);
		  found:
		    str_free($1);
		  }
		| TOK_NUMBER
		  {
		    $$ = exp_number($1);
		  }
		| "EXTERN" TOK_ID
		  {
		    /* Note: "EXTERN" is now obsolete and redundant. */
		    $$ = exp_extern_id($2);
		    str_free($2);
		  }
		| TOK_STRING
		  {
		    $$ = exp_string($1);
		    str_free($1);
		  }
;

codeblock:	  TOK_CODE_START code TOK_CODE_END
		  {
		    $2->filename = instr_current_filename;
		    $2->quoted_filename = str_quote(instr_current_filename);
		    $2->linenum = $1;
		    $$ = $2;
		  }
;

code:		  /* empty */
		  {
		    $$ = codeblock_new();
		  }

		| code TOK_CODE_LINE
		  {
		    list_add($1->lines, $2);
		    $$ = $1;
		  }
;


%%

/* Use own method for extending the parser stack, to remove bisons references
* to alloca(). This must appear in the .y file to pick up the right #defines.
*/
static Pool parser_pool = NULL;	/* Pool of parser allocations. */

int
mc_yyoverflow(char *msg,
	   short **ssp, int sssz,
	   YYSTYPE **vsp, int vssz,
#ifdef YYLSP_NEEDED
	   YYLTYPE **lsp, int lssz,
#endif
	   int *yystacksize
	   )
{
  short *nssp;
  YYSTYPE *nvsp;
#ifdef YYLSP_NEEDED
  YYLTYPE *nlsp;
#endif

  if(*yystacksize >= YYMAXDEPTH)
    fatal_error("%s\n", msg);
  *yystacksize *= 2;
  if(*yystacksize >= YYMAXDEPTH)
    *yystacksize = YYMAXDEPTH;

  nssp = pool_mem(parser_pool, *yystacksize*sizeof(*nssp));
  memcpy(nssp, *ssp, sssz);
  *ssp = nssp;
  nvsp = pool_mem(parser_pool, *yystacksize*sizeof(*nvsp));
  memcpy(nvsp, *vsp, vssz);
  *vsp = nvsp;
#ifdef YYLSP_NEEDED
  nlsp = pool_mem(parser_pool, *yystacksize*sizeof(*nlsp));
  memcpy(nlsp, *lsp, lssz);
  *lsp = nlsp;
#endif
  return 0;
}

static int mc_yyparse(void)
{
  int ret;
  Pool oldpool;

  oldpool = parser_pool;
  parser_pool = pool_create();
  ret = yyparse();
  pool_free(parser_pool);
  parser_pool = oldpool;
  return ret;
}
    
/* Name of the file currently being parsed. */
char *instr_current_filename = NULL;
/* Number of the line currently being parsed. */
int instr_current_line = 0;

/* Result from parsing instrument definition. */
struct instr_def *instrument_definition;

/* Map from names to component instances. */
Symtab comp_instances;

/* List of components, in the order they where declared in the instrument
   definition. */
List comp_instances_list;

/* Filename for outputting generated simulation program ('-' means stdin). */
static char *output_filename;

/* Map of already-read components. */
Symtab read_components = NULL;


/* Print a summary of the command usage and exit with error. */
static void
print_usage(void)
{
  fprintf(stderr, "Usage:\n"
	  "  mcstas [-o file] [-I dir1 ...] [-t] [-v] "
	  "[--no-main] [--no-runtime] file\n");
  exit(1);
}

/* Print McStas version and copyright. */
static void
print_version(void)
{
  printf("McStas version 1.03 ALPHA, March 1999\n"
	  "Copyright (C) Risoe National Laboratory, 1997-1999\n"
	  "All rights reserved\n");
  exit(0);
}

/* Construct default filename for simulation output from instrument file
   name. Strip any leading directory path and trailing .instr, and add .c to
   the end. */
static char *
make_output_filename(char *name)
{
  char *p;
  int l;

  /* Find basename */
  p = strrchr(name, '/');
  if(p == NULL)
    p = name;			/* No initial path. */
  else
    p++;			/* Point past last '/' character. */

  /* Check for trailing .instr suffix. */
  l = strlen(p);
  if(l > 6 && !strcmp(&p[l - 6], ".instr"))
  {
    char *tmp = str_dup(p);
    tmp[l - 6] = '\0';
    p = str_cat(tmp, ".c", NULL);
    str_free(tmp);
  }
  else
    p = str_cat(p, ".c", NULL);
  return p;
}


static void
set_output_filename(char *name)
{
  output_filename = str_dup(name);
}

/* Parse command line options. */
static void
parse_command_line(int argc, char *argv[])
{
  int i;

  output_filename = NULL;
  instr_current_filename = NULL;
  instrument_definition->use_default_main = 1;
  instrument_definition->include_runtime = 1;
  instrument_definition->enable_trace = 0;
  for(i = 1; i < argc; i++)
  {
    if(!strcmp("-o", argv[i]) && (i + 1) < argc)
      set_output_filename(argv[++i]);
    else if(!strncmp("-o", argv[i], 2))
      set_output_filename(&argv[i][2]);
    else if(!strcmp("--output-file", argv[i]) && (i + 1) < argc)
      set_output_filename(argv[++i]);
    else if(!strncmp("--output-file=", argv[i], 14))
      set_output_filename(&argv[i][14]);
    else if(!strcmp("-I", argv[i]) && (i + 1) < argc)
      add_search_dir(argv[++i]);
    else if(!strncmp("-I", argv[i], 2))
      add_search_dir(&argv[i][2]);
    else if(!strcmp("--search-dir", argv[i]) && (i + 1) < argc)
      add_search_dir(argv[++i]);
    else if(!strncmp("--search-dir=", argv[i], 13))
      add_search_dir(&argv[i][13]);
    else if(!strcmp("-t", argv[i]))
      instrument_definition->enable_trace = 1;
    else if(!strcmp("--trace", argv[i]))
      instrument_definition->enable_trace = 1;
    else if(!strcmp("-v", argv[i]))
      print_version();
    else if(!strcmp("--version", argv[i]))
      print_version();
    else if(!strcmp("--no-main", argv[i]))
      instrument_definition->use_default_main = 0;
    else if(!strcmp("--no-runtime", argv[i]))
      instrument_definition->include_runtime = 0;
    else if(argv[i][0] != '-')
    {
      if(instr_current_filename != NULL)
	print_usage();		/* Multiple instruments given. */
      instr_current_filename = str_dup(argv[i]);
    }
    else
      print_usage();
  }

  /* Instrument filename must be given. */
  if(instr_current_filename == NULL)
    print_usage();
  /* If no '-o' option was given for INSTR.instr, default to INSTR.c  */
  if(output_filename == NULL)
    output_filename = make_output_filename(instr_current_filename);
}


int
main(int argc, char *argv[])
{
  FILE *file;
  int err;
  
  yydebug = 0;			/* If 1, then bison gives verbose parser debug info. */

  palloc(instrument_definition); /* Allocate instrument def. structure. */
  parse_command_line(argc, argv);
  if(!strcmp(instr_current_filename, "-"))
  {
    instrument_definition->source = str_dup("<stdin>");
    file = fdopen(0, "r");	/* Lone '-' designates stdin. */
  }
  else
  {
    instrument_definition->source = str_dup(instr_current_filename);
    file = fopen(instr_current_filename, "r");
  }
  if(file == NULL)
    fatal_error("Instrument definition file `%s' not found\n",
		instr_current_filename);
  instr_current_line = 1;
  lex_new_file(file);
  read_components = symtab_create(); /* Create table of components. */
  err = mc_yyparse();
  fclose(file);
  if(err != 0 || error_encountered != 0)
    print_error("Errors encountered during parse.\n");
  else
    cogen(output_filename, instrument_definition);

  exit(0);
}


int
yyerror(char *s)
{
  print_error("%s at line %d.\n", s, instr_current_line);
}


/*******************************************************************************
* Check the actual parameters to a component against the formal parameters.
*******************************************************************************/
void
comp_formals_actuals(struct comp_inst *comp, Symtab actuals)
{
  int error = 0;		/* Set to 1 in case of error. */
  List_handle liter;
  char *formal;
  struct Symtab_entry *entry;
  Symtab defpar, setpar;
  Symtab_handle siter;
  
  /* We need to check
     1. That all actual parameters correspond to formal parameters.
     2. That all formal parameters are assigned catual parameters. */

  /* First check the formal parameters one by one. */
  defpar = symtab_create();
  setpar = symtab_create();
  liter = list_iterate(comp->def->def_par);
  while(formal = list_next(liter))
  {
    entry = symtab_lookup(actuals, formal);
    if(entry == NULL)
    {
      print_error("Unassigned definition parameter %s for component %s.\n",
		  formal, comp->name);
      symtab_add(defpar, formal, exp_number(0));
      error = 1;
    } else {
      symtab_add(defpar, formal, entry->val);
    }
  }
  list_iterate_end(liter);
  liter = list_iterate(comp->def->set_par);
  while(formal = list_next(liter))
  {
    entry = symtab_lookup(actuals, formal);
    if(entry == NULL)
    {
      print_error("Unassigned setting parameter %s for component %s.\n",
		  formal, comp->name);
      symtab_add(setpar, formal, exp_number(0));
      error = 1;
    } else {
      symtab_add(setpar, formal, entry->val);
    }
  }
  list_iterate_end(liter);

  /* Now check the actual parameters one by one. */
  siter = symtab_iterate(actuals);
  while(entry = symtab_next(siter))
  {
    if(symtab_lookup(defpar, entry->name) == NULL &&
       symtab_lookup(setpar, entry->name) == NULL)
    {
      print_error("Unmatched actual parameter %s for component %s.\n",
		  entry->name, comp->name);
      error = 1;
    }
  }
  comp->defpar = defpar;
  comp->setpar = setpar;
}


/*******************************************************************************
* This is the main entry point for reading a component. When a component
* definition is needed, this function is called with the name of the
* component. A map of previously read components is maintained. If a
* component definition (struct comp)def) is found, it is returned. Otherwise
* an attempt is made to read the component definition from a file with the
* same name as the component with added file extension ".com".
* If for some reasons the component cannot be read, NULL is returned; else a
* pointer to a struct comp_def is returned. Since components definitions can
* be used multiple times, the returned structure is shared and should not be
* modified.
*******************************************************************************/
struct comp_def *
read_component(char *name)
{
  struct Symtab_entry *entry;
  
  /* Look for an existing definition for the component. */
  entry = symtab_lookup(read_components, name);
  if(entry != NULL)
  {
    return entry->val;		/* Return it if found. */
  }
  else
  {
    FILE *file;
    int err;
    
    /* Attempt to read definition from file components/<name>.com. */
    file = open_component_search(name);
    if(file == NULL)
    {
      print_error(
	"Cannot find file containing definition of component `%s'.\n", name);
      return NULL;
    }
    push_autoload(file);
    /* Note: the str_dup copy of the file name is stored in codeblocks, and
       must not be freed. */
    instr_current_filename = component_pathname;
    instr_current_line = 1;
    err = mc_yyparse();		/* Read definition from file. */
    if(err != 0)
      fatal_error("Errors encountered during autoload of component %s.\n",
		  name);
    fclose(file);
    /* Now check if the file contained the required component definition. */
    entry = symtab_lookup(read_components, name);
    if(entry != NULL)
    {
      return entry->val;
    }
    else
    {
      print_error("Definition of component %s not found.\n", name);
      return NULL;
    }
  }
}
