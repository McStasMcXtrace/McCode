/*******************************************************************************
* Bison parser for instrument definition files.
*
*	Project: Monte Carlo Simulation of Tripple Axis Spectrometers
*	File name: instrument.y
*
*	Author: K.N.			Jul  1, 1997
*
*	$Id: instrument.y,v 1.10 1998-09-24 11:18:27 kn Exp $
*
*	$Log: not supported by cvs2svn $
*	Revision 1.9  1998/09/23 13:50:47  kn
*	Allow multiple component definitions in the file (before the instrument
*	definition).
*	Make the use of EXTERN optional.
*
*	Revision 1.8  1998/08/26 12:43:49  kn
*	Merged in the functionality from component.y.
*
*	Revision 1.7  1998/08/21 12:08:18  kn
*	Added `-o' command line option.
*	Output generated C simulation code in file rather than on stdout.
*
*	Revision 1.6  1997/09/07 20:16:08  kn
*	Added FINALLY construct.
*
*	Revision 1.5  1997/09/07 17:57:54  kn
*	Snapshot with (untested) code generation complete.
*
*	Revision 1.4  1997/08/13 09:14:59  kn
*	First version to properly parse instrument definition files.
*
*	Revision 1.3  1997/07/02 07:28:19  kn
*	Misc. cleanup.
*
*	Revision 1.2  1997/07/01 08:27:19  kn
*	Fixed problem when scanning identifiers: lexer now returns a persistent
*	copy of the name.
*
*
* Copyright (C) Risoe National Laboratory, 1991-1997, All rights reserved
*******************************************************************************/


%{
#include <math.h>
#include <string.h>
#include <stdio.h>

#include "mcstas.h"

#define YYERROR_VERBOSE 1
#define YYDEBUG 1

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
  struct {List def, set, state;} parms;	/* Parameter lists. */
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
%token <number> TOK_NUMBER
%token <linenum> TOK_CODE_START
%token TOK_CODE_END
%token <string> TOK_CODE_LINE
%token TOK_INVALID

%type <instance> component compref reference
%type <ccode> code codeblock declare initialize trace finally
%type <coords>  coords
%type <exp> exp
%type <actuals> actuallist actuals actuals1
%type <formals> formallist formals formals1 def_par set_par state_par
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

compdef:	  "DEFINE" "COMPONENT" TOK_ID parameters declare initialize trace finally "END"
		  {
		    struct comp_def *c;
		    palloc(c);
		    c->name = $3;
		    c->def_par = $4.def;
		    c->set_par = $4.set;
		    c->state_par = $4.state;
		    c->decl_code = $5;
		    c->init_code = $6;
		    c->trace_code = $7;
		    c->finally_code = $8;

		    /* Put component definition in table. */
		    symtab_add(read_components, c->name, c);
		  }
;

parameters:	  def_par set_par state_par
		  {
		    $$.def = $1;
		    $$.set = $2;
		    $$.state = $3;
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

state_par:	  "STATE" "PARAMETERS" formallist
		  {
		    $$ = $3;
		  }
;

instrument:	  "DEFINE" "INSTRUMENT" TOK_ID formallist
			{ instrument_definition->formals = $4; }
		  declare initialize complist finally "END"
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

complist:	  /* empty */
		  {
		    comp_instances = symtab_create();
		    comp_instances_list = list_create();
		  }
		| complist component
		  {
		    symtab_add(comp_instances, $2->name, $2);
		    list_add(comp_instances_list, $2);
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
;

codeblock:	  TOK_CODE_START code TOK_CODE_END
		  {
		    $2->filename = instr_current_filename;
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
  fprintf(stderr, "Usage: mcstas [-o output file] instrument-file\n");
  exit(1);
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


/* Parse command line options. */
static void
parse_command_line(int argc, char *argv[])
{
  int i;

  output_filename = NULL;
  instr_current_filename = NULL;
  for(i = 1; i < argc; i++)
  {
    if(argv[i][0] == '-')
    {
      switch(argv[i][1])
      {
	case 'o':
	  if(argv[i][2])
	    output_filename = str_dup(&argv[i][2]);
	  else if(i + 1 < argc)
	    output_filename = str_dup(argv[++i]);
	  else
	    print_usage();
	  break;
	default:
	  print_usage();
      }
    }
    else
    {
      if(instr_current_filename != NULL)
	print_usage();		/* Multiple instruments given. */
      instr_current_filename = str_dup(argv[i]);
    }
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

  parse_command_line(argc, argv);
  if(!strcmp(instr_current_filename, "-"))
    file = fdopen(0, "r");	/* Lone '-' designates stdin. */
  else
    file = fopen(instr_current_filename, "r");
  if(file == NULL)
    fatal_error("Instrument definition file `%s' not found\n",
		instr_current_filename);
  instr_current_line = 1;
  lex_new_file(file);
  read_components = symtab_create(); /* Create table of components. */
  palloc(instrument_definition); /* Allocate instrument def. structure. */
  err = yyparse();
  fclose(file);
  if(err != 0)
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
    char *filename;
    FILE *file;
    int err;
    
    /* Attempt to read definition from file components/<name>.com. */
    filename = str_cat("components/", name, ".com", NULL);
    file = fopen(filename, "r");
    if(file == NULL)
    {
      print_error("Cannot find file `%s' "
		  "while looking for definition of component `%s'.\n",
		  filename, name);
      return NULL;
    }
    push_autoload(file);
    /* Note: the str_dup copy of the file name is stored in codeblocks, and
       must not be freed. */
    instr_current_filename = str_dup(filename);
    instr_current_line = 1;
    err = yyparse();		/* Read definition from file. */
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
