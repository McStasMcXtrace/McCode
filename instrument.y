/*******************************************************************************
* Bison parser for instrument definition files.
*
*	Project: Monte Carlo Simulation of Tripple Axis Spectrometers
*	File name: instrument.y
*
*	Author: K.N.			Jul  1, 1997
*
*	$Id: instrument.y,v 1.5 1997-09-07 17:57:54 kn Exp $
*
*	$Log: not supported by cvs2svn $
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
#include <stdio.h>

#include "mcstas.h"

#define YYERROR_VERBOSE 1
#define YYDEBUG 1

%}

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
  struct instr_def *instrument;	/* Instrument definition. */
  struct comp_inst *instance;	/* Component instance. */
  struct comp_place place;	/* Component place. */
  struct comp_orientation ori;	/* Component orientation. */
}


%token TOK_ABSOLUTE	"ABSOLUTE"
%token TOK_AT		"AT"
%token TOK_COMPONENT	"COMPONENT"
%token TOK_DECLARE	"DECLARE"
%token TOK_DEFINE	"DEFINE"
%token TOK_END		"END"
%token TOK_EXTERN	"EXTERN"
%token TOK_INITIALIZE	"INITIALIZE"
%token TOK_INSTRUMENT	"INSTRUMENT"
%token TOK_RELATIVE	"RELATIVE"
%token TOK_ROTATED	"ROTATED"

/*******************************************************************************
* Declarations of terminals and nonterminals.
*******************************************************************************/

%token <string> TOK_ID		/* Note: returns new str_dup()'ed copy each time. */
%token <number> TOK_NUMBER
%token <linenum> TOK_CODE_START
%token TOK_CODE_END
%token <string> TOK_CODE_LINE
%token TOK_INVALID

%type <instance> compdef compref reference
%type <ccode> code codeblock initialize declare
%type <coords>  coords
%type <exp> exp
%type <actuals> actuallist actuals actuals1
%type <formals> formallist, formals, formals1
%type <place> place
%type <ori> orientation
%%


instrument:	  "DEFINE" "INSTRUMENT" TOK_ID formallist declare initialize complist "END"
		  {
		    struct instr_def *instr;

		    palloc(instr);
		    instr->name = $3;
		    instr->formals = $4;
		    instr->decls = $5;
		    instr->inits = $6;
		    instr->compmap = comp_instances;
		    instr->complist = comp_instances_list;
		    /* The result from parsing is stored in a global pointer
                       instrument_definition. */
		    instrument_definition = instr;
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

complist:	  /* empty */
		  {
		    comp_instances = symtab_create();
		    comp_instances_list = list_create();
		  }
		| complist compdef
		  {
		    symtab_add(comp_instances, $2->name, $2);
		    list_add(comp_instances_list, $2);
		  }
;

compdef:	  "COMPONENT" TOK_ID '=' TOK_ID actuallist place orientation
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
		    comp->pos->orientation_rel = $7.orientation_rel;
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

place:		  /* empty */
		  {
		    $$.place = coords_exp_origo(); /* Default to (0,0,0). */
		    $$.place_rel = NULL;	   /* Not relative to instance. */
		  }
		| "AT" coords reference
		  {
		    $$.place = $2;
		    $$.place_rel = $3;
		  }
;

orientation:	  /* empty */
		  {
		    $$.orientation = coords_exp_origo(); /* Default to (0,0,0). */
		    $$.orientation_rel = NULL;		 /* Not relative. */
		  }
		| "ROTATED" coords reference
		  {
		    $$.orientation = $2;
		    $$.orientation_rel = $3;
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
		    $$ = exp_id($1);
		    str_free($1);
		  }
		| TOK_NUMBER
		  {
		    $$ = exp_number($1);
		  }
		| "EXTERN" TOK_ID
		  {
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


int
main(int argc, char *argv[])
{
  FILE *file;
  int err;
  
  yydebug=0;			/* If 1, then bison gives verbose parser debug info. */

  if(argc == 2)
  {
    file = fopen(argv[1], "r");
    if(file == NULL)
      fatal_error("Instrument definition file `%s' not found.\n", argv[1]);
    instr_current_filename = str_dup(argv[1]);
    instr_current_line = 1;
    yyrestart(file);
    err = yyparse();
    fclose(file);
    if(err != 0)
      print_error("Errors encountered during parse.\n");
    else
      cogen(instrument_definition);
  }
  else
  {
    print_error("Usage: %s file.\n", argv[0]);
  }

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
