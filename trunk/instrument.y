/*******************************************************************************
* Bison parser for instrument definition files.
*
*	Project: Monte Carlo Simulation of Tripple Axis Spectrometers
*	File name: instrument.y
*
*	Author: K.N.			Jul  1, 1997
*
*	$Id: instrument.y,v 1.4 1997-08-13 09:14:59 kn Exp $
*
*	$Log: not supported by cvs2svn $
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
  List ccode;			/* User-supplied C code block. */
  CExp exp;			/* Expression datatype (for arguments). */
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
%token TOK_CODE_START
%token TOK_CODE_END
%token <string> TOK_CODE_LINE
%token TOK_INVALID

%type <instrument> instrument define
%type <instance> comp compdef compref reference
%type <ccode> code codeblock initialize declare
%type <coords>  coords
%type <exp> exp
%type <actuals> actuallist actuals actuals1
%type <formals> formallist, formals, formals1
%type <place> place
%type <ori> orientation
%%


instrument:	  declare initialize define
		  {
		    /* The result from parsing is stored in a global pointer
                       instrument_definition. */
		    instrument_definition = $3;
		    instrument_definition->decls = $1;
		    instrument_definition->inits = $2;
		  }
;

declare:	  /* empty */
		  {
		    $$ = list_create();
		  }
		| "DECLARE" codeblock
		  {
		    $$ = $2;
		  }
;

initialize:	  /* empty */
		  {
		    $$ = list_create();
		  }
		| "INITIALIZE" codeblock
		  {
		    $$ = $2;
		  }
;

define:		  "DEFINE" "INSTRUMENT" TOK_ID formallist complist "END"
		  {
		    struct instr_def *instr;

		    palloc(instr);
		    instr->name = $3;
		    instr->formals = $4;
		    instr->components = comp_instances;
		    /* instr->decls and instr->inits will be set later. */
		    $$ = instr;
		  }
;


complist:	  /* empty */
		  {
		    comp_instances = symtab_create();
		  }
		| complist compdef
		  {
		    symtab_add(comp_instances, $2->name, $2);
		  }
;

compdef:	  "COMPONENT" TOK_ID '=' comp
		  {
		    $4 -> name = $2;
		    $$ = $4;
		    debugn((DEBUG_HIGH, "Component: %s.\n", $2));
		  }
;

comp:		  TOK_ID actuallist place orientation
		  {
		    struct comp_def *def;
		    struct comp_inst *comp;

		    def = read_component($1);
		    palloc(comp); /* Allocate new instance. */
		    comp->def = def;
		    palloc(comp->pos);
		    comp->pos->place = $3.place;
		    comp->pos->place_rel = $3.place_rel;
		    comp->pos->orientation = $4.orientation;
		    comp->pos->orientation_rel = $4.orientation_rel;
		    if(def != NULL)
		    {
		      /* Check actual parameters against definition and
                         setting parameters. */
		    }
		    str_free($1);
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
		    $$ = $2;
		  }
;

code:		  /* empty */
		  {
		    $$ = list_create();
		  }

		| code TOK_CODE_LINE
		  {
		    list_add($1, $2);
		    $$ = $1;
		  }
;


%%

/* Number of the line currently being parsed. */
int instr_current_line = 0;

/* Result from parsing instrument definition. */
struct instr_def *instrument_definition;

/* Map from names to component instances. */
Symtab comp_instances;


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
    yyrestart(file);
    err = yyparse();
    fclose(file);
    if(err != 0)
      print_error("Errors encountered during parse.\n");
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
