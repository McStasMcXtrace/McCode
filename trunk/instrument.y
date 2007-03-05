/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright (C) 1997-2006, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Kernel: instrument.y
*
* %Identification
* Written by: K.N.
* Date: Jul  1, 1997
* Origin: Risoe
* Release: McStas 1.6
* Version: $Revision: 1.71 $
*
* Bison parser for instrument definition files.
*
* $Id: instrument.y,v 1.71 2007-03-05 19:02:54 farhi Exp $
*
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
  char *number;
  char *string;
  struct code_block *ccode; /* User-supplied C code block */
  CExp exp;     /* Expression datatype (for arguments) */
  int linenum;      /* Starting line number for code block */
  Coords_exp coords;    /* Coordinates for location or rotation */
  List formals;     /* List of formal parameters */
  List iformals;    /* List of formal instrument parameters */
  List comp_iformals;   /* List of formal comp. input parameters */
  struct instr_formal *iformal; /* Single formal instrument parameter */
  struct comp_iformal *cformal; /* Single formal component input parameter */
  Symtab actuals;   /* Values for formal parameters */
  char **polform;   /* Polarisation state formal parameter */
  struct {List def, set, out, state;
    char **polarisation;} parms;  /* Parameter lists */
  struct instr_def *instrument; /* Instrument definition */
  struct comp_inst *instance; /* Component instance */
  struct comp_place place;  /* Component place */
  struct comp_orientation ori;  /* Component orientation */
  struct NXinfo *nxinfo;  /* Info for NeXus interface */
  struct group_inst *groupinst;
  struct jump_struct *jump;
  List   jumps;
  struct jump_condition jumpcondition;
  struct jump_name      jumpname;
}

%token TOK_RESTRICTED TOK_GENERAL

%token TOK_ABSOLUTE   "ABSOLUTE"
%token TOK_AT         "AT"
%token TOK_COMPONENT  "COMPONENT"
%token TOK_DECLARE    "DECLARE"
%token TOK_DEFINE     "DEFINE"
%token TOK_DEFINITION "DEFINITION"
%token TOK_END        "END"
%token TOK_FINALLY    "FINALLY"
%token TOK_EXTERN     "EXTERN"  /* optional */
%token TOK_INITIALIZE "INITIALIZE"
%token TOK_INSTRUMENT "INSTRUMENT"
%token TOK_MCDISPLAY  "MCDISPLAY"
%token TOK_OUTPUT     "OUTPUT"
%token TOK_PARAMETERS "PARAMETERS"
%token TOK_POLARISATION "POLARISATION"
%token TOK_RELATIVE   "RELATIVE"
%token TOK_ROTATED    "ROTATED"
%token TOK_PREVIOUS   "PREVIOUS"
%token TOK_SETTING    "SETTING"
%token TOK_STATE      "STATE"
%token TOK_TRACE      "TRACE"
%token TOK_SHARE      "SHARE"
%token TOK_EXTEND     "EXTEND"
%token TOK_GROUP      "GROUP"   /* extended McStas grammar */
%token TOK_SAVE       "SAVE"
%token TOK_NEXUS      "NEXUS"   /* optional */
%token TOK_JUMP       "JUMP"    /* extended McStas grammar */
%token TOK_WHEN       "WHEN"    /* extended McStas grammar */
%token TOK_NEXT       "NEXT"    /* extended McStas grammar */
%token TOK_ITERATE    "ITERATE" /* extended McStas grammar */
%token TOK_MYSELF     "MYSELF"  /* extended McStas grammar */
%token TOK_COPY       "COPY"    /* extended McStas grammar */

/*******************************************************************************
* Declarations of terminals and nonterminals.
*******************************************************************************/

%token <string> TOK_ID    /* Note: returns new str_dup()'ed copy each time. */
%token <string> TOK_STRING
%token <number> TOK_NUMBER
%token <string> TOK_CTOK
%token <linenum> TOK_CODE_START
%token TOK_CODE_END
%token <string> TOK_CODE_LINE
%token TOK_INVALID

%type <instance> component compref reference instref
%type <groupinst> groupdef groupref
%type <ccode>   code codeblock share declare initialize trace extend save finally mcdisplay
%type <coords>  coords
%type <exp>     exp topexp topatexp genexp genatexp when
%type <actuals> actuallist actuals actuals1
%type <comp_iformals> comp_iformallist comp_iformals comp_iformals1
%type <cformal> comp_iformal
%type <formals> formallist formals formals1 def_par set_par out_par state_par
%type <iformals> instrpar_list instr_formals instr_formals1
%type <iformal> instr_formal
%type <polform> polarisation_par
%type <parms>   parameters
%type <place>   place
%type <ori>     orientation
%type <nxinfo> nexus
%type <string>  instname
%type <number>  hdfversion
%type <jump>    jump
%type <jumps>   jumps jumps1
%type <jumpname> jumpname
%type <jumpcondition> jumpcondition
%%

main:     TOK_GENERAL compdefs instrument
    | TOK_RESTRICTED compdef
;

compdefs:   /* empty */
    | compdefs compdef
;

compdef:    "DEFINE" "COMPONENT" TOK_ID parameters share declare initialize trace save finally mcdisplay "END"
      {
        struct comp_def *c;
        palloc(c);
        c->name = $3;
        /* ADD: E. Farhi, Aug 14th, 2002 */
        c->source = str_quote(instr_current_filename);
        c->def_par = $4.def;
        c->set_par = $4.set;
        c->out_par = $4.out;
        c->state_par = $4.state;
        c->polarisation_par = $4.polarisation;
        c->share_code = $5;  /* ADD: E. Farhi Sep 20th, 2001 */
        c->decl_code = $6;  /* MOD: E. Farhi Sep 20th, 2001, shifted param numbs */
        c->init_code = $7;
        c->trace_code = $8;
        c->save_code = $9;  /* ADD: E. Farhi Aug 25th, 2002+shifted indexes */
        c->finally_code = $10;
        c->mcdisplay_code = $11;
        c->comp_inst_number = 0; /* ADD: E. Farhi Sep 20th, 2001 */

        /* Check definition and setting params for uniqueness */
        check_comp_formals(c->def_par, c->set_par, c->name);
        /* Put component definition in table. */
        symtab_add(read_components, c->name, c);
        /* checks */
        if (!list_len(c->state_par)) print_error("No particule state parameters defined in component "
          "%s at line %s:%d.\n", $3, instr_current_filename, instr_current_line);
        if (verbose) fprintf(stderr, "Embedding component %s from file %s\n", c->name, c->source);
      }
    | "DEFINE" "COMPONENT" TOK_ID "COPY" TOK_ID parameters share declare initialize trace save finally mcdisplay "END"
      {
        /* create a copy of a comp, and initiate it with given blocks */
        /* all redefined blocks override */
        struct comp_def *def;
        def = read_component($5);
        struct comp_def *c;
        palloc(c);
        c->name = $3;
        c->source = str_quote(instr_current_filename);
        /* only catenate if defined as non empty  */
        c->def_par   = list_create(); list_cat(c->def_par, def->def_par);
        if (list_len($6.def)) list_cat(c->def_par,$6.def);

        c->set_par   = list_create(); list_cat(c->set_par, def->set_par);
        if (list_len($6.set)) list_cat(c->set_par,$6.set);

        c->out_par   = list_create(); list_cat(c->out_par, def->out_par);
        if (list_len($6.out)) list_cat(c->out_par,$6.out);

        c->state_par = (list_len($6.state) ? $6.state : def->state_par);
        c->polarisation_par = ($6.polarisation ? $6.polarisation : def->polarisation_par);

        c->share_code = ($7->linenum ?  $7  : def->share_code);
        c->decl_code  = ($8->linenum ?  $8  : def->decl_code);
        c->init_code  = ($9->linenum ?  $9  : def->init_code);
        c->trace_code = ($10->linenum ? $10 : def->trace_code);
        c->save_code  = ($11->linenum ? $11 : def->save_code);
        c->finally_code = ($12->linenum ? $12 : def->finally_code);
        c->mcdisplay_code = ($13->linenum ? $13 : def->mcdisplay_code);
        c->comp_inst_number = 0;

        /* Check definition and setting params for uniqueness */
        check_comp_formals(c->def_par, c->set_par, c->name);
        /* Put component definition in table. */
        symtab_add(read_components, c->name, c);
        /* checks */
        if (!list_len(c->state_par)) print_error("No particule state parameters defined in (copied) component "
          "%s at line %s:%d.\n", $3, instr_current_filename, instr_current_line);
        if (verbose) fprintf(stderr, "Embedding component %s from file %s\n", c->name, c->source);

      }
;

parameters:   def_par set_par out_par state_par polarisation_par
      {
        $$.def = $1;
        $$.set = $2;
        $$.out = $3;
        $$.state = $4;
        $$.polarisation = $5;
      }
;


def_par:    /* empty */
      {
        $$ = list_create();
      }
    | "DEFINITION" "PARAMETERS" comp_iformallist
      {
        $$ = $3;
      }
;

set_par:    /* empty */
      {
        $$ = list_create();
      }
    | "SETTING" "PARAMETERS" comp_iformallist
      {
        $$ = $3;
      }
;

out_par:    /* empty */
      {
        $$ = list_create();
      }
    | "OUTPUT" "PARAMETERS" formallist
      {
        $$ = $3;
      }
;

state_par:    /* empty */
      {
        $$ = list_create();
      }
    | "STATE" "PARAMETERS" formallist
      {
        $$ = $3;
      }
;

polarisation_par: /* empty */
      {
        $$ = NULL;
      }
    | "POLARISATION" "PARAMETERS" '(' TOK_ID ',' TOK_ID ',' TOK_ID ')'
      {
        char **polform;
        nalloc(polform, 3);
        polform[0] = $4;
        polform[1] = $6;
        polform[2] = $8;
        $$ = polform;
      }
;

comp_iformallist: '(' comp_iformals ')'
      {
        $$ = $2;
      }
;

comp_iformals:    /* empty */
      {
        $$ = list_create();
      }
    | comp_iformals1
      {
        $$ = $1;
      }
;

comp_iformals1:   comp_iformal
      {
        $$ = list_create();
        list_add($$, $1);
      }
    | comp_iformals1 ',' comp_iformal
      {
        list_add($1, $3);
        $$ = $1;
      }
;

comp_iformal:  TOK_ID TOK_ID
      {
        struct comp_iformal *formal;
        palloc(formal);
        if(!strcmp($1, "double")) {
          formal->type = instr_type_double;
        } else if(!strcmp($1, "int")) {
          formal->type = instr_type_int;
        } else if(!strcmp($1, "string")) {
          formal->type = instr_type_string;
        } else {
          print_error("Illegal type %s for component "
          "parameter %s at line %s:%d.\n", $1, $2, instr_current_filename, instr_current_line);
          formal->type = instr_type_double;
        }
        formal->id = $2;
        $$ = formal;
      }
    | TOK_ID '*' TOK_ID
      {
        struct comp_iformal *formal;
        palloc(formal);
        if(!strcmp($1, "char")) {
          formal->type = instr_type_string;
        } else {
          print_error("Illegal type %s* for component "
          "parameter %s at line $s:%d.\n", $1, $3, instr_current_filename, instr_current_line);
          formal->type = instr_type_double;
        }
        formal->id = $3;
        $$ = formal;
      }
    | TOK_ID
      {
        struct comp_iformal *formal;
        palloc(formal);
        formal->id = str_dup($1);
        formal->isoptional = 0; /* No default value */
        formal->type = instr_type_double;
        $$ = formal;
      }
    | TOK_ID '=' exp
      {
        struct comp_iformal *formal;
        palloc(formal);
        formal->id = $1;
        formal->isoptional = 1; /* Default value available */
        formal->default_value = $3;
        formal->type = instr_type_double;
        $$ = formal;
      }
    | TOK_ID TOK_ID '=' exp
      {
        struct comp_iformal *formal;
        palloc(formal);
        if(!strcmp($1, "double")) {
          formal->type = instr_type_double;
        } else if(!strcmp($1, "int")) {
          formal->type = instr_type_int;
        } else if(!strcmp($1, "string")) {
          formal->type = instr_type_string;
        } else {
          print_error("Illegal type %s for component "
          "parameter %s at line %s:%d.\n", $1, $2, instr_current_filename, instr_current_line);
          formal->type = instr_type_double;
        }
        formal->id = $2;
        formal->isoptional = 1; /* Default value available */
        formal->default_value = $4;
        $$ = formal;
      }
    | TOK_ID '*' TOK_ID '=' exp
      {
        struct comp_iformal *formal;
        palloc(formal);
        formal->id = $3;
        formal->isoptional = 1; /* Default value available */
        formal->default_value = $5;
        if(!strcmp($1, "char")) {
          formal->type = instr_type_string;
        } else {
          print_error("Illegal type %s* for component "
          "parameter %s at line %s:%d.\n", $1, $3, instr_current_filename, instr_current_line);
          formal->type = instr_type_double;
        }
        $$ = formal;
      }
;

instrument:   "DEFINE" "INSTRUMENT" TOK_ID instrpar_list
      { instrument_definition->formals = $4; instrument_definition->name = $3; }
      declare initialize nexus instr_trace save finally "END"
      {
        instrument_definition->decls = $6;
        instrument_definition->inits = $7;
        instrument_definition->nxinfo = $8;
        instrument_definition->saves  = $10;
        instrument_definition->finals = $11;
        instrument_definition->compmap = comp_instances;
        instrument_definition->groupmap = group_instances;  /* ADD: E. Farhi Sep 25th, 2001 */
        instrument_definition->complist = comp_instances_list;
        instrument_definition->grouplist = group_instances_list;  /* ADD: E. Farhi Sep 25th, 2001 */

        /* Check instrument parameters for uniqueness */
        check_instrument_formals(instrument_definition->formals,
               instrument_definition->name);
        if (verbose) fprintf(stderr, "Creating instrument %s (with %i component instances)\n", instrument_definition->name, comp_current_index);
      }
;

/* The instrument parameters may be either double (for numeric input),
   int (for integer-only), or char * (for strings). */
instrpar_list:    '(' instr_formals ')'
      {
        $$ = $2;
      }
;

instr_formals:    /* empty */
      {
        $$ = list_create();
      }
    | instr_formals1
      {
        $$ = $1;
      }
;

instr_formals1:   instr_formal
      {
        $$ = list_create();
        list_add($$, $1);
      }
    | instr_formals1 ',' instr_formal
      {
        list_add($1, $3);
        $$ = $1;
      }
;

instr_formal:   TOK_ID TOK_ID
      {
        struct instr_formal *formal;
        palloc(formal);
        if(!strcmp($1, "double")) {
          formal->type = instr_type_double;
        } else if(!strcmp($1, "int")) {
          formal->type = instr_type_int;
        } else if(!strcmp($1, "string")) {
          formal->type = instr_type_string;
        } else {
          print_error("Illegal type %s for instrument "
          "parameter %s at line %s:%d.\n", $1, $2, instr_current_filename, instr_current_line);
          formal->type = instr_type_double;
        }
        formal->id = $2;
        $$ = formal;
      }
    | TOK_ID '*' TOK_ID
      {
        struct instr_formal *formal;
        palloc(formal);
        if(!strcmp($1, "char")) {
          formal->type = instr_type_string;
        } else {
          print_error("Illegal type $s* for instrument "
          "parameter %s at line %s:%d.\n", $1, $3, instr_current_filename, instr_current_line);
          formal->type = instr_type_double;
        }
        formal->id = $3;
        $$ = formal;
      }
    | TOK_ID  /* Default type is "double" */
      {
        struct instr_formal *formal;
        palloc(formal);
        formal->type = instr_type_double;
        formal->id = $1;
        formal->isoptional = 0; /* No default value */
        $$ = formal;
      }
    | TOK_ID '=' exp
      {
        struct instr_formal *formal;
        palloc(formal);
        formal->id = $1;
        formal->isoptional = 1; /* Default value available */
        formal->default_value = $3;
        formal->type = instr_type_double;
        $$ = formal;
      }
    | TOK_ID TOK_ID '=' exp
      {
        struct instr_formal *formal;
        palloc(formal);
        if(!strcmp($1, "double")) {
          formal->type = instr_type_double;
        } else if(!strcmp($1, "int")) {
          formal->type = instr_type_int;
        } else if(!strcmp($1, "string")) {
          formal->type = instr_type_string;
        } else {
          print_error("Illegal type %s for instrument "
          "parameter %s at line %s:%d.\n", $1, $2, instr_current_filename, instr_current_line);
          formal->type = instr_type_double;
        }
        formal->id = $2;
        formal->isoptional = 1; /* Default value available */
        formal->default_value = $4;
        $$ = formal;
      }
    | TOK_ID '*' TOK_ID '=' exp
      {
        struct instr_formal *formal;
        palloc(formal);
        formal->id = $3;
        formal->isoptional = 1; /* Default value available */
        formal->default_value = $5;
        if(!strcmp($1, "char")) {
          formal->type = instr_type_string;
        } else {
          print_error("Illegal type %s* for instrument "
          "parameter %s at line %s:%d.\n", $1, $3, instr_current_filename, instr_current_line);
          formal->type = instr_type_double;
        }
        $$ = formal;
      }
;

/* NeXus output support */
nexus:      /* empty: default NeXus support */
      {
        struct NXinfo *nxinfo;
        palloc(nxinfo);
        nxinfo->any = 0;
        nxinfo->hdfversion    = NULL;
        $$ = nxinfo;
      }
    | nexus "NEXUS" hdfversion
      { /* specify NeXus version */
        struct NXinfo *nxinfo = $1;
        nxinfo->hdfversion    = $3;
        nxinfo->any = 1;
        $$ = nxinfo;
      }
;
hdfversion: /* empty: default HDF version */
      {
        $$ = "5";
      }
    | hdfversion TOK_STRING
      {
        $$ = $1;
      }
;


declare:    /* empty */
      {
        $$ = codeblock_new();
      }
    | "DECLARE" codeblock
      {
        $$ = $2;
      }
    | "DECLARE" "COPY" TOK_ID
      {
        struct comp_def *def;
        def = read_component($3);
        $$ = def->decl_code;
      }
    | "DECLARE" "COPY" TOK_ID "EXTEND" codeblock
      {
        struct comp_def   *def;
        struct code_block *cb;
        cb = codeblock_new();
        def = read_component($3);
        cb->filename        = def->decl_code->filename;
        cb->quoted_filename = def->decl_code->quoted_filename;
        cb->linenum         = def->decl_code->linenum;
        list_cat(cb->lines, def->decl_code->lines);
        list_cat(cb->lines, $5->lines);
        $$ = cb;
      }
;

initialize:   /* empty */
      {
        $$ = codeblock_new();
      }
    | "INITIALIZE" "COPY" TOK_ID
      {
        struct comp_def *def;
        def = read_component($3);
        $$ = def->init_code;
      }
    | "INITIALIZE" "COPY" TOK_ID "EXTEND" codeblock
      {
        struct comp_def   *def;
        struct code_block *cb;
        cb = codeblock_new();
        def = read_component($3);
        cb->filename        = def->init_code->filename;
        cb->quoted_filename = def->init_code->quoted_filename;
        cb->linenum         = def->init_code->linenum;
        list_cat(cb->lines, def->init_code->lines);
        list_cat(cb->lines, $5->lines);
        $$ = cb;
      }
    | "INITIALIZE" codeblock
      {
        $$ = $2;
      }
;

/* SHARE component block included once. Toggle comp_inst_number sign from neg to pos in cogen.c */
share:    /* empty */
      {
        $$ = codeblock_new();
      }
    | "SHARE" codeblock
      {
        $$ = $2;
      }
    | "SHARE" "COPY" TOK_ID
      {
        struct comp_def *def;
        def = read_component($3);
        $$ = def->share_code;
      }
    | "SHARE" "COPY" TOK_ID "EXTEND" codeblock
      {
        struct comp_def *def;
        struct code_block *cb;
        cb = codeblock_new();
        def = read_component($3);
        cb->filename        = def->share_code->filename;
        cb->quoted_filename = def->share_code->quoted_filename;
        cb->linenum         = def->share_code->linenum;
        list_cat(cb->lines, def->share_code->lines);
        list_cat(cb->lines, $5->lines);
        $$ = cb;
      }

;


trace: /* empty */
      {
        $$ = codeblock_new();
      }
    | "TRACE" codeblock
      {
        $$ = $2;
      }
    | "TRACE" "COPY" TOK_ID
      {
        struct comp_def *def;
        def = read_component($3);
        $$ = def->trace_code;
      }
    | "TRACE" "COPY" TOK_ID "EXTEND" codeblock
      {
        struct comp_def *def;
        struct code_block *cb;
        cb = codeblock_new();
        def = read_component($3);
        cb->filename        = def->trace_code->filename;
        cb->quoted_filename = def->trace_code->quoted_filename;
        cb->linenum         = def->trace_code->linenum;
        list_cat(cb->lines, def->trace_code->lines);
        list_cat(cb->lines, $5->lines);
        $$ = cb;
      }
;

save:   /* empty */
      {
        $$ = codeblock_new();
      }
    | "SAVE" codeblock
      {
        $$ = $2;
      }
    | "SAVE" "COPY" TOK_ID
      {
        struct comp_def *def;
        def = read_component($3);
        $$ = def->save_code;
      }
    | "SAVE" "COPY" TOK_ID "EXTEND" codeblock
      {
        struct comp_def *def;
        struct code_block *cb;
        cb = codeblock_new();
        def = read_component($3);
        cb->filename        = def->save_code->filename;
        cb->quoted_filename = def->save_code->quoted_filename;
        cb->linenum         = def->save_code->linenum;
        list_cat(cb->lines, def->save_code->lines);
        list_cat(cb->lines, $5->lines);
        $$ = cb;
      }
;

finally:    /* empty */
      {
        $$ = codeblock_new();
      }
    | "FINALLY" codeblock
      {
        $$ = $2;
      }
    | "FINALLY" "COPY" TOK_ID
      {
        struct comp_def *def;
        def = read_component($3);
        $$ = def->finally_code;
      }
    | "FINALLY" "COPY" TOK_ID "EXTEND" codeblock
      {
        struct comp_def *def;
        struct code_block *cb;
        cb = codeblock_new();
        def = read_component($3);
        cb->filename        = def->finally_code->filename;
        cb->quoted_filename = def->finally_code->quoted_filename;
        cb->linenum         = def->finally_code->linenum;
        list_cat(cb->lines, def->finally_code->lines);
        list_cat(cb->lines, $5->lines);
        $$ = cb;
      }
;

mcdisplay:    /* empty */
      {
        $$ = codeblock_new();
      }
    | "MCDISPLAY" codeblock
      {
        $$ = $2;
      }
    | "MCDISPLAY" "COPY" TOK_ID
      {
        struct comp_def *def;
        def = read_component($3);
        $$ = def->mcdisplay_code;
      }
    | "MCDISPLAY" "COPY" TOK_ID "EXTEND" codeblock
      {
        struct comp_def *def;
        struct code_block *cb;
        cb = codeblock_new();
        def = read_component($3);
        cb->filename        = def->mcdisplay_code->filename;
        cb->quoted_filename = def->mcdisplay_code->quoted_filename;
        cb->linenum         = def->mcdisplay_code->linenum;
        list_cat(cb->lines, def->mcdisplay_code->lines);
        list_cat(cb->lines, $5->lines);
        $$ = cb;
      }
;

instr_trace:    "TRACE" complist
;
complist:   /* empty */
      {
        comp_instances      = symtab_create();
        comp_instances_list = list_create();
        group_instances     = symtab_create();
        group_instances_list= list_create();
      }
    | complist component
      {
        /* Check that the component instance name has not
                       been used before. */
        if(symtab_lookup(comp_instances, $2->name))
        {
          print_error("Multiple use of component instance name "
          "'%s' at line %s:%d.\n", $2->name, instr_current_filename, instr_current_line);
          /* Since this is an error condition, we do not
             worry about freeing the memory allocated for
       the component instance. */
        }
        else
        {
          symtab_add(comp_instances, $2->name, $2);
          list_add(comp_instances_list, $2);
          if($2->def)
          {
            /* Check if the component handles polarisation. */
            if($2->def->polarisation_par)
            {
              instrument_definition->polarised = 1;
            }
          }
        }
      }
;

instname: "COPY" '(' TOK_ID ')'
      {
        char str_index[10];
        sprintf(str_index, "_%i", comp_current_index+1);
        $$ = str_cat($3, str_index, NULL);
      }
    | TOK_ID
      {
        $$ = $1;
      }
;

instref: "COPY" '(' compref ')' actuallist /* make a copy of a previous instance, with def+set */
      {
        struct comp_inst *comp_src;
        struct comp_inst *comp;
        comp_src = $3;
        palloc(comp);
        comp->type   = comp_src->type;
        comp->def    = comp_src->def;
        comp->extend = comp_src->extend;
        comp->group  = comp_src->group;
        comp->jump   = comp_src->jump;
        comp->when   = comp_src->when;
        /* now catenate src and actual parameters */
        comp->actuals= symtab_create();
        symtab_cat(comp->actuals, $5);
        symtab_cat(comp->actuals, comp_src->actuals);
        comp_formals_actuals(comp, comp->actuals);
        $$ = comp;
      }
    | "COPY" '(' compref ')'
      {
        struct comp_inst *comp_src;
        struct comp_inst *comp;
        comp_src = $3;
        palloc(comp);
        comp->type   = comp_src->type;
        comp->defpar = comp_src->defpar;
        comp->setpar = comp_src->setpar;
        comp->def    = comp_src->def;
        comp->extend = comp_src->extend;
        comp->group  = comp_src->group;
        comp->jump   = comp_src->jump;
        comp->when   = comp_src->when;
        comp->actuals= comp_src->actuals;
        $$ = comp;
      }
    | TOK_ID actuallist /* define new instance with def+set parameters */
      {
        struct comp_def *def;
        struct comp_inst *comp;
        def = read_component($1);
        if (def != NULL) def->comp_inst_number--;
        palloc(comp);
        comp->type         = $1;
        comp->def          = def;
        comp->extend = codeblock_new();
        comp->group  = NULL;
        comp->jump   = list_create();
        comp->when   = NULL;
        comp->actuals= $2;
        if(def != NULL)
        {
          /* Check actual parameters against definition and
                         setting parameters. */
          comp_formals_actuals(comp, comp->actuals);
        }
        $$ = comp;
      }
;

component:    "COMPONENT" instname '=' instref when place orientation groupref extend jumps
      {

        struct comp_inst *comp;

        comp = $4;
        if (comp->def != NULL) comp->def->comp_inst_number--;

        comp->name = $2;

        if ($5) comp->when  = $5;

        palloc(comp->pos);
        comp->pos->place = $6.place;
        comp->pos->place_rel = $6.place_rel;
        comp->pos->orientation = $7.orientation;
        comp->pos->orientation_rel =
            $7.isdefault ? $6.place_rel : $7.orientation_rel;

        if ($8)            comp->group = $8;   /* component is part of an exclusive group */
        if ($9->linenum)   comp->extend= $9;  /* EXTEND block*/
        if (list_len($10)) comp->jump = $10;
        comp->index = ++comp_current_index;     /* index of comp instance */

        debugn((DEBUG_HIGH, "Component[%i]: %s = %s().\n", comp_current_index, $2, $4->type));
        previous_comp = comp; /* this comp will be 'previous' for the next */
        $$ = comp;

      }
;

formallist:   '(' formals ')'
      {
        $$ = $2;
      }
;


formals:    /* empty */
      {
        $$ = list_create();
      }
    | formals1
      {
        $$ = $1;
      }
;

formals1:   TOK_ID
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

actuallist:   '(' actuals ')'
      {
        $$ = $2;
      }
;

actuals:    /* empty */
      {
        $$ = symtab_create();
      }
    | actuals1
      {
        $$ = $1;
      }
;

actuals1:   TOK_ID '=' exp
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

when: /* empty */
    {
      $$ = NULL;
    }
  | "WHEN" exp
    {
      $$ = $2;
    }
;

place:      "AT" coords reference
      {
        $$.place = $2;
        $$.place_rel = $3;
      }
;

orientation:    /* empty */
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


reference:    "ABSOLUTE"
      {
        $$ = NULL;
      }
    | "RELATIVE" "ABSOLUTE"
      {
        $$ = NULL; /* tolerate this reference error */
      }
    | "RELATIVE" compref
      {
        $$ = $2;
      }
;

/* component is part of an exclusive group */
groupref:  /* empty */
      {
        $$ = NULL;
      }
    | "GROUP" groupdef
      {
        $$ = $2;
      }
;

groupdef:   TOK_ID
      {
        struct group_inst *group;
        struct Symtab_entry *ent;

        ent = symtab_lookup(group_instances, $1);
        if(ent == NULL)
        {
          palloc(group);    /* create new group instance */
          group->name = $1;
          group->index= 0;
          symtab_add(group_instances, $1, group);
          list_add(group_instances_list, group);
        }
        else
          group = ent->val;
        $$ = group;
      }
;

compref: "PREVIOUS"
      {
        if (previous_comp) {
          $$ = previous_comp;
        } else {
          print_warn(NULL, "Found invalid PREVIOUS reference at line %s:%d. Using ABSOLUTE.\n", instr_current_filename, instr_current_line);
          $$ = NULL;
        }
      }
    | "PREVIOUS" '(' TOK_NUMBER ')'
      {
        /* get the $3 previous item in comp_instances */
        struct Symtab_entry *entry;
        int index;
        index = atoi($3);
        entry = symtab_previous(comp_instances, index);
        if (!index || !entry) { /* invalid index reference */
          print_warn(NULL, "Found invalid PREVIOUS(%i) reference at line %s:%d. Using ABSOLUTE.\n", index, instr_current_filename, instr_current_line);
          $$ = NULL;
        } else {
          $$ = entry->val;
        }
      }
    | TOK_ID
      {
        struct comp_inst *comp;
        struct Symtab_entry *ent;

        ent = symtab_lookup(comp_instances, $1);
        comp = NULL;
        if(ent == NULL)
          print_error("Reference to undefined component %s at line %s:%d.\n",
          $1, instr_current_filename, instr_current_line);
        else
          comp = ent->val;
        str_free($1);
        $$ = comp;
      }
;

coords:     '(' exp ',' exp ',' exp ')'
      {
        $$.x = $2;
        $$.y = $4;
        $$.z = $6;
      }
;

/* EXTEND block executed after component instance */
extend:   /* empty */
      {
        $$ = codeblock_new();
      }
    | "EXTEND" codeblock
      {
        $$ = $2;
      }
;

jumps: /* empty */
    {
      $$ = list_create();
    }
  | jumps1
    {
      $$ = $1;
    }
;

jumps1: jump
    {
      $$ = list_create();
      list_add($$, $1);
    }
  | jumps1 jump
    {
      list_add($1, $2);
      $$ = $1;
    }
;

jump: "JUMP" jumpname jumpcondition
    {
      struct jump_struct *jump;
      palloc(jump);
      jump->target      =$2.name;
      jump->target_index=$2.index;
      jump->condition  = $3.condition;
      jump->iterate    = $3.iterate;
      $$=jump;
    }
;

jumpcondition: "WHEN" exp
    {
      $$.condition = $2;
      $$.iterate   = 0;
    }
  | "ITERATE" exp
    {
      $$.condition = $2;
      $$.iterate   = 1;
    }
;

jumpname: "PREVIOUS"
    {
      $$.name  = NULL;
      $$.index = -1;
    }
  | "PREVIOUS" '(' TOK_NUMBER ')'
    {
      $$.name  = NULL;
      $$.index = -atoi($3);
    }
  | "MYSELF"
    {
      $$.name  = NULL;
      $$.index = 0;
    }
  | "NEXT"
    {
      $$.name  = NULL;
      $$.index = +1;
    }
  | "NEXT" '(' TOK_NUMBER ')'
    {
      $$.name  = NULL;
      $$.index = +atoi($3);    }
  | TOK_ID
    {
      $$.name  = str_dup($1);
      $$.index = 0;
    }
;

/* C expressions used to give component actual parameters.
   Top-level comma (',') operator NOT allowed. */
exp:      { $<linenum>$ = instr_current_line; } topexp
      {
        CExp e = $2;
        exp_setlineno(e, $<linenum>1 );
        $$ = e;
      }
    | "EXTERN" { $<linenum>$ = instr_current_line; } TOK_ID
      {
        CExp e;
        /* Note: "EXTERN" is now obsolete and redundant. */
        e = exp_extern_id($3);
        exp_setlineno(e, $<linenum>2 );
        str_free($3);
        $$ = e;
      }
;

topexp:     topatexp
      {
        $$ = $1;
      }
    | topexp topatexp
      {
        $$ = exp_compound(2, $1, $2);
        exp_free($2);
        exp_free($1);
      }
;

/* An atomic top-level C expression: either a parenthesized expression, or a
   single token that is NOT comma (','). */
topatexp:   TOK_ID
      {
        List_handle liter;
        struct instr_formal *formal;
        /* Check if this is an instrument parameter or not. */
        /* ToDo: This will be inefficient if the number of
                       instrument parameters is really huge. */
        liter = list_iterate(instrument_definition->formals);
        while(formal = list_next(liter))
        {
          if(!strcmp($1, formal->id))
          {
            /* It was an instrument parameter */
            $$ = exp_id($1);
            goto found;
          }
        }
        list_iterate_end(liter);
        /* It was an external id. */
        $$ = exp_extern_id($1);
      found:
        str_free($1);
      }
    | TOK_NUMBER
      {
        $$ = exp_number($1);
        str_free($1);
      }
    | TOK_STRING
      {
        $$ = exp_string($1);
        str_free($1);
      }
    | TOK_CTOK
      {
        $$ = exp_ctoken($1);
        str_free($1);
      }
    | '='
      {
        $$ = exp_ctoken("=");
      }
    | '*'
      {
        $$ = exp_ctoken("*");
      }
    | '(' genexp ')'
      {
        CExp p1 = exp_ctoken("(");
        CExp p2 = exp_ctoken(")");
        $$ = exp_compound(3, p1, $2, p2);
        exp_free(p2);
        exp_free(p1);
        exp_free($2);
      }
    | '(' ')'
      {
        CExp p1 = exp_ctoken("(");
        CExp p2 = exp_ctoken(")");
        $$ = exp_compound(2, p1, p2);
        exp_free(p2);
        exp_free(p1);
      }
    | '[' genexp ']'
      {
        CExp p1 = exp_ctoken("[");
        CExp p2 = exp_ctoken("]");
        $$ = exp_compound(3, p1, $2, p2);
        exp_free(p2);
        exp_free(p1);
        exp_free($2);
      }
    | '[' ']'
      {
        CExp p1 = exp_ctoken("[");
        CExp p2 = exp_ctoken("]");
        $$ = exp_compound(2, p1, p2);
        exp_free(p2);
        exp_free(p1);
      }
    | '{' genexp '}'
      {
        CExp p1 = exp_ctoken("{");
        CExp p2 = exp_ctoken("}");
        $$ = exp_compound(3, p1, $2, p2);
        exp_free(p2);
        exp_free(p1);
        exp_free($2);
      }
    | '{' '}'
      {
        CExp p1 = exp_ctoken("{");
        CExp p2 = exp_ctoken("}");
        $$ = exp_compound(2, p1, p2);
        exp_free(p2);
        exp_free(p1);
      }
;

/* Any C expression, including a top-level comma (',') operator. */
genexp:     genatexp
      {
        $$ = $1;
      }
    | genexp genatexp
      {
        $$ = exp_compound(2, $1, $2);
        exp_free($2);
        exp_free($1);
      }
;

genatexp:   topatexp
      {
        $$ = $1;
      }
    | ','
      {
        $$ = exp_ctoken(",");
      }
;

codeblock:    TOK_CODE_START code TOK_CODE_END
      {
        $2->filename = instr_current_filename;
        $2->quoted_filename = str_quote(instr_current_filename);
        $2->linenum = $1;
        $$ = $2;
      }
;

code:     /* empty */
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

static Pool parser_pool = NULL; /* Pool of parser allocations. */

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
/* current instance index */
long comp_current_index=0;

/* Result from parsing instrument definition. */
struct instr_def *instrument_definition;

/* Map from names to component instances. */
Symtab comp_instances;

/* Will store component instance for PREVIOUS reference */
struct comp_inst *previous_comp=NULL;

/* ADD: E. Farhi Sep 24th, 2001 Map from names to component group instances. */
Symtab group_instances;

/* Map from names to embedded libraries */
Symtab lib_instances;

/* List of components, in the order they where declared in the instrument
   definition. */
List comp_instances_list;

/* List of component groups, in the order they where declared in the instrument
   definition. */
List group_instances_list;

/* Filename for outputting generated simulation program ('-' means stdin). */
static char *output_filename;

/* Verbose parsing/code generation */
char verbose;

/* Map of already-read components. */
Symtab read_components = NULL;

/* Print a summary of the command usage and exit with error. */
static void
print_usage(void)
{
  fprintf(stderr, "Usage:\n"
    "  mcstas [-o file] [-I dir1 ...] [-t] [-p] [-v] "
    "[--no-main] [--no-runtime] [--verbose] file\n");
  fprintf(stderr, "      -o FILE --output-file=FILE Place C output in file FILE.\n");
  fprintf(stderr, "      -I DIR  --search-dir=DIR   Append DIR to the component search list. \n");
  fprintf(stderr, "      -t      --trace            Enable 'trace' mode for instrument display.\n");
  fprintf(stderr, "      -v      --version          Prints McStas version.\n");
  fprintf(stderr, "      --no-main                  Do not create main(), for external embedding.\n");
  fprintf(stderr, "      --no-runtime               Do not embed run-time libraries.\n");
  fprintf(stderr, "      --verbose                  Display compilation process steps.\n");
  fprintf(stderr, "  The file will be processed and translated into a C code program.\n");
  fprintf(stderr, "  The default component search list is usually defined by the 'MCSTAS'\n");
  fprintf(stderr, "  environment variable. Use 'mcrun' to both run mcstas and the C compiler.\n");
  fprintf(stderr, "  Use 'mcgui' to run the McStas GUI.\n");
  fprintf(stderr, "  If run-time libraries are not embedded, you will have to pre-compile\n");
  fprintf(stderr, "  them (.c -> .o) before assembling the program.\n");
  fprintf(stderr, "SEE ALSO: mcrun, mcplot, mcdisplay, mcresplot, mcstas2vitess, mcgui\n");
  fprintf(stderr, "DOC:      Please visit " PACKAGE_BUGREPORT "\n");
  exit(1);
}

/* Print McStas version and copyright. */
static void
print_version(void)
{ /* MOD: E. Farhi Sep 20th, 2001 version number */
  printf("McStas version " MCSTAS_VERSION "\n"
    "Copyright (C) Risoe National Laboratory, 1997-2006\n"
    "Additions (C) Institut Laue Langevin, 2003-2006\n"
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
    p = name;     /* No initial path. */
  else
    p++;      /* Point past last '/' character. */

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
  verbose = 0;
  instr_current_filename = NULL;
  instrument_definition->use_default_main = 1;
  instrument_definition->include_runtime = 1;
  instrument_definition->enable_trace = 0;
  instrument_definition->portable = 0;
  instrument_definition->polarised = 0;
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
    else if(!strcmp("-p", argv[i]))
      instrument_definition->portable = 1;
    else if(!strcmp("--portable", argv[i]))
      instrument_definition->portable = 1;
    else if(!strcmp("-v", argv[i]))
      print_version();
    else if(!strcmp("--version", argv[i]))
      print_version();
    else if(!strcmp("--verbose", argv[i]))
      verbose = 1;
    else if(!strcmp("--no-main", argv[i]))
      instrument_definition->use_default_main = 0;
    else if(!strcmp("--no-runtime", argv[i]))
      instrument_definition->include_runtime = 0;
    else if(argv[i][0] != '-')
    {
      if(instr_current_filename != NULL)
        print_usage();    /* Multiple instruments given. */
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

#ifdef MAC
  argc = ccommand(&argv);
#endif

  yydebug = 0;      /* If 1, then bison gives verbose parser debug info. */

  palloc(instrument_definition); /* Allocate instrument def. structure. */
  parse_command_line(argc, argv);
  if(!strcmp(instr_current_filename, "-"))
  {
    instrument_definition->source = str_dup("<stdin>");
    file = fdopen(0, "r");  /* Lone '-' designates stdin. */
  }
  else
  {
    instrument_definition->source = str_dup(instr_current_filename);
    file = fopen(instr_current_filename, "r");
  }
  if(file == NULL)
    fatal_error("Instrument definition file `%s' not found\n",
    instr_current_filename);
  instrument_definition->quoted_source =
    str_quote(instrument_definition->source);
  instr_current_line = 1;
  lex_new_file(file);
  read_components = symtab_create(); /* Create table of components. */
  lib_instances   = symtab_create(); /* Create table of libraries. */
  err = mc_yyparse();
  fclose(file);
  if(err != 0 || error_encountered != 0)
  {
    print_error("Errors encountered during parse.\n");
    exit(1);
  }
  else
  {

    cogen(output_filename, instrument_definition);
    if (verbose) fprintf(stderr, "Generating code     %s\n", output_filename);
    exit(0);
  }
}


int
yyerror(char *s)
{
  print_error("%s at line %d.\n", s, instr_current_line);
  return 0;
}


/*******************************************************************************
* Check that all formal parameters of a component definition are unique.
*******************************************************************************/
void
check_comp_formals(List deflist, List setlist, char *compname)
{
  Symtab formals;
  struct comp_iformal *formal;
  struct Symtab_entry *entry;
  List_handle liter;

  /* We check the uniqueness by adding all the formals to a symbol table with
     a dummy pointer value. Any formal parameter that already appears in the
     symbol table is an error. */
  formals = symtab_create();
  liter = list_iterate(deflist);
  while(formal = list_next(liter))
  {
    entry = symtab_lookup(formals, formal->id);
    if(entry != NULL)
      print_error("Definition parameter name %s is used multiple times "
      "in component %s\n", formal->id, compname);
    else
      symtab_add(formals, formal->id, NULL);
  }
  list_iterate_end(liter);
  liter = list_iterate(setlist);
  while(formal = list_next(liter))
  {
    entry = symtab_lookup(formals, formal->id);
    if(entry != NULL)
      print_error("Setting parameter name %s is used multiple times "
      "in component %s\n", formal->id, compname);
    else
      symtab_add(formals, formal->id, NULL);
  }
  list_iterate_end(liter);
  symtab_free(formals, NULL);
}


/*******************************************************************************
* Check that all formal parameters of an instrument definition are unique.
*******************************************************************************/
void
check_instrument_formals(List formallist, char *instrname)
{
  Symtab formals;
  struct instr_formal *formal;
  struct Symtab_entry *entry;
  List_handle liter;

  /* We check the uniqueness by adding all the formals to a symbol table with
     a dummy pointer value. Any formal parameter that already appears in the
     symbol table is an error. */
  formals = symtab_create();
  liter = list_iterate(formallist);
  while(formal = list_next(liter))
  {
    entry = symtab_lookup(formals, formal->id);
    if(entry != NULL)
      print_error("Instrument parameter name %s is used multiple times "
      "in instrument %s\n", formal->id, instrname);
    else
      symtab_add(formals, formal->id, NULL);
  }
  list_iterate_end(liter);
  symtab_free(formals, NULL);
}

/*******************************************************************************
* Check the actual parameters to a component against the formal parameters.
*******************************************************************************/
void
comp_formals_actuals(struct comp_inst *comp, Symtab actuals)
{
  List_handle liter;
  struct comp_iformal *formal;
  struct Symtab_entry *entry;
  Symtab defpar, setpar;
  Symtab_handle siter;

  /* We need to check
     1. That all actual parameters correspond to formal parameters.
     2. That all formal parameters are assigned actual parameters. */

  /* First check the formal parameters one by one. */
  defpar = symtab_create();
  setpar = symtab_create();
  liter = list_iterate(comp->def->def_par);
  while(formal = list_next(liter))
  {
    entry = symtab_lookup(actuals, formal->id);
    if(entry == NULL)
    {
      if(formal->isoptional)
      {
        /* Use default value for unassigned optional parameter */
        symtab_add(defpar, formal->id, formal->default_value);
      } else {
        print_error("Unassigned DEFINITION parameter %s for component %s at line %s:%d.\n",
              formal->id, comp->type,
              instr_current_filename, instr_current_line);
        symtab_add(defpar, formal->id, exp_number("0.0"));
      }
    } else {
      symtab_add(defpar, formal->id, entry->val);
      /* Ensure that the actual DEFINITION parameters are all values
         (identifiers, constant numbers, and constant strings). This is
         necessary to avoid duplication of computations or side effects in the
         expressions for the actual parameters, since DEFINITION parameters
         are assigned using #define's. */
      if(!exp_isvalue(entry->val))
      {
  print_warn(NULL, "Using DEFINITION parameter of component %s (potential syntax error) at line %s:%d\n"
    "  %s=%s\n",
    comp->type, instr_current_filename, instr_current_line,
    formal->id, exp_tostring(entry->val));
      }
    }
  }
  list_iterate_end(liter);
  liter = list_iterate(comp->def->set_par);
  while(formal = list_next(liter))
  {
    entry = symtab_lookup(actuals, formal->id);
    if(entry == NULL)
    {
      if(formal->isoptional)
      {
        /* Use default value for unassigned optional parameter */
        symtab_add(setpar, formal->id, formal->default_value);
      } else {
        print_error("Unassigned SETTING parameter %s for component %s at line %s:%d.\n",
              formal->id, comp->type,
              instr_current_filename, instr_current_line);
        symtab_add(setpar, formal->id, exp_number("0.0"));
      }
    } else {
      symtab_add(setpar, formal->id, entry->val);
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
      Symtab_handle siter2;
      struct Symtab_entry *entry2;

      fprintf(stderr, "Unmatched actual parameter %s for component %s at line %s:%d.\n",
        entry->name, comp->type,
        instr_current_filename, instr_current_line);
      siter2 = symtab_iterate(defpar);
      fprintf(stderr,"  Definition parameters: ");
      while(entry2 = symtab_next(siter2))
        fprintf(stderr, "%s ", entry2->name);
      symtab_iterate_end(siter2);
      siter2 = symtab_iterate(setpar);
      fprintf(stderr,"\n  Setting parameters: ");
      while(entry2 = symtab_next(siter2))
        fprintf(stderr, "%s ", entry2->name);
      symtab_iterate_end(siter2);
      print_error("\n");
    }
  }
  symtab_iterate_end(siter);
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
    return entry->val;    /* Return it if found. */
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
    err = mc_yyparse();   /* Read definition from file. */
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
