/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
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
* Version: 1.3
*
* Bison parser for instrument definition files.
*
*	$Id: instrument.y,v 1.46 2003-01-21 08:47:01 pkwi Exp $
*
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
  char *number;
  char *string;
  struct code_block *ccode;	/* User-supplied C code block */
  CExp exp;			/* Expression datatype (for arguments) */
  int linenum;			/* Starting line number for code block */
  Coords_exp coords;		/* Coordinates for location or rotation */
  List formals;			/* List of formal parameters */
  List iformals;		/* List of formal instrument parameters */
  List comp_iformals;		/* List of formal comp. input parameters */
  struct instr_formal *iformal;	/* Single formal instrument parameter */
  struct comp_iformal *cformal;	/* Single formal component input parameter */
  Symtab actuals;		/* Values for formal parameters */
  char **polform;		/* Polarisation state formal parameter */
  struct {List def, set, out, state;
	  char **polarisation;} parms;	/* Parameter lists */
  struct instr_def *instrument;	/* Instrument definition */
  struct comp_inst *instance;	/* Component instance */
  struct comp_place place;	/* Component place */
  struct comp_orientation ori;	/* Component orientation */
  struct NXDinfo *nxdinfo;	/* Info for NeXus dictionary interface */
  struct group_inst *groupinst;
}

%token TOK_RESTRICTED TOK_GENERAL

%token TOK_ABSOLUTE	  "ABSOLUTE"
%token TOK_AT		      "AT"
%token TOK_COMPONENT	"COMPONENT"
%token TOK_DECLARE	  "DECLARE"
%token TOK_DEFINE	    "DEFINE"
%token TOK_DEFINITION	"DEFINITION"
%token TOK_END		    "END"
%token TOK_FINALLY	  "FINALLY"
%token TOK_EXTERN	    "EXTERN"
%token TOK_INITIALIZE	"INITIALIZE"
%token TOK_INSTRUMENT	"INSTRUMENT"
%token TOK_MCDISPLAY	"MCDISPLAY"
%token TOK_OUTPUT	    "OUTPUT"
%token TOK_PARAMETERS	"PARAMETERS"
%token TOK_POLARISATION	"POLARISATION"
%token TOK_RELATIVE	  "RELATIVE"
%token TOK_ROTATED	  "ROTATED"
%token TOK_SETTING	  "SETTING"
%token TOK_STATE	    "STATE"
%token TOK_TRACE	    "TRACE"
%token TOK_SHARE	    "SHARE"   /* ADD: E. Farhi Sep 20th, 2001 shared code (shared declare) */
%token TOK_EXTEND	    "EXTEND"  /* ADD: E. Farhi Sep 20th, 2001 extend code */
%token TOK_GROUP	    "GROUP"   /* ADD: E. Farhi Sep 24th, 2001 component is part of an exclusive group */
%token TOK_SAVE       "SAVE"    /* ADD: E. Farhi Aug 25th, 2002 data save code */
%token TOK_NEXUS     	"NEXUS"   /* ADD: E. Farhi Aug 6th, 2002 will use NeXus files for saving */
%token TOK_DICTFILE	  "DICTFILE"  /* ADD: E. Farhi Aug 6th, 2002 Name of NeXus dictionary file */
%token TOK_HDF   	    "HDF"     /* ADD: E. Farhi Aug 6th, 2002 HDF version number (4,5) */

/*******************************************************************************
* Declarations of terminals and nonterminals.
*******************************************************************************/

%token <string> TOK_ID		/* Note: returns new str_dup()'ed copy each time. */
%token <string> TOK_STRING
%token <number> TOK_NUMBER
%token <string> TOK_CTOK
%token <linenum> TOK_CODE_START
%token TOK_CODE_END
%token <string> TOK_CODE_LINE
%token TOK_INVALID

%type <instance> component compref reference   /* MOD: E. Farhi Sep 24th, 2001 add group */
%type <groupinst> groupdef groupref   /* ADD: E. Farhi Sep 24th, 2001 add group */
%type <ccode>   code codeblock share declare initialize trace extend save finally mcdisplay /* MOD: E. Farhi Sep 20th, 2001, add 'shared' and 'extend'+'save' 25/8/02 */
%type <coords>  coords
%type <exp>     exp topexp topatexp genexp genatexp
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
%type <nxdinfo> nexus
%type <string>  dictfile
%type <number>  hdfversion
%%

main:		  TOK_GENERAL compdefs instrument
		| TOK_RESTRICTED compdef
;

compdefs:	  /* empty */
		| compdefs compdef
;

compdef:	  "DEFINE" "COMPONENT" TOK_ID parameters share declare initialize trace save finally mcdisplay "END"
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
        if (verbose) fprintf(stderr, "Embedding component %s\n", c->name);
		  }
;

parameters:	  def_par set_par out_par state_par polarisation_par
		  {
		    $$.def = $1;
		    $$.set = $2;
		    $$.out = $3;
		    $$.state = $4;
		    $$.polarisation = $5;
		  }
;


def_par:	  "DEFINITION" "PARAMETERS" comp_iformallist
		  {
		    $$ = $3;
		  }
;

set_par:	  "SETTING" "PARAMETERS" comp_iformallist
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

comp_iformals:	  /* empty */
		  {
		    $$ = list_create();
		  }
		| comp_iformals1
		  {
		    $$ = $1;
		  }
;

comp_iformals1:	  comp_iformal
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

comp_iformal:	 TOK_ID TOK_ID
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
				  "parameter %s.\n", $1, $2);
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
				  "parameter %s.\n", $1, $3);
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
		| TOK_ID '=' TOK_NUMBER
		  {
		    struct comp_iformal *formal;
		    palloc(formal);
		    formal->id = $1;
		    formal->isoptional = 1; /* Default value available */
		    formal->default_value = exp_number($3);
        formal->type = instr_type_double;
		    str_free($3);
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
				  "parameter %s.\n", $1, $2);
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
				  "parameter %s.\n", $1, $3);
		      formal->type = instr_type_double;
		    }
		    $$ = formal;
      }        
;

instrument:	  "DEFINE" "INSTRUMENT" TOK_ID instrpar_list
			{ instrument_definition->formals = $4; instrument_definition->name = $3; }
		  declare initialize nexus instr_trace save finally "END"
		  {
		    instrument_definition->decls = $6;
		    instrument_definition->inits = $7;
		    instrument_definition->nxdinfo = $8;
        instrument_definition->saves  = $10;
		    instrument_definition->finals = $11;
		    instrument_definition->compmap = comp_instances;
        instrument_definition->groupmap = group_instances;  /* ADD: E. Farhi Sep 25th, 2001 */
		    instrument_definition->complist = comp_instances_list;
        instrument_definition->grouplist = group_instances_list;  /* ADD: E. Farhi Sep 25th, 2001 */

		    /* Check instrument parameters for uniqueness */
		    check_instrument_formals(instrument_definition->formals,
					     instrument_definition->name);
        if (verbose) fprintf(stderr, "Creating instrument %s\n", instrument_definition->name);
		  }
;

/* The instrument parameters may be either double (for numeric input),
   int (for integer-only), or char * (for strings). */
instrpar_list:	  '(' instr_formals ')'
		  {
		    $$ = $2;
		  }
;

instr_formals:	  /* empty */
		  {
		    $$ = list_create();
		  }
		| instr_formals1
		  {
		    $$ = $1;
		  }
;

instr_formals1:	  instr_formal
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

instr_formal:	  TOK_ID TOK_ID
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
		      print_error("Illegal type for instrument "
				  "parameter %s.\n", $2);
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
		      print_error("Illegal type for instrument "
				  "parameter %s.\n", $3);
		      formal->type = instr_type_double;
		    }
		    formal->id = $3;
		    $$ = formal;
		  }
		| TOK_ID	/* Default type is "double" */
		  {
		    struct instr_formal *formal;
		    palloc(formal);
		    formal->type = instr_type_double;
		    formal->id = $1;
		    $$ = formal;
		  }

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

/* ADD: E. Farhi Sep 20th, 2001 SHARE component block included once */
share:	  /* empty */
		  {
		    $$ = codeblock_new();
		  }
		| "SHARE" codeblock
		  {
		    $$ = $2;
		  }
;


nexus:		  /* empty: no NeXus support */
		  {
		    struct NXDinfo *nxdinfo;
		    palloc(nxdinfo);
		    nxdinfo->nxdfile = NULL;
		    nxdinfo->any = 0;
		    $$ = nxdinfo;
		  }
		| nexus "NEXUS" dictfile hdfversion
		  { /* ADD: E.Farhi Aug 6th 2002: use default NeXus dictionary file */
		    struct NXDinfo *nxdinfo = $1;
		    if(nxdinfo->nxdfile)
		    {
		      print_error("Multiple NeXus DICTFILE declarations found (%s).\n"
				  "At most one NeXus DICTFILE declarations may "
				  "be used in an instrument", nxdinfo->nxdfile);
		    }
		    else 
        {
          nxdinfo->nxdfile     = $3;
          nxdinfo->hdfversion = atof($4);
        }
		    nxdinfo->any = 1; /* Now need NeXus support in runtime */
		    $$ = nxdinfo;
		  }
;
dictfile: /* empty: default dictionary */
		  {
		    $$ = str_cat(instrument_definition->name, ".dic", NULL);
		  }
    | dictfile "DICTFILE" TOK_STRING
      {
        $$ = $3
      }
;
hdfversion: /* empty: default HDF version */
		  {
		    $$ = "4";
		  }
    | hdfversion "HDF" TOK_NUMBER
      {
        $$ = $3;
      }
;

trace:		  "TRACE" codeblock
		  {
		    $$ = $2;
		  }
;

save:	  /* empty */
		  {
		    $$ = codeblock_new();
		  }
		| "SAVE" codeblock
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

complist:	  /* empty */
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
				  "'%s'.\n", $2->name);
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

component:	  "COMPONENT" TOK_ID '=' TOK_ID actuallist place orientation groupref extend
		  {
		    struct comp_def *def;
		    struct comp_inst *comp;
        
        def = read_component($4);
        if (def != NULL) def->comp_inst_number--;
		    palloc(comp); /* Allocate new instance. */
		    comp->name = $2;
		    comp->def = def;
		    palloc(comp->pos);
        comp->group = $8;           /* ADD: E. Farhi Sep 24th, 2001 component is part of an exclusive group */
        comp->extend = $9;  /* ADD: E. Farhi Sep 20th, 2001 EXTEND block*/
        comp->index = 0;       /* ADD: E. Farhi Sep 20th, 2001 index of comp instance */
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

/* ADD: E. Farhi Sep 24th, 2001 component is part of an exclusive group */
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

/* ADD: E. Farhi Sep 20th, 2001 EXTEND block executed after component instance */
extend:	  /* empty */
		  {
		    $$ = codeblock_new();
		  }
		| "EXTEND" codeblock
		  {
		    $$ = $2;
		  }
;

/* C expressions used to give component actual parameters.
   Top-level comma (',') operator NOT allowed. */
exp:		  { $<linenum>$ = instr_current_line; } topexp
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

topexp:		  topatexp
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
topatexp:	  TOK_ID
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
genexp:		  genatexp
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

genatexp:	  topatexp
		  {
		    $$ = $1;
		  }
		| ','
		  {
		    $$ = exp_ctoken(",");
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
	  "[--no-main] [--no-runtime] file\n");
  exit(1);
}

/* Print McStas version and copyright. */
static void
print_version(void)
{ /* MOD: E. Farhi Sep 20th, 2001 version number */
  printf("McStas version " MCSTAS_VERSION "\n"
	  "Copyright (C) Risoe National Laboratory, 1997-2002\n"
    "Additions (C) Institut Laue Langevin, 2002\n"
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

#ifdef MAC
  argc = ccommand(&argv);
#endif

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
        print_error("Unassigned definition parameter %s for component %s.\n",
		          formal->id, comp->name);
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
        static int seenb4 = 0;	/* Only print long error the first time */
        print_error("Illegal expression for DEFINITION parameter %s of component %s.\n%s",
          formal->id, comp->name,
          ( seenb4++ ? "" :
	        "(Only variable names, constant numbers, and constant strings\n"
	        "are allowed for DEFINITION parameters.)\n") );
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
        print_error("Unassigned setting parameter %s for component %s.\n",
		          formal->id, comp->name);
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
      print_error("Unmatched actual parameter %s for component %s.\n",
		  entry->name, comp->name);
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
