/*******************************************************************************
* Bison parser for instrument definition files.
*
*	Project: Monte Carlo Simulation of Tripple Axis Spectrometers
*	File name: instrument.y
*
*	Author: K.N.			Jul  1, 1997
*
*	$Id: instrument.y,v 1.3 1997-07-02 07:28:19 kn Exp $
*
*	$Log: not supported by cvs2svn $
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
%}


%union {
  double number;
  char *string;
}


%token TOK_ABSOLUTE	"ABSOLUTE"
%token TOK_AT		"AT"
%token TOK_COMPONENT	"COMPONENT"
%token TOK_DEFINE	"DEFINE"
%token TOK_END		"END"
%token TOK_INSTRUMENT	"INSTRUMENT"
%token TOK_RELATIVE	"RELATIVE"
%token TOK_ROTATED	"ROTATED"

%token <string> TOK_ID		/* Note: returns new malloc()'ed copy each time. */
%token <number> TOK_NUMBER
%token TOK_INVALID

%%


instrument:	  "DEFINE" "INSTRUMENT" TOK_ID formallist complist "END"

;


complist:	  /* empty */

		| complist compdef

;


compdef:	  "COMPONENT" TOK_ID '=' comp
			{
			  debugn((DEBUG_HIGH, "Component: %s.\n", $2));
			  str_free($2);
			}
;


comp:		  TOK_ID actuallist position { }

;


formallist:	  '(' formals ')'

;


formals:	  /* empty */

		| formals1

;


formals1:	  formal  { }

		| formals1 ',' formal

;


formal:		  TOK_ID  { }

;


actuallist:	  '(' actuals ')'

;


actuals:	  /* empty */

		| actuals1

;


actuals1:	  actual

		| actuals1 ',' actual

;


actual:		  TOK_ID '=' exp  { }

;


position:	  place orientation

;


place:		  /* empty */

		| "AT" coords reference

;


orientation:	  /* empty */

		| "ROTATED" coords reference

;


reference:	  "ABSOLUTE"

		| "RELATIVE" compref

;


compref:	  TOK_ID  { }

;


coords:		  '(' exp ',' exp ',' exp ')'

;


exp:		  TOK_ID  { }

		| TOK_NUMBER  { }

;




%%


int
main(int argc, char *argv[])
{
  if(argc == 2)
  {
    yyparse();
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
  print_error("%s\n", s);
}
