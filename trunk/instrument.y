
%{
#include <math.h>
#include <stdio.h>
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

%token <string> TOK_ID
%token <number> TOK_NUMBER
%token TOK_INVALID

%%


instrument:	  "DEFINE" "INSTRUMENT" TOK_ID formallist complist "END"

;


complist:	  /* empty */

		| complist compdef

;


compdef:	  "COMPONENT" TOK_ID '=' comp

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
    fprintf(stderr, "Usage: %s file.\n", argv[0]);
  }

  exit(0);
}


int
yyerror(char *s)
{
  fprintf(stderr, "%s\n", s);
}
