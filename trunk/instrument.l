/* In Emacs, please make this -*-c-*- mode. Thanks. */

/* Flex scanner for instrument definition files. */


/* Definition section. */

%{
#include <math.h>

#include "instrument.tab.h"
%}


DIGIT		[0-9]
ALPHA		[A-ZÆØÅa-zæøå]
ALPHANUM	{ALPHA}|{DIGIT}|"_"

ID		{DIGIT}*{ALPHA}{ALPHANUM}*
NUMBER		{DIGIT}+("."{DIGIT}+)?([Ee][+-]?{DIGIT}+)?

%%

ABSOLUTE	return TOK_ABSOLUTE;
AT		return TOK_AT;
COMPONENT	return TOK_COMPONENT;
DEFINE		return TOK_DEFINE;
END		return TOK_END;
INSTRUMENT	return TOK_INSTRUMENT;
RELATIVE	return TOK_RELATIVE;
ROTATED		return TOK_ROTATED;

"("|")"|","	return yytext[0]; /* Punctuation. */
"="		return yytext[0]; /* Operator. */

{ID}		yylval.string = yytext; return TOK_ID;
{NUMBER}	yylval.number = atof(yytext); return TOK_NUMBER;

[ \t\n]		/* Ignore whitespace. */

.               fprintf(stderr, "Invalid character `%s'.\n", yytext); return TOK_INVALID;



%%

/* User code section. */


