/* In Emacs, please make this -*-c-*- mode. Thanks. */

/*******************************************************************************
* Flex scanner for instrument definition files.
*
*	Project: Monte Carlo Simulation of Tripple Axis Spectrometers
*	File name: instrument.l
*
*	Author: K.N.			Jul  1, 1997
*
*	$Id: instrument.l,v 1.3 1997-07-02 07:27:32 kn Exp $
*
*	$Log: not supported by cvs2svn $
*	Revision 1.2  1997/07/01 08:26:21  kn
*	Fixed problem when scanning identifiers: now returns a persistent copy
*	of the name.
*
*
* Copyright (C) Risoe National Laboratory, 1991-1997, All rights reserved
*******************************************************************************/


/* Definition section. */

/* Do not use the `yywrap feature' - only scan a single file (see Flex manual). */
%option noyywrap


%{
#include <string.h>
#include <math.h>

#include "mcstas.h"
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

{ID}		yylval.string = str_dup(yytext); return TOK_ID;
{NUMBER}	yylval.number = atof(yytext); return TOK_NUMBER;

[ \t\n]		/* Ignore whitespace. */

.               print_error("Invalid character `%s'.\n", yytext); return TOK_INVALID;



%%

/* User code section. */


