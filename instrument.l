/* In Emacs, please make this -*-c-*- mode. Thanks. */

/*******************************************************************************
* Flex scanner for instrument definition files.
*
*	Project: Monte Carlo Simulation of Tripple Axis Spectrometers
*	File name: instrument.l
*
*	Author: K.N.			Jul  1, 1997
*
*	$Id: instrument.l,v 1.7 1998-03-16 08:35:31 kn Exp $
*
*	$Log: not supported by cvs2svn $
*	Revision 1.6  1997/09/07 20:16:03  kn
*	Added FINALLY construct.
*
*	Revision 1.5  1997/09/07 17:57:40  kn
*	Snapshot with (untested) code generation complete.
*
*	Revision 1.4  1997/08/13 09:14:48  kn
*	First version to properly parse instrument definition files.
*
*	Revision 1.3  1997/07/02 07:27:32  kn
*	Misc. cleanup.
*
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


/* Lexer states. The state ccomment is used for scanning c-style comments. The
   state ccode is used when scanning embedded C code blocks. */
%x ccomment ccode


/* Abbreviations. */
DIGIT		[0-9]
ALPHA		[A-ZÆØÅa-zæøå]
ALPHANUM	{ALPHA}|{DIGIT}|"_"

ID		{DIGIT}*{ALPHA}{ALPHANUM}*
NUMBER		-?{DIGIT}+("."{DIGIT}+)?([Ee][+-]?{DIGIT}+)?

%%

ABSOLUTE	return TOK_ABSOLUTE;
AT		return TOK_AT;
COMPONENT	return TOK_COMPONENT;
DECLARE		return TOK_DECLARE;
DEFINE		return TOK_DEFINE;
END		return TOK_END;
FINALLY		return TOK_FINALLY;
EXTERN		return TOK_EXTERN;
INITIALIZE	return TOK_INITIALIZE;
INSTRUMENT	return TOK_INSTRUMENT;
RELATIVE	return TOK_RELATIVE;
ROTATED		return TOK_ROTATED;

"("|")"|","	return yytext[0]; /* Punctuation. */
"="		return yytext[0]; /* Operator. */

{ID}		yylval.string = str_dup(yytext); return TOK_ID;
{NUMBER}	yylval.number = atof(yytext); return TOK_NUMBER;

 /* Scanning embedded C code. */

"%""{"[\t ]*\n	{
		  yylval.linenum = instr_current_line;
		  instr_current_line++;
		  BEGIN(ccode);
		  return TOK_CODE_START;
		}
"%""{"[^\n]*\n	{
		  instr_current_line++;
		  print_error("%%{ token not on a line by itself.\n");
		  return TOK_INVALID;
		}

<ccode>{
 [\t ]*"%""}"[\t ]*\n	instr_current_line++; BEGIN(INITIAL); return TOK_CODE_END;
 [^\n]*"%""}"[^\n]*\n	{
			  instr_current_line++;
			  BEGIN(INITIAL);
			  print_error("%%} terminator not on a line by itself.\n");
			  return TOK_CODE_END;
			}
 [^\n]*\n		instr_current_line++; yylval.string = str_dup(yytext); return TOK_CODE_LINE;
}

 /* %-style comments - ignore everything to end of line. */
"%"\n		instr_current_line++; /* Ignore comment. */
"%"[^{}\n][^\n]*\n instr_current_line++; /* Ignore comment. */

 /* C-style comments. */
"/*"		BEGIN(ccomment);
<ccomment>{
 [^*\n]*	/* Ignore comment. */
 [^*\n]*\n	instr_current_line++; /* Ignore comment. */
 "*"+[^*/\n]*	/* Ignore comment. */
 "*"+[^*/\n]*\n	instr_current_line++; /* Ignore comment. */
 "*"+"/"	BEGIN(INITIAL);	/* End of comment. */
}

[ \t]+		/* Ignore whitespace. */
[ \t]*\n	instr_current_line++; /* Ignore whitespace. */

.               print_error("Invalid character `%s'.\n", yytext); return TOK_INVALID;



%%

/* User code section. */


