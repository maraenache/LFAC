%{
#include <stdio.h>
#include "y.tab.h"
%}
%option noyywrap 
%%
"int"|"float"|"char"|"string"|"bool" {yylval.str=strdup(yytext); return TIP;}
"ARRAY" {return ARRAY;}
"int_main()" {return BGIN;}
"(" {return PDESCHISA;}
")" {return PINCHISA;}
"{" {return ADESCHISA;}
"}" {return AINCHISA;}
"[" {return BOPEN;}
"if" {return IF;}
"for" {return FOR;}
"while" {return WHILE;}
"else" {return ELSE;}
"variabile_globale" {return START;}
"sectiune_functii" {return START2;}
"user_datatypes" {return START3;}
"const" {return CONST;}
"]" { return BCLOSE;}
"," {return VIRGULA;}
"<" {return MIC;}
">" {return MARE;}
"<=" {return MICEGAL;}
">=" {return MAREEGAL;}
"==" {return VERIFIC;}
"&&" {return AND;}
"||" {return OR;}
"!=" {return DIFERIT;}
"!" {return NOT;}
";" {return PV;}
"FALSE" {yylval.num=0; return BOOL;}
"TRUE" {yylval.num=1; return BOOL;}
"SUM" {return SUM;}
"DIF" {return DIF;}
"class" {return CLASS;}
"&Eval" {return EVAL;}
"&TypeOf" {return TYPEOF;}
"MUL" {return MUL;}
"private:" {return PRIVATE;}
"public:" {return PUBLIC;}
"DIV" {return DIV;}
"MOD" {return MOD;}
"return_0" {return END;}
#[_a-zA-Z][_a-zA-Z0-9]* {yylval.str=strdup(yytext); return ID_OBIECT;}
@[_a-zA-Z][_a-zA-Z0-9]* {yylval.str=strdup(yytext); return ID;}
$[_a-zA-Z][_a-zA-Z0-9]* {yylval.str=strdup(yytext); return IDFUNCTIE;}
%[_a-zA-Z][_a-zA-Z0-9]* {yylval.str=strdup(yytext); return IDCLASA;}
['][_a-zA-Z]['] {yylval.car= yytext[1];return CARACTER;}
[0-9]*[.][0-9]+   {yylval.val_float = atof(yytext);return FLOAT;}
["][a-zA-Z0-9 ]*["] {yylval.str=strdup(yytext);return STRING;}
"=" {return ASSIGN;}
[0-9]+ {yylval.num=atoi(yytext); return NR;}
[ \t] ;
\n {yylineno++;}
. {return yytext[0];}

 