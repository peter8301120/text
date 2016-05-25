digit [0-9]
letter [A-Za-z]
id ({letter}({letter}|{digit})*)
digits ({digit}+)
optional_faction (.{digits})?
optional_exp ([Ee^][+-]?{digits})?
real_number ({digits}{optional_faction}{optional_exp})
 /* 字串 由"開頭和結尾 中間可以包含任何不是"的字，或是""的closure */
string (\"([^\"](\"\")?)*\")


%s COMMENT
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pro2.tab.h"
#define MAX_LINE_LENG 256
#define LIST strcat(buf,yytext)
#define token(t) {printf("<%s>\n",t);}
 /* #define token(t) {LIST; printf("<%s>\n",t);} */
#define tokenInteger(t,i) {LIST; printf("<%s:%d>\n",t,i);}
#define tokenFloat(t,f) {LIST; printf("<%s:%f>\n",t,f);}
#define tokenString(t,s) {LIST; printf("<%s:%s>\n",t,s);}
 /* stmbol table size */
#define tablesize  100
 /* 判斷字串中的"是否為偶數的" */
int ifd = 0;
 /* linenum從1開始數 */
int linenum = 1;
char buf[MAX_LINE_LENG];



%}

%%
 /* comma */
<INITIAL>"," {LIST;token("','");return ',';}
 /* colon */
<INITIAL>":" {LIST;token("':'");return ':';}
 /* semicolon */
<INITIAL>";" {LIST;token("';'");return ';';}
 /* parentheses */
<INITIAL>"(" {LIST;token("'('");return '(';}
<INITIAL>")" {LIST;token("')'");return ')';}
 /* square brackets */
<INITIAL>"[" {LIST;token("'['");return '[';}
<INITIAL>"]" {LIST;token("']'");return ']';}
 /* brackets */
<INITIAL>"{" {LIST;token("'{'");return '{';}
<INITIAL>"}" {LIST;token("'}'");return '}';}
 /* arithmetic */
<INITIAL>"+" {LIST;token("'+'");return '+';}
<INITIAL>"-" {LIST;token("'-'");return '-';}
<INITIAL>"*" {LIST;token("'*'");return '*';}
<INITIAL>"/" {LIST;token("'/'");return '/';}
 /* exponentiation */
<INITIAL>"^" {LIST;token("'^'");return '^';}
 /* remainder */
<INITIAL>"%" {LIST;token("'%'");return '%';}
 /* relational */
<INITIAL>"<" {LIST;token("'<'");return '<';}
<INITIAL>"<=" {LIST;token("'<='");return LTE;}
<INITIAL>">=" {LIST;token("'>='");return GTE;}
<INITIAL>">" {LIST;token("'>'");return '>';}
<INITIAL>"==" {LIST;token("'=='");return EQ;}
<INITIAL>"~=" {LIST;token("'~='");return NEQ;}
 /* logical */
<INITIAL>"and" {LIST;token("AND");return AND;}
<INITIAL>"or" {LIST;token("OR");return OR;}
<INITIAL>"not" {LIST;token("NOT");return NOT;}
 /* assignment */
<INITIAL>"=" {LIST;token("'='");return ASSIGN;}
 /* keywords */
<INITIAL>"boolean" {LIST;token("BOOLEAN");return BOOLEAN;}
<INITIAL>"const" {LIST;token("CONST");return CONST;}
<INITIAL>"do" {LIST;token("DO");return DO;}
<INITIAL>"else" {LIST;token("ELSE");return ELSE;}
<INITIAL>"end" {LIST;token("END");return END;}
 /* boolean */
<INITIAL>"false"    {
					LIST;
					token("BOOLEAN_VAL");
					/* 把值傳給yacc */
				    yylval.s_val = (char *)malloc(sizeof(char) * (strlen(yytext)+1));			
				    strcpy(yylval.s_val,yytext);
					return BOOLEAN_VAL;
					}
<INITIAL>"for" {LIST;token("FOR");return FOR;}
<INITIAL>"function" {LIST;token("FUNCTION");return FUNCTION;}
<INITIAL>"if" {LIST;token("IF");return IF;}
<INITIAL>"import" {LIST;token("IMPORT");return IMPORT;}
<INITIAL>"in" {LIST;token("IN");return IN;}
<INITIAL>"integer" {LIST;token("INTEGER");return INTEGER;}
<INITIAL>"local" {LIST;token("LOCAL");return LOCAL;}
<INITIAL>"nil" {LIST;token("NIL");return NIL;}
<INITIAL>"print" {LIST;token("PRINT");return PRINT;}
<INITIAL>"println" {LIST;token("PRINTLN");return PRINTLN;}
<INITIAL>"real" {LIST;token("REAL");return REAL;}
<INITIAL>"repeat" {LIST;token("REPEAT");return REPEAT;}
<INITIAL>"return" {LIST;token("RETURN");return RETURN;}
<INITIAL>"string" {LIST;token("STRING");return STRING;}
<INITIAL>"read" {LIST;token("READ");return READ;}
<INITIAL>"then" {LIST;token("THEN");return THEN;}
 /* boolean */
<INITIAL>"true"	{
				LIST;
				token("BOOLEAN_VAL");
				/* 把值傳給yacc */
				yylval.s_val = (char *)malloc(sizeof(char) * (strlen(yytext)+1));			
				strcpy(yylval.s_val,yytext);
				return BOOLEAN_VAL;
				}
<INITIAL>"until" {LIST;token("UNTIL");return UNTIL;}
<INITIAL>"while" {LIST;token("WHILE");return WHILE;}
<INITIAL>"void" {LIST;token("VOID");return VOID;}
 /* id 遇到id 放進symbol table 中 */
<INITIAL>{id}	{  
					yylval.s_val = (char *)malloc(sizeof(char) * (strlen(yytext)+1));			
				    strcpy(yylval.s_val,yytext);
					insertID(yytext,nowtable);					
					tokenString("identifier",yytext);
					return ID;
				}
 /* number and real number */
<INITIAL>{digits}	{
					tokenInteger("INTEGER_VAL",atoi(yytext));
					/* 把值傳給yacc */
					yylval.i_val = atoi(yytext) ;
					return INTEGER_VAL;
					}
<INITIAL>{real_number}	{
						tokenFloat("REAL_VAL",atof(yytext));
						/* 把值傳給yacc */
						yylval.d_val = atof(yytext);
						return REAL_VAL;
						}
 /* string  */
<INITIAL>{string}	{
						int i = 0;char s[80] = {};int j = 0;
						/* i是來找目前yytext上的第i個char j則是目前s中的位置 */
						for (i ; i<=yyleng-3;i++){
						    /* (yytext+1) = yytext去掉開頭的" */
						    if ((yytext+1)[i] == '"')
							{
								/* 如果是第一個" 就存進s裡 */
								if(ifd == 0)
								{
									s[j] = (yytext+1)[i];
									j++;
									ifd = 1;
								}
								/* 如果是第二個 就丟掉 */
								else
								{
									ifd = 0;
								}
							}
							else
							{
								s[j] = (yytext+1)[i];
								j++;
							}
						}
						tokenString("STRING_VAL",s);
						/* 把值傳給yacc */
						yylval.s_val = (char *)malloc(sizeof(char) * (strlen(s)+1));			
				        strcpy(yylval.s_val,s);
						return STRING_VAL;
					}
 /* newline and tab */
<INITIAL>\n {LIST;printf("%d: %s",linenum,buf);linenum++;buf[0] = '\0';}
<INITIAL>[ \t]* {LIST;}
 /* 遇到//的註解，把內容印出來，linenum+1，buf清空 */
<INITIAL>("/""/"[^\n]*\n) {LIST;printf("%d: %s",linenum,buf);buf[0] = '\0';linenum++;}
 /* 遇到/*進入註解模式，並印出所在linenum */
<INITIAL>"/*" {BEGIN COMMENT;LIST;}
 /* 註解模式中，即使遇到//也只是存進buf裡 */
<COMMENT>"/""/"   {LIST;}
 /* 註解模式中，遇到newline存進buf且印出linenum,內容 buf清空 */
<COMMENT>\n   {LIST;printf("%d: %s",linenum,buf);linenum++;buf[0] = '\0';}
 /* 註解模式中，遇到newline和(* /)以外的就存進buf裡 */
<COMMENT>[^[*/\n]]* {LIST;}
<COMMENT>[*]* {LIST;}
 /* 註解模式中，遇到(* /)存進buf裡，並且回到初始模式 */
<COMMENT>"*/" {LIST;BEGIN INITIAL;}

%%
 
int yywrap() {  
	return 1;
}
