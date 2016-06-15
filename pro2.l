digit [0-9]
letter [A-Za-z]
id ({letter}({letter}|{digit})*)
digits ({digit}+)
optional_faction (.{digits})?
optional_exp ([Ee^][+-]?{digits})?
real_number ({digits}{optional_faction}{optional_exp})
 /* 字串 由"開頭和結尾 中間可以包含任何不是"的字，或是""的closure */
string (\"([^\"](\"\")?)*\")
errID ({digit}({letter})+)
errSTR (\"([^\"])*(\")([^\"])*\")

%s COMMENT
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "y.tab.h"
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
<INITIAL>"," {LIST;return ',';}
 /* colon */
<INITIAL>":" {LIST;return ':';}
 /* semicolon */
<INITIAL>";" {LIST;return ';';}
 /* parentheses */
<INITIAL>"(" {LIST;return '(';}
<INITIAL>")" {LIST;return ')';}
 /* square brackets */
<INITIAL>"[" {LIST;return '[';}
<INITIAL>"]" {LIST;return ']';}
 /* brackets */
<INITIAL>"{" {LIST;return '{';}
<INITIAL>"}" {LIST;return '}';}
 /* arithmetic */
<INITIAL>"+" {LIST;return '+';}
<INITIAL>"-" {LIST;return '-';}
<INITIAL>"*" {LIST;return '*';}
<INITIAL>"/" {LIST;return '/';}
 /* exponentiation */
<INITIAL>"^" {LIST;return '^';}
 /* remainder */
<INITIAL>"%" {LIST;return '%';}
 /* relational */
<INITIAL>"<" {LIST;return '<';}
<INITIAL>"<=" {LIST;return LTE;}
<INITIAL>">=" {LIST;return GTE;}
<INITIAL>">" {LIST;return '>';}
<INITIAL>"==" {LIST;return EQ;}
<INITIAL>"~=" {LIST;return NEQ;}
 /* logical */
<INITIAL>"and" {LIST;return AND;}
<INITIAL>"or" {LIST;return OR;}
<INITIAL>"not" {LIST;return NOT;}
 /* assignment */
<INITIAL>"=" {LIST;return ASSIGN;}
 /* keywords */
<INITIAL>"boolean" {LIST;return BOOLEAN;}
<INITIAL>"const" {LIST;return CONST;}
<INITIAL>"do" {LIST;return DO;}
<INITIAL>"else" {LIST;return ELSE;}
<INITIAL>"end" {LIST;return END;}
 /* boolean */
<INITIAL>"false"    {
					LIST;
					/* 把值傳給yacc */
				    yylval.s_val = (char *)malloc(sizeof(char) * (strlen(yytext)+1));			
				    strcpy(yylval.s_val,yytext);
					return BOOLEAN_VAL;
					}
<INITIAL>"for" {LIST;return FOR;}
<INITIAL>"function" {LIST;return FUNCTION;}
<INITIAL>"if" {LIST;return IF;}
<INITIAL>"import" {LIST;return IMPORT;}
<INITIAL>"in" {LIST;return IN;}
<INITIAL>"integer" {LIST;return INTEGER;}
<INITIAL>"local" {LIST;return LOCAL;}
<INITIAL>"nil" {LIST;return NIL;}
<INITIAL>"print" {LIST;return PRINT;}
<INITIAL>"println" {LIST;return PRINTLN;}
<INITIAL>"real" {LIST;return REAL;}
<INITIAL>"repeat" {LIST;return REPEAT;}
<INITIAL>"return" {LIST;return RETURN;}
<INITIAL>"string" {LIST;return STRING;}
<INITIAL>"read" {LIST;return READ;}
<INITIAL>"then" {LIST;return THEN;}
 /* boolean */
<INITIAL>"true"	{
				LIST;
				/* 把值傳給yacc */
				yylval.s_val = (char *)malloc(sizeof(char) * (strlen(yytext)+1));			
				strcpy(yylval.s_val,yytext);
				return BOOLEAN_VAL;
				}
<INITIAL>"until" {LIST;return UNTIL;}
<INITIAL>"while" {LIST;return WHILE;}
<INITIAL>"void" {LIST;return VOID;}
 /* id 遇到id 放進symbol table 中 */
<INITIAL>{id}	{  
					yylval.s_val = (char *)malloc(sizeof(char) * (strlen(yytext)+1));			
				    strcpy(yylval.s_val,yytext);
					insertID(yytext,nowtable);					
					LIST;
					return ID;
				}
 /* 不正確的ID */
<INITIAL>{errID}	{  
                    fprintf(stderr, "linenum: %d ,This is not a correct ID\n",linenum);
					}
 /* number and real number */
<INITIAL>{digits}	{
					LIST;
					/* 把值傳給yacc */
					yylval.i_val = atoi(yytext) ;
					return INTEGER_VAL;
					}
<INITIAL>{real_number}	{
						LIST;
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
						LIST;
						/* 把值傳給yacc */
						yylval.s_val = (char *)malloc(sizeof(char) * (strlen(s)+1));			
				        strcpy(yylval.s_val,s);
						return STRING_VAL;
					}
 /* 不正確的STRING */
<INITIAL>{errSTR}	{  
                    fprintf(stderr, "linenum: %d ,This is not a correct string\n",linenum);
					}
 /* newline and tab */
<INITIAL>\n {printf("/* %d: %s */\n",linenum,buf);linenum++;buf[0] = '\0';}
<INITIAL>[ \t]* {LIST;}
 /* 遇到//的註解，把內容印出來，linenum+1，buf清空 */
<INITIAL>("/""/"[^\n]*\n) {buf[0] = '\0';linenum++;}
 /* 遇到/*進入註解模式，並印出所在linenum */
<INITIAL>"/*" {BEGIN COMMENT;}
 /* 註解模式中，即使遇到//也只是存進buf裡 */
<COMMENT>"/""/"   {/* no action */}
 /* 註解模式中，遇到newline存進buf且印出linenum,內容 buf清空 */
<COMMENT>\n   {linenum++;buf[0] = '\0';}
 /* 註解模式中，遇到newline和(* /)以外的就存進buf裡 */
<COMMENT>[^[*/\n]]* {/* no action*/}
<COMMENT>[*]* {/* no action */}
 /* 註解模式中，遇到(* /)存進buf裡，並且回到初始模式 */
<COMMENT>"*/\n" {linenum++;BEGIN INITIAL;}

%%
 
int yywrap() {  
	return 1;
}
