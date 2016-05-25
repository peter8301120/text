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
#define MAX_LINE_LENG 256
#define LIST strcat(buf,yytext)
#define token(t) {LIST; printf("<%s>\n",t);}
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
 /* stmbol table */
struct symnode {
   char *id;
};
 /* 每個table除了自己的table之外 還會有指標指向前一個 */ 
struct symtable {
   struct symnode thistable[tablesize];  
   struct symtable *pretable;
};
 /* 建最初的table start */
 struct symtable start = {};
 /* create */
struct symtable create(struct symtable *st);
 /* lookup */
int lookup(char *s , struct symtable *st);
 /* insert */
int insert(char *s , struct symtable *st);
 /* dump */
void dump(struct symtable *st);
%}

%%
 /* comma */
<INITIAL>"," {token("','");}
 /* colon */
<INITIAL>":" {token("':'");}
 /* semicolon */
<INITIAL>";" {token("';'");}
 /* parentheses */
<INITIAL>"(" {token("'('");}
<INITIAL>")" {token("')'");}
 /* square brackets */
<INITIAL>"[" {token("'['");}
<INITIAL>"]" {token("']'");}
 /* brackets */
<INITIAL>"{" {token("'{'");}
<INITIAL>"}" {token("'}'");}
 /* arithmetic */
<INITIAL>"+" {token("'+'");}
<INITIAL>"-" {token("'-'");}
<INITIAL>"*" {token("'*'");}
<INITIAL>"/" {token("'/'");}
 /* exponentiation */
<INITIAL>"^" {token("'^'");}
 /* remainder */
<INITIAL>"%" {token("'%'");}
 /* relational */
<INITIAL>"<" {token("'<'");}
<INITIAL>"<=" {token("'<='");}
<INITIAL>">=" {token("'>='");}
<INITIAL>">" {token("'>'");}
<INITIAL>"==" {token("'=='");}
<INITIAL>"~=" {token("'~='");}
 /* logical */
<INITIAL>"and" {token("and");}
<INITIAL>"or" {token("or");}
<INITIAL>"not" {token("not");}
 /* assignment */
<INITIAL>"=" {token("'='");}
 /* keywords */
<INITIAL>"boolean" {token("BOOLEAN");}
<INITIAL>"const" {token("CONST");}
<INITIAL>"do" {token("DO");}
<INITIAL>"else" {token("ELSE");}
<INITIAL>"end" {token("END");}
<INITIAL>"false" {token("FALSE");}
<INITIAL>"for" {token("FOR");}
<INITIAL>"function" {token("FUNCTION");}
<INITIAL>"if" {token("IF");}
<INITIAL>"import" {token("IMPORT");}
<INITIAL>"in" {token("IN");}
<INITIAL>"integer" {token("INTEGER");}
<INITIAL>"local" {token("LOCAL");}
<INITIAL>"nil" {token("NIL");}
<INITIAL>"print" {token("PRINT");}
<INITIAL>"println" {token("PRINTLN");}
<INITIAL>"real" {token("REAL");}
<INITIAL>"repeat" {token("REPEAT");}
<INITIAL>"return" {token("RETURN");}
<INITIAL>"string" {token("STRING");}
<INITIAL>"then" {token("THEN");}
<INITIAL>"true" {token("TRUE");}
<INITIAL>"until" {token("UNTIL");}
<INITIAL>"while" {token("WHILE");}
 /* id 遇到id 放進symbol table 中 */
<INITIAL>{id}	{   
					insert(yytext,&start);
					tokenString("identifier",yytext);	
				}
 /* number and real number */
<INITIAL>{digits} {tokenInteger("integer",atoi(yytext));}
<INITIAL>{real_number} {tokenFloat("float",atof(yytext));}
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
						tokenString("string",s);
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

void main() {    
	yylex(); /* start the  analysis*/   
    /* dump Symbol Table */
	printf("\nSymbol Table:\n");
	dump(&start); 
	return 0;  
}   

int yywrap() {  
	return 1;
}

struct symtable create(struct symtable *st)
{
	/* 初始化 */
    struct symtable stb = {} ;
    stb.pretable = st;
    return stb;
}
 /*  找到symbol table中s的位置並回傳 */
int lookup(char *s , struct symtable *st)
{
    int i = 0;
	/* 不等於NULL */
    while ( st->thistable[i].id )
    {
		/* 相等 */
        if (!strcmp(st->thistable[i].id, s))
        {
            return i;                 
        }
        i++;
    }
    return -1;  
}
 /* 將s放入symbol table中*/
int insert(char *s , struct symtable *st)
{
	/* 先找s有沒有在裡面 */
    int index = lookup(s , st);
	/* 沒有的話 */
    if( index == -1)
    {
        int i = 0;
        while ( i <= tablesize-1)
        {
			/* 找到等於NULL地方 */
            if (!st->thistable[i].id )
            {
				/* 把s放進去 */
				st->thistable[i].id = (char *)malloc(sizeof(char) * (strlen(s)+1));			
                strcpy(st->thistable[i].id, s);
                return i;                 
            }
            i++;
        }
    }
    else
    {
        return index;
    }
}
 /* 印出symbol table內的所有id */
void dump(struct symtable *st)
{
    int i = 0;
    for (i ; i <= tablesize - 1 ; i++)
    {
		/* 不等於NULL */
        if(st->thistable[i].id)
		{ 
            printf( "%s\n" , st->thistable[i].id);
		} 
    }
}
