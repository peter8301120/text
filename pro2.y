%{
#define Trace(t)        printf(t)
extern int yylex();
 /* stmbol table size */
#define tablesize  100
 /* stmbol table */
struct symnode {
   char* id;
   char* type;
   union 
   {
         int vi;
         double vd;
         char* vs;
   } value;
};
 /* 每個table除了自己的table之外 還會有指標指向前一個 */ 
struct symtable {
   struct symnode thistable[tablesize];  
   struct symtable *pretable;
};
 /* 最初的symbol table */
struct symtable start = {{},&start};
 /* 目前使用的symbol table */
struct symtable *nowtable = &start;
 /* 放function local variable 的table */
struct symtable func_table = {{},&func_table};
 /*記錄function 在function table 的位置 */
 int func_posi = 0;
 /* create */
struct symtable create(struct symtable *st);
 /* lookup */
int lookup(char *s , struct symtable *st);
 /* lookup type*/
char* lookuptype(char* s ,struct symtable *st);
 /* insertID */
int insertID(char *s , struct symtable *st);
 /* insertType */
void insertType(int index ,char *type,struct symtable *st);
 /* insertIntvalue */
void insertInt_val(int index ,int i_val,struct symtable *st);
 /* insertreal_val */
void insertReal_val(int index ,double real_val,struct symtable *st);
 /* insertstring_val */
void insertString_val(int index ,char *s_val,struct symtable *st);
 /* drop the symbol table*/
void droptable(struct symtable *st);
 /* dump */
void dump(struct symtable *st);

/* type */ 
char *Type_of_ID;
/* use for count how many symbol table */
int nest =0;

/* integer value */
int int_input;
/* real value */
double real_input;
/* boolean or string value */
char *bs_input;
/* newtable */
struct symtable newtable [20];
struct symtable *table [20];
%}
/* union */
%union{
    double d_val;
	int i_val;
	char *s_val;
}
/* tokens */
%token <i_val> INTEGER_VAL  
%token <d_val> REAL_VAL
%token <s_val> STRING_VAL BOOLEAN_VAL ID

%token ',' ':' ';' '(' ')' '[' ']' '{' '}' 
%token '+' '-' '*' '/' '^' '%' '<' LTE GTE '>' EQ NEQ AND OR NOT ASSIGN 
%token BOOLEAN CONST DO ELSE END FOR FUNCTION IF IMPORT IN INTEGER LOCAL NIL
%token PRINT PRINTLN REAL REPEAT RETURN STRING THEN UNTIL WHILE
%token VOID READ
/* precedence */
%left OR
%left AND
%left NOT
%left '<' LTE EQ GTE '>' NEQ
%left '-' '+'
%left '*' '/' '%'
%left '^'
%nonassoc UMINUS

%type <s_val> expression boolean_exp
%%
/* start symbol */
program:        con_var_declaration function_declaration statement_list{Trace("Reducing to program\n");}
				;

/* zero or more con_var_declaration */		
con_var_declaration:        {/* empty */}
							/* variable declaration 
							   use left recursion to produce more than one declaration */
							/* 把type放在symbol table 中 */
							| con_var_declaration e_type ID	{ 
															/* 放type 進去 symbol table */
															insertType(lookup($3,nowtable),Type_of_ID,nowtable);
															Trace("Reducing to var_declaration\n");
															}
							| con_var_declaration e_type ID ASSIGN constant_exp	{
																				/* 放type 進去 symbol table */
																				insertType(lookup($3,nowtable),Type_of_ID,nowtable);
																				Trace("Reducing to var_declaration\n");
																				}
							/* constant declaration */
							/* const 需要把type還有value都放入symbol table 中 */
							| con_var_declaration CONST e_type ID ASSIGN constant_exp 	{/* 先放type 進去 symbol table */
																						insertType(lookup($4,nowtable),Type_of_ID,nowtable);
																						/* 根據type判斷要放入的value形態 */
																						if (strcmp(Type_of_ID, "integer") == 0)
																							insertInt_val(lookup($4,nowtable),int_input,nowtable);
																						else if (strcmp(Type_of_ID, "real") == 0)
																							insertReal_val(lookup($4,nowtable),real_input,nowtable);
																						else if (strcmp(Type_of_ID, "void") != 0)
																							insertString_val(lookup($4,nowtable),bs_input,nowtable);
																						Trace("Reducing to con_declaration\n");
																						}
							/* Array declaration */
							| con_var_declaration e_type ID '[' constant_exp ']'	{
																					/* 放type 進去 symbol table */
																					insertType(lookup($3,nowtable),Type_of_ID,nowtable);
																					Trace("Reducing to Array_declaration\n");
																					}
							;							
/* type in con_var_declaration can not include "void" */
e_type:			BOOLEAN   {Type_of_ID = "boolean" ; Trace("Reducing to e_type\n");} 
				| INTEGER {Type_of_ID = "integer" ; Trace("Reducing to e_type\n");}
				| REAL    {Type_of_ID = "real" ;    Trace("Reducing to e_type\n");}
				| STRING  {Type_of_ID = "string" ;  Trace("Reducing to e_type\n");}
                ;
constant_exp:		/* 把value 往上傳 */
					BOOLEAN_VAL    {bs_input = $1;  Trace("Reducing to constant_exp\n");}
					| INTEGER_VAL  {int_input = $1; Trace("Reducing to constant_exp\n");}
					| REAL_VAL     {real_input= $1; Trace("Reducing to constant_exp\n");}
					| STRING_VAL   {bs_input = $1;  Trace("Reducing to constant_exp\n");}
					;				

/* zero or more function_declaration */					
function_declaration:       {/* empty(zero) */} 
							/* use left recursion to produce more than one function_declaration */
							| function_declaration FUNCTION type ID 
								{
								    /* 存function ID and type進function table 且value把他設為-1 */
								    insertID($4,&func_table);
									insertType(lookup($4,&func_table),Type_of_ID,&func_table);
									insertInt_val(lookup($4,&func_table),-1,&func_table);
									insertType(lookup($4,nowtable),Type_of_ID,nowtable);
									/* create a now symbol table */
									table[nest] = nowtable;
									newtable[nest] = create(table[nest]);
									nowtable = &newtable[nest];
									nest++;
								}
							'(' formal_argument ')' Block {/* drop the new symbol table */ droptable(nowtable);nest--;}
							END {Trace("Reducing to function_declaration\n");}
							;
/* type in the function_declaration */
type:			BOOLEAN   {Type_of_ID = "boolean" ; Trace("Reducing to type\n");} 
				| INTEGER {Type_of_ID = "integer" ; Trace("Reducing to type\n");}
				| REAL    {Type_of_ID = "real" ;    Trace("Reducing to type\n");}
				| VOID    {Type_of_ID = "void" ;    Trace("Reducing to type\n");}   
				| STRING  {Type_of_ID = "string" ;  Trace("Reducing to type\n");} 
                ;
formal_argument:    {/* empty */}
					/* without comma */
					| formal_argument type ID   
					{
					    /* 存function ID and type進function table */
						insertID($3,&func_table);
						insertType(lookup($3,&func_table),Type_of_ID,&func_table);
					    insertType(lookup($3,nowtable),Type_of_ID,nowtable);
						Trace("Reducing to formal_argument\n");
					}
					/* with comma */
					| formal_argument ',' type ID   
					{
					    /* 存function ID and type進function table */
						insertID($4,&func_table);
						insertType(lookup($4,&func_table),Type_of_ID,&func_table);
					    insertType(lookup($4,nowtable),Type_of_ID,nowtable);
						Trace("Reducing to formal_argument\n");
				    }
					;	
		
/* zero or more statement */		
Block:			de_or_st con_var_declaration statement_list {Trace("Reducing to Block\n");}
                ;
de_or_st:		{/* */}
				| de_or_st con_var_declaration statement_list
				;
statement_list:	{/* empty */}
                /* with semicolon */
				| statement_list statement 
				/* without semicolon */
                | statement_list statement ';'	
				;
statement:		ID ASSIGN expression 
                {   /* check type */
                    if (strcmp(lookuptype($1,nowtable),$3)) 
                        yyerror("type does not matched");
                    Trace("Reducing to statement\n");
                }
				| ID '[' INTEGER_VAL ']' ASSIGN expression 
                {   /* check type */
                    if (strcmp(lookuptype($1,nowtable),$6)) 
                        yyerror("type does not matched");
					Trace("Reducing to statement\n");
				}
				| PRINT expression {Trace("Reducing to statement\n");}
				| PRINTLN expression {Trace("Reducing to statement\n");}
				| READ ID {Trace("Reducing to statement\n");}
				| RETURN expression {Trace("Reducing to statement\n");}
				| RETURN {Trace("Reducing to statement\n");}
				/* conditional statement */
				/* 如果是if while for 就必須在進入Block前新建一個symbol table 且離開後把他drop掉 */
				| IF '(' boolean_exp ')' 	{table[nest] = nowtable;newtable[nest] = create(table[nest]);nowtable = &newtable[nest];nest++;}									    
				THEN Block {dump(nowtable);droptable(nowtable);nest--;}
				elsestatement END {Trace("Reducing to conditional statement\n");}
				/* while loop statement*/
				| WHILE '(' boolean_exp ')' DO 	{table[nest] = nowtable;newtable[nest] = create(table[nest]);nowtable = &newtable[nest];nest++;}
				Block {dump(nowtable);droptable(nowtable);nest--;}
				END {Trace("Reducing to while loop statement\n");}
				/* for loop statement*/
				| FOR ID ASSIGN expression ',' expression DO	{table[nest] = nowtable;newtable[nest] = create(table[nest]);nowtable = &newtable[nest];nest++;}		
				Block {dump(nowtable);droptable(nowtable);nest--;}
				END {Trace("Reducing to for loop statement\n");}
                ;		
elsestatement:	{/* empty */}	
				| ELSE	{table[nest] = nowtable;newtable[nest] = create(table[nest]);nowtable = &newtable[nest];nest++;}
				Block {dump(nowtable);nest--; droptable(nowtable);Trace("Reducing to else Block\n");}
				;
				
expression:		'-' expression %prec UMINUS {$$ = $2; Trace("- exp");}
				| expression '^' expression  {if (strcmp($1, $3)) yyerror("type does not matched");else $$ = $1 ;Trace("Reducing to exp :exp ^ exp\n");}
				| expression '*' expression  {if (strcmp($1, $3)) yyerror("type does not matched");else $$ = $1 ;Trace("Reducing to exp :exp * exp\n");}
				| expression '/' expression  {if (strcmp($1, $3)) yyerror("type does not matched");else $$ = $1 ;Trace("Reducing to exp :exp / exp\n");}
				| expression '%' expression  {if (strcmp($1, $3)) yyerror("type does not matched");else $$ = $1 ;Trace("Reducing to exp :exp % exp\n");}
				| expression '-' expression  {if (strcmp($1, $3)) yyerror("type does not matched");else $$ = $1 ;Trace("Reducing to exp :exp - exp\n");}
				| expression '+' expression  {if (strcmp($1, $3)) yyerror("type does not matched");else $$ = $1 ;Trace("Reducing to exp :exp + exp\n");}
				| boolean_exp {$$ = $1;Trace("Reducing to boolean_exp\n");}
				| '(' expression ')' {$$ = $2;Trace("Reducing to exp :(exp)\n");}
				| STRING_VAL {$$ = "string";Trace("Reducing to exp :exp = string \n");}
				| ID {$$ = lookuptype($1,nowtable);Trace("Reducing to exp :exp = ID\n");}
				/* function Invocation */
				| ID '(' {func_posi = lookup($1,&func_table);}
				argument ')' {$$ = lookuptype($1,&func_table); ;Trace("Reducing to exp :exp = function invocation\n");}
				| ID '[' INTEGER_VAL ']' {$$ = lookuptype($1,nowtable);Trace("Reducing to exp :exp = Array\n");}
				| INTEGER_VAL {$$ = "integer";Trace("Reducing to exp :exp = integer\n");}
				| REAL {$$ = "real";Trace("Reducing to exp :exp = real\n");}
				;
/* boolean expression */
boolean_exp:	expression '<' expression   {if (strcmp($1, $3)) yyerror("type does not matched");else $$ = "boolean" ;Trace("Reducing to boolean_exp :exp < exp\n");}
				| expression LTE expression {if (strcmp($1, $3)) yyerror("type does not matched");else $$ = "boolean" ;Trace("Reducing to boolean_exp :exp <= exp\n");}
				| expression EQ expression  {if (strcmp($1, $3)) yyerror("type does not matched");else $$ = "boolean" ;Trace("Reducing to boolean_exp :exp == exp\n");}
				| expression GTE expression {if (strcmp($1, $3)) yyerror("type does not matched");else $$ = "boolean" ;Trace("Reducing to boolean_exp :exp >= exp\n");}
				| expression '>' expression {if (strcmp($1, $3)) yyerror("type does not matched");else $$ = "boolean" ;Trace("Reducing to boolean_exp :exp > exp\n");}
				| expression NEQ expression {if (strcmp($1, $3)) yyerror("type does not matched");else $$ = "boolean" ;Trace("Reducing to boolean_exp :exp ~= exp\n");}
				| NOT expression            {$$ = $2 ;Trace("Reducing to boolean_exp :exp  = not exp\n");}
				| expression AND expression {if (strcmp($1, $3)) yyerror("type does not matched");else $$ = "boolean" ;Trace("Reducing to boolean_exp :exp and exp\n");}
				| expression OR expression  {if (strcmp($1, $3)) yyerror("type does not matched");else $$ = "boolean" ;Trace("Reducing to boolean_exp :exp or exp\n");}
				;
/* arguments in function Invocation */
argument:   { /* empty(zero) */ }
			/* without comma */
			| argument expression   
			{
			    func_posi++;
				/* argument 形態不對 或是引數數目超出宣告的數目 */
				if (!func_table.thistable[func_posi].id)
				    yyerror("type of function invocation does not matched");
				else if(strcmp(func_table.thistable[func_posi].type,$2) | func_table.thistable[func_posi].value.vi == -1 )
				    yyerror("type of function invocation does not matched");
				else 
				    {Trace("Reducing to argument\n");}
			}
			/* with comma */
			| argument ',' expression   
			{
			    func_posi++;
				/* argument 形態不對 或是引數數目超出宣告的數目 */
				if (!func_table.thistable[func_posi].id)
				    yyerror("type of function invocation does not matched");
				else if(strcmp(func_table.thistable[func_posi].type,$3) | func_table.thistable[func_posi].value.vi == -1 )
				    yyerror("type of function invocation does not matched");
				else 
				    {Trace("Reducing to argument\n");}
			}
			;
				
%%
#include "lex.yy.c"

yyerror(msg)
char *msg;
{
    fprintf(stderr, "%s, linenum: %d\n", msg, linenum);
}

int main(int argc, char* argv[])
{
    /* open the source program file */
    if (argc != 2) {
        printf ("Usage: sc filename\n");
        exit(1);
    }
    yyin = fopen(argv[1], "r");         /* open input file */
    /* perform parsing */
    if (yyparse() == 1)                 /* parsing */
        yyerror("Parsing error !");     /* syntax error */
}

struct symtable create(struct symtable *st)
{
        /* 初始化 */
        struct symtable stb = {{},st};
        return stb;
}
 /*  找到symbol table中s的位置並回傳 */
int lookup(char *s , struct symtable *st)
{
    int i = 0;
	/* 不等於NULL */
	if (st->thistable){
		while ( st->thistable[i].id && i <= tablesize-1 )
		{
			/* 相等 */
			if (!strcmp(st->thistable[i].id, s))
			{
				return i;                 
			}
			i++;
		}
	}
    return -1;  
}
 /* 找到存在symbol table 中ID 的形態 */
char* lookuptype(char* s,struct symtable *st)
{
    char* type ; 
	int i =0;
	/* 還不是最開始的table時 */
	struct symtable* tempst = st;
	while(tempst != &start && i <= tablesize-1)
	{
	    /* table exist*/
		if (tempst->thistable){
			/* 有type */
			if (tempst->thistable[lookup(s,tempst)].type){
				type = (char *)malloc(sizeof(char) * (strlen(tempst->thistable[lookup(s,tempst)].type)+1));			
				strcpy(type, tempst->thistable[lookup(s,tempst)].type);
				return type;
			}
			else
				tempst = tempst->pretable;
				i++;
		}
		else 
			return "no exist";
	}
	if (tempst->thistable[lookup(s,tempst)].type){
        type = (char *)malloc(sizeof(char) * (strlen(tempst->thistable[lookup(s,tempst)].type)+1));			
        strcpy(type, tempst->thistable[lookup(s,tempst)].type);
    }
	else 
	    type = "no exist";
	return type;
}
 /* 將s放入symbol table中*/
int insertID(char *s , struct symtable *st)
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
				/* put s into table */
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
/* 將形態放到symbol table 中*/
void insertType(int index ,char *type,struct symtable *st)
{
	/* 將形態放入對應ID的type中 */
    st->thistable[index].type = (char *)malloc(sizeof(char) * (strlen(type)+1));			
    strcpy(st->thistable[index].type, type);
}
/* 把value 放入symbol table 中*/
/* integer value*/ 
void insertInt_val(int index ,int i_val,struct symtable *st)
{    
	/* 將value放入對應ID的vlaue中 */
    st->thistable[index].value.vi =  i_val;		
}     
/* double value*/ 
void insertReal_val(int index ,double real_val,struct symtable *st)
{    
	/* 將value放入對應ID的vlaue中 */
    st->thistable[index].value.vd =  real_val;		
}   
/* string or boolean value*/ 
void insertString_val(int index ,char *s_val,struct symtable *st)
{    
	/* 將value放入對應ID的value中 */
    st->thistable[index].value.vs = (char *)malloc(sizeof(char) * (strlen(s_val)+1));			
    strcpy(st->thistable[index].value.vs, s_val);
}   
/* drop the symbol table*/
void droptable(struct symtable *st)
{
	if (st)
		nowtable = nowtable->pretable;
}

 /* 印出symbol table內的所有id */
void dump(struct symtable *st)
{
    int i = 0;
	if (st){
		for (i ; i <= tablesize-1 ; i++)
		{
			/* 不等於NULL */
			if(st->thistable[i].id)
			{ 
				if (st->thistable[i].type){
					printf( "%s\t" , st->thistable[i].id);
					printf( "%s\n" , st->thistable[i].type);
				}
				else
					printf( "%s\n" , st->thistable[i].id);
			} 
		}
	}
}
