%{
#define Trace(t)        printf(t)
extern int yylex();
 /* stmbol table size */
#define tablesize  100
 /* stmbol table */
struct symnode {
   char* id;
   char* type;
   int index;
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
 /* lookup global variable index*/
int lookupglo_index(char *s);
 /* lookup local variable index*/
int lookupindex(char *s , struct symtable *st);
 /* lookupInt_val */
int lookupInt_val(char *s , struct symtable *st);
 /* lookupString_val */
char* lookupString_val(char *s , struct symtable *st);
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
/* judge if it is global variable */
int global_v = 1;
/* symbol table counter*/
int s_counter = 1;
/*  count for label number */
int L_cnt = 1;
/* count for while loop label number */
int WL_cnt = 1;
/* filename */
char filename[tablesize];
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
program:        {printf("class %s\n{\n",filename);} con_var_declaration {global_v = 0;}
                function_declaration {printf("  method public static void main(java.lang.String[])\n  max_stack 15\n  max_locals 15\n  {\n");}
				statement_list{printf("    return\n  }\n}");}
				;

/* zero or more con_var_declaration */		
con_var_declaration:        {/* empty */}
							/* variable declaration 
							   use left recursion to produce more than one declaration */
							/* 把type放在symbol table 中 */
							| con_var_declaration e_type ID { 
															/* 判斷是否為global變數 */
															if (global_v == 1)
															{
															    if (strcmp(Type_of_ID, "integer") == 0)
																    printf("  field static int %s\n",$3);
															}	
														    else
															{
																nowtable->thistable[lookup($3,nowtable)].index = s_counter;
																s_counter++;																
															}
															/* 放type 進去 symbol table */
															insertType(lookup($3,nowtable),Type_of_ID,nowtable);
															}
							| con_var_declaration e_type ID ASSIGN constant_exp	{
																				/* 判斷是否為global變數 */
																				if (global_v == 1)
																				{
																				    if (strcmp(Type_of_ID, "integer") == 0)
																					    printf("  field static int %s = %d\n",$3,int_input);  
																				}
																				else
																				{
																				    int s_temp = s_counter - 1;
																				    printf("    sipush %d\n    istore %d\n",int_input,s_temp);
																					nowtable->thistable[lookup($3,nowtable)].index = s_counter;
																					s_counter++;
																				}
																				/* 放type 進去 symbol table */
																				insertType(lookup($3,nowtable),Type_of_ID,nowtable);
																				}
							/* constant declaration */
							/* const 需要把type還有value都放入symbol table 中 */
							| con_var_declaration CONST e_type ID ASSIGN constant_exp 	{/* 先放type 進去 symbol table */
																						insertType(lookup($4,nowtable),Type_of_ID,nowtable);
																						/* index == 10000 代表是const */
																						nowtable->thistable[lookup($4,nowtable)].index = 10000;
																						/* 根據type判斷要放入的value形態 */
																						if (strcmp(Type_of_ID, "integer") == 0)
																							insertInt_val(lookup($4,nowtable),int_input,nowtable);
																						else if (strcmp(Type_of_ID, "real") == 0)
																							insertReal_val(lookup($4,nowtable),real_input,nowtable);
																						else if (strcmp(Type_of_ID, "void") != 0)
																							insertString_val(lookup($4,nowtable),bs_input,nowtable);
																						}
							/* Array declaration */
							| con_var_declaration e_type ID '[' constant_exp ']'	{
																					/* 放type 進去 symbol table */
																					insertType(lookup($3,nowtable),Type_of_ID,nowtable);
																					}
							;							
/* type in con_var_declaration can not include "void" */
e_type:			BOOLEAN   {Type_of_ID = "boolean" ; } 
				| INTEGER {Type_of_ID = "integer" ; }
				| REAL    {Type_of_ID = "real" ;    }
				| STRING  {Type_of_ID = "string" ;  }
                ;
constant_exp:		/* 把value 往上傳 */
					BOOLEAN_VAL    {bs_input = $1;  }
					| INTEGER_VAL  {int_input = $1; }
					| REAL_VAL     {real_input= $1; }
					| STRING_VAL   {bs_input = $1;  }
					;				

/* zero or more function_declaration */					
function_declaration:       {/* empty(zero) s_counter = 1;*/} 
							/* use left recursion to produce more than one function_declaration */
							| function_declaration FUNCTION type ID 
								{
								    if (strcmp(Type_of_ID, "integer") == 0)
								        printf("  method public static int %s(",$4);
									else if (strcmp(Type_of_ID, "void") == 0)
									    printf("  method public static void %s(",$4);
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
							'(' formal_argument ')' {printf(")\n  max_stack 15\n  max_locals 15\n  {\n");}
							Block {/* drop the new symbol table */ droptable(nowtable);nest--;}
							END {printf("  }\n");s_counter = 1;}
							;
/* type in the function_declaration */
type:			BOOLEAN   {Type_of_ID = "boolean" ; } 
				| INTEGER {Type_of_ID = "integer" ; }
				| REAL    {Type_of_ID = "real" ;    }
				| VOID    {Type_of_ID = "void" ;    }   
				| STRING  {Type_of_ID = "string" ;  } 
                ;
formal_argument:    {/* empty */}
					/* without comma */
					| formal_argument type ID   
					{
					    printf("int");
					    /* 存function ID and type進function table */
						insertID($3,&func_table);
						insertType(lookup($3,&func_table),Type_of_ID,&func_table);
					    insertType(lookup($3,nowtable),Type_of_ID,nowtable);
						nowtable->thistable[lookup($3,nowtable)].index = s_counter;
						s_counter++;
					}
					/* with comma */
					| formal_argument ',' type ID   
					{
					    printf(", int");
					    /* 存function ID and type進function table */
						insertID($4,&func_table);
						insertType(lookup($4,&func_table),Type_of_ID,&func_table);
					    insertType(lookup($4,nowtable),Type_of_ID,nowtable);
						nowtable->thistable[lookup($4,nowtable)].index = s_counter;
						s_counter++;
				    }
					;	
		
/* zero or more statement */		
Block:			de_or_st con_var_declaration statement_list 
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
					else
					{
					    /* id is a local variable */
						if (lookuptype($1,nowtable) && lookupindex($1,nowtable) != -1 )
						{
						    printf("    istore %d\n",lookupindex($1,nowtable));
						}
						/* id is a globle variable */
						else if (lookup($1,&start) != -1 && lookupglo_index($1) != 10000)
						{
						    printf("    putstatic int %s.%s\n",filename,$1);
						}
					}
                }
				| ID '[' INTEGER_VAL ']' ASSIGN expression 
                {   /* check type */
                    if (strcmp(lookuptype($1,nowtable),$6)) 
                        yyerror("type does not matched");
				}
				| PRINT {printf("    getstatic java.io.PrintStream java.lang.System.out\n");}
				  expression {  /* exp is string */
								if (strcmp($3, "string") == 0){
								    printf("    invokevirtual void java.io.PrintStream.print(java.lang.String)\n");
								} 
								/* exp is int  */
								else if(strcmp($3, "integer") == 0) {
								    printf("    invokevirtual void java.io.PrintStream.print(int)\n");
								}
								/* exp is boolean  */
								else if (strcmp($3, "boolean") == 0){
								    printf("    invokevirtual void java.io.PrintStream.print(boolean)\n");
								}
							 }
				| PRINTLN {printf("    getstatic java.io.PrintStream java.lang.System.out\n");}
				  expression {  /* exp is string */
								if (strcmp($3, "string") == 0){
								    printf("    invokevirtual void java.io.PrintStream.println(java.lang.String)\n");
								} 
								/* exp is int  */
								else if(strcmp($3, "integer") == 0) {
								    printf("    invokevirtual void java.io.PrintStream.println(int)\n");
								}
								/* exp is boolean  */
								else if (strcmp($3, "boolean") == 0){
								    printf("    invokevirtual void java.io.PrintStream.println(boolean)\n");
								}
							 }
				| READ ID 
				| RETURN expression {printf("    ireturn\n");}
				| RETURN {printf("    return\n");}
				/* conditional statement */
				/* 如果是if while for 就必須在進入Block前新建一個symbol table 且離開後把他drop掉 */
				| IF '(' boolean_exp ')' 	{table[nest] = nowtable;newtable[nest] = create(table[nest]);nowtable = &newtable[nest];nest++;}									    
				THEN {printf("    ifeq L%d\n",L_cnt);L_cnt++;}
				Block {droptable(nowtable);nest--; printf("    goto L%d\n",L_cnt);printf("L%d: \n",(L_cnt-1));}
				elsestatement END {printf("L%d: \n",L_cnt);L_cnt++; }
				/* while loop statement*/
				| WHILE {printf("Lb%d: \n",WL_cnt);}
				'(' boolean_exp ')' DO 	{printf("    ifeq Le%d\n",(WL_cnt+1));table[nest] = nowtable;newtable[nest] = create(table[nest]);nowtable = &newtable[nest];nest++;}
				Block {droptable(nowtable);nest--;printf("    goto Lb%d\n",WL_cnt);}
				END {printf("Le%d: \n",(WL_cnt+1));WL_cnt = WL_cnt+2;}
				/* for loop statement*/
				| FOR ID ASSIGN expression ',' expression DO	{table[nest] = nowtable;newtable[nest] = create(table[nest]);nowtable = &newtable[nest];nest++;}		
				Block {droptable(nowtable);nest--;}
				END 
                ;		
elsestatement:	{/* empty */}	
				| ELSE	{table[nest] = nowtable;newtable[nest] = create(table[nest]);nowtable = &newtable[nest];nest++;}
				Block {nest--; droptable(nowtable);}
				;
				
expression:		'-' expression %prec UMINUS {$$ = $2;printf("    ineg\n");}
				| expression '^' expression  {if (strcmp($1, $3)) yyerror("type does not matched");else $$ = $1 ;}
				| expression '*' expression  {if (strcmp($1, $3)) yyerror("type does not matched");else {$$ = $1 ;printf("    imul\n");}}
				| expression '/' expression  {if (strcmp($1, $3)) yyerror("type does not matched");else {$$ = $1 ;printf("    idiv\n");}}
				| expression '%' expression  {if (strcmp($1, $3)) yyerror("type does not matched");else {$$ = $1 ;printf("    irem\n");}}
				| expression '-' expression  {if (strcmp($1, $3)) yyerror("type does not matched");else {$$ = $1 ;printf("    isub\n");}}
				| expression '+' expression  {if (strcmp($1, $3)) yyerror("type does not matched");else {$$ = $1 ;printf("    iadd\n");}}
				| boolean_exp {$$ = $1;}
				| '(' expression ')' {$$ = $2;}
				| STRING_VAL {$$ = "string";printf("    ldc \"%s\"\n",$1);}
				| ID{
				        /* put type into symbol table */
				        $$ = lookuptype($1,nowtable);						
						/* local variable */
						if (lookuptype($1,nowtable) && lookupindex($1,nowtable) != -1 )
						{
							printf("    iload %d\n",lookupindex($1,nowtable));
						}
						/* global variable */
						else if (lookup($1,&start) != -1 && lookupglo_index($1) != 10000)
						{
							printf("    getstatic int %s.%s\n",filename,$1);		
						}
						/* const */  
						else if (lookupindex($1,nowtable) == 10000 || lookupglo_index($1) == 10000)
						{   /* const integer */
						    if (strcmp(lookuptype($1,nowtable), "integer") == 0)
							    printf("    sipush %d\n",lookupInt_val($1,nowtable));
							/* const string */
							else if (strcmp(lookuptype($1,nowtable), "string") == 0)
								printf("    ldc \"%s\"\n",lookupString_val($1,nowtable));
							/* const boolean */
							else if (strcmp(lookuptype($1,nowtable), "boolean") == 0)
							{
							    /* true */
								if (strcmp(lookupString_val($1,nowtable), "true") == 0)
								    printf("    iconst_1\n");
								/* false */
								else if (strcmp(lookupString_val($1,nowtable), "false") == 0)
								    printf("    iconst_0\n");
							}	
						}
					}
				/* function Invocation */
				| ID '(' {func_posi = lookup($1,&func_table);}
				argument ')' {
								$$ = lookuptype($1,&func_table); 
								if (strcmp($$, "integer") == 0)
								    printf("    invokestatic int %s.%s(",filename,$1);
								else if (strcmp($$, "void") == 0)
								    printf("    invokestatic void %s.%s(",filename,$1);
								/* parameter */
								int f_temp = lookup($1,&func_table)+1;
								while (func_table.thistable[f_temp].id && func_table.thistable[f_temp].value.vi != -1)
								{
								    f_temp++;
									if (func_table.thistable[f_temp].id && func_table.thistable[f_temp].value.vi != -1)
								        printf("int, ");
									else if (!func_table.thistable[f_temp].id || func_table.thistable[f_temp].value.vi == -1)
									    printf("int)\n");
								}
							 }
				| ID '[' INTEGER_VAL ']' {$$ = lookuptype($1,nowtable);}
				| INTEGER_VAL {$$ = "integer"; printf("    sipush %d\n",$1);}
				| REAL {$$ = "real";}
				;
/* boolean expression */
boolean_exp:	expression '<' expression   {if (strcmp($1, $3)) 
											    yyerror("type does not matched");
											 else 
											 {
												$$ = "boolean" ;
												printf("    isub\n    iflt L%d\n    iconst_0\n    goto L%d\nL%d: \n    iconst_1\nL%d: \n",L_cnt,(L_cnt+1),L_cnt,(L_cnt+1));
												L_cnt = L_cnt + 2; 
											 }
											}
				| expression LTE expression {
											if (strcmp($1, $3)) yyerror("type does not matched");
											else 
											{
											    $$ = "boolean" ;
												printf("    isub\n    ifle L%d\n    iconst_0\n    goto L%d\nL%d: \n    iconst_1\nL%d: \n",L_cnt,(L_cnt+1),L_cnt,(L_cnt+1));
												L_cnt = L_cnt + 2; 
											}
											}
				| expression EQ expression  {
											if (strcmp($1, $3)) yyerror("type does not matched");
											else 
											{
												$$ = "boolean" ;
												printf("    isub\n    ifeq L%d\n    iconst_0\n    goto L%d\nL%d: \n    iconst_1\nL%d: \n",L_cnt,(L_cnt+1),L_cnt,(L_cnt+1));
												L_cnt = L_cnt + 2; 
											}
											}
				| expression GTE expression {
											if (strcmp($1, $3)) yyerror("type does not matched");
											else 
											{
												$$ = "boolean" ;
												printf("    isub\n    ifge L%d\n    iconst_0\n    goto L%d\nL%d: \n    iconst_1\nL%d: \n",L_cnt,(L_cnt+1),L_cnt,(L_cnt+1));
												L_cnt = L_cnt + 2; 
											}
											}
				| expression '>' expression {
											if (strcmp($1, $3)) yyerror("type does not matched");
											else 
											{
												$$ = "boolean" ;
												printf("    isub\n    ifgt L%d\n    iconst_0\n    goto L%d\nL%d: \n    iconst_1\nL%d: \n",L_cnt,(L_cnt+1),L_cnt,(L_cnt+1));
												L_cnt = L_cnt + 2; 
											}
											}
				| expression NEQ expression {
											if (strcmp($1, $3)) yyerror("type does not matched");
											else 
											{
												$$ = "boolean" ;
												printf("    isub\n    ifne L%d\n    iconst_0\n    goto L%d\nL%d: \n    iconst_1\nL%d: \n",L_cnt,(L_cnt+1),L_cnt,(L_cnt+1));
												L_cnt = L_cnt + 2; 
											}
											}
				| NOT expression            {$$ = $2 ;printf("    ixor\n");}
				| expression AND expression {if (strcmp($1, $3)) yyerror("type does not matched");else {$$ = "boolean" ;printf("    iand\n");}}
				| expression OR expression  {if (strcmp($1, $3)) yyerror("type does not matched");else {$$ = "boolean" ;printf("    ior\n");}}
				| BOOLEAN_VAL {{$$ = "boolean"; if (strcmp($1, "true") == 0) printf("    iconst_1\n"); else if (strcmp($1, "false") == 0) printf("    iconst_0\n"); }}
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
				    {/* do nothing */}
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
				    {/* do nothing */}
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
	int i ;
	for (i = 0; i <= strlen(argv[1]);i++)
	{
		if(argv[1][i] != '.')
		{
		    filename[i] = argv[1][i];
		}
		else
		{
		    filename[i] = '\0';
			break;
		}
	}
	
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
		    if (lookup(s,tempst) != -1)
			{
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
				tempst = tempst->pretable;
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
 /* lookup global variable index*/
int lookupglo_index(char *s)
{
    if (lookup(s,&start) != -1)
	{
	    if (start.thistable[lookup(s,&start)].index)
		    return start.thistable[lookup(s,&start)].index;
		else 
		    return -1;
	}
	else
	    return -1;
}
 /* find index of local variable */
int lookupindex(char *s , struct symtable *st)
{
    int temp ; 
	int i =0;
	/* 還不是最開始的table時 */
	struct symtable* tempst = st;
	while(tempst != &start && i <= tablesize-1)
	{
	    /* table exist*/
		if (tempst->thistable){
			/* 有type */
			if (lookup(s,tempst) != -1 )
			{
				if (tempst->thistable[lookup(s,tempst)].index){
					temp = tempst->thistable[lookup(s,tempst)].index;
					return temp-1;
				}
				else
					tempst = tempst->pretable;
					i++;
			}
			else 
				tempst = tempst->pretable;
		}
		else 
			return -1;
	}
	return -1;
}
 /* lookupInt_val */
int lookupInt_val(char *s , struct symtable *st)
{
    int temp ; 
	int i =0;
	/* 還不是最開始的table時 */
	struct symtable* tempst = st;
	while(tempst != &start && i <= tablesize-1)
	{
	    /* table exist*/
		if (tempst->thistable){
	        if (lookup(s,tempst) != -1){
				/* 有type */
				if (tempst->thistable[lookup(s,tempst)].value.vi){
					temp = tempst->thistable[lookup(s,tempst)].value.vi;
					return temp;
				}
				else
					tempst = tempst->pretable;
					i++;
			}
			else{
			    tempst = tempst->pretable;
			}
		}
		else 
			return -1;
	}
	if (tempst->thistable[lookup(s,tempst)].value.vi){
        temp = tempst->thistable[lookup(s,tempst)].value.vi;
    }
	else 
	    temp = -1;
	return temp;
}
 /* lookupString_val */
char* lookupString_val(char *s , struct symtable *st)
{
    char* string_val; ; 
	int i =0;
	/* 還不是最開始的table時 */
	struct symtable* tempst = st;
	while(tempst != &start && i <= tablesize-1)
	{
	    /* table exist*/
		if (tempst->thistable){
		    if(lookup(s,tempst) !=-1 )
			{
				/* 有type */
				if (tempst->thistable[lookup(s,tempst)].value.vs){
					string_val = (char *)malloc(sizeof(char) * (strlen(tempst->thistable[lookup(s,tempst)].value.vs)+1));			
					strcpy(string_val, tempst->thistable[lookup(s,tempst)].value.vs);
					return string_val;
				}
				else
					tempst = tempst->pretable;
					i++;
			}
			else 
				tempst = tempst->pretable;
		}
		else 
			return "no exist";
	}
	if (tempst->thistable[lookup(s,tempst)].value.vs){
        string_val = (char *)malloc(sizeof(char) * (strlen(tempst->thistable[lookup(s,tempst)].value.vs)+1));			
        strcpy(string_val, tempst->thistable[lookup(s,tempst)].value.vs);
    }
	else 
	    string_val = "no exist";
	return string_val;
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
