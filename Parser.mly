%{
open Core
%}

%token <bool> FALSE
%token <bool> TRUE
%token <int> INT
%token <string> ID
%token CLASS FIELD METHOD WHILE IF THEN ELSE VAR VAL IN FORK WAIT NEWLOCK
%token SHARED SYNC NOT DIFF BIGGER BIGGER_EQ SMALLER SMALLER_EQ PLUS MINUS 
%token MULT DIV MOD POINT_COMMA TWO_POINT_EQ POINT COMMA LPAR RPAR LPAR2
%token RPAR2 LPAR3 RPAR3 EQUAL NEW TYPE_INT TYPE_LOCK TYPE_THREAD OR AND
%token PRINT THIS

%start init

%type <Core.classD> init

%left POINT_COMMA
%left WHILE IF VAR VAL PRINT FORK WAIT SHARED SYNC
%left TWO_POINT_EQ
%left AND 
%left OR
%left NOT
%left EQUAL DIFF BIGGER SMALLER
%left PLUS MINUS
%left MULT DIV MOD
%left POINT NEW

%% /* Grammar rules and actions follow */

init: CLASS ID LPAR2 fieldsAndMethods RPAR2 { let (f, m) = $4 in ($2, f, m) };

fieldsAndMethods: { ([],[]) } /* nothing */
|	typeL ID POINT_COMMA fieldsAndMethods 
	{ let (f, m) = $4 in (($1, $2)::f, m) }
|	typeL ID LPAR methodArgs RPAR LPAR2 exp RPAR2 fieldsAndMethods
 	{ let (f, m) = $9 in (f, (($1, $2, $4), $7)::m) }
;

exp:
	ID { ASTId($1) }
|	INT { ASTInt($1) }
|	NOT exp { ASTNot($2) }
|	MINUS exp { ASTMinus(ASTInt(0), $2) }
|	op { $1 }
|	exp POINT_COMMA exp { ASTSeq($1, $3) }
|	ID TWO_POINT_EQ exp { ASTAssign($1, $3) }
|	WHILE LPAR exp RPAR LPAR2 exp RPAR2 { ASTWhile($3, $6) }
|	IF exp THEN exp ELSE exp { ASTIfElse($2, $4, $6) }
|	VAR varArgs IN exp { ASTVar($2, $4) }
|	VAL valArgs IN exp { ASTVal($2, $4) }
|	PRINT exp { ASTPrint($2) }
|	NEW typeL { ASTNew($2) };
|	exp POINT ID LPAR callArgs RPAR { ASTCall($1, $3, $5) }
|	THIS { ASTThis }
|	FORK exp { ASTFork($2) }
|	WAIT exp { ASTWait($2) }
|	NEWLOCK { ASTNewlock }
|	SHARED LPAR exp RPAR LPAR2 exp RPAR2 { ASTShared($3, $6) }
|	SYNC LPAR exp RPAR LPAR2 exp RPAR2 { ASTSync($3, $6) }
|	LPAR exp RPAR { $2 }	
;

op:
	exp PLUS exp { ASTPlus($1, $3) } 
|	exp MINUS exp { ASTMinus($1, $3) } 
|	exp MULT exp { ASTMult($1, $3) }
|	exp DIV exp { ASTDiv($1, $3) }
|	exp MOD exp { ASTMod($1, $3) }
|	exp EQUAL exp { ASTEqual($1, $3) }
|	exp DIFF exp { ASTNot(ASTEqual($1, $3)) }
|	exp BIGGER exp { ASTBigger($1, $3) }
|	exp SMALLER exp { ASTSmaller($1, $3) }
|	exp AND exp { ASTAnd($1, $3) }
|	exp OR exp { ASTOr($1, $3) }
;

methodArgs: /*nothing*/ { [] }
|	typeL ID { [($1, $2)] }
|	typeL ID COMMA methodArgs { ($1, $2)::$4 }
;

callArgs: /*nothing*/ { [] }
|	exp { [$1] }
|	exp COMMA callArgs { $1::$3 }
;

varArgs:
	typeL ID { [($1, $2)] }
|	typeL ID COMMA varArgs { ($1, $2)::$4 }
;

valArgs:
	typeL ID EQUAL exp { [($1, $2, $4)] }
|	typeL ID EQUAL exp COMMA valArgs { ($1, $2, $4)::$6 }
;

typeL: TYPE_INT { Int }
|	TYPE_LOCK { Lock }
|	TYPE_THREAD SMALLER typeL BIGGER { Thread($3) }
|	ID { ClassV($1) }
%%
