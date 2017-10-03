{
open Parser
let keyword_table = Hashtbl.create 53
let _ = List.iter ( fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok ) 
	[
		"class", CLASS;
		"field", FIELD;
		"method", METHOD;
		"while", WHILE;
		"if", IF; "then", THEN; "else", ELSE;
		"var", VAR;
		"val", VAL;
		"in", IN;
		"print", PRINT;
		"new", NEW;
		"this", THIS;
		"fork", FORK;
		"wait", WAIT;
		"newlock", NEWLOCK;
		"shared", SHARED;
		"sync", SYNC;
		"int", TYPE_INT;
		"lock", TYPE_LOCK;
		"th", TYPE_THREAD;
	]
}

let digit = ['0'-'9']
let char = ['a'-'z' 'A'-'Z']

rule token = parse
	| [' ' '\t' '\n'] { token lexbuf }
	| '=' { EQUAL } | "!=" { DIFF } | '>' { BIGGER } | '<' { SMALLER }
	| '+' { PLUS } | '-' { MINUS } | '*' { MULT } | '/' { DIV } | '%' { MOD }
	| '&' { AND } | '|' { OR }
	| "!" { NOT }
	| ';' { POINT_COMMA }
	| ":=" { TWO_POINT_EQ }
	| '.' { POINT }
	| ',' { COMMA }
	| '(' { LPAR }
	| ')' { RPAR }
	| '{' { LPAR2 }
	| '}' { RPAR2 }
	| "false" as boolean {FALSE (bool_of_string boolean)}
	| "true" as boolean {TRUE (bool_of_string boolean)}
	| digit+ as num { INT (int_of_string num) }
	| char (char | digit)* as word { try Hashtbl.find keyword_table word with Not_found -> ID word }
	| _ { token lexbuf }
	| eof { raise End_of_file }
