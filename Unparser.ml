open TMVM
open Util

(*
	Funcao generica que chama a função f, que recebe cada elemento da lista l,
	o canal ch e o separador sep. Devolve um valor com tipo unit
*)
let rec unparseList f l ch sep =
	match l with
		[] -> ()
	|	[h] -> f h ch
	|	h::t -> f h ch; output_string ch sep; unparseList f t ch sep


let rec unparseType t ch =
	match t with
		Int -> output_string ch "int"
	|	Lock -> output_string ch "lock"
	|	Thread(param) -> output_string ch "thread<"; 
			unparseType param ch; output_string ch ">";
	|	ClassV(id) -> output_string ch id
	|	ParamClassV(id, param) -> output_string ch (id^"<"); 
			unparseType param ch; output_string ch ">"


let unparseSig s ch =
	match s with
		(ret, id, args) -> unparseType ret ch; output_string ch (" "^id^"(");
			unparseList unparseType args ch ","; output_string ch ")"


let unparseInst inst ch  =
	match inst with
		RET -> output_string ch "ret"
	|	BR(l) -> output_string ch ("br "^l)
	|	ADD -> output_string ch "add"
	|	SUB -> output_string ch "sub"
	|	MUL -> output_string ch "mul"
	|	DIV -> output_string ch "div"
	|	MOD -> output_string ch "mod"
	|	CEQ -> output_string ch "ceq"
	|	CGT -> output_string ch "cgt"
	|	CLT -> output_string ch "clt"
	|	AND -> output_string ch "and"
	|	OR -> output_string ch "or"
	|	NOT -> output_string ch "not"
	|	POP -> output_string ch "pop"
	|   DUP -> output_string ch "dup"
	|   LDC(n) -> output_string ch "ldint "; output_string ch (string_of_int n)
	|   BRFALSE(l) -> output_string ch ("brfalse "^l)
	|	LDARG(n) -> output_string ch "ldarg "; 
			output_string ch (string_of_int n)
	|	LDLOC(n) -> output_string ch "ldloc ";
			output_string ch (string_of_int n)
	|	STLOC(n) -> output_string ch "stloc "; 
			output_string ch (string_of_int n)
	|	PRINTINT -> output_string ch "printint "; 
	|   NEWOBJ(t) -> output_string ch "newobj "; unparseType t ch
	|   CALL(ret, id, args) -> 
			output_string ch "call "; unparseSig (ret, id, args) ch
	|   LDFLD(f) -> output_string ch ("ldfld "^f)
	|   STFLD(f) -> output_string ch ("stfld "^f)
	|   FORK(ret, id, args) -> 
			output_string ch "fork "; unparseSig (ret, id, args) ch
	|   WAIT -> output_string ch "wait"
	|   NEWLOCK -> output_string ch "newlock"
	|   SHARED(ret, id, args) -> 
			output_string ch "shared "; unparseSig (ret, id, args) ch
	|   SYNC(ret, id, args) -> 
			output_string ch "sync "; unparseSig (ret, id, args) ch


let unparseBlock b ch =
	match b with
		(id, [], insts) -> output_string ch (id^":empty\n"); 
			unparseList unparseInst insts ch "\n"   
	|	(id, stack, insts) -> output_string ch (id^":{");
			unparseList unparseType (List.rev stack) ch ","; 
			output_string ch ("}\n"); unparseList unparseInst insts ch "\n"


let unparseMethod m ch =
	match m with
		(s, locals, blocks) -> output_string ch "method "; unparseSig s ch;
			output_string ch "\n{\n"; 
			if List.length locals > 0 then
			(
				output_string ch "locals("; 
				unparseList unparseType locals ch ",";
				output_string ch ")\n\n"
			);
			unparseList unparseBlock blocks ch "\n\n"; output_string ch "\n}\n"


let unparseField f ch =
	match f with
		(t, id) -> output_string ch "field "; unparseType t ch; 
			output_string ch (" "^id^";")


let unparseClassBody fields methods ch =
	output_string ch "\n{\n"; unparseList unparseField fields ch "\n";
	output_string ch "\n\n"; unparseList unparseMethod methods ch "\n\n";
	output_string ch "\n}\n"


let unparseClass c ch =
	match c with
		Class(id, fs, ms) -> output_string ch ("class "^id); 
			unparseClassBody fs ms ch
	|	ParamClass(id, t, fs, ms) -> output_string ch ("class "^id^"<");
			unparseType t ch; output_string ch ">"; unparseClassBody fs ms ch
