open Util
open Exceptions
open Core


(*
	Substitui x0 por x1 na ast
*)
let rec subset x0 x1 ast =
	match ast with
		ASTId(x) -> if x = x0 then ASTId(x1) else ast
	|	ASTInt(n) -> ast
	|	ASTNot(e) -> ASTNot(subset x0 x1 e)
	|	ASTEqual(e1, e2) -> ASTEqual(subset x0 x1 e1, subset x0 x1 e2)
	|	ASTBigger(e1, e2) -> ASTBigger(subset x0 x1 e1, subset x0 x1 e2)
	|	ASTSmaller(e1, e2) -> ASTSmaller(subset x0 x1 e1, subset x0 x1 e2)
	|	ASTPlus(e1, e2) -> ASTPlus(subset x0 x1 e1, subset x0 x1 e2)
	|	ASTMinus(e1, e2) -> ASTMinus(subset x0 x1 e1, subset x0 x1 e2)
	|	ASTMult(e1, e2) -> ASTMult(subset x0 x1 e1, subset x0 x1 e2)
	|	ASTDiv(e1, e2) -> ASTDiv(subset x0 x1 e1, subset x0 x1 e2)
	|	ASTMod(e1, e2) -> ASTMod(subset x0 x1 e1, subset x0 x1 e2)
	|	ASTAnd(e1, e2) -> ASTAnd(subset x0 x1 e1, subset x0 x1 e2)
	|   ASTOr(e1, e2) -> ASTOr(subset x0 x1 e1, subset x0 x1 e2)
	|	ASTSeq(e1, e2) -> ASTSeq(subset x0 x1 e1, subset x0 x1 e2)
	|	ASTAssign(x, e) -> if x = x0 then ASTAssign(x1, subset x0 x1 e) else ASTAssign(x, subset x0 x1 e)
	|	ASTWhile(cond, body) -> ASTWhile(subset x0 x1 cond, subset x0 x1 body)
	|	ASTIfElse(cond, e1, e2) -> ASTIfElse(subset x0 x1 cond,subset x0 x1 e1, subset x0 x1 e2)
	|	ASTVar(args, body) -> subsetVars args body x0 x1
	|	ASTVal(args, body) -> subsetVals args body x0 x1
	|	ASTPrint(e) -> ASTPrint(subset x0 x1 e)
	|	ASTNew(c) -> ast
	|	ASTCall(obj, id, args) -> ASTCall(subset x0 x1 obj, id, applyList (subset x0 x1) args)
	|	ASTThis -> ast
	|	ASTFork(e) -> ASTFork(subset x0 x1 e)
	|	ASTWait(e) -> ASTWait(subset x0 x1 e)
	|	ASTNewlock -> ast
	|	ASTShared(lock, body) -> ASTShared(subset x0 x1 lock, subset x0 x1 body)
	|	ASTSync(lock, body) -> ASTSync(subset x0 x1 lock, subset x0 x1 body)
	
and

subsetVals args body x0 x1 =
	match args with
		[] -> raise (InvalidType "val")
	|	[arg] -> subsetVal arg body x0 x1
	|	arg::t -> subsetVal arg (ASTVal(t, body)) x0 x1

and

subsetVal arg body x0 x1 = 
	match arg with
		(t, x, e) -> 
			let arg2 = (t, x, subset x0 x1 e) in
				if x = x0 then
					ASTVal([arg2], body)
				else
					ASTVal([arg2], subset x0 x1 body)

and

subsetVars args body x0 x1 =
	match args with
		[] -> raise (InvalidType "var")
	|	[arg] -> subsetVar arg body	x0 x1
	|	arg::t -> subsetVar arg (ASTVar(t, body)) x0 x1

and

subsetVar arg body x0 x1 =
	match arg with
		(t, x) ->
			if x = x0 then
				ASTVar([arg], body)
			else
				ASTVar([arg], subset x0 x1 body)


(*
	subsitui os sucessivos ids por -1,-2,-3,...
*)
let rec subsetNeg e ids index = 
	match ids with
		[] -> e
	|	h::t -> subsetNeg (subset h (string_of_int index) e) t (index-1)


(*
	substitui os ids -1,-2,-3,... por 0,1,2,....
*)
let rec subsetNotNeg e count =
	if count = 0 then
		e
	else
		let 
			x0 = string_of_int (-1*count) and
			x1 = string_of_int (count-1)
		in
			subsetNotNeg (subset x0 x1 e) (count-1)

(*
	Pre-condicao: se um id for um numero, tem que ser nao negativo
	
	Esta funcao (subsets) substitui sucessivos ids por 0,1,2,3,... 
	Primeiro substitui os sucessivos ids por -1,-2,-3,-4...
	Depois substitui -1,-2,-3,-4 por 0,1,2,3,...
	Tudo isto apenas para aproveitar a funcao subset já realizada,
	porque se quizermos substituir os identificadores 
	sem recorrer ao passo intermedio anterior (numeros negativos),
	por exemplo substituir 1 com 0 e 0 com 1 na expressão ASTSum(ASTId(1),ASTId(0)),
	ficaria ASTSum(ASTId(0),ASTId(0)) e depois ASTSum(ASTId(1),ASTId(1)
	
	Alterar, mais tarde, o subset (primeira funcao deste ficheiro) para percorrrer a AST e substituir multiplos ids
*)
let rec subsets e ids = subsetNotNeg (subsetNeg e ids (-1)) (List.length ids)
