open Util
open Exceptions
open Core
open Env


(*
	Devolve uma lista com os ids livres, isto e,
	que nao estao em env
*)
let rec freeIds env ast =
	match ast with
		ASTId(x) -> if List.exists (equal x) env then [] else [x]
	|	ASTInt(n) -> []
	|	ASTNot(e) -> freeIds env e
	|	ASTEqual(e1, e2) -> merge (freeIds env e1) (freeIds env e2)
	|	ASTBigger(e1, e2) -> merge (freeIds env e1) (freeIds env e2)
	|	ASTSmaller(e1, e2) -> merge (freeIds env e1) (freeIds env e2)
	|	ASTPlus(e1, e2) -> merge (freeIds env e1) (freeIds env e2)
	|	ASTMinus(e1, e2) -> merge (freeIds env e1) (freeIds env e2)
	|	ASTMult(e1, e2) -> merge (freeIds env e1) (freeIds env e2)
	|	ASTDiv(e1, e2) -> merge (freeIds env e1) (freeIds env e2)
	|	ASTMod(e1, e2) -> merge (freeIds env e1) (freeIds env e2)
	|	ASTAnd(e1, e2) -> merge (freeIds env e1) (freeIds env e2)
	|   ASTOr(e1, e2) -> merge (freeIds env e1) (freeIds env e2)
	|	ASTSeq(e1, e2) -> merge (freeIds env e1) (freeIds env e2)
	|	ASTAssign(x, e) -> freeIds env e
	|	ASTWhile(cond, body) -> merge (freeIds env cond) (freeIds env body)
	|	ASTIfElse(cond, e1, e2) -> 
			let tmp = merge (freeIds env e1) (freeIds env e2) in 
				merge (freeIds env cond) tmp
	|	ASTVar(args, body) -> freeIdsVars args body env
	|	ASTVal(args, body) -> freeIdsVals args body env
	|	ASTPrint(e) -> freeIds env e
	|	ASTNew(c) -> []
	|	ASTCall(obj, id, args) -> merge (freeIds env obj) (applyListList (freeIds env) args)
	|	ASTThis -> []
	|	ASTFork(e) -> freeIds env e
	|	ASTWait(e) -> freeIds env e
	|	ASTNewlock -> []
	|	ASTShared(lock, body) -> merge (freeIds env lock) (freeIds env body)
	|	ASTSync(lock, body) -> merge (freeIds env lock) (freeIds env body)
	
and

freeIdsVals args body env =
	match args with
		[] -> raise (InvalidType "val")
	|	[arg] -> freeIdsVal arg body env
	|	arg::t -> freeIdsVal arg (ASTVal(t, body)) env

and

freeIdsVal arg body env = 
	match arg with
		(t, x, e) -> merge (freeIds env e) (freeIds (x::env) body)

and

freeIdsVars args body env =
	match args with
		[] -> raise (InvalidType "var")
	|	[arg] -> freeIdsVar arg body env
	|	arg::t -> freeIdsVar arg (ASTVar(t, body)) env

and

freeIdsVar arg body env =
	match arg with
		(t, x) -> freeIds (x::env) body
