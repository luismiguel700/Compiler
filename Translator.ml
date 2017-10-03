open Util
open Exceptions
open Core
open TMVM
open Env
open Subset
open FreeIds


(*
	Geracao de novos nomes para métodos
	Assume que nenhum metodo da ling core tem o nome "methodGenerated"
*)
let methodId = ref (-1)
let getMethodId () = "methodGenerated"^(string_of_int !methodId)
let nextMethodId () = (methodId := !methodId + 1); getMethodId ()
let restartMethodId () = methodId := 0


(*
	Geracao de novas etiquetas para blocos
*)
let label = ref (-1)
let getLabel () = "l"^(string_of_int !label)
let nextLabel () = (label := !label + 1); getLabel ()
let restartLabel () = label := 0
let rec genLabels n = if n = 0 then [] else (nextLabel ())::(genLabels (n-1))


(*
	Geracao de novos ids para vars locais
*)
let var = ref (-1)
let getVar () = string_of_int !var
let nextVar () = var := !var + 1; getVar ()
let restartVar () = var := -1
let setVar n = var := n


(*
	Traduz um tipo da linguagem de alto nível para a linguagem intermédia
*)
let rec translateType t =
	match t with
		Core.Int -> TMVM.Int
	|	Core.Lock -> TMVM.Lock
	|	Core.Thread(t2) -> TMVM.Thread(translateType t2)
	|	Core.ClassV(c) -> TMVM.ClassV(c)


(*********************** Traducao de Expressoes ******************************)


(*
Argumentos:
	s -> pilha de tipos
	l -> etiqueta destino
	envG -> ambiente global (id -> (campos, assinaturas de metodos))
	envL -> ambiente local (id -> (constantes | variáveis locais))
	c -> id da classe corrente
	m -> id do metodo corrente
	ast -> no da arvore a traduzir
Resultados:
	a -> sequencia de instrucoes que acaba com br ou ret
	bs -> conjunto de blocos
	ms -> conjunto de metodos
	envL -> conjunto de declarações (id -> (constantes | variáveis locais))
	t -> tipo da expressão traduzida
*)
let rec translate s l envG envL c m ast =
	match ast with
		ASTId(x) -> translateId x l envG envL c m
	|	ASTInt(n) -> ([LDC(n); BR(l)], [], [], [], TMVM.Int)
	|	ASTNot(e) -> translateNot e s l envG envL c m
	|	ASTEqual(e1, e2) -> translateBinop e1 e2 CEQ s l envG envL c m
	|	ASTBigger(e1, e2) -> translateBinop e1 e2 CGT s l envG envL c m
	|	ASTSmaller(e1, e2) -> translateBinop e1 e2 CLT s l envG envL c m
	|	ASTPlus(e1, e2) -> translateBinop e1 e2 ADD s l envG envL c m
	|	ASTMinus(e1, e2) -> translateBinop e1 e2 SUB s l envG envL c m
	|	ASTMult(e1, e2) -> translateBinop e1 e2 MUL s l envG envL c m
	|	ASTDiv(e1, e2) -> translateBinop e1 e2 DIV s l envG envL c m
	|	ASTMod(e1, e2) -> translateBinop e1 e2 MOD s l envG envL c m
	|	ASTAnd(e1, e2) -> translateBinop e1 e2 AND s l envG envL c m
	|   ASTOr(e1, e2) -> translateBinop e1 e2 OR s l envG envL c m
	|	ASTSeq(e1, e2) -> translateSeq e1 e2 s l envG envL c m
	|	ASTAssign(x, e) -> translateAssign x e s l envG envL c m
	|	ASTWhile(cond, body) -> translateWhile cond body s l envG envL c m
	|	ASTIfElse(cond, e1, e2) -> translateIfElse cond e1 e2 s l envG envL c m
	|	ASTVar(args, body) -> translateVars args body s l envG envL c m
	|	ASTVal(args, body) -> translateVals args body s l envG envL c m
	|	ASTPrint(e) -> translatePrint e s l envG envL c m
	|	ASTNew(t) -> let t2 = translateType t in ([NEWOBJ(t2); BR(l)], [], [], [], t2)
	|	ASTCall(obj, id, args) -> translateCall obj id args s l envG envL c m
	|	ASTThis -> ([LDARG(0); BR(l)], [], [], [], TMVM.ClassV(c))
	|	ASTFork(e) -> translateFork e l envG envL c m
	|	ASTWait(e) -> translateWait e s l envG envL c m
	|	ASTNewlock -> ([NEWLOCK; BR(l)], [], [], [], TMVM.Lock)
	|	ASTShared(lock, body) -> translateSafe true lock body s l envG envL c m (*falta os ids livres*)
	|	ASTSync(lock, body) -> translateSafe false lock body s l envG envL c m (*falta os ids livres*)

and

translateId x l envG envL c m =
	if (hasLocal envL x) then (* constante ou variavel local *)
		let t = findLocal envL x in
			match t with 
				ParamClassV("Cell", paramT) -> 
					([LDLOC(int_of_string x); CALL(paramT, "get", []); BR(l)], [], [], [], paramT)
			|	_ -> ([LDLOC(int_of_string x); BR(l)], [], [], [], t)
	else
		let (fs, ms) = findClass envG c in
			if hasMethod ms m && (* argumento *)
				let (_, _, args) = findMethod ms m in 
					hasLocal args x
			then
				let (_, _, args) = findMethod ms m in
					let 
						i = findLocalIndex args x and
						t = translateType (findLocal args x)
					in
						([LDARG(i); BR(l)], [], [], [], t)
			else (* field *)
				let t = translateType (findField fs x) in
					([LDARG(0); LDFLD(x); BR(l)], [], [], [], t)

and

translateNot ast s l1 envG envL1 c m =
	let l0 = nextLabel () in
		let (a0, bs0, ms0, envL0, _) = translate s l0 envG envL1 c m ast in
			(a0, [(l0, TMVM.Int::s, [NOT; BR(l1)])]@bs0, ms0, envL0, TMVM.Int)

and

translateBinop ast0 ast1 inst s l2 envG envL2 c m =
	let
		l0 = nextLabel () and
		l1 = nextLabel ()
	in	
	let	
		(a0, bs0, ms0, envL0, _) = 
			translate s l0 envG envL2 c m ast0 and
		(a1, bs1, ms1, envL1, _) = 
			translate (TMVM.Int::s) l1 envG envL2 c m ast1
	in
	let 
		bs2 = 
			[(l0, TMVM.Int::s, a1); 
			(l1, TMVM.Int::TMVM.Int::s, [inst; BR(l2)])] 
	in
		(a0, bs0@bs1@bs2, ms0@ms1, envL0@envL1, TMVM.Int)
		
and

translateSeq ast0 ast1 s l1 envG envL2 c m =
	let
		l0 = nextLabel ()
	in	
	let	
		(a0, bs0, ms0, envL0, t0) = 
			translate s l0 envG envL2 c m ast0 and
		(a1, bs1, ms1, envL1, t1) = 
			translate s l1 envG envL2 c m ast1
	in
	let 
		bs2 = 
			[(l0, t0::s, POP::a1)] 
	in
		(a0, bs0@bs1@bs2, ms0@ms1, envL0@envL1, t1)

and

translateAssign x e s l0 envG envL0 c m =
	let
		l1 = nextLabel ()
	in
		if hasLocal envL0 x then (* variavel local *)
			let t = findLocal envL0 x in
				match t with
					ParamClassV("Cell", paramT) -> 
						let (a1, bs0, ms0, envL1, t) = 
							translate (ParamClassV("Cell", paramT)::s) l1 envG envL0 c m e 
							and
							b = (l1, 
								paramT::ParamClassV("Cell", paramT)::s, 
								[CALL(paramT, "set", [paramT]); BR(l0)])
						in
							(LDLOC(int_of_string x)::a1, b::bs0, ms0, envL1, paramT)
				|	_ -> raise (InvalidType "assign")
							
		else (* field *)
			let (a1, bs0, ms0, envL1, t) = 
				translate (TMVM.ClassV(c)::s) l1 envG envL0 c m e
			in
				let 
					b = (l1, 
						t::ClassV(c)::s, 
						[STFLD(x); LDARG(0); LDFLD(x); BR(l0)])
				in
					(LDARG(0)::a1, b::bs0, ms0, envL1, t)

and

translateWhile cond body s l4 envG envL2 c m =
	let
		l0 = nextLabel () and
		l1 = nextLabel () and
		l2 = nextLabel () and
		l3 = nextLabel ()
	in
		let
			(a0, bs0, ms0, envL0, _) = translate s l0 envG envL2 c m cond and
			(a1, bs1, ms1, envL1, t) = translate s l1 envG envL2 c m body
		in
			let
				bs2 = [
					(l2, s, a0);
					(l0, TMVM.Int::s, BRFALSE(l3)::a1);
					(l1, t::s, [POP; BR(l2)]);
					(l3, s, [LDC(0); BR(l4)])
				]
			in
				([BR(l2)], bs0@bs1@bs2, ms0@ms1, envL0@envL1, TMVM.Int)
				
and

translateIfElse cond body1 body2 s l2 envG envL3 c m =
	let
		l0 = nextLabel () and
		l1 = nextLabel ()
	in
	let
		(a0, bs0, ms0, envL0, _) = translate s l0 envG envL3 c m cond and
		(a1, bs1, ms1, envL1, _) = translate s l2 envG envL3 c m body1 and
		(a2, bs2, ms2, envL2, t) = translate s l2 envG envL3 c m body2
	in
	let 
		bs3 = [
			(l0, TMVM.Int::s, BRFALSE(l1)::a1); 
			(l1, s, a2)
			]
	in
		(a0, bs0@bs1@bs2@bs3, ms0@ms1@ms2, envL1@envL2@envL3, t)

and

translateVars args body s l envG envL c m =
	match args with
		[] -> raise (EmptyList "ASTVar")
	|	[arg] -> translateVar arg body s l envG envL c m
	|	arg::t -> translateVar arg (ASTVar(t, body)) s l envG envL c m

and

translateVar arg body s l envG envL0 c m =
	match arg with
		(t, x0) ->
			let t0 = translateType t and x1 = nextVar() in
			let ast = subset x0 x1 body in 
			let (a0, bs0, ms0, envL2, t1) = 
				translate s l envG ((TMVM.ParamClassV("Cell", t0), x1)::envL0) c m ast 
			in
			let a1 = NEWOBJ(TMVM.ParamClassV("Cell", t0))::STLOC(int_of_string x1)::a0 in
				(a1, bs0, ms0, (TMVM.ParamClassV("Cell", t0), x1)::envL2, t1)

and

translateVals args body s l envG envL c m =
	match args with
		[] -> raise (EmptyList "ASTVal")
	|	[arg] -> translateVal arg body s l envG envL c m
	|	arg::t -> translateVal arg (ASTVal(t, body)) s l envG envL c m

and

translateVal arg body s lf envG envL0 c m =
	match arg with
		(t, x0, ast2) ->
			let l0 = nextLabel () and x1 = nextVar() in
			let (a1, bs2, ms1, envL2, _) = translate s l0 envG envL0 c m ast2 in
			let ast1 = subset x0 x1 body in
			let (a0, bs0, ms0, envL1, t1) = translate s lf envG ((translateType t, x1)::envL0) c m ast1 in
			let block = (l0, (translateType t)::s, STLOC(int_of_string x1)::a0) in
				(a1, block::bs2@bs0, ms0@ms1, (translateType t, x1)::envL1@envL2, t1)

and

translatePrint ast s l1 envG envL1 c m =
	let l0 = nextLabel () in
		let (a0, bs0, ms0, envL0, _) = translate s l0 envG envL1 c m ast in
			let block = (l0, TMVM.Int::s, [DUP; PRINTINT; BR(l1)]) in
				(a0, block::bs0, ms0, envL0, TMVM.Int)

and

translateCall obj id args s l envG envL c m =
	let 
		l0 = nextLabel () and
		ln = genLabels (List.length args)
	in
	let 
		(a0, bs0, ms0, envL0, t0) = translate s l0 envG envL c m obj
	in
	let
		cId = match t0 with
				TMVM.ClassV(id) -> id
			|	_ -> raise (InvalidType ("in object type of call function"^id))
	in
	let
		argsT = translateArgs args ln (t0::s) envG envL c m
	in
	let 
		(an, bsn, msn, envLn, tn) = joinInfoArgs argsT
	in
	let
		blocks = makePushArgs an (l0::ln) tn (t0::s) and
		(ret, _, _) = findMethod (snd (findClass envG cId)) id
	in
	let
		lastBlock = (
			List.hd (List.rev (l0::ln)),
			(List.rev tn)@[t0]@s, 
			[CALL(translateType ret, id, tn); BR(l)]
		)
	in
		(a0, lastBlock::blocks@bsn@bs0, ms0@msn, envL0@envLn, translateType ret)

and

translateArgs args ln s envG envL c m = (* auxiliar do translateCall *)
	match (args, ln) with
		([], _) -> []
	|	(_, []) -> []
	|	(arg::tailArgs, label::tailLabels) -> 
			let (a, bs, ms, envL, t) =  translate s label envG envL c m arg in
				(a, bs, ms, envL, t)::(translateArgs tailArgs tailLabels (t::s) envG envL c m)

and

joinInfoArgs args = (* auxiliar do translateCall *)
	match args with
		[] -> ([], [], [], [], [])
	|	(a, bs, ms, envL, t)::tail -> 
			let (an, bsn, msn, envLn, tn) = joinInfoArgs tail in
				(a::an, bs@bsn, ms@msn, envL@envLn, t::tn)

and

makePushArgs an ln tn s = (* auxiliar do translateCall *)
	match (an, ln, tn) with
		([], _, _) -> []
	|	(_, [], _) -> []
	|	(_, _, []) -> []
	|	(a::aTail, l::lTail, t::tTail) -> (l, s, a)::(makePushArgs aTail lTail tTail (t::s))

and

translateFork e lf envG envL c m =
	let 
		m0 = nextMethodId () and
		(fs, ms) = findClass envG c
	in
	let 
		ids = freeIds (snd (splitPairsList fs)) e and
		args = if hasMethod ms m then let (_, _, args) = findMethod ms m in args else []
	in
	let
		pushIds = makePushIds ids envL args and 
		typeIds = makeTypeList ids envL args and
		varCurrMethod = int_of_string (getVar ())
	in
		setVar (List.length ids);
		let ms = makeAuxMethods e m0 ids typeIds envG c in
			setVar varCurrMethod;
			let ret = getRetType (List.hd ms) in
				let a1 = LDARG(0)::pushIds@[FORK(ret, m0, typeIds); BR(lf)] in
					(a1, [], ms, [], TMVM.Thread(ret))

and

translateWait ast s l1 envG envL1 c m =
	let l0 = nextLabel () in
	let 
		(a0, bs0, ms0, envL0, t0) = translate s l0 envG envL1 c m ast
	in
	let
		t1 = match t0 with
				TMVM.Thread(t) -> t
			|	_ -> raise (InvalidType "wait")
	in
		(a0, [(l0, t0::s, [WAIT; BR(l1)])]@bs0, ms0, envL0, t1)

and

translateSafe isShared e1 e0 s lf envG envL0 c m  = 
	let 
		l2 = nextLabel () and
		m0 = nextMethodId () and
		(fs, ms) = findClass envG c
	in
	let 
		ids = freeIds (snd (splitPairsList fs)) e0 and
		args = if hasMethod ms m then let (_, _, args) = findMethod ms m in args else []
	in
	let
		pushIds = makePushIds ids envL0 args and 
		typeIds = makeTypeList ids envL0 args and
		varCurrMethod = int_of_string (getVar ())
	in
		setVar (List.length ids);
		let ms = makeAuxMethods e0 m0 ids typeIds envG c in
			setVar varCurrMethod;
			let 
				ret = getRetType (List.hd ms) and
				(a2, bs2, ms2, envL2, _) = translate s l2 envG envL0 c m e1
			in
				let inst = if isShared then SHARED(ret, m0, typeIds) else SYNC(ret, m0, typeIds) in
					let b3 = (l2, TMVM.Lock::s, LDARG(0)::pushIds@[inst; BR(lf)]) in
						(a2, b3::bs2, ms, envL2, ret)

and

makeAuxMethods e m freeIds typeIds envG c = (* auxiliar do translateFork e translateSafe *)
	let
		l0 = nextLabel () and
		l1 = nextLabel ()
	in
	let 
		ldargsStlocs = args2locals (List.length freeIds) and
		e2 = subsets e freeIds
	in
	let 
		(a0, bs0, ms0, envL1, t) = translate [] l0 envG (mergePairsList typeIds (makeIds 0 10)) c m e2
	in
	let 
		bs1 = [(l1, [], ldargsStlocs@a0); (l0, [t], [RET])] 
	in
		((t, m, typeIds), typeIds@(fst (splitPairsList envL1)), bs1@bs0)::ms0

and

makeTypeList ids envL args = (* auxiliar do translateFork e translateSafe *)
	let typeArgsInTMVM = (applyList translateType (fst (splitPairsList args))) in
		let	argsInTMVM = mergePairsList typeArgsInTMVM (snd (splitPairsList args)) in
			applyList (findLocal (argsInTMVM@envL)) ids

and

makePushIds ids envL args = (* auxiliar do translateFork e translateSafe *)
	match ids with
		[] -> []
	|	h::t -> 
			let inst = 
				if hasLocal envL h then  (* var local *)
					LDLOC(int_of_string h)
				else (* argumento *)
					LDARG(findLocalIndex args h)
			in
				inst::(makePushIds t envL args)

and

args2locals index = (* auxiliar do translateFork e translateSafe *)
	if index = 0 then
		[]
	else
		LDARG(index)::STLOC(index-1)::(args2locals (index-1))


(************************** Traducao do Programa *****************************)


let translateMethod envG c methodD = 
	match methodD with
		((ret, m, args), ast) ->
			let 
				l0 = nextLabel () and
				l1 = nextLabel () 
			in
			let 
				(a, bs, ms, envL, t) = translate [] l1 envG [] c m ast and
				argsTypes = fst (splitPairsList args) 
			in
			let 
				argsTypesTrans = applyList translateType argsTypes and
				b0 = (l0, [], a) and
			 	b1 = (l1, [t], [RET]) and
				locals = fst (splitPairsList envL)
			in
				restartLabel (); restartVar ();
				((translateType ret, m, argsTypesTrans), locals, b0::b1::bs)::ms

				
let translateField f =
	match f with
		(t, id) -> (translateType t, id)


let translateClass envG classD =
	match classD with
		(c, fields, methods) ->
			let 
				fieldsT = applyList translateField fields and
				methodsT = applyListList (translateMethod envG c) methods
			in
				restartMethodId (); Class(c, fieldsT, methodsT)


let translateProgram classes = 
	applyList (translateClass (makeEnvClasses classes)) classes
