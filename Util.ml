(*
	Divide uma lista de pares em duas listas
*)
let rec splitPairsList l =
	match l with
		[] -> ([], [])
	|	(h1,h2)::t -> 
			let splited = splitPairsList t in
				(h1::(fst splited), h2::(snd splited))


(*
	Divide uma lista de pares em duas listas
*)
let rec mergePairsList l1 l2 =
	match l1,l2 with
		[], _ -> []
	|	_, [] -> []
	|	h1::t1, h2::t2 -> (h1, h2)::(mergePairsList t1 t2)


(* 
	Funcao generica que devolve uma lista resultante da aplicacao da funcao f
	a cada elemento da lista l
*)
let rec applyList f l = 
	match l with
		[] -> []
	|	h::t -> (f h)::(applyList f t)


(*
	Funcao generica que devolve uma lista resultante de concatenacoes da
	aplicacao da funcao f a cada elemento da lista l
*)
let rec applyListList f l =
	match l with
		[] -> []
	|	h::t -> (f h)@(applyListList f t)


(*
	Funcao auxiliar da funcao merge
*)
let rec equal x y =
	x = y


(*
	Recebe duas listas de strings e devolve a juncao de ambas,
	eliminando as repeticoes
*)
let rec merge l1 l2 =
	match l1 with
		[] -> l2
	|	h::t -> 
				if List.exists (equal h) l2 then 
					merge t l2
				else 
					h::(merge t l2)

(*
	Gera os ids de start até end
*)
let rec makeIds start finish =
	if start > finish then
		[]
	else
		(string_of_int start)::(makeIds (start+1) finish)
