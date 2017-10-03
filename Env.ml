open Exceptions

let rec hasLocal env x =
	match env with
		[] -> false
	|	(t, id)::tail -> if id = x then true else hasLocal tail x


let rec findLocal env x =
	match env with
		[] -> raise (IdNotFound x)
	|	(t, id)::tail -> if id = x then t else findLocal tail x
	

(*
	devolve o indice de 1 a n
*)
let rec findLocalIndex env x =
	match env with
		[] -> raise (IdNotFound x)
	|	(t, id)::tail -> if id = x then 1 else 1+(findLocalIndex tail x)


let rec makeEnvMethods methods = 
	match methods with
		[] -> []
	|	(signature, ast)::t -> signature::(makeEnvMethods t)


let rec makeEnvClasses classes =
	match classes with
		[] -> []
	|	(id, fs, ms)::t -> (id, (fs, (makeEnvMethods ms)))::(makeEnvClasses t)


let rec hasMethod env m =
	match env with
		[] -> false
	|	(ret, id, args)::t ->
			if m = id then
				true
			else
				hasMethod t m

let rec findMethod env m =
	match env with
		[] -> raise (IdNotFound m)
	|	(ret, id, args)::t ->
			if m = id then
				(ret, id, args)
			else
				findMethod t m


let rec findField env f =
	match env with
		[] -> raise (IdNotFound f)
	|	(t, id)::tail ->
			if f = id then
				t
			else
				findField tail f


let rec findClass env c =
	match env with
		[] -> raise (IdNotFound c)
	|	(id, (fs, ms))::t ->
			if c = id then
				(fs, ms)
			else
				findClass t c
