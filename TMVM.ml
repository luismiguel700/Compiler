type typeV =
	Int
|	Lock
|	Thread of typeV
|	ClassV of string
|	ParamClassV of string * typeV

type signature = typeV * string * typeV list

type instruction =
	RET
|	BR of string
|	CEQ | CGT | CLT 
|	ADD | SUB | MUL | DIV | MOD
|	AND | OR
|	NOT
|   POP
|   DUP
|   LDC of int
|   BRFALSE of string
|	LDARG of int
|	LDLOC of int
|	STLOC of int
|	PRINTINT
|   NEWOBJ of typeV
|   CALL of signature
|   LDFLD of string
|   STFLD of string
|   FORK of signature
|   WAIT
|   NEWLOCK
|   SHARED of signature
|   SYNC of signature

type block = string * typeV list * instruction list

type methodD = signature * typeV list * block list

type field = typeV * string

type classD = 
	Class of string * field list * methodD list
|	ParamClass of string * typeV * field list * methodD list

let getRetType m =
	match m with
		((ret, _, _), _, _) -> ret
