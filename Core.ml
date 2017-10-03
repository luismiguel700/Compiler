type typeV =
	Int
|	Lock
|	Thread of typeV
|	ClassV of string

type ast =
	ASTId of string
|	ASTInt of int
|	ASTNot of ast
|	ASTEqual of ast * ast | ASTBigger of ast * ast | ASTSmaller of ast * ast
|	ASTPlus of ast * ast | ASTMinus of ast * ast | ASTMult of ast * ast | ASTDiv of ast * ast | ASTMod of ast * ast
|	ASTAnd of ast * ast | ASTOr of ast * ast
|	ASTSeq of ast * ast
|	ASTAssign of string * ast
|	ASTWhile of ast * ast
|	ASTIfElse of ast * ast * ast
|	ASTVar of (typeV * string) list * ast
|	ASTVal of (typeV * string * ast) list * ast
|	ASTPrint of ast
|	ASTNew of typeV
|	ASTCall of ast * string * ast list
|	ASTThis
|	ASTFork of ast
|	ASTWait of ast
|	ASTNewlock
|	ASTSync of ast * ast
|	ASTShared of ast * ast

type signature = typeV * string * (typeV * string) list

type methodD = signature * ast

type field = typeV * string

type classD = string * field list * methodD list
