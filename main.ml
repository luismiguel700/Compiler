open Parser
open Translator
open Unparser
open String


(*
let main () =
	let file = Sys.argv.(1) in
		let
			input = open_in file and
			output = open_out ((sub file 0 (rindex file '.'))^".tmvm")
		in
			let lexbuf = Lexing.from_channel input in
				let highClass = Parser.init Lexer.token lexbuf in
					let lowClasses = translateProgram [highClass] in
						unparseProgram lowClasses output
*)


let inExt = "in"
let outExt = "tmvm"


let rec getExt file = 
	if contains file '.' then
		let extIndex = rindex file '.' in
			if extIndex > 0 && extIndex+1 < (length file) then
				sub file (extIndex+1) ((length file)-extIndex-1)
			else
				""
	else
		""


let rec parseFiles dir dirName =
	try
		let file = Unix.readdir dir in
			if getExt file = inExt then
				let input = open_in (dirName^"/"^file) in
					let buf = Lexing.from_channel input in
						(Parser.init Lexer.token buf)::(parseFiles dir dirName)
			else
				(parseFiles dir dirName)
	with
		End_of_file -> []


let rec unparseProgram classes dirName =
	match classes with
		[] -> ()
	|	TMVM.Class(c, fs, ms)::t ->
			let output = open_out (dirName^"/"^c^"."^outExt) in
				unparseClass (TMVM.Class(c, fs, ms)) output;
				unparseProgram t dirName
	|	TMVM.ParamClass(c, param, fs, ms)::t ->
			let output = open_out (dirName^"/"^c^"."^outExt) in
				unparseClass (TMVM.ParamClass(c, param, fs, ms)) output;
				unparseProgram t dirName


let main () =
	let dirName = Sys.argv.(1) in
		let highClasses = parseFiles (Unix.opendir dirName) dirName in
			let lowClasses = translateProgram highClasses in
				unparseProgram lowClasses dirName


let _ = Printexc.print main ()
