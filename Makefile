all: main

Util.cmo Util.cmi: Util.ml
	ocamlc -c Util.ml
	
Exceptions.cmi Exceptions.cmo: Exceptions.ml
	ocamlc -c Exceptions.ml

Core.cmo Core.cmi: Core.ml Util.cmo
	ocamlc -c Core.ml

TMVM.cmi TMVM.cmo: TMVM.ml Util.cmo
	ocamlc -c TMVM.ml

Parser.ml Parser.mli: Parser.mly Core.cmo Core.cmi
	ocamlyacc Parser.mly

Parser.cmi Parser.cmo: Parser.ml Parser.mli
	ocamlc -c Parser.mli Parser.ml

Lexer.ml: Lexer.mll 
	ocamllex Lexer.mll

Lexer.cmo Lexer.cmi: Lexer.ml Parser.mli
	ocamlc -c Parser.mli Lexer.ml

Unparser.cmi Unparser.cmo: Unparser.ml
	ocamlc -c Unparser.ml

Env.cmi Env.cmo: Env.ml Core.cmo
	ocamlc -c Env.ml

Subset.cmi Subset.cmo: Subset.ml Core.cmo
	ocamlc -c Subset.ml
	
FreeIds.cmi FreeIds.cmo: FreeIds.ml Core.cmo
	ocamlc -c FreeIds.ml

Translator.cmi Translator.cmo: Translator.ml Core.cmo TMVM.cmo Env.cmo Subset.cmo FreeIds.cmo
	ocamlc -c Translator.ml

main: Util.cmi Exceptions.cmi Parser.cmi Lexer.cmi Core.cmi TMVM.cmi Env.cmi Subset.cmi FreeIds.cmi Translator.cmi Unparser.cmi main.ml
	ocamlc Unix.cma Util.cmo Exceptions.cmo Parser.cmo Lexer.cmo Core.cmo TMVM.cmo Env.cmo Subset.cmo FreeIds.cmo Translator.cmo Unparser.cmo main.ml -o main
	rm Util.cmi Util.cmo Exceptions.cmi Exceptions.cmo Core.cmi Core.cmo Parser.cmi Parser.cmo Parser.ml Parser.mli Lexer.cmo Lexer.cmi Lexer.ml Translator.cmi Translator.cmo TMVM.cmi TMVM.cmo Unparser.cmi Unparser.cmo Env.cmi Env.cmo Subset.cmi Subset.cmo FreeIds.cmi FreeIds.cmo main.cmi main.cmo
