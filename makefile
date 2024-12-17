all: delete
	@echo "Creating executable... 'main'"
	ocamlc -g -c str.cma ast.ml operationalTypes.ml prettyPrint.ml staticSemantics.ml operationalSemantics.ml 
	ocamllex lexer.mll
	menhir --explain parser.mly
	ocamlc -g -c parser.mli
	ocamlc -g -c lexer.ml
	ocamlc -g -c parser.ml
	ocamlc -g -c main.ml
	ocamlc -g -o interpreter str.cma ast.cmo operationalTypes.cmo prettyPrint.cmo staticSemantics.cmo operationalSemantics.cmo lexer.cmo parser.cmo main.cmo
	@echo "Done. Executable created. Run using './interpreter < input1.oo' OR run './interpreter' to type miniOO in terminal (press ctrl + D twice to end)."

delete:
	/bin/rm -f interpreter main.cmi main.cmo lexer.cmi lexer.cmo lexer.ml parser.cmi parser.cmo parser.ml parser.mli parser.conflicts
	/bin/rm -f ast.cmi ast.cmo operationalSemantics.cmi operationalSemantics.cmo operationalTypes.cmi operationalTypes.cmo prettyPrint.cmi prettyPrint.cmo staticSemantics.cmi staticSemantics.cmo makefile~
	@echo "Done. Deleted. Run 'make all' to compile again."