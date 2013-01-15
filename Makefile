all: krivinec krivinerun

krivinerun: machine.cmo krivinerun.cmo
	ocamlc -o $@ machine.cmo krivinerun.cmo

krivinec: machine.cmo parser.cmi parser.cmo lexer.cmo krivinec.cmo
	ocamlc -o $@ machine.cmo parser.cmo lexer.cmo krivinec.cmo

%.cmi: %.mli
	ocamlc $^

.SUFFIXES: .mll .mly .mli .ml .cmi .cmo .cmx

.mll.mli:
	ocamllex $<

.mll.ml:
	ocamllex $<

.mly.mli:
	ocamlyacc $<

.mly.ml:
	ocamlyacc $<

.mli.cmi:
	ocamlc -c $^

.ml.cmo:
	ocamlc -c $^

test: krivinec krivinerun
	./krivinec test.source -v
	./krivinerun test.bytecode -v

clean:
	rm -rf *.cm* krivinec *~ \#*\# *.mli *.bytecode krivinerun
