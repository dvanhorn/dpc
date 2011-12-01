all:
	env STUDENTS=1 raco scribble --latex \
		--prefix dpc-prefix.tex \
		--style dpc-style.tex dpc.scrbl
	xelatex dpc; xelatex dpc

clean:
	-rm *~ *log *aux *out *toc pict*png
	-rm -rf compiled
	-rm dpc.tex

