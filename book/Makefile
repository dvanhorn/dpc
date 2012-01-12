all: class dpc.pdf dpc-2up.pdf dpc.html

scrbls := $(wildcard *.scrbl)

class:
	raco setup class

dpc.tex: $(scrbls)
	env STUDENTS=1 raco make dpc.scrbl
	env STUDENTS=1 raco scribble --latex \
		--prefix dpc-prefix.tex \
		--style dpc-style.tex dpc.scrbl

dpc.pdf: dpc.tex
	xelatex dpc; xelatex dpc

dpc-2up.pdf: dpc.pdf
	pdfnup --nup '2x1' dpc.pdf --outfile dpc-2up.pdf

dpc.html: $(scrbls)
	env STUDENTS=1 raco scribble --html \
		 dpc.scrbl

clean:
	-rm *~ *log *aux *out *toc pict*png
	-rm -rf compiled
	-rm dpc.tex
	-rm racket.css scribble.css scribble-style.css scribble-common.js

