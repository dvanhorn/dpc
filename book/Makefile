all: class dpc.pdf dpc-2up.pdf dpc.html

scrbls := $(wildcard *.scrbl) \
	$(wildcard chapter/*.scrbl) \
	$(wildcard chapter/01/*.scrbl) \
	$(wildcard chapter/02/*.scrbl) \
	$(wildcard solution/*.scrbl)

class:
	raco setup class

dpc.tex: $(scrbls)
	env STUDENTS=1 raco make dpc.scrbl
	env STUDENTS=1 raco scribble --latex \
		++xref-in setup/xref load-collections-xref \
		--prefix dpc-prefix.tex \
		--style dpc-style.tex dpc.scrbl

dpc.pdf: dpc.tex
	xelatex dpc; xelatex dpc

dpc-2up.pdf: dpc.pdf
	pdfnup --nup '2x1' dpc.pdf --outfile dpc-2up.pdf

dpc.html: $(scrbls)
	env STUDENTS=1 raco scribble --html \
		++xref-in setup/xref load-collections-xref \
		dpc.scrbl

clean:
	-rm *~ *log *aux *out *toc pict*png
	-rm -rf compiled
	-rm dpc.tex
	-rm racket.css scribble.css scribble-style.css scribble-common.js


