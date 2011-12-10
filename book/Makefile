all:
	env STUDENTS=1 raco scribble --latex \
		--prefix dpc-prefix.tex \
		--style dpc-style.tex dpc.scrbl
	xelatex dpc; xelatex dpc
	pdfnup --nup '2x1' dpc.pdf --outfile dpc-2up.pdf

clean:
	-rm *~ *log *aux *out *toc pict*png
	-rm -rf compiled
	-rm dpc.tex

