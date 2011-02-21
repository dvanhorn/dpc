all:
	find ../class* -name compiled -type d | xargs rm -fr	
	env STUDENTS=1 raco scribble --latex \
		--prefix dpc-prefix.tex \
		--style dpc-style.tex notes.scrbl
	xelatex notes
	pdfnup --nup '2x1' notes.pdf -o notes-2up.pdf
