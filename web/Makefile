main:
	rm -rf compiled/
	raco make 2510H.scrbl
	raco scribble --htmls ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --style 2510H.css 2510H.scrbl

web:
	rm -rf compiled/
	STUDENTS=1 raco make 2510H.scrbl
	raco scribble --htmls ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --style 2510H.css 2510H.scrbl

