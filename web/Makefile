main:
	find -name compiled | xargs rm -rf
	raco make 2510H.scrbl
	raco scribble --htmls ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --style 2510H.css 2510H.scrbl

web:
	@STUDENTS=1 make main

web-install: web	
	scp 2510H/* login.ccs.neu.edu:/course/cs2510h/.www/


