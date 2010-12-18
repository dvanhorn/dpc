main:
	rm -rf compiled/ lectures/compiled/ labs/compiled/ assignments/compiled/ class/compiled/
	raco make 2510H.scrbl
	raco scribble --htmls ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --style 2510H.css 2510H.scrbl

web:
	rm -rf 2510H/
	env STUDENTS=1 make main

web-install: web	
	scp -r 2510H/* login.ccs.neu.edu:/course/cs2510h/.www/

push: web-install
