CURRENT_CLASS=class-system-01-20.plt

main: plt
	-find .. -name compiled -type d -exec rm -fr {} \;
	env CURRENT_CLASS=$(CURRENT_CLASS) raco make 2510H.scrbl
	raco scribble --htmls ++extra fonts/kulminoituva-webfont.eot ++extra fonts/kulminoituva-webfont.svg ++extra fonts/kulminoituva-webfont.ttf ++extra fonts/kulminoituva-webfont.woff ++extra $(CURRENT_CLASS) ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --style 2510H.css 2510H.scrbl

plt:
	(cd .. && raco pack -v ++setup class0 ++setup class1 ++setup scribblings/main --at-plt --replace web/$(CURRENT_CLASS) class0/ class1/)

web:
	rm -rf 2510H/
	env STUDENTS=1 make main

web-install: web	
	scp -r 2510H/* login.ccs.neu.edu:/course/cs2510h/.www/

push: web-install
