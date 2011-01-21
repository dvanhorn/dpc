CLASS=class-system-01-20.plt

all: clean class web

clean: clean-class clean-web

#
# class
#

.PHONY: class
class: clean-class
	(cd .. && raco pack \
		-v --at-plt --replace \
		++setup class0 \
		++setup class1 \
		++setup scribblings/main \
		$(CLASS) \
		class0/ class1/)

clean-class:
	find ../class* -name compiled -type d | xargs rm -fr
	rm -fr ../class-system-*.plt

#
# web
#

web:
	ln -sf ../class-system-*.plt .
	env CLASS=$(CLASS) raco make 2510H.scrbl
	raco scribble --htmls \
		++extra fonts/kulminoituva-webfont.eot \
		++extra fonts/kulminoituva-webfont.svg \
		++extra fonts/kulminoituva-webfont.ttf \
		++extra fonts/kulminoituva-webfont.woff \
		++extra $(CLASS) \
		++xref-in setup/xref load-collections-xref \
		--redirect-main http://docs.racket-lang.org/ \
		--style 2510H.css \
		2510H.scrbl

web-remote: clean-web
	env STUDENTS=1 make web

clean-web:
	find . -name compiled -type d | xargs rm -fr
	rm -rf 2510H/ class-system-*.plt

#
# web-install
#

push: web-remote	
	scp -r 2510H/* login.ccs.neu.edu:/course/cs2510h/.www/

web-install: push
