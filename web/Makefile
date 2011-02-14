CLASS=class-system-02-13.plt
CLASS_LATEST=class-system-latest.plt
CCS=login.ccs.neu.edu
CCS_PATH=/course/cs2510h/.www/

all:   clean class web
push:  clean class web-scp

clean: clean-class clean-web

#
# class
#

.PHONY: class
class: clean-class copy-doc
	(cd .. && raco pack \
		-v --at-plt --replace \
		++setup class0 \
		++setup class1 \
		++setup class2 \
		++setup class3 \
		++setup scribblings/main \
		$(CLASS) \
		class0/ class1/ class2/ class3/)

clean-class:
	find ../class* -name compiled -type d | xargs rm -fr
	rm -fr ../class-system-*.plt

copy-doc:
	rm -fr ../class3/scribblings/
	rm -fr ../class2/scribblings/
	rm -fr ../class1/scribblings/
	rm -fr ../class0/scribblings/
	cp -r ./class/ ../class3/scribblings/
	cp -r ./class/ ../class2/scribblings/
	cp -r ./class/ ../class1/scribblings/
	cp -r ./class/ ../class0/scribblings/


#
# web
#

web:
	ln -sf ../$(CLASS) $(CLASS)
	ln -sf ../$(CLASS) $(CLASS_LATEST)
	env CLASS=$(CLASS) raco make 2510H.scrbl
	raco scribble --htmls \
		++extra fonts/kulminoituva-webfont.eot \
		++extra fonts/kulminoituva-webfont.svg \
		++extra fonts/kulminoituva-webfont.ttf \
		++extra fonts/kulminoituva-webfont.woff \
		++extra $(CLASS) \
		++extra $(CLASS_LATEST) \
		++extra ../notes/notes.pdf \
		++extra ../notes/notes-2up.pdf \
		++xref-in setup/xref load-collections-xref \
		--redirect-main http://docs.racket-lang.org/ \
		--style 2510H.css \
		2510H.scrbl

web-students:
	env STUDENTS=1 make web

clean-web:
	find . -name compiled -type d | xargs rm -fr
	rm -rf 2510H/ class-system-*.plt

#
# web-scp
#

web-scp: clean-web web-students	
	scp -r 2510H/* $(CCS):$(CCS_PATH)
	make web-perms

web-perms:
	ssh $(CCS) chmod -Rf g+w $(CCS_PATH) || true
