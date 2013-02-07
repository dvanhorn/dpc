all: quick-lists1.png quick-lists2.png quick-lists3.png quick-lists4.png

%.png: %.pdf
	convert -define pdf:use-cropbox=true -transparent "#000000" $< $@

