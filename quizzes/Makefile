TARGETS = $(basename $(wildcard *.tex))

all: $(addsuffix .pdf,$(TARGETS))

%.pdf: %.tex FORCE
	pdf-slatex $<
	@#$(RUBBER) --pdf $* \
		|| { cat $*.log | make grep-hidden-errors; rm -f $@; exit 1; } 

FORCE:

clean:
	@rubber -vv --clean --pdf $(TARGETS) 2>&1 | grep ^removing || true
	@rm -fv *.out .Z*.tex

SHELL = /bin/bash
