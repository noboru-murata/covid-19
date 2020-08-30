.SUFFIXES: .org .tex .pdf .R
MSG = "debug"
ORG = $(shell ls [^R]*.org)
TEX = $(ORG:%.org=%.tex)
PDF = $(TEX:%.tex=%.pdf)
RCODE = $(ORG:%.org=%.R)

all:	$(PDF) $(RCODE)

.org.tex:
	echo you need to export latex file from $<

.org.R:
	echo you need to tangle R source code from $<

.tex.pdf:
	latexmk $<

$(TEX): $(ORG)

$(PDF): $(TEX)

$(RCODE): $(ORG)

push:
	git add -u
	git commit -m ${MSG}
	git push -u origin master

clean:
	latexmk -C $(TEX)
	rm -f *~

