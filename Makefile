.SUFFIXES: .org .tex .pdf .R
MSG = "debug"
SRC = $(shell ls *.org)
TEX = $(SRC:%.org=%.tex)
PDF = $(TEX:%.tex=%.pdf)
RCODE = $(SRC:%.org=%.R)

all:	$(PDF) $(RCODE)

.org.tex:
	echo you need to export latex file from $<

.org.R:
	echo you need to tangle R source code from $<

.tex.pdf:
	latexmk $<

$(PDF): $(TEX)

$(RCODE): $(SRC)

push:
	git add -u
	git commit -m ${MSG}
	git push -u origin master

clean:
	latexmk -C $(TEX)
	rm -f *~

