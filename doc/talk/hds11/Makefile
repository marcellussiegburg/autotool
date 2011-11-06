all : pres.pdf hand.pdf

%.pdf : %.tex
	pdflatex $<

.PHONY : $(wildcard *.tex)