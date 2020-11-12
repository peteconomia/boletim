MAIN_TEX=boletim.tex
OUTPUT=boletim.pdf

# build using pdflatex - pdftex - engine
create:
	latexmk -pdf ${MAIN_TEX}
	@make clean

basic:
	pdflatex ${MAIN_TEX}
	@make clean

clean:
	rm -f *~ *.aux *.log *.dvi *.ps *.bbl *.blg *.out *.snm *.toc *.nav *.lof *.fls *.xdv *.fdb_latexmk .log __latexindent_temp.tex

cleanall: clean
	rm ${OUTPUT}
