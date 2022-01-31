MAIN_TEX=boletim.tex
OUTPUT=boletim.pdf

# build using pdflatex - pdftex - engine
build:
	latexmk -pdf ${MAIN_TEX}

basic:
	pdflatex ${MAIN_TEX}

buildcaps:
	for i in conteudo/**/main.tex; do \
		latexmk -pdf -cd $$i; \
	done

clean:
	rm -f *~ *.aux *.log *.dvi *.ps *.bbl *.blg *.out *.snm *.toc *.nav *.lof *.fls *.xdv *.fdb_latexmk .log __latexindent_temp.tex

cleanall: clean
	rm ${OUTPUT}

publish: clean build