


$.PHONY: draft clean distclean

draft: draft.tex
	pdflatex draft.tex && \
		bibtex draft.aux && \
		pdflatex draft.tex && \
		pdflatex draft.tex


# Ignore these file types in general:
clean:
	-rm *.aux
	-rm *.bbl
	-rm *.blg
	-rm *.log
	-rm *.out

distclean:
	-rm draft.pdf
