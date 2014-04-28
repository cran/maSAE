NAME=maSAE
R_HOME=/usr/lib/R
all: 
	$(MAKE) ${NAME}.pdf
	$(MAKE) clean
${NAME}.pdf: ${NAME}.tex
	texi2dvi --pdf ${NAME}.tex
${NAME}.tex:
	"$(R_HOME)/bin/R" CMD Sweave ${NAME}.Rnw
clean:	
	rm ${NAME}.log || true
	rm ${NAME}.aux || true
	rm ${NAME}.blg || true
	rm ${NAME}.bbl || true
	rm ${NAME}.tex
