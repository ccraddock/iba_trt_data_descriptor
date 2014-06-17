all: vtcri_trt_scidata.tex vtcri_trt.bib demo_table.tex scanning_params_table.tex
	pdflatex vtcri_trt_scidata
	bibtex vtcri_trt_scidata
	pdflatex vtcri_trt_scidata
	pdflatex vtcri_trt_scidata
	