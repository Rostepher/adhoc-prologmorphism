# Source files
TEX_FILES=adhoc_language.tex work_report.tex

# Binaries
PDFLATEX=pdflatex

# Generated files
PDF_FILES=$(TEX_FILES:.tex=.pdf)
CLEAN_FILES=*.aux *.log *.out

.PHONY: all
all: pdf

.PHONY: pdf
pdf: $(PDF_FILES)

.PHONY: clean
clean:
	$(RM) $(PDF_FILES) $(CLEAN_FILES)

%.pdf %.log: %.tex
	$(PDFLATEX) $<
