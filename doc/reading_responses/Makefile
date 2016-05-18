# Source files
TEX_FILES= \
	a_language_for_computational_algebra.tex \
	a_second_look_at_overloading.tex \
	how_to_make_ad-hoc_polymorphism_less_ad_hoc.tex \
	implementing_haskell_overloading.tex \
	type_classes_in_haskell.tex

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
