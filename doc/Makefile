# Source files
TEXFILES=adhoc_language.tex

# Binaries
PDFLATEX=pdflatex

# Generated files
PDFFILES=$(TEXFILES:.tex=.pdf)

.PHONY: all clean pdf

all: pdf

pdf: $(PDFFILES)

clean:
	$(RM) $(PDFFILES)

%.pdf %.log: %.tex
	$(PDFLATEX) $<
