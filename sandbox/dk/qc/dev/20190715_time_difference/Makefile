SOURCES=$(wildcard *.Rmd)
OBJECTS=$(SOURCES:.Rmd=.pdf)
all: $(OBJECTS)
%.pdf: %.Rmd
	R --no-save -e 'library(rmarkdown); render("$<", "pdf_document")'
