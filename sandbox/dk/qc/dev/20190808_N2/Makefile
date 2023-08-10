SOURCES=$(wildcard *.R)
OBJECTS=$(SOURCES:.R=.pdf)
all: $(OBJECTS)
%.pdf: %.R
	R --no-save < $<
