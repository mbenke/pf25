MATHJAXURL="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML"
PANDOC=pandoc --mathjax=$(MATHJAXURL) 
SLIDY=$(PANDOC) -s -t slidy --slide-level=3
HTMLFILES=w01intro.html w02typy.html w03klasy.html w04nat.html w05list.html \
    w05io.html w06lazy.html w08functor.html w09monad1.html w10rap.html \
    w11test.html w12parse.html \
    w04nat-list.html


#TARGET=benke.org:www.marcin.org/pf25/
TARGET=las.marcin.org:www/pf25/

all: Makefile wyklady

up: all
	rsync -z *.html $(TARGET)

wyklady: $(HTMLFILES)


%.html: %.md
	$(SLIDY) -o $@ $<

%.pptx: %.md
	$(PANDOC) -t pptx -o $@ $<

distclean:
	-rm -f $(HTMLFILES)
