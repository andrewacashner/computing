dirs = build
xml = $(wildcard *.xml)
xsl = ly.xsl
out = build/score.pdf

.PHONY: all view clean

all : $(out)

$(out) : $(xml) $(xsl)

build/%.pdf : %.ly | $(dirs)
	lilypond -o $(basename $@) $<

%.ly : xi-%.xml $(xsl) 
	saxon -s:$<i -xsl:$(xsl) -o:$@

xi-%.xml : %.xml
	xmllint --xinclude $< --output $@

$(dirs) :
	-mkdir -p $(dirs)

view : all
	mupdf $(out)

clean : 
	-rm -rf $(dirs)
