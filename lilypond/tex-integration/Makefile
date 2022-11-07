dirs    = aux build build/img
tex_in  = $(wildcard *.tex)
pdf_out = $(addprefix build/,$(tex_in:%.tex=%.pdf))

ly_in   = $(wildcard music-examples/*.ly)
ly_img  = $(addprefix aux/,$(notdir $(ly_in:%.ly=%.pdf)))
img_out = $(addprefix build/img/,$(notdir $(ly_img)))

.PHONY : all clean

all : $(pdf_out) $(img_out)

build/%.pdf : aux/%.pdf
	cp $< $@

aux/%.pdf : %.tex $(img_out) | $(dirs)
	latexmk -outdir=aux -pdf $<

build/img/%.pdf : aux/%.pdf
	pdfcrop $< $@

aux/%.pdf : music-examples/%.ly | $(dirs)
	lilypond --output=aux/ $<

$(dirs) :
	mkdir -p $(dirs)

clean :
	rm -rf $(dirs)
