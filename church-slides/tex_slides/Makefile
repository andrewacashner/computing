# Makefile for slide-making system
# Andrew A. Cashner, 2020/02/23

dirs 	= aux build
in 	= $(wildcard *.tex)
pdf_out = $(addprefix build/,$(in:%.tex=%.pdf))
ppt_out = $(pdf_out:%.pdf=%.pptx)
tex_lib = $(wildcard *.cls)

.PHONY : all pdf ppt clean view-pdf view-ppt

all : ppt 

ppt : $(ppt_out)

pdf : $(pdf_out)

$(ppt_out) : $(in)

build/%.pptx : build/%.pdf
	soffice --infilter="impress_pdf_import" --convert-to pptx \
	    --outdir build $<

build/%.pdf : aux/%.pdf
	cp $< $@

aux/%.pdf : %.tex $(tex_lib) | $(dirs)
	latexmk -outdir=aux -pdf $<

$(dirs) : 
	mkdir -p $(dirs)

view-pdf : pdf
	xdg-open $(pdf_out) &

view-ppt : ppt
	xdg-open $(ppt_out) &

clean : 
	rm -rf $(dirs)
