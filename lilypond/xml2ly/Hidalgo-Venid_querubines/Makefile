base 	= score
input   = $(base).xml
include = music.xml
mei_out	= $(base).mei
pdf_out	= $(base).pdf
output 	= $(mei_out) $(pdf_out)
tomei 	= lirio2mei.xsl
toly 	= lirio2ly.xsl
libly 	= $(HOME)/ly

.PHONY: all pdf mei clean

all: pdf mei

pdf: $(pdf_out)

mei: $(mei_out)

$(output) : $(input) $(include)


%.mei : %-noxi.mei
	xmllint --xinclude --format $< --output $@

%-noxi.mei : %.xml $(tomei)
	saxon -s:$< -xsl:$(tomei) -o:$@


%.pdf : %.ly $(libly)
	lilypond -I $(libly) $<

%.ly : %-xi.xml $(toly)
	saxon -s:$< -xsl:$(toly) -o:$@

%-xi.xml : %.xml 
	xmllint --xinclude $< --output $@

view : pdf
	mupdf $(pdf_out)

clean : 
	-rm -rf $(output)

