dirs = build doc
src = $(wildcard *.java)
package = com/andrewcashner/musarithmetic
class = $(addprefix build/$(package)/,$(src:%.java=%.class))
doc = doc/index.html
jar = build/Musarithmetic.jar
manifest = MANIFEST.MF

.PHONY: all view clean

all: $(doc) $(jar)

$(dirs):
	mkdir -p $(dirs)

build/$(package)/%.class:	%.java | $(dirs)
	javac -cp build -d build $<

$(doc): $(class)
	javadoc -d doc -author -version *.java

$(jar): $(class) $(manifest) 
	jar --create --file $(jar) --manifest $(manifest) -C build .

view:	$(doc)
	xdg-open $(doc) &

clean: 
	rm -rf $(dirs)
