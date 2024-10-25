# You'll need to install the sdl development library
# On Debian: apt install libsdl2-dev

dirs = bin bin/img
in = $(wildcard *.c)
out = $(addprefix bin/,$(notdir $(basename $(in))))
img_in = $(wildcard img/*.bmp)
img_out = $(addprefix bin/img/,$(notdir $(img_in)))

.PHONY : all clean

all : $(out) $(img_out)

bin/% : %.c $(img_out) | $(dirs)
	gcc -Wall -pedantic -o $@ $< \
		`pkg-config sdl2 --cflags --libs`

bin/img/%.bmp : img/%.bmp | $(dirs)
	cp -u $< $@

$(dirs) :
	mkdir -p $(dirs)

clean :
	rm -rf $(dirs)

