# You'll need to install the gtk development library
# On Debian: apt install libgtk-3-devel

dirs = bin
in = $(wildcard *.c)
out = $(addprefix bin/,$(notdir $(basename $(in))))

.PHONY : all clean

all : $(out)

bin/% : %.c | $(dirs)
	gcc -Wall -pedantic -o $@ $< \
		`pkg-config gtk+-3.0 --cflags --libs`

$(dirs) :
	mkdir -p $(dirs)

clean :
	rm -rf $(dirs)

