# You'll need to install the gtk development library
# On Debian: apt install libgtk-3-devel

dirs = bin
in = $(wildcard *.c)
out = $(addprefix bin/,$(notdir $(basename $(in))))

.PHONY : all clean

all : $(out)

bin/% : %.c | $(dirs)
	gcc `pkg-config --cflags gtk+-3.0` \
		-Wall -pedantic -o $@ $< \
		`pkg-config --libs gtk+-3.0`

$(dirs) :
	mkdir -p $(dirs)

clean :
	rm -rf $(dirs)

