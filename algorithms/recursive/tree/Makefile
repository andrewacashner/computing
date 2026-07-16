dirs = build
input = tree.c
output = build/tree.cx

gcc_make = gcc -Wall --pedantic --std=c23 -D_DEFAULT_SOURCE 

.PHONY: all clean

all: $(output)

build/%.cx : %.c | $(dirs)
	$(gcc_make) -o $@ $<

$(dirs) : 
	mkdir -p $(dirs)

clean:
	rm -rf $(dirs)
