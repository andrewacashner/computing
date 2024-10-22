prog = parse
dirs = obj bin

lib = $(wildcard src/*.c)
obj = $(addprefix obj/,$(notdir $(lib:%.c=%.o)))
main = $(prog).c
bin = bin/$(prog)

CFLAGS = -std=c17 -Wall -pedantic

# to debug, run 'make mode=debug'
ifeq ($(mode), debug)
	CFLAGS += -DDEBUG
endif

.PHONY : all debug run clean

all : $(bin)

$(bin) : $(main) $(obj) | $(dirs)
	gcc $(CFLAGS) -o $(bin) $(obj) $(main) -lm

$(dirs) :
	mkdir -p $(dirs)

obj/%.o : src/%.c | $(dirs)
	gcc $(CFLAGS) -c -o $@ $< -lm

run : 
	./$(bin)

clean :
	rm -rf obj bin
	




