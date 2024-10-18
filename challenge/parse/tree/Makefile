prog = parse
dirs = obj bin

lib = $(wildcard src/*.c)
obj = $(addprefix obj/,$(notdir $(lib:%.c=%.o)))
main = $(prog).c
bin = bin/$(prog)

gcc = gcc -std=c17 -Wall -pedantic

.PHONY : all run clean

all : $(bin)

$(bin) : $(main) $(obj) | $(dirs)
	$(gcc) -o $(bin) $(obj) $(main)

$(dirs) :
	mkdir -p $(dirs)

obj/%.o : src/%.c | $(dirs)
	$(gcc) -c -o $@ $<

run : 
	./$(bin)

clean :
	rm -rf obj bin
	




