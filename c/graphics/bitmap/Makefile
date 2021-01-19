CFLAGS = -Wall -ansi -pedantic

src = $(wildcard *.c)
obj = $(src:%.c=%.o)
exe = $(src:%.c=%)

.PHONY: all clean

all : $(exe)

$(exe) : $(obj)

$(obj) : $(src)

clean :
	rm -i $(exe)

