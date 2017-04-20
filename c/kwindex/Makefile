CC = gcc

CFLAGS = -g -Wall -Wextra -pedantic -ansi

# LFLAGS

# LIBS

TARGET = kwindex.cx

INCLUDES = -I./include 

SOURCES = $(wildcard src/*.c)

OBJECTS = $(SOURCES:.c=.o)

MAIN = kwindex

.PHONY: depend clean

all: $(MAIN)
	@echo Compiled kwindex

$(MAIN): $(OBJECTS)
	$(CC) $(CFLAGS) $(INCLUDES) -o $(MAIN).cx $(OBJECTS) $(LFLAGS) $(LIBS)

.c.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@

clean:
	$(RM) *.o *~ $(MAIN)

depend: $(SOURCES)
	makedepend $(INCLUDES) $^




