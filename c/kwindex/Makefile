# Makefile for kwindex.c
# Andrew Cashner, 2014/04/20

CC = gcc
CFLAGS = -Wall -Wextra -pedantic -ansi

LFLAGS = -lsqlite3 -lpthread -ldl 
# -DSQLITE_ENABLE_ICU -L/usr/local/lib/ -lSqliteIcu

TARGET = kwindex.cx

INCLUDEDIR = include
SOURCEDIR = src

INCLUDES = -I$(INCLUDEDIR) 
SOURCES = $(wildcard $(SOURCEDIR)/*.c)

OBJECTS = $(patsubst $(SOURCEDIR)/%.c, $(BUILDDIR)/%.o,$(SOURCES:.c=.o))

MAIN = kwindex.cx

.PHONY: depend clean

all: executable
	
executable: $(MAIN)
	@echo Compiled $(TARGET)

$(MAIN): $(OBJECTS)
	$(CC) $(CFLAGS) $(INCLUDES) -o $(MAIN) $(OBJECTS) $(LFLAGS) 

debug: CFLAGS += -g -DDEBUG
debug: executable
	@echo Debug mode set

.c.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@

clean:
	$(RM) $(OBJECTS) $(MAIN)

depend: $(SOURCES)
	makedepend $(INCLUDES) $^




