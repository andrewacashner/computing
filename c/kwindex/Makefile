# Makefile for kwindex.c
# Andrew Cashner, 2014/04/20

CC = gcc
CFLAGS = -Wall -Wextra -pedantic -ansi

DEBUG ?= 0 
ifeq ($(DEBUG), 0)
    CFLAGS += -g -DDEBUG
endif

LFLAGS = -lsqlite3 -lpthread -ldl

# LIBS

TARGET = kwindex.cx

INCLUDEDIR = include
SOURCEDIR = src

INCLUDES = -I$(INCLUDEDIR) 
SOURCES = $(wildcard $(SOURCEDIR)/*.c)

OBJECTS = $(patsubst $(SOURCEDIR)/%.c, $(BUILDDIR)/%.o,$(SOURCES:.c=.o))

MAIN = kwindex.cx

.PHONY: depend clean

all: $(MAIN)
	@echo Compiled $(TARGET)

$(MAIN): $(OBJECTS)
	$(CC) $(CFLAGS) $(INCLUDES) -o $(MAIN) $(OBJECTS) $(LFLAGS) $(LIBS)

.c.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@

clean:
	$(RM) $(OBJECTS) $(MAIN)

depend: $(SOURCES)
	makedepend $(INCLUDES) $^




