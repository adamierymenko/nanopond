# Makefile to compile nanopond.c

CC=gcc
CFLAGS=-O6 -fomit-frame-pointer -ftree-vectorize -Wall -pipe

all:
	$(CC) $(CFLAGS) -o nanopond nanopond.c -lSDL

clean:
	rm -f nanopond nanopond.gc??
