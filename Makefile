# Makefile to compile nanopond.c

CC=gcc
CFLAGS=-O6 -march=athlon-mp -mtune=athlon-mp -mmmx -msse -fomit-frame-pointer -fprofile-use -ftree-vectorize -funroll-loops -fstrength-reduce -ftree-vectorizer-verbose=5 -Wall -pipe -s

all:
	$(CC) $(CFLAGS) -o nanopond nanopond.c -lSDL

clean:
	rm -f nanopond nanopond.gc??
