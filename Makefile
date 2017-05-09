all:
	cc -Ofast -ftree-vectorize -o nanopond nanopond.c -lSDL2

clean:
	rm -rf *.o nanopond *.dSYM
