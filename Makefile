all:
	cc -O3 -o nanopond nanopond.c -lSDL2

clean:
	rm -rf *.o nanopond *.dSYM
