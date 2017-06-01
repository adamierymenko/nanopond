all:
	cc -I/usr/local/include -L/usr/local/lib -Ofast -ftree-vectorize -o nanopond nanopond.c -lSDL2

clean:
	rm -rf *.o nanopond *.dSYM
