all:
	cc -I/usr/local/include -L/usr/local/lib -Ofast -o nanopond nanopond.c -lSDL2

clean:
	rm -rf *.o nanopond *.dSYM
