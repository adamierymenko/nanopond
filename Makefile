all:
	cc -I/usr/local/include -L/usr/local/lib -Ofast -o nanopond nanopond.c -lSDL2 -lpthread

clean:
	rm -rf *.o nanopond *.dSYM
