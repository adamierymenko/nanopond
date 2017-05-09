/* *********************************************************************** */
/*                                                                         */
/* Nanopond version 2.0 -- A teeny tiny artificial life virtual machine    */
/* Copyright (C) Adam Ierymenko                                            */
/* MIT license -- see LICENSE.txt                                          */
/*                                                                         */
/* *********************************************************************** */

/*
 * Changelog:
 *
 * 1.0 - Initial release
 * 1.1 - Made empty cells get initialized with 0xffff... instead of zeros
 *       when the simulation starts. This makes things more consistent with
 *       the way the output buf is treated for self-replication, though
 *       the initial state rapidly becomes irrelevant as the simulation
 *       gets going.  Also made one or two very minor performance fixes.
 * 1.2 - Added statistics for execution frequency and metabolism, and made
 *       the visualization use 16bpp color.
 * 1.3 - Added a few other statistics.
 * 1.4 - Replaced SET with KILL and changed EAT to SHARE. The SHARE idea
 *       was contributed by Christoph Groth (http://www.falma.de/). KILL
 *       is a variation on the original EAT that is easier for cells to
 *       make use of.
 * 1.5 - Made some other instruction changes such as XCHG and added a
 *       penalty for failed KILL attempts. Also made access permissions
 *       stochastic.
 * 1.6 - Made cells all start facing in direction 0. This removes a bit
 *       of artificiality and requires cells to evolve the ability to
 *       turn in various directions in order to reproduce in anything but
 *       a straight line. It also makes pretty graphics.
 * 1.7 - Added more statistics, such as original lineage, and made the
 *       genome dump files CSV files as well.
 * 1.8 - Fixed LOOP/REP bug reported by user Sotek.  Thanks!  Also
 *       reduced the default mutation rate a bit.
 * 1.9 - Added a bunch of changes suggested by Christoph Groth: a better
 *       coloring algorithm, right click to switch coloring schemes (two
 *       are currently supported), and a few speed optimizations. Also
 *       changed visualization so that cells with generations less than 2
 *       are no longer shown.
 * 2.0 - Ported to SDL2 by Charles Huber, and added simple pthread based
 *       threading to make it take advantage of modern machines.
 */

/*
 * Nanopond is just what it says: a very very small and simple artificial
 * life virtual machine.
 *
 * It is a "small evolving program" based artificial life system of the same
 * general class as Tierra, Avida, and Archis.  It is written in very tight
 * and efficient C code to make it as fast as possible, and is so small that
 * it consists of only one .c file.
 *
 * How Nanopond works:
 *
 * The Nanopond world is called a "pond."  It is an NxN two dimensional
 * array of Cell structures, and it wraps at the edges (it's toroidal).
 * Each Cell structure consists of a few attributes that are there for
 * statistics purposes, an energy level, and an array of POND_DEPTH
 * four-bit values.  (The four-bit values are actually stored in an array
 * of machine-size words.)  The array in each cell contains the genome
 * associated with that cell, and POND_DEPTH is therefore the maximum
 * allowable size for a cell genome.
 *
 * The first four bit value in the genome is called the "logo." What that is
 * for will be explained later. The remaining four bit values each code for
 * one of 16 instructions. Instruction zero (0x0) is NOP (no operation) and
 * instruction 15 (0xf) is STOP (stop cell execution). Read the code to see
 * what the others are. The instructions are exceptionless and lack fragile
 * operands. This means that *any* arbitrary sequence of instructions will
 * always run and will always do *something*. This is called an evolvable
 * instruction set, because programs coded in an instruction set with these
 * basic characteristics can mutate. The instruction set is also
 * Turing-complete, which means that it can theoretically do anything any
 * computer can do. If you're curious, the instruciton set is based on this:
 * http://www.muppetlabs.com/~breadbox/bf/
 *
 * At the center of Nanopond is a core loop. Each time this loop executes,
 * a clock counter is incremented and one or more things happen:
 *
 * - Every REPORT_FREQUENCY clock ticks a line of comma seperated output
 *   is printed to STDOUT with some statistics about what's going on.
 * - Every INFLOW_FREQUENCY clock ticks a random x,y location is picked,
 *   energy is added (see INFLOW_RATE_MEAN and INFLOW_RATE_DEVIATION)
 *   and it's genome is filled with completely random bits.  Statistics
 *   are also reset to generation==0 and parentID==0 and a new cell ID
 *   is assigned.
 * - Every tick a random x,y location is picked and the genome inside is
 *   executed until a STOP instruction is encountered or the cell's
 *   energy counter reaches zero. (Each instruction costs one unit energy.)
 *
 * The cell virtual machine is an extremely simple register machine with
 * a single four bit register, one memory pointer, one spare memory pointer
 * that can be exchanged with the main one, and an output buffer. When
 * cell execution starts, this output buffer is filled with all binary 1's
 * (0xffff....). When cell execution is finished, if the first byte of
 * this buffer is *not* 0xff, then the VM says "hey, it must have some
 * data!". This data is a candidate offspring; to reproduce cells must
 * copy their genome data into the output buffer.
 *
 * When the VM sees data in the output buffer, it looks at the cell
 * adjacent to the cell that just executed and checks whether or not
 * the cell has permission (see below) to modify it. If so, then the
 * contents of the output buffer replace the genome data in the
 * adjacent cell. Statistics are also updated: parentID is set to the
 * ID of the cell that generated the output and generation is set to
 * one plus the generation of the parent.
 *
 * A cell is permitted to access a neighboring cell if:
 *    - That cell's energy is zero
 *    - That cell's parentID is zero
 *    - That cell's logo (remember?) matches the trying cell's "guess"
 *
 * Since randomly introduced cells have a parentID of zero, this allows
 * real living cells to always replace them or eat them.
 *
 * The "guess" is merely the value of the register at the time that the
 * access attempt occurs.
 *
 * Permissions determine whether or not an offspring can take the place
 * of the contents of a cell and also whether or not the cell is allowed
 * to EAT (an instruction) the energy in it's neighbor.
 *
 * If you haven't realized it yet, this is why the final permission
 * criteria is comparison against what is called a "guess." In conjunction
 * with the ability to "eat" neighbors' energy, guess what this permits?
 *
 * Since this is an evolving system, there have to be mutations. The
 * MUTATION_RATE sets their probability. Mutations are random variations
 * with a frequency defined by the mutation rate to the state of the
 * virtual machine while cell genomes are executing. Since cells have
 * to actually make copies of themselves to replicate, this means that
 * these copies can vary if mutations have occurred to the state of the
 * VM while copying was in progress.
 *
 * What results from this simple set of rules is an evolutionary game of
 * "corewar." In the beginning, the process of randomly generating cells
 * will cause self-replicating viable cells to spontaneously emerge. This
 * is something I call "random genesis," and happens when some of the
 * random gak turns out to be a program able to copy itself. After this,
 * evolution by natural selection takes over. Since natural selection is
 * most certainly *not* random, things will start to get more and more
 * ordered and complex (in the functional sense). There are two commodities
 * that are scarce in the pond: space in the NxN grid and energy. Evolving
 * cells compete for access to both.
 *
 * If you want more implementation details such as the actual instruction
 * set, read the source. It's well commented and is not that hard to
 * read. Most of it's complexity comes from the fact that four-bit values
 * are packed into machine size words by bit shifting. Once you get that,
 * the rest is pretty simple.
 *
 * Nanopond, for it's simplicity, manifests some really interesting
 * evolutionary dynamics. While I haven't run the kind of multiple-
 * month-long experiment necessary to really see this (I might!), it
 * would appear that evolution in the pond doesn't get "stuck" on just
 * one or a few forms the way some other simulators are apt to do.
 * I think simplicity is partly reponsible for this along with what
 * biologists call embeddedness, which means that the cells are a part
 * of their own world.
 *
 * Run it for a while... the results can be... interesting!
 *
 * Running Nanopond:
 *
 * Nanopond can use SDL (Simple Directmedia Layer) for screen output. If
 * you don't have SDL, comment out USE_SDL below and you'll just see text
 * statistics and get genome data dumps. (Turning off SDL will also speed
 * things up slightly.)
 *
 * After looking over the tunable parameters below, compile Nanopond and
 * run it. Here are some example compilation commands from Linux:
 *
 * For Pentiums:
 *  gcc -O6 -march=pentium -funroll-loops -fomit-frame-pointer -s
 *   -o nanopond nanopond.c -lSDL
 *
 * For Athlons with gcc 4.0+:
 *  gcc -O6 -msse -mmmx -march=athlon -mtune=athlon -ftree-vectorize
 *   -funroll-loops -fomit-frame-pointer -o nanopond nanopond.c -lSDL
 *
 * The second line is for gcc 4.0 or newer and makes use of GCC's new
 * tree vectorizing feature. This will speed things up a bit by
 * compiling a few of the loops into MMX/SSE instructions.
 *
 * This should also work on other Posix-compliant OSes with relatively
 * new C compilers. (Really old C compilers will probably not work.)
 * On other platforms, you're on your own! On Windows, you will probably
 * need to find and download SDL if you want pretty graphics and you
 * will need a compiler. MinGW and Borland's BCC32 are both free. I
 * would actually expect those to work better than Microsoft's compilers,
 * since MS tends to ignore C/C++ standards. If stdint.h isn't around,
 * you can fudge it like this:
 *
 * #define uintptr_t unsigned long (or whatever your machine size word is)
 * #define uint8_t unsigned char
 * #define uint16_t unsigned short
 * #define uint64_t unsigned long long (or whatever is your 64-bit int)
 *
 * When Nanopond runs, comma-seperated stats (see doReport() for
 * the columns) are output to stdout and various messages are output
 * to stderr. For example, you might do:
 *
 * ./nanopond >>stats.csv 2>messages.txt &
 *
 * To get both in seperate files.
 *
 * Have fun!
 */

/* ----------------------------------------------------------------------- */
/* Tunable parameters                                                      */
/* ----------------------------------------------------------------------- */

/* Frequency of comprehensive reports-- lower values will provide more
 * info while slowing down the simulation. Higher values will give less
 * frequent updates. */
/* This is also the frequency of screen refreshes if SDL is enabled. */
#define REPORT_FREQUENCY 100000

/* Mutation rate -- range is from 0 (none) to 0xffffffff (all mutations!) */
/* To get it from a float probability from 0.0 to 1.0, multiply it by
 * 4294967295 (0xffffffff) and round. */
#define MUTATION_RATE 21475 /* p=~0.000005 */

/* How frequently should random cells / energy be introduced?
 * Making this too high makes things very chaotic. Making it too low
 * might not introduce enough energy. */
#define INFLOW_FREQUENCY 100

/* Base amount of energy to introduce per INFLOW_FREQUENCY ticks */
#define INFLOW_RATE_BASE 4000

/* A random amount of energy between 0 and this is added to
 * INFLOW_RATE_BASE when energy is introduced. Comment this out for
 * no variation in inflow rate. */
#define INFLOW_RATE_VARIATION 8000

/* Size of pond in X and Y dimensions. */
#define POND_SIZE_X 800
#define POND_SIZE_Y 600

/* Depth of pond in four-bit codons -- this is the maximum
 * genome size. This *must* be a multiple of 16! */
#define POND_DEPTH 512

/* This is the divisor that determines how much energy is taken
 * from cells when they try to KILL a viable cell neighbor and
 * fail. Higher numbers mean lower penalties. */
#define FAILED_KILL_PENALTY 2

/* Define this to use SDL. To use SDL, you must have SDL headers
 * available and you must link with the SDL library when you compile. */
/* Comment this out to compile without SDL visualization support. */
#define USE_SDL 1

/* Define this to use threads, and how many threads to create */
#define USE_PTHREADS_COUNT 4

/* ----------------------------------------------------------------------- */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef USE_PTHREADS_COUNT
#include <pthread.h>
#endif

#ifdef USE_SDL
#ifdef _MSC_VER
#include <SDL.h>
#else
#include <SDL2/SDL.h>
#endif /* _MSC_VER */
#endif /* USE_SDL */

volatile uint64_t prngState[2];
static inline uintptr_t getRandom()
{
	// https://en.wikipedia.org/wiki/Xorshift#xorshift.2B
	uint64_t x = prngState[0];
	const uint64_t y = prngState[1];
	prngState[0] = y;
	x ^= x << 23;
	const uint64_t z = x ^ y ^ (x >> 17) ^ (y >> 26);
	prngState[1] = z;
	return (uintptr_t)(z + y);
}

/* Pond depth in machine-size words.  This is calculated from
 * POND_DEPTH and the size of the machine word. (The multiplication
 * by two is due to the fact that there are two four-bit values in
 * each eight-bit byte.) */
#define POND_DEPTH_SYSWORDS (POND_DEPTH / (sizeof(uintptr_t) * 2))

/* Number of bits in a machine-size word */
#define SYSWORD_BITS (sizeof(uintptr_t) * 8)

/* Constants representing neighbors in the 2D grid. */
#define N_LEFT 0
#define N_RIGHT 1
#define N_UP 2
#define N_DOWN 3

/* Word and bit at which to start execution */
/* This is after the "logo" */
#define EXEC_START_WORD 0
#define EXEC_START_BIT 4

/* Number of bits set in binary numbers 0000 through 1111 */
static const uintptr_t BITS_IN_FOURBIT_WORD[16] = { 0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4 };

/**
 * Structure for a cell in the pond
 */
struct Cell
{
	/* Globally unique cell ID */
	uint64_t ID;
	
	/* ID of the cell's parent */
	uint64_t parentID;
	
	/* Counter for original lineages -- equal to the cell ID of
	 * the first cell in the line. */
	uint64_t lineage;
	
	/* Generations start at 0 and are incremented from there. */
	uintptr_t generation;
	
	/* Energy level of this cell */
	uintptr_t energy;

	/* Memory space for cell genome (genome is stored as four
	 * bit instructions packed into machine size words) */
	uintptr_t genome[POND_DEPTH_SYSWORDS];

#ifdef USE_PTHREADS_COUNT
	pthread_mutex_t lock;
#endif
};

/* The pond is a 2D array of cells */
static struct Cell pond[POND_SIZE_X][POND_SIZE_Y];

/* This is used to generate unique cell IDs */
static volatile uint64_t cellIdCounter = 0;

/* Currently selected color scheme */
enum { KINSHIP,LINEAGE,MAX_COLOR_SCHEME } colorScheme = KINSHIP;
static const char *colorSchemeName[2] = { "KINSHIP", "LINEAGE" };

#ifdef USE_SDL
static SDL_Window *window;
static SDL_Surface *winsurf;
static SDL_Surface *screen;
#endif

volatile struct {
	/* Counts for the number of times each instruction was
	 * executed since the last report. */
	double instructionExecutions[16];
	
	/* Number of cells executed since last report */
	double cellExecutions;
	
	/* Number of viable cells replaced by other cells' offspring */
	uintptr_t viableCellsReplaced;
	
	/* Number of viable cells KILLed */
	uintptr_t viableCellsKilled;
	
	/* Number of successful SHARE operations */
	uintptr_t viableCellShares;
} statCounters;

static void doReport(const uint64_t clock)
{
	static uint64_t lastTotalViableReplicators = 0;
	
	uintptr_t x,y;
	
	uint64_t totalActiveCells = 0;
	uint64_t totalEnergy = 0;
	uint64_t totalViableReplicators = 0;
	uintptr_t maxGeneration = 0;
	
	for(x=0;x<POND_SIZE_X;++x) {
		for(y=0;y<POND_SIZE_Y;++y) {
			struct Cell *const c = &pond[x][y];
			if (c->energy) {
				++totalActiveCells;
				totalEnergy += (uint64_t)c->energy;
				if (c->generation > 2)
					++totalViableReplicators;
				if (c->generation > maxGeneration)
					maxGeneration = c->generation;
			}
		}
	}
	
	/* Look here to get the columns in the CSV output */
	
	/* The first five are here and are self-explanatory */
	printf("%llu,%llu,%llu,%llu,%llu,%llu,%llu,%llu",
		(uint64_t)clock,
		(uint64_t)totalEnergy,
		(uint64_t)totalActiveCells,
		(uint64_t)totalViableReplicators,
		(uint64_t)maxGeneration,
		(uint64_t)statCounters.viableCellsReplaced,
		(uint64_t)statCounters.viableCellsKilled,
		(uint64_t)statCounters.viableCellShares
		);
	
	/* The next 16 are the average frequencies of execution for each
	 * instruction per cell execution. */
	double totalMetabolism = 0.0;
	for(x=0;x<16;++x) {
		totalMetabolism += statCounters.instructionExecutions[x];
		printf(",%.4f",(statCounters.cellExecutions > 0.0) ? (statCounters.instructionExecutions[x] / statCounters.cellExecutions) : 0.0);
	}
	
	/* The last column is the average metabolism per cell execution */
	printf(",%.4f\n",(statCounters.cellExecutions > 0.0) ? (totalMetabolism / statCounters.cellExecutions) : 0.0);
	fflush(stdout);
	
	if ((lastTotalViableReplicators > 0)&&(totalViableReplicators == 0))
		fprintf(stderr,"[EVENT] Viable replicators have gone extinct. Please reserve a moment of silence.\n");
	else if ((lastTotalViableReplicators == 0)&&(totalViableReplicators > 0))
		fprintf(stderr,"[EVENT] Viable replicators have appeared!\n");
	
	lastTotalViableReplicators = totalViableReplicators;
	
	/* Reset per-report stat counters */
	for(x=0;x<sizeof(statCounters);++x)
		((uint8_t *)&statCounters)[x] = (uint8_t)0;
}

/**
 * Dumps the genome of a cell to a file.
 *
 * @param file Destination
 * @param cell Source
 */
static void dumpCell(FILE *file, struct Cell *cell)
{
	uintptr_t wordPtr,shiftPtr,inst,stopCount,i;

	if (cell->energy&&(cell->generation > 2)) {
		wordPtr = 0;
		shiftPtr = 0;
		stopCount = 0;
		for(i=0;i<POND_DEPTH;++i) {
			inst = (cell->genome[wordPtr] >> shiftPtr) & 0xf;
			/* Four STOP instructions in a row is considered the end.
			 * The probability of this being wrong is *very* small, and
			 * could only occur if you had four STOPs in a row inside
			 * a LOOP/REP pair that's always false. In any case, this
			 * would always result in our *underestimating* the size of
			 * the genome and would never result in an overestimation. */
			fprintf(file,"%x",(unsigned int)inst);
			if (inst == 0xf) { /* STOP */
				if (++stopCount >= 4)
					break;
			} else stopCount = 0;
			if ((shiftPtr += 4) >= SYSWORD_BITS) {
				if (++wordPtr >= POND_DEPTH_SYSWORDS) {
					wordPtr = EXEC_START_WORD;
					shiftPtr = EXEC_START_BIT;
				} else shiftPtr = 0;
			}
		}
	}
	fprintf(file,"\n");
}

static inline struct Cell *getNeighbor(const uintptr_t x,const uintptr_t y,const uintptr_t dir)
{
	/* Space is toroidal; it wraps at edges */
	switch(dir) {
		case N_LEFT:
			return (x) ? &pond[x-1][y] : &pond[POND_SIZE_X-1][y];
		case N_RIGHT:
			return (x < (POND_SIZE_X-1)) ? &pond[x+1][y] : &pond[0][y];
		case N_UP:
			return (y) ? &pond[x][y-1] : &pond[x][POND_SIZE_Y-1];
		case N_DOWN:
			return (y < (POND_SIZE_Y-1)) ? &pond[x][y+1] : &pond[x][0];
	}
	return &pond[x][y]; /* This should never be reached */
}

static inline int accessAllowed(struct Cell *const c2,const uintptr_t c1guess,int sense)
{
	/* Access permission is more probable if they are more similar in sense 0,
	 * and more probable if they are different in sense 1. Sense 0 is used for
	 * "negative" interactions and sense 1 for "positive" ones. */
	return sense ? (((getRandom() & 0xf) >= BITS_IN_FOURBIT_WORD[(c2->genome[0] & 0xf) ^ (c1guess & 0xf)])||(!c2->parentID)) : (((getRandom() & 0xf) <= BITS_IN_FOURBIT_WORD[(c2->genome[0] & 0xf) ^ (c1guess & 0xf)])||(!c2->parentID));
}

static inline uint8_t getColor(struct Cell *c)
{
	uintptr_t i,j,word,sum,opcode,skipnext;

	if (c->energy) {
		switch(colorScheme) {
			case KINSHIP:
				/*
				 * Kinship color scheme by Christoph Groth
				 *
				 * For cells of generation > 1, saturation and value are set to maximum.
				 * Hue is a hash-value with the property that related genomes will have
				 * similar hue (but of course, as this is a hash function, totally
				 * different genomes can also have a similar or even the same hue).
				 * Therefore the difference in hue should to some extent reflect the grade
				 * of "kinship" of two cells.
				 */
				if (c->generation > 1) {
					sum = 0;
					skipnext = 0;
					for(i=0;i<POND_DEPTH_SYSWORDS&&(c->genome[i] != ~((uintptr_t)0));++i) {
						word = c->genome[i];
						for(j=0;j<SYSWORD_BITS/4;++j,word >>= 4) {
							/* We ignore 0xf's here, because otherwise very similar genomes
							 * might get quite different hash values in the case when one of
							 * the genomes is slightly longer and uses one more maschine
							 * word. */
							opcode = word & 0xf;
							if (skipnext)
								skipnext = 0;
							else {
								if (opcode != 0xf)
									sum += opcode;
								if (opcode == 0xc) /* 0xc == XCHG */
									skipnext = 1; /* Skip "operand" after XCHG */
							}
						}
					}
					/* For the hash-value use a wrapped around sum of the sum of all
					 * commands and the length of the genome. */
					return (uint8_t)((sum % 192) + 64);
				}
				return 0;
			case LINEAGE:
				/*
				 * Cells with generation > 1 are color-coded by lineage.
				 */
				return (c->generation > 1) ? (((uint8_t)c->lineage) | (uint8_t)1) : 0;
			case MAX_COLOR_SCHEME:
				/* ... never used... to make compiler shut up. */
				break;
		}
	}
	return 0; /* Cells with no energy are black */
}

volatile int exitNow = 0;

static void *run(void *targ)
{
	const uintptr_t threadNo = (uintptr_t)targ;
	uintptr_t x,y,i;
	uintptr_t clock = 0;

	/* Buffer used for execution output of candidate offspring */
	uintptr_t outputBuf[POND_DEPTH_SYSWORDS];

	/* Miscellaneous variables used in the loop */
	uintptr_t currentWord,wordPtr,shiftPtr,inst,tmp;
	struct Cell *pptr,*tmpptr;
	
	/* Virtual machine memory pointer register (which
	 * exists in two parts... read the code below...) */
	uintptr_t ptr_wordPtr;
	uintptr_t ptr_shiftPtr;
	
	/* The main "register" */
	uintptr_t reg;
	
	/* Which way is the cell facing? */
	uintptr_t facing;
	
	/* Virtual machine loop/rep stack */
	uintptr_t loopStack_wordPtr[POND_DEPTH];
	uintptr_t loopStack_shiftPtr[POND_DEPTH];
	uintptr_t loopStackPtr;
	
	/* If this is nonzero, we're skipping to matching REP */
	/* It is incremented to track the depth of a nested set
	 * of LOOP/REP pairs in false state. */
	uintptr_t falseLoopDepth;

#ifdef USE_SDL
	SDL_Event sdlEvent;
	const uintptr_t sdlPitch = screen->pitch;
#endif

	/* If this is nonzero, cell execution stops. This allows us
	 * to avoid the ugly use of a goto to exit the loop. :) */
	int stop;

	/* Main loop */
	while (!exitNow) {
		/* Increment clock and run reports periodically */
		/* Clock is incremented at the start, so it starts at 1 */
		++clock;
		if ((threadNo == 0)&&(!(clock % REPORT_FREQUENCY))) {
			doReport(clock);
			/* SDL display is also refreshed every REPORT_FREQUENCY */
#ifdef USE_SDL
			while (SDL_PollEvent(&sdlEvent)) {
				if (sdlEvent.type == SDL_QUIT) {
					fprintf(stderr,"[QUIT] Quit signal received!\n");
					exitNow = 1;
				} else if (sdlEvent.type == SDL_MOUSEBUTTONDOWN) {
					switch (sdlEvent.button.button) {
						case SDL_BUTTON_LEFT:
							fprintf(stderr,"[INTERFACE] Genome of cell at (%d, %d):\n",sdlEvent.button.x, sdlEvent.button.y);
							dumpCell(stderr, &pond[sdlEvent.button.x][sdlEvent.button.y]);
							break;
						case SDL_BUTTON_RIGHT:
							colorScheme = (colorScheme + 1) % MAX_COLOR_SCHEME;
							fprintf(stderr,"[INTERFACE] Switching to color scheme \"%s\".\n",colorSchemeName[colorScheme]);
							for (y=0;y<POND_SIZE_Y;++y) {
								for (x=0;x<POND_SIZE_X;++x)
									((uint8_t *)screen->pixels)[x + (y * sdlPitch)] = getColor(&pond[x][y]);
							}
							break;
					}
				}
			}
			SDL_BlitSurface(screen, NULL, winsurf, NULL);
			SDL_UpdateWindowSurface(window);
#endif /* USE_SDL */
		}

		/* Introduce a random cell somewhere with a given energy level */
		/* This is called seeding, and introduces both energy and
		 * entropy into the substrate. This happens every INFLOW_FREQUENCY
		 * clock ticks. */
		if (!(clock % INFLOW_FREQUENCY)) {
			x = getRandom() % POND_SIZE_X;
			y = getRandom() % POND_SIZE_Y;
			pptr = &pond[x][y];

#ifdef USE_PTHREADS_COUNT
			pthread_mutex_lock(&(pptr->lock));
#endif

			pptr->ID = cellIdCounter;
			pptr->parentID = 0;
			pptr->lineage = cellIdCounter;
			pptr->generation = 0;
#ifdef INFLOW_RATE_VARIATION
			pptr->energy += INFLOW_RATE_BASE + (getRandom() % INFLOW_RATE_VARIATION);
#else
			pptr->energy += INFLOW_RATE_BASE;
#endif /* INFLOW_RATE_VARIATION */
			for(i=0;i<POND_DEPTH_SYSWORDS;++i) 
				pptr->genome[i] = getRandom();
			++cellIdCounter;
		
			/* Update the random cell on SDL screen if viz is enabled */
#ifdef USE_SDL
			((uint8_t *)screen->pixels)[x + (y * sdlPitch)] = getColor(pptr);
#endif /* USE_SDL */

#ifdef USE_PTHREADS_COUNT
			pthread_mutex_unlock(&(pptr->lock));
#endif
		}

		/* Pick a random cell to execute */
		i = getRandom();
		x = i % POND_SIZE_X;
		y = ((i / POND_SIZE_X) >> 1) % POND_SIZE_Y;
		pptr = &pond[x][y];

		/* Reset the state of the VM prior to execution */
		for(i=0;i<POND_DEPTH_SYSWORDS;++i)
			outputBuf[i] = ~((uintptr_t)0); /* ~0 == 0xfffff... */
		ptr_wordPtr = 0;
		ptr_shiftPtr = 0;
		reg = 0;
		loopStackPtr = 0;
		wordPtr = EXEC_START_WORD;
		shiftPtr = EXEC_START_BIT;
		facing = 0;
		falseLoopDepth = 0;
		stop = 0;

		/* We use a currentWord buffer to hold the word we're
		 * currently working on.  This speeds things up a bit
		 * since it eliminates a pointer dereference in the
		 * inner loop. We have to be careful to refresh this
		 * whenever it might have changed... take a look at
		 * the code. :) */
		currentWord = pptr->genome[0];

		/* Keep track of how many cells have been executed */
		statCounters.cellExecutions += 1.0;

		/* Core execution loop */
		while ((pptr->energy)&&(!stop)) {
			/* Get the next instruction */
			inst = (currentWord >> shiftPtr) & 0xf;

			/* Randomly frob either the instruction or the register with a
			 * probability defined by MUTATION_RATE. This introduces variation,
			 * and since the variation is introduced into the state of the VM
			 * it can have all manner of different effects on the end result of
			 * replication: insertions, deletions, duplications of entire
			 * ranges of the genome, etc. */
			if ((getRandom() & 0xffffffff) < MUTATION_RATE) {
				tmp = getRandom(); /* Call getRandom() only once for speed */
				if (tmp & 0x80) /* Check for the 8th bit to get random boolean */
					inst = tmp & 0xf; /* Only the first four bits are used here */
				else reg = tmp & 0xf;
			}

			/* Each instruction processed costs one unit of energy */
			--pptr->energy;

			/* Execute the instruction */
			if (falseLoopDepth) {
				/* Skip forward to matching REP if we're in a false loop. */
				if (inst == 0x9) /* Increment false LOOP depth */
					++falseLoopDepth;
				else if (inst == 0xa) /* Decrement on REP */
					--falseLoopDepth;
			} else {
				/* If we're not in a false LOOP/REP, execute normally */
				
				/* Keep track of execution frequencies for each instruction */
				statCounters.instructionExecutions[inst] += 1.0;
				
				switch(inst) {
					case 0x0: /* ZERO: Zero VM state registers */
						reg = 0;
						ptr_wordPtr = 0;
						ptr_shiftPtr = 0;
						facing = 0;
						break;
					case 0x1: /* FWD: Increment the pointer (wrap at end) */
						if ((ptr_shiftPtr += 4) >= SYSWORD_BITS) {
							if (++ptr_wordPtr >= POND_DEPTH_SYSWORDS)
								ptr_wordPtr = 0;
							ptr_shiftPtr = 0;
						}
						break;
					case 0x2: /* BACK: Decrement the pointer (wrap at beginning) */
						if (ptr_shiftPtr)
							ptr_shiftPtr -= 4;
						else {
							if (ptr_wordPtr)
								--ptr_wordPtr;
							else ptr_wordPtr = POND_DEPTH_SYSWORDS - 1;
							ptr_shiftPtr = SYSWORD_BITS - 4;
						}
						break;
					case 0x3: /* INC: Increment the register */
						reg = (reg + 1) & 0xf;
						break;
					case 0x4: /* DEC: Decrement the register */
						reg = (reg - 1) & 0xf;
						break;
					case 0x5: /* READG: Read into the register from genome */
						reg = (pptr->genome[ptr_wordPtr] >> ptr_shiftPtr) & 0xf;
						break;
					case 0x6: /* WRITEG: Write out from the register to genome */
						pptr->genome[ptr_wordPtr] &= ~(((uintptr_t)0xf) << ptr_shiftPtr);
						pptr->genome[ptr_wordPtr] |= reg << ptr_shiftPtr;
						currentWord = pptr->genome[wordPtr]; /* Must refresh in case this changed! */
						break;
					case 0x7: /* READB: Read into the register from buffer */
						reg = (outputBuf[ptr_wordPtr] >> ptr_shiftPtr) & 0xf;
						break;
					case 0x8: /* WRITEB: Write out from the register to buffer */
						outputBuf[ptr_wordPtr] &= ~(((uintptr_t)0xf) << ptr_shiftPtr);
						outputBuf[ptr_wordPtr] |= reg << ptr_shiftPtr;
						break;
					case 0x9: /* LOOP: Jump forward to matching REP if register is zero */
						if (reg) {
							if (loopStackPtr >= POND_DEPTH)
								stop = 1; /* Stack overflow ends execution */
							else {
								loopStack_wordPtr[loopStackPtr] = wordPtr;
								loopStack_shiftPtr[loopStackPtr] = shiftPtr;
								++loopStackPtr;
							}
						} else falseLoopDepth = 1;
						break;
					case 0xa: /* REP: Jump back to matching LOOP if register is nonzero */
						if (loopStackPtr) {
							--loopStackPtr;
							if (reg) {
								wordPtr = loopStack_wordPtr[loopStackPtr];
								shiftPtr = loopStack_shiftPtr[loopStackPtr];
								currentWord = pptr->genome[wordPtr];
								/* This ensures that the LOOP is rerun */
								continue;
							}
						}
						break;
					case 0xb: /* TURN: Turn in the direction specified by register */
						facing = reg & 3;
						break;
					case 0xc: /* XCHG: Skip next instruction and exchange value of register with it */
						if ((shiftPtr += 4) >= SYSWORD_BITS) {
							if (++wordPtr >= POND_DEPTH_SYSWORDS) {
								wordPtr = EXEC_START_WORD;
								shiftPtr = EXEC_START_BIT;
							} else shiftPtr = 0;
						}
						tmp = reg;
						reg = (pptr->genome[wordPtr] >> shiftPtr) & 0xf;
						pptr->genome[wordPtr] &= ~(((uintptr_t)0xf) << shiftPtr);
						pptr->genome[wordPtr] |= tmp << shiftPtr;
						currentWord = pptr->genome[wordPtr];
						break;
					case 0xd: /* KILL: Blow away neighboring cell if allowed with penalty on failure */
						tmpptr = getNeighbor(x,y,facing);
						if (accessAllowed(tmpptr,reg,0)) {
							if (tmpptr->generation > 2)
								++statCounters.viableCellsKilled;

							/* Filling first two words with 0xfffff... is enough */
							tmpptr->genome[0] = ~((uintptr_t)0);
							tmpptr->genome[1] = ~((uintptr_t)0);
							tmpptr->ID = cellIdCounter;
							tmpptr->parentID = 0;
							tmpptr->lineage = cellIdCounter;
							tmpptr->generation = 0;
							++cellIdCounter;
						} else if (tmpptr->generation > 2) {
							tmp = pptr->energy / FAILED_KILL_PENALTY;
							if (pptr->energy > tmp)
								pptr->energy -= tmp;
							else pptr->energy = 0;
						}
						break;
					case 0xe: /* SHARE: Equalize energy between self and neighbor if allowed */
						tmpptr = getNeighbor(x,y,facing);
						if (accessAllowed(tmpptr,reg,1)) {
#ifdef USE_PTHREADS_COUNT
							pthread_mutex_lock(&(tmpptr->lock));
#endif
							if (tmpptr->generation > 2)
								++statCounters.viableCellShares;
							tmp = pptr->energy + tmpptr->energy;
							tmpptr->energy = tmp / 2;
							pptr->energy = tmp - tmpptr->energy;
#ifdef USE_PTHREADS_COUNT
							pthread_mutex_unlock(&(tmpptr->lock));
#endif
						}
						break;
					case 0xf: /* STOP: End execution */
						stop = 1;
						break;
				}
			}
			
			/* Advance the shift and word pointers, and loop around
			 * to the beginning at the end of the genome. */
			if ((shiftPtr += 4) >= SYSWORD_BITS) {
				if (++wordPtr >= POND_DEPTH_SYSWORDS) {
					wordPtr = EXEC_START_WORD;
					shiftPtr = EXEC_START_BIT;
				} else shiftPtr = 0;
				currentWord = pptr->genome[wordPtr];
			}
		}

		/* Copy outputBuf into neighbor if access is permitted and there
		 * is energy there to make something happen. There is no need
		 * to copy to a cell with no energy, since anything copied there
		 * would never be executed and then would be replaced with random
		 * junk eventually. See the seeding code in the main loop above. */
		if ((outputBuf[0] & 0xff) != 0xff) {
			tmpptr = getNeighbor(x,y,facing);
#ifdef USE_PTHREADS_COUNT
			pthread_mutex_lock(&(tmpptr->lock));
#endif
			if ((tmpptr->energy)&&accessAllowed(tmpptr,reg,0)) {
				/* Log it if we're replacing a viable cell */
				if (tmpptr->generation > 2)
					++statCounters.viableCellsReplaced;
				
				tmpptr->ID = ++cellIdCounter;
				tmpptr->parentID = pptr->ID;
				tmpptr->lineage = pptr->lineage; /* Lineage is copied in offspring */
				tmpptr->generation = pptr->generation + 1;
				for(i=0;i<POND_DEPTH_SYSWORDS;++i)
					tmpptr->genome[i] = outputBuf[i];
			}
#ifdef USE_PTHREADS_COUNT
			pthread_mutex_unlock(&(tmpptr->lock));
#endif
		}

		/* Update the neighborhood on SDL screen to show any changes. */
#ifdef USE_SDL
		((uint8_t *)screen->pixels)[x + (y * sdlPitch)] = getColor(pptr);
		if (x) {
			((uint8_t *)screen->pixels)[(x-1) + (y * sdlPitch)] = getColor(&pond[x-1][y]);
			if (x < (POND_SIZE_X-1))
				((uint8_t *)screen->pixels)[(x+1) + (y * sdlPitch)] = getColor(&pond[x+1][y]);
			else ((uint8_t *)screen->pixels)[y * sdlPitch] = getColor(&pond[0][y]);
		} else {
			((uint8_t *)screen->pixels)[(POND_SIZE_X-1) + (y * sdlPitch)] = getColor(&pond[POND_SIZE_X-1][y]);
			((uint8_t *)screen->pixels)[1 + (y * sdlPitch)] = getColor(&pond[1][y]);
		}
		if (y) {
			((uint8_t *)screen->pixels)[x + ((y-1) * sdlPitch)] = getColor(&pond[x][y-1]);
			if (y < (POND_SIZE_Y-1))
				((uint8_t *)screen->pixels)[x + ((y+1) * sdlPitch)] = getColor(&pond[x][y+1]);
			else ((uint8_t *)screen->pixels)[x] = getColor(&pond[x][0]);
		} else {
			((uint8_t *)screen->pixels)[x + ((POND_SIZE_Y-1) * sdlPitch)] = getColor(&pond[x][POND_SIZE_Y-1]);
			((uint8_t *)screen->pixels)[x + sdlPitch] = getColor(&pond[x][1]);
		}
#endif /* USE_SDL */
	}

	return (void *)0;
}

/**
 * Main method
 *
 * @param argc Number of args
 * @param argv Argument array
 */
int main(int argc,char **argv)
{
	uintptr_t i,x,y;

	/* Seed and init the random number generator */
	prngState[0] = (uint64_t)time(NULL);
	srand(time(NULL));
	prngState[1] = (uint64_t)rand();

	/* Reset per-report stat counters */
	for(x=0;x<sizeof(statCounters);++x)
		((uint8_t *)&statCounters)[x] = (uint8_t)0;
	
	/* Set up SDL if we're using it */
#ifdef USE_SDL
	if (SDL_Init(SDL_INIT_VIDEO) < 0 ) {
		fprintf(stderr,"*** Unable to init SDL: %s ***\n",SDL_GetError());
		exit(1);
	}
	atexit(SDL_Quit);
	window = SDL_CreateWindow("nanopond", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, POND_SIZE_X, POND_SIZE_Y, 0);
	if (!window) {
		fprintf(stderr, "*** Unable to create SDL window: %s ***\n", SDL_GetError());
		exit(1);
	}
	winsurf = SDL_GetWindowSurface(window);
	if (!winsurf) {
		fprintf(stderr, "*** Unable to get SDL window surface: %s ***\n", SDL_GetError());
		exit(1);
	}
	screen = SDL_CreateRGBSurface(0, POND_SIZE_X, POND_SIZE_Y, 8, 0, 0, 0, 0);
	if (!screen) {
		fprintf(stderr, "*** Unable to create SDL window surface: %s ***\n", SDL_GetError());
		exit(1);
	}
	/* Set palette entries to match the default SDL 1.2.15 palette */
	{
		Uint8 r[8] = {0, 36, 73, 109, 146, 182, 219, 255};
		Uint8 g[8] = {0, 36, 73, 109, 146, 182, 219, 255};
		Uint8 b[4] = {0, 85, 170, 255};
		int curColor = 0;
		for(unsigned int i = 0; i < 8; ++i) {
			for(unsigned int j = 0; j < 8; ++j) {
				for(unsigned int k = 0; k < 4; ++k) {
					SDL_Color color = {r[i], g[j], b[k], 255};
					SDL_SetPaletteColors(screen->format->palette, &color, curColor, 1);
					curColor++;
				}
			}
		}
	}
#endif /* USE_SDL */
 
	/* Clear the pond and initialize all genomes
	 * to 0xffff... */
	for(x=0;x<POND_SIZE_X;++x) {
		for(y=0;y<POND_SIZE_Y;++y) {
			pond[x][y].ID = 0;
			pond[x][y].parentID = 0;
			pond[x][y].lineage = 0;
			pond[x][y].generation = 0;
			pond[x][y].energy = 0;
			for(i=0;i<POND_DEPTH_SYSWORDS;++i)
				pond[x][y].genome[i] = ~((uintptr_t)0);
#ifdef USE_PTHREADS_COUNT
			pthread_mutex_init(&(pond[x][y].lock),0);
#endif
		}
	}

#ifdef USE_PTHREADS_COUNT
	pthread_t threads[USE_PTHREADS_COUNT];
	for(i=1;i<USE_PTHREADS_COUNT;++i)
		pthread_create(&threads[i],0,run,(void *)i);
	run((void *)0);
	for(i=1;i<USE_PTHREADS_COUNT;++i)
		pthread_join(threads[i],(void **)0);
#else
	run((void *)0);
#endif

#ifdef USE_SDL
	SDL_FreeSurface(screen);
	SDL_DestroyWindow(window);
#endif /* USE_SDL */

	return 0;
}
