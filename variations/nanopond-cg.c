/* *********************************************************************** */
/*                                                                         */
/* Nanopond version 1.1 -- A teeny tiny artificial life virtual machine    */
/* Copyright (C) 2005 Adam Ierymenko - http://www.greythumb.org/people/api */
/* Copyright (C) 2005 Christoph Groth - http://falma.de/                   */
/*                                                                         */
/* This program is free software; you can redistribute it and/or modify    */
/* it under the terms of the GNU General Public License as published by    */
/* the Free Software Foundation; either version 2 of the License, or       */
/* (at your option) any later version.                                     */
/*                                                                         */
/* This program is distributed in the hope that it will be useful,         */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of          */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           */
/* GNU General Public License for more details.                            */
/*                                                                         */
/* You should have received a copy of the GNU General Public License       */
/* along with this program; if not, write to the Free Software             */
/* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110 USA     */
/*                                                                         */
/* *********************************************************************** */

/*

 Changes in 1.1:

   * Changed EAT to SHARE.

   * Different color schemes (switch with right mouse button)

   * Dumping of genomes of single cells to stderr (press left mouse button)

   * Random number generator called only once per command

   * Replaced INFLOW_RATE_MEAN and INFLOW_RATE_DEVIATION by INFLOW_RATE_BASE
     and INFLOW_RATE_VARIATION.  Same effect but a little bit faster.
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
 * - Every DUMP_FREQUENCY clock ticks, all viable replicators (cells whose
 *   generation is >= 2) are dumped to a file on disk.
 * - Every INFLOW_FREQUENCY clock ticks a random x,y location is picked,
 *   energy is added (see INFLOW_RATE_BASE and INFLOW_RATE_VARIATION)
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
 * real living cells to always replace them or share energy with them.
 *
 * The "guess" is merely the value of the register at the time that the
 * access attempt occurs.
 *
 * Permissions determine whether or not an offspring can take the place
 * of the contents of a cell and also whether or not the cell is allowed
 * to SHARE (an instruction) the energy with it's neighbor.
 *
 * If you haven't realized it yet, this is why the final permission
 * criteria is comparison against what is called a "guess." In conjunction
 * with the ability to "share" neighbors' energy, guess what this permits?
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
 * #define uint64_t unsigned long long (or whatever is a 64-bit int)
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

/* If this is defined, one is dropped occasionally into the world */
/* This is only for debugging, and makes things less interesting.
 * You should probably leave it undefined. */
/* #define SYNTHETIC_REPLICATOR 1 */

/* Frequency of comprehensive reports-- lower values will provide more
 * info while slowing down the simulation. Higher values will give less
 * frequent updates. The default of 250000 is good on a 2ghz Athlon.
 * This is also the frequency of screen refreshes if SDL is enabled. */
#define REPORT_FREQUENCY 500000

/* Frequency at which to dump all viable replicators (generation > 2)
 * to a file named <clock>.dump in the current directory.  Comment
 * out to disable. The cells are dumped in hexadecimal, which is
 * semi-human-readable if you look at the big switch() statement
 * in the main loop to see what instruction is signified by each
 * four-bit value. */
#define DUMP_FREQUENCY 100000000

/* Mutation rate -- range is from 0 (none) to 0xffffffff (all mutations!) */
/* To get it from a float probability from 0.0 to 1.0, multiply it by
 * 4294967295 (0xffffffff) and round. */
// #define MUTATION_RATE  42949 /* p=~0.00001 */
#define MUTATION_RATE 128849*10 /* p=~0.0003 */
/* #define MUTATION_RATE 429496 /\* p=~0.0001 *\/ */

/* How frequently should random cells / energy be introduced?
 * Making this too high makes things very chaotic. Making it too low
 * might not introduce enough energy. */
#define INFLOW_FREQUENCY 100

/* Base amount of energy to introduce every INFLOW_FREQUENCY ticks. */
#define INFLOW_RATE_BASE 6000

/* A random amount of energy not bigger than this value will be added to
   INFLOW_RATE_BASE. */
#define INFLOW_RATE_VARIATION 8000

/* Size of pond in X and Y dimensions. */
#define POND_SIZE_X 600
#define POND_SIZE_Y 400

/* Depth of pond in four-bit codons -- this is the maximum
 * genome size. This *must* be a multiple of 16! */
#define POND_DEPTH 512

/* Define this to use SDL. To use SDL, you must have SDL headers
 * available and you must link with the SDL library when you compile. */
/* Comment this out to compile without SDL visualization support. */
#define USE_SDL 1

/* ----------------------------------------------------------------------- */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#ifdef USE_SDL
#ifdef _MSC_VER
#include <SDL.h>
#else
#include <SDL/SDL.h>
#endif /* _MSC_VER */
#endif /* USE_SDL */

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

/**
 * Structure for a cell in the pond
 */
struct Cell
{
  /* Globally unique cell ID */
  uint64_t ID;
  
  /* ID of the cell's parent */
  uint64_t parentID;
  
  /* Generations start at 0 and are incremented from there. */
  uintptr_t generation;
  
  /* Energy level of this cell */
  uintptr_t energy;

  /* Memory space for cell genome (genome is stored as four
   * bit instructions packed into machine size words) */
  uintptr_t genome[POND_DEPTH_SYSWORDS];
};

/* The pond is a 2D array of cells */
struct Cell pond[POND_SIZE_X][POND_SIZE_Y];

/* This adds to __randCounter so that getRandom() will span
 * all 64 bits even if rand() is 32-bit on 64-bit boxes. */
static uintptr_t __randCounter = 0; /* Both of these are set to time() */
static uintptr_t __randState = 0;
/* Note that rand_r() is faster even though we're not multithreaded. If
 * you get errors about rand_r not being defined, change it to rand() or
 * go find a better PRNG like Mersenne Twister and link against that. */
#define getRandom() (__randCounter += (uintptr_t)rand_r(&__randState))

enum { MIXED, KINSHIP, MAX_COLOR_SCHEME } color_scheme = MIXED;
char *color_scheme_names[] = {"mixed", "relation-dependant"};

/**
 * Output a line of comma-seperated statistics data
 *
 * @param clock Current clock
 */
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
  
  /* Here are the columns in the CSV output if you're curious */
  printf("%llu,%llu,%llu,%llu,%llu\n",
    (uint64_t)clock,
    (uint64_t)totalEnergy,
    (uint64_t)totalActiveCells,
    (uint64_t)totalViableReplicators,
    (uint64_t)maxGeneration
    );
  
  if ((lastTotalViableReplicators > 0)&&(totalViableReplicators == 0))
    fprintf(stderr,"[EVENT] Viable replicators have gone extinct. Please reserve a moment of silence.\n");
  else if ((lastTotalViableReplicators == 0)&&(totalViableReplicators > 0))
    fprintf(stderr,"[EVENT] Viable replicators have appeared!\n");
  
  lastTotalViableReplicators = totalViableReplicators;
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
      fprintf(file,"%x",inst);
      if (inst == 0xf) { /* STOP */
        if (++stopCount >= 4)
          break;
      } else stopCount = 0;
      if ((shiftPtr += 4) >= SYSWORD_BITS) {
        if (++wordPtr >= POND_DEPTH_SYSWORDS) {
          wordPtr = 0;
          shiftPtr = 4;
        } else shiftPtr = 0;
      }
    }
    fprintf(file,"\n");
  }
}

/**
 * Dumps all viable (generation > 2) cells to a file called <clock>.dump
 *
 * @param clock Clock value
 */
static void doDump(const uint64_t clock)
{
  char buf[POND_DEPTH*2];
  FILE *d;
  uintptr_t x,y;
  
  sprintf(buf,"%llu.dump",clock);
  d = fopen(buf,"w");
  if (!d) {
    fprintf(stderr,"[WARNING] Could not open %s for writing.\n",buf);
    return;
  }
  
  fprintf(stderr,"[INFO] Dumping viable cells to %s\n",buf);
  
  for(x=0;x<POND_SIZE_X;++x) {
    for(y=0;y<POND_SIZE_Y;++y)
      dumpCell(d, &pond[x][y]);
  }
  
  fclose(d);
}

/**
 * Get a neighbor in the pond
 *
 * @param x Starting X position
 * @param y Starting Y position
 * @param dir Direction to get neighbor from
 * @return Pointer to neighboring cell
 */
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

/**
 * Determines if c1 is allowed to access c2
 *
 * Access is permitted if:
 *   c2 has no energy
 *   c2 has a parent ID of zero
 *   c2's logo equals c1's "guess"
 *
 * @param c1 Cell trying to access
 * @param c2 Cell being accessed
 * @param c1guess c1's "guess"
 * @return True or false (1 or 0)
 */
static inline int accessAllowed(struct Cell *const c1,struct Cell *const c2,const uintptr_t c1guess)
{
  if (!c2->energy)
    return 1;
  if (!c2->parentID)
    return 1;
  if ((c2->genome[0] & 0xf) == (c1guess & 0xf))
    return 1;
  return 0;
}

#ifdef USE_SDL
/**
 * Sets the palette.
 *
 * The first 64 colors are grays.
 * The last 192 colors are a HSV-rainbow.
 */
static void initPalette(SDL_Surface *surface)
{
  uintptr_t i,h,f;
  SDL_Color palette[256];

  /* Make grays. */
  for (i=0; i<64;++i) {
      f = i * 4;
      palette[i].r=f; palette[i].g=f; palette[i].b=f;
  }

  /* Make rainbow. */
  for (i=64;i<256;++i) {
    h = (i-64) >> 5;
    f = (i-64) & 0x1f;
    if (h&1) f = 0x20 - f; /* if h is odd */
    f <<= 3;
    switch (h) {
    case 0:
      palette[i].r=255; palette[i].g=f; palette[i].b=0;
      break;
    case 1:
      palette[i].r=f; palette[i].g=255; palette[i].b=0;
      break;
    case 2:
      palette[i].r=0; palette[i].g=255; palette[i].b=f;
      break;
    case 3:
      palette[i].r=0; palette[i].g=f; palette[i].b=255;
      break;
    case 4:
      palette[i].r=f; palette[i].g=0; palette[i].b=255;
      break;
    case 5:
      palette[i].r=255; palette[i].g=0; palette[i].b=f;
      break;
    }
  }

  /* Set palette. */
  SDL_SetColors(surface, palette, 0, 256);
}
#endif /* USE_SDL */

/**
 * Gets the color that a cell should be
 *
 * This is only used if SDL is enabled.
 *
 * The colors are determined according to different color schemes.  (The
 * schemes can be switched with the right mouse button.)
 *
 * @param c Cell to get color for
 * @return 8-bit color value
 */
static inline Uint8 getColor(struct Cell *c)
{
  uintptr_t sum,i,j,word,opcode;

  switch (color_scheme) {
  case MIXED:
    /*
     * For cells of generation > 1, saturation and value are set to maximum.
     * The hue will be (usually) different even for slightly differing genomes.
     *
     * A cell with generation <= 1 is gray, the lightness depending on its
     * energy.  Cells with no energy are black.
     */
    if (c->energy) {
      if (c->generation > 1) {
         sum = 0;
        for(i=0;i<POND_DEPTH_SYSWORDS&&(c->genome[i] != ~((uintptr_t)0));++i)
          sum += c->genome[i];
        j = 0;
        for(i=0;i<sizeof(uintptr_t);++i) {
          j ^= sum & 0xff;
          sum >>= 8;
        }
        return (j % 192) + 64;
      }
      return (c->energy * 64) / (INFLOW_RATE_BASE + INFLOW_RATE_VARIATION);
    }
    return 0;
  case KINSHIP:
    /*
     * For cells of generation > 1, saturation and value are set to maximum.
     * Hue is a hash-value with the property that related genomes will have
     * similar hue (but of course, as this is a hash function, totally
     * different genomes can also have a similar or even the same hue).
     * Therefore the difference in hue should to some extent reflect the grade
     * of "kinship" of two cells.
     *
     * A cell with generation <= 1 is gray, the lightness depending on its
     * energy.  Cells with no energy are black.
     */
    if (c->energy) {
      if (c->generation > 1) {
        sum = 0;
        for(i=0;i<POND_DEPTH_SYSWORDS&&(c->genome[i] != ~((uintptr_t)0));++i) {
  	  word=c->genome[i];
  	  for(j=0;j<SYSWORD_BITS/4;++j,word >>= 4) {
            /* We ignore 0xf's here, because otherwise very similar genomes
  	     * might get quite different hash values in the case when one of
  	     * the genomes is slightly longer and uses one more maschine
  	     * word. */
  	    opcode = word & 0xf;
  	    if (opcode == 0xf) continue;
	    sum += opcode;
  	  }
        }
        /* For the hash-value use a wrapped around sum of the sum of all
  	   commands and the length of the genome. */
  	return (sum % 192) + 64;
      }
      return (c->energy * 64) / (INFLOW_RATE_BASE + INFLOW_RATE_VARIATION);
    }
    return 0;
  case MAX_COLOR_SCHEME:
    /* Suppress warning. */
    return 0;
  }
  return 0;
}

/**
 * Main method
 *
 * @param argc Number of args
 * @param argv Argument array
 */
int main(int argc,char **argv)
{
  uintptr_t i;
  
  /* Buffer used for execution output of candidate offspring */
  uintptr_t outputBuf[POND_DEPTH_SYSWORDS];
  
  /* Seed and init the random number generator */
  __randCounter = __randState = time(NULL);
  for(i=0;i<1024;++i)
    getRandom();
  
  /* Set up SDL if we're using it */
#ifdef USE_SDL
  SDL_Surface *screen;
  SDL_Event sdlEvent;
  if (SDL_Init(SDL_INIT_VIDEO) < 0 ) {
    fprintf(stderr,"*** Unable to init SDL: %s ***\n",SDL_GetError());
    exit(1);
  }
  atexit(SDL_Quit);
  SDL_WM_SetCaption("nanopond","nanopond");
  screen = SDL_SetVideoMode(POND_SIZE_X,POND_SIZE_Y,8,SDL_SWSURFACE);
  if (!screen) {
    fprintf(stderr, "*** Unable to create SDL window: %s ***\n", SDL_GetError());
    exit(1);
  }
  const uintptr_t sdlPitch = screen->pitch;
  initPalette(screen);
#endif /* USE_SDL */
 
  /* Clear the pond */
  for(i=0;i<sizeof(pond);++i)
    ((uint8_t *)&pond)[i] = (uint8_t)0;
  
  /* Clock is incremented on each core loop */
  uint64_t clock = 0;
  
  /* This is used to generate unique cell IDs */
  uint64_t cellIdCounter = 0;
  
  /* Miscellaneous variables used in the loop */
  uintptr_t currentWord,wordPtr,shiftPtr,inst,x,y,tmp;
  struct Cell *pptr,*tmpptr;
  
  /* Virtual machine memory pointer register (which
   * exists in two parts... read the code below...) */
  uintptr_t ptr_wordPtr;
  uintptr_t ptr_shiftPtr;
  
  /* "Spare" register that can be swapped to/from */
  uintptr_t sptr_wordPtr;
  uintptr_t sptr_shiftPtr;
  
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
  
  /* If this is nonzero, execution stops. This allows us
   * to avoid the ugly use of a goto to exit the loop. :) */
  int stop;
  
  /* Main loop */
  for(;;) {
    /* Increment clock and run reports periodically */
    /* Clock is incremented at the start, so it starts at 1 */
    if (!(++clock % REPORT_FREQUENCY)) {
      doReport(clock);
      /* SDL display is also refreshed every REPORT_FREQUENCY */
#ifdef USE_SDL
      while (SDL_PollEvent(&sdlEvent)) {
        if (sdlEvent.type == SDL_QUIT) {
          fprintf(stderr,"[QUIT] Quit signal received!\n");
          exit(0);
        }
        if (sdlEvent.type == SDL_MOUSEBUTTONDOWN) {
	  switch (sdlEvent.button.button) {
	  case SDL_BUTTON_LEFT:
	    fprintf(stderr,"[INTERFACE] Genome of cell at (%d, %d):\n",
		    sdlEvent.button.x, sdlEvent.button.y);
	    dumpCell(stderr, &pond[sdlEvent.button.x][sdlEvent.button.y]);
	    break;
	  case SDL_BUTTON_RIGHT:
	    /* Switch color scheme. */
	  {
	    uintptr_t x, y;

	    color_scheme = (color_scheme + 1) % MAX_COLOR_SCHEME;
	    fprintf(stderr,"[INTERFACE] Switching to color scheme \"%s\".\n",
		    color_scheme_names[color_scheme]);
	    for (y=0;y<POND_SIZE_Y;++y)
	      for (x=0;x<POND_SIZE_X;++x) {
		((uint8_t *)screen->pixels)[x + (y * sdlPitch)]
		    = getColor(&pond[x][y]);
	      }
	    break;
	  }
	  }
	}
      }
      SDL_UpdateRect(screen,0,0,POND_SIZE_X,POND_SIZE_Y);
#endif /* USE_SDL */
    }

    /* Periodically dump the viable population if defined */
#ifdef DUMP_FREQUENCY
    if (!(clock % DUMP_FREQUENCY))
      doDump(clock);
#endif /* DUMP_FREQUENCY */

    /* Introduce a random cell somewhere with a given energy level */
    /* This is called seeding, and introduces both energy and
     * entropy into the substrate. This happens every INFLOW_FREQUENCY
     * clock ticks. */
    if (!(clock % INFLOW_FREQUENCY)) {
      x = getRandom() % POND_SIZE_X;
      y = getRandom() % POND_SIZE_Y;
      pptr = &pond[x][y];
      pptr->ID = ++cellIdCounter;
      pptr->parentID = 0;
      pptr->generation = 0;
      pptr->energy += INFLOW_RATE_BASE + (getRandom() % INFLOW_RATE_VARIATION);
      for(i=0;i<POND_DEPTH_SYSWORDS;++i) 
        pptr->genome[i] = getRandom();
      
      /* If enabled, make this a synthetic replicator every once in a while.
       * This is mainly just for debugging and is normally not defined. */
#ifdef SYNTHETIC_REPLICATOR
      if ((getRandom() & 0xffff) < 3) {
        /* These digits constitute a very simple self-replicating loop. */
        pptr->genome[0] = 0x0a185933;
        fprintf(stderr,"[INFO] Dropped a synthetic replicator!\n");
      }
#endif /* SYNTHETIC_REPLICATOR */
    
      /* Update the random cell on SDL screen if viz is enabled */
#ifdef USE_SDL
      if (SDL_MUSTLOCK(screen))
        SDL_LockSurface(screen);
      ((uint8_t *)screen->pixels)[x + (y * sdlPitch)] = getColor(&pond[x][y]);
      if (SDL_MUSTLOCK(screen))
        SDL_UnlockSurface(screen);
#endif /* USE_SDL */
    }
    
    /* Pick a random cell to execute */
    x = getRandom() % POND_SIZE_X;
    y = getRandom() % POND_SIZE_Y;
    pptr = &pond[x][y];

    /* Reset the state of the VM prior to execution */
    for(i=0;i<POND_DEPTH_SYSWORDS;++i)
      outputBuf[i] = ~((uintptr_t)0); /* ~0 == 0xfffff... */
    ptr_wordPtr = 0;
    ptr_shiftPtr = 0;
    sptr_wordPtr = 0;
    sptr_shiftPtr = 0;
    reg = 0;
    loopStackPtr = 0;
    wordPtr = 0;
    shiftPtr = 4; /* The first four bits are the 'logo' and are skipped! */
    facing = getRandom() & 3; /* Cells start facing in a random direction */
    falseLoopDepth = 0;
    stop = 0;
    
    /* We use a currentWord buffer to hold the word we're
     * currently working on.  This speeds things up a bit
     * since it eliminates a pointer dereference in the
     * inner loop. We have to be careful to refresh this
     * whenever it might have changed... take a look at
     * the code. :) */
    currentWord = pptr->genome[0];

    /* Core execution loop */
    while (pptr->energy&&(!stop)) {
      /* Get the next instruction */
      inst = (currentWord >> shiftPtr) & 0xf;
      
      /* Randomly frob either the instruction or the register with a
       * probability defined by MUTATION_RATE. This introduces variation,
       * and since the variation is introduced into the state of the VM
       * it can have all manner of different effects on the end result of
       * replication: insertions, deletions, duplications of entire
       * ranges of the genome, etc. */

      tmp = getRandom(); /* Call getRandom() only once for speed */
      if ((tmp & 0xffffffff) < MUTATION_RATE) {
        if (tmp & 0x80) /* Check for the 8th bit to get random boolean */
          inst = tmp & 0xf; /* Only the first four bits are used here */
        else reg = tmp & 0xf;
      }
      
      /* Each instruction processed costs one unit of energy */
      --pptr->energy;
      
      /* Execute the instruction */
      if (falseLoopDepth) {
        /* Skip forward to matching REP if we're in a false loop. */
        if (inst == 0x8) /* Increment false LOOP depth */
          ++falseLoopDepth;
        else if (inst == 0x9) /* Decrement on REP */
          --falseLoopDepth;
      } else {
        /* If we're not in a false LOOP/REP, execute normally */
        switch(inst) {
          case 0x0: /* NOP: No operation */
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
          case 0xc: /* SWAP: Swap active and saved copies of pointer */
            tmp = ptr_wordPtr;
            ptr_wordPtr = sptr_wordPtr;
            sptr_wordPtr = tmp;
            tmp = ptr_shiftPtr;
            ptr_shiftPtr = sptr_shiftPtr;
            sptr_shiftPtr = tmp;
            break;
          case 0xd: /* SET: Skip next instruction and set register equal to it */
            if ((shiftPtr += 4) >= SYSWORD_BITS) {
              if (++wordPtr >= POND_DEPTH_SYSWORDS) {
                wordPtr = 0;
                shiftPtr = 4;
              } else shiftPtr = 4;
              currentWord = pptr->genome[wordPtr];
            }
            reg = (pptr->genome[wordPtr] >> shiftPtr) & 0xf;
            break;
          case 0xe: /* SHARE: Attempt to share energy (50-50) with neighbor */
            tmpptr = getNeighbor(x,y,facing);
            if (accessAllowed(pptr,tmpptr,reg)) {
	      tmp = pptr->energy + tmpptr->energy;
              tmpptr->energy = tmp / 2;
              pptr->energy = tmp - tmpptr->energy;
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
          wordPtr = 0;
          shiftPtr = 4; /* Remember: first four bits are the 'logo' */
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
      if ((tmpptr->energy)&&accessAllowed(pptr,tmpptr,reg)) {
        tmpptr->ID = ++cellIdCounter;
        tmpptr->parentID = pptr->ID;
        tmpptr->generation = pptr->generation + 1;
        for(i=0;i<POND_DEPTH_SYSWORDS;++i)
          tmpptr->genome[i] = outputBuf[i];
      }
    }

    /* Update the neighborhood on SDL screen to show any changes. */
#ifdef USE_SDL
    if (SDL_MUSTLOCK(screen))
      SDL_LockSurface(screen);
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
    if (SDL_MUSTLOCK(screen))
      SDL_UnlockSurface(screen);
#endif /* USE_SDL */
  }
}
