/* *********************************************************************** */
/*                                                                         */
/* Nanopond-MV version 1.0 -- Multivalent nanopond                         */
/* Copyright (C) 2005 Adam Ierymenko - http://www.greythumb.com/people/api */
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

/* ----------------------------------------------------------------------- */
/* Tunable parameters                                                      */
/* ----------------------------------------------------------------------- */

/* Iteration to stop at. Comment this out to run forever. */
/* #define STOP_AT 150000000000ULL */

/* Frequency of comprehensive reports-- lower values will provide more
 * info while slowing down the simulation. Higher values will give less
 * frequent updates. */
/* This is also the frequency of screen refreshes if SDL is enabled. */
#define REPORT_FREQUENCY 100000

/* Uncomment this to save a BMP image of the screen every
 * REPORT_FREQUENCY ticks. Requires that SDL (see below) be enabled. */
#define SAVE_BMPS 1

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
#define MUTATION_RATE 21475

/* How frequently should random cells be introduced? */
#define INFLOW_FREQUENCY 100

/* Size of pond in X and Y dimensions. */
#define POND_SIZE_X 640
#define POND_SIZE_Y 480

/* Depth of pond in four-bit codons -- this is the maximum
 * genome size. This *must* be a multiple of 16! */
#define POND_DEPTH 512

/* Base and variation for energy of each valence in pond */
#define POND_ENERGY_BASE 50000
/* #define POND_ENERGY_VARIATION 10000 */

/* Define this to use SDL. To use SDL, you must have SDL headers
 * available and you must link with the SDL library when you compile. */
/* Comment this out to compile without SDL visualization support. */
#define USE_SDL 1

/* Default color scheme */
#define DEFAULT_COLOR_SCHEME VALENCE

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

/* ----------------------------------------------------------------------- */
/* This is the Mersenne Twister by Makoto Matsumoto and Takuji Nishimura   */
/* http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/MT2002/emt19937ar.html  */
/* ----------------------------------------------------------------------- */

/* A few very minor changes were made by me - Adam */

/* 
   A C-program for MT19937, with initialization improved 2002/1/26.
   Coded by Takuji Nishimura and Makoto Matsumoto.

   Before using, initialize the state by using init_genrand(seed)  
   or init_by_array(init_key, key_length).

   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
   All rights reserved.                          

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

     3. The names of its contributors may not be used to endorse or promote 
        products derived from this software without specific prior written 
        permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


   Any feedback is very welcome.
   http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
   email: m-mat @ math.sci.hiroshima-u.ac.jp (remove space)
*/

/* Period parameters */  
#define N 624
#define M 397
#define MATRIX_A 0x9908b0dfUL   /* constant vector a */
#define UPPER_MASK 0x80000000UL /* most significant w-r bits */
#define LOWER_MASK 0x7fffffffUL /* least significant r bits */

static unsigned long mt[N]; /* the array for the state vector  */
static int mti=N+1; /* mti==N+1 means mt[N] is not initialized */

/* initializes mt[N] with a seed */
static void init_genrand(unsigned long s)
{
    mt[0]= s & 0xffffffffUL;
    for (mti=1; mti<N; mti++) {
        mt[mti] = 
	    (1812433253UL * (mt[mti-1] ^ (mt[mti-1] >> 30)) + mti); 
        /* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */
        /* In the previous versions, MSBs of the seed affect   */
        /* only MSBs of the array mt[].                        */
        /* 2002/01/09 modified by Makoto Matsumoto             */
        mt[mti] &= 0xffffffffUL;
        /* for >32 bit machines */
    }
}

/* generates a random number on [0,0xffffffff]-interval */
static inline uint32_t genrand_int32()
{
    uint32_t y;
    static uint32_t mag01[2]={0x0UL, MATRIX_A};
    /* mag01[x] = x * MATRIX_A  for x=0,1 */

    if (mti >= N) { /* generate N words at one time */
        int kk;

        for (kk=0;kk<N-M;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        for (;kk<N-1;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        y = (mt[N-1]&UPPER_MASK)|(mt[0]&LOWER_MASK);
        mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1UL];

        mti = 0;
    }
  
    y = mt[mti++];

    /* Tempering */
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680UL;
    y ^= (y << 15) & 0xefc60000UL;
    y ^= (y >> 18);

    return y;
}

/* ----------------------------------------------------------------------- */

/* Size of genome instructions in bits */
#define INSTRUCTION_WORD_SIZE 4

/* Bit mask for instruction words */
#define INSTRUCTION_MASK 0xf

/* Instructions per machine size word */
#define INSTRUCTIONS_PER_UINTPTR ((sizeof(uintptr_t) * 8) / INSTRUCTION_WORD_SIZE)

/* Execution start position in instruction words */
#define EXEC_START_POSITION 2

/* Constants representing neighbors in the 2D grid. */
#define N_LEFT 0
#define N_RIGHT 1
#define N_UP 2
#define N_DOWN 3

#ifdef USE_SDL
/* 16 primary colors plus one extra (brown), initialized in main() */
static uint8_t w3cPrimaryColors[17];
#endif /* USE_SDL */

/**
 * Structure for a cell in the pond
 */
struct Cell
{
  /* Globally unique cell ID */
  uint64_t ID;
  
  /* Generations start at 0 and are incremented from there. */
  /* Generations > 2 are considered "viable replicators." */
  uintptr_t generation;
  
  /* Energy level of this cell for each type of energy */
  uintptr_t energy[16];

  /* Memory space for cell genome */
  uintptr_t genome[POND_DEPTH / INSTRUCTIONS_PER_UINTPTR];
};

/* The pond is a 2D array of cells */
struct Cell pond[POND_SIZE_X][POND_SIZE_Y];

/* Currently selected color scheme */
enum { CHECKSUM,LOGO,VALENCE,MAX_COLOR_SCHEME } colorScheme = DEFAULT_COLOR_SCHEME;
const char *colorSchemeName[MAX_COLOR_SCHEME] = { "CHECKSUM", "LOGO", "VALENCE" };

/* Macro for getting byte x from genome */
#define genomeAt(g,x) ((g[(x) / INSTRUCTIONS_PER_UINTPTR] >> (((x) % INSTRUCTIONS_PER_UINTPTR) * INSTRUCTION_WORD_SIZE)) & INSTRUCTION_MASK)

/* Function for setting the value of byte x in the genome */
static inline void genomeSet(uintptr_t *g,uintptr_t x,uintptr_t v)
{
  const uintptr_t idx = x / INSTRUCTIONS_PER_UINTPTR;
  const uintptr_t shift = (x % INSTRUCTIONS_PER_UINTPTR) * INSTRUCTION_WORD_SIZE;
  g[idx] &= ~(((uintptr_t)INSTRUCTION_MASK) << shift);
  g[idx] |= (v & INSTRUCTION_MASK) << shift;
}

/* Macro for determining whether a cell with a given generation is a
 * probable viable replicator.  Defining this as a macro makes code
 * easier to read. */
#define isViableReplicator(g) ((g) > 2)

/**
 * Get a random number
 *
 * @return Random number
 */
static inline uintptr_t getRandom()
{
  /* A good optimizing compiler should optimize this 'if' out */
  /* This is to make it work on 64-bit boxes */
  if (sizeof(uintptr_t) == 8)
    return (uintptr_t)((((uint64_t)genrand_int32()) << 32) ^ ((uint64_t)genrand_int32()));
  else return (uintptr_t)genrand_int32();
}

/* Get the valence of a genome */
#define valence(g) (genomeAt(g,1))

/* Get the logo of an 8-bit codon */
#define logo(g) (genomeAt(g,2))

/* Macro to calculate the corresponding valence to a valence value */
#define correspondingValence(v) ((v) ^ 0xf)

/**
 * Structure for keeping some running tally type statistics
 */
struct PerReportStatCounters
{
  /* Counts for the number of times each instruction was
   * executed since the last report. */
  double instructionExecutions[16];
  
  /* Number of cells executed since last report */
  double cellExecutions;
  
  /* Number of successful SHARE operations */
  uint64_t viableCellShares;
};

/* Global statistics counters */
struct PerReportStatCounters statCounters;

/**
 * Output a line of comma-seperated statistics data
 *
 * @param clock Current clock
 */
static void doReport(const uint64_t clock)
{
  static uint64_t lastTotalViableReplicators = 0;
  
  uintptr_t x,y;
  
  uint64_t totalViableReplicators = 0ULL;
  uint64_t maxGeneration = 0ULL;
  uint64_t g = 0ULL;
  
  for(x=0;x<POND_SIZE_X;++x) {
    for(y=0;y<POND_SIZE_Y;++y) {
      g = (uint64_t)pond[x][y].generation;
      if (isViableReplicator(g))
        ++totalViableReplicators;
      if (g > maxGeneration)
        maxGeneration = g;
    }
  }
  
  /* Look here to get the columns in the CSV output */
  
  /* The first five are here and are self-explanatory */
  printf("%llu,%llu,%llu,%llu",
    clock,
    totalViableReplicators,
    maxGeneration,
    statCounters.viableCellShares
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
  memset((void *)&statCounters,0,sizeof(statCounters));
}

/**
 * Dumps all probable viable cells to a file called <clock>.dump
 *
 * @param clock Clock value
 */
static void doDump(const uint64_t clock)
{
  char buf[POND_DEPTH*2];
  FILE *d;
  struct Cell *pptr;
  uintptr_t x,y,i,val;
  
  sprintf(buf,"%llu.dump.csv",clock);
  d = fopen(buf,"w");
  if (!d) {
    fprintf(stderr,"[WARNING] Could not open %s for writing.\n",buf);
    return;
  }

  for(x=0;x<POND_SIZE_X;++x) {
    for(y=0;y<POND_SIZE_Y;++y) {
      pptr = &pond[x][y];
      fprintf(d,"%llu,%llu,%llu.%llu,",(uint64_t)x,(uint64_t)y,(uint64_t)pptr->ID,(uint64_t)pptr->generation);
      val = valence(pptr->genome);
      for(i=0;i<POND_DEPTH;++i)
        fprintf(d,"%x",genomeAt(pptr->genome,i));
      fprintf(d,",");
      for(i=0;i<POND_DEPTH;++i)
        fprintf(d,"%x",genomeAt(pptr->genome,i) ^ val);
      fprintf(d,"\n");
    }
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
 * @param c1 First cell attempting access
 * @param c2 Cell c1 is attempting to access
 * @return True or false (1 or 0)
 */
static inline int accessAllowed(struct Cell *const c1,struct Cell *const c2)
{
  if (!isViableReplicator(c2->generation))
    return 1;
  const uintptr_t l1 = logo(c1->genome);
  const uintptr_t l2 = logo(c2->genome);
  if (l1 > l2)
    return (getRandom() & 0xf) <= (l2 - l1);
  else return (getRandom() & 0xf) <= (l1 - l2);
}

/**
 * Gets the color that a cell should be
 *
 * @param c Cell to get color for
 * @return 8-bit color value
 */
static inline uint8_t getColor(struct Cell *c)
{
  uintptr_t i;
  uintptr_t tmp,tmp2,val;
  uint8_t color;
  
  if (isViableReplicator(c->generation)) {
    switch(colorScheme) {
      case CHECKSUM:
        /*
         * Cells are color coded by genome checksum
         */
        val = valence(c->genome);
        color = (uint8_t)val;
        tmp2 = 0;
        for(i=EXEC_START_POSITION;i<POND_DEPTH;++i) {
          tmp = genomeAt(c->genome,i) ^ val;
          if ((tmp != 0xf)&&(tmp2 != 0xd))
            color += (uint8_t)tmp; /* Ignore XCHG operands and STOPs */
          tmp2 = tmp;
        }
        return color;
      case LOGO:
        /*
         * Color code by logo
         *
         * Black is never used.
         */
        return w3cPrimaryColors[logo(c->genome) + 1];
      case VALENCE:
        /*
         * Color code by valence
         *
         * Black is never used.
         */
        return w3cPrimaryColors[valence(c->genome) + 1];
      case MAX_COLOR_SCHEME:
        /* ... never used... to make compiler shut up. */
        break;
    }
  }
  return 0; /* Cells with no energy are black */
}

/**
 * Main method
 *
 * @param argc Number of args
 * @param argv Argument array
 */
int main(int argc,char **argv)
{
#ifdef SAVE_BMPS
  char tmpbuf[1024];
#endif /* SAVE_BMPS */
  uintptr_t i,x,y;
  
  /* Seed and init the random number generator */
  init_genrand(time(NULL));
  for(i=0;i<1024;++i)
    getRandom();

  /* Reset per-report stat counters */
  memset((void *)&statCounters,0,sizeof(statCounters));
  
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
  w3cPrimaryColors[0 ] = SDL_MapRGB(screen->format,0x00,0x00,0x00);
  w3cPrimaryColors[1 ] = SDL_MapRGB(screen->format,0xb0,0xb0,0xb0);
  w3cPrimaryColors[2 ] = SDL_MapRGB(screen->format,0x70,0x70,0x70);
  w3cPrimaryColors[3 ] = SDL_MapRGB(screen->format,0xff,0xff,0xff);
  w3cPrimaryColors[4 ] = SDL_MapRGB(screen->format,0x80,0x00,0x00);
  w3cPrimaryColors[5 ] = SDL_MapRGB(screen->format,0xff,0x00,0x00);
  w3cPrimaryColors[6 ] = SDL_MapRGB(screen->format,0x80,0x00,0x80);
  w3cPrimaryColors[7 ] = SDL_MapRGB(screen->format,0xff,0x00,0xff);
  w3cPrimaryColors[8 ] = SDL_MapRGB(screen->format,0x00,0x80,0x00);
  w3cPrimaryColors[9 ] = SDL_MapRGB(screen->format,0x00,0xff,0x00);
  w3cPrimaryColors[10] = SDL_MapRGB(screen->format,0x80,0x80,0x00);
  w3cPrimaryColors[11] = SDL_MapRGB(screen->format,0xff,0xff,0x00);
  w3cPrimaryColors[12] = SDL_MapRGB(screen->format,0x00,0x00,0x80);
  w3cPrimaryColors[13] = SDL_MapRGB(screen->format,0x00,0x00,0xff);
  w3cPrimaryColors[14] = SDL_MapRGB(screen->format,0x00,0x80,0x80);
  w3cPrimaryColors[15] = SDL_MapRGB(screen->format,0x00,0xff,0xff);
  w3cPrimaryColors[16] = SDL_MapRGB(screen->format,0x8b,0x45,0x13);
#endif /* USE_SDL */

  /* Clear the pond and initialize all genomes
   * to 0xffff... */
  for(x=0;x<POND_SIZE_X;++x) {
    for(y=0;y<POND_SIZE_Y;++y) {
      pond[x][y].ID = 0ULL;
      pond[x][y].generation = 0;
      for(i=0;i<16;++i) {
#ifdef POND_ENERGY_VARIATION
        pond[x][y].energy[i] = POND_ENERGY_BASE + (getRandom() % POND_ENERGY_VARIATION);
#else
        pond[x][y].energy[i] = POND_ENERGY_BASE;
#endif /* POND_ENERGY_VARIATION */
      }
      for(i=0;i<(POND_DEPTH / INSTRUCTIONS_PER_UINTPTR);++i)
        pond[x][y].genome[i] = 0;
    }
  }
  
  /* Clock is incremented on each core loop */
  uint64_t clock = 0ULL;
  
  /* This is used to generate unique cell IDs */
  uint64_t cellIdCounter = 1ULL;
  
  /* Miscellaneous variables used in the loop */
  uintptr_t ip,inst,tmp,val,correspondingVal;
  struct Cell *pptr;
  
  /* Memory pointer register */
  uintptr_t ptr;
  
  /* The main "register" */
  uintptr_t reg;
  
  /* Which way is the cell facing? */
  uintptr_t facing;
  
  /* Neighbors (precalculated at start) */
  struct Cell *neighbor[4];
  
  /* Is access allowed to the cell we're facing? This
   * is calculated for each possible neighbor. */
  uintptr_t facingAllowed[4];
  
  /* Write counts are used to determine when a neighbor
   * should be considered a "child." */
  uintptr_t neighborWriteCounts[4];
  
  /* Virtual machine loop/rep stack */
  uintptr_t loopStack[POND_DEPTH];
  uintptr_t loopStackPtr;
  
  /* If this is nonzero, we're skipping to matching REP */
  /* It is incremented to track the depth of a nested set
   * of LOOP/REP pairs in false state. */
  uintptr_t falseLoopDepth;
  
  /* If this is nonzero, cell execution stops. This allows us
   * to avoid the ugly use of a goto to exit the loop. :) */
  int stop;
  
  /* Main loop */
  for(;;) {
    /* Stop at STOP_AT if defined */
#ifdef STOP_AT
    if (clock >= STOP_AT) {
      /* Also do a final dump if dumps are enabled */
#ifdef DUMP_FREQUENCY
      doDump(clock);
#endif /* DUMP_FREQUENCY */
      fprintf(stderr,"[QUIT] STOP_AT clock value reached\n");
      break;
    }
#endif /* STOP_AT */

    /* Increment clock and run reports periodically */
    /* Clock is incremented at the start, so it starts at 1 */
    if (!(++clock % REPORT_FREQUENCY)) {
      doReport(clock);
      /* SDL display is also refreshed every REPORT_FREQUENCY */
#ifdef USE_SDL
      if (SDL_MUSTLOCK(screen))
        SDL_LockSurface(screen);
      while (SDL_PollEvent(&sdlEvent)) {
        if (sdlEvent.type == SDL_QUIT) {
          fprintf(stderr,"[QUIT] Quit signal received!\n");
          exit(0);
        } else if (sdlEvent.type == SDL_MOUSEBUTTONDOWN) {
          switch (sdlEvent.button.button) {
            case SDL_BUTTON_LEFT:
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
      SDL_UpdateRect(screen,0,0,POND_SIZE_X,POND_SIZE_Y);
#ifdef SAVE_BMPS
      sprintf(tmpbuf,"%llu.bmp",clock);
      SDL_SaveBMP(screen,tmpbuf);
#endif /* SAVE_BMPS */
      if (SDL_MUSTLOCK(screen))
        SDL_UnlockSurface(screen);
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
      pptr->ID = cellIdCounter++;
      pptr->generation = 0;
      for(i=0;i<(POND_DEPTH / INSTRUCTIONS_PER_UINTPTR);++i) 
        pptr->genome[i] = getRandom();
      
      /* Update the random cell on SDL screen if viz is enabled */
#ifdef USE_SDL
      if (SDL_MUSTLOCK(screen))
        SDL_LockSurface(screen);
      ((uint8_t *)screen->pixels)[x + (y * sdlPitch)] = getColor(pptr);
      if (SDL_MUSTLOCK(screen))
        SDL_UnlockSurface(screen);
#endif /* USE_SDL */
    }
    
    /* Pick a random cell to execute */
    x = getRandom() % POND_SIZE_X;
    y = getRandom() % POND_SIZE_Y;
    pptr = &pond[x][y];
    
    /* Don't execute empty cells */
    if (!pptr->ID)
      continue;
    
    /* Reset the state of the VM prior to execution */
    ptr = 0;
    reg = 0;
    loopStackPtr = 0;
    ip = EXEC_START_POSITION;
    facing = 0;
    neighbor[0] = getNeighbor(x,y,0);
    neighbor[1] = getNeighbor(x,y,1);
    neighbor[2] = getNeighbor(x,y,2);
    neighbor[3] = getNeighbor(x,y,3);
    facingAllowed[0] = accessAllowed(pptr,neighbor[0]);
    facingAllowed[1] = accessAllowed(pptr,neighbor[1]);
    facingAllowed[2] = accessAllowed(pptr,neighbor[2]);
    facingAllowed[3] = accessAllowed(pptr,neighbor[3]);
    neighborWriteCounts[0] = 0;
    neighborWriteCounts[1] = 0;
    neighborWriteCounts[2] = 0;
    neighborWriteCounts[3] = 0;
    falseLoopDepth = 0;
    stop = 0;
    
    /* Get valence of this cell */
    val = valence(pptr->genome);
    correspondingVal = correspondingValence(val);
    
    /* Keep track of how many cells have been executed */
    statCounters.cellExecutions += 1.0;

    /* Core execution loop */
    while (!stop) {
      /* Get next instruction, which is XORed with cell valence */
      /* Cell valence determines instruction mapping! */
      inst = genomeAt(pptr->genome,ip) ^ val;
      
      /* Randomly frob either the instruction or the register with a
       * probability defined by MUTATION_RATE. This introduces variation,
       * and since the variation is introduced into the state of the VM
       * it can have all manner of different effects on the end result of
       * replication: insertions, deletions, duplications of entire
       * ranges of the genome, etc. */
      if ((getRandom() & 0xffffffff) < MUTATION_RATE) {
        tmp = getRandom(); /* Call getRandom() only once for speed */
        if (tmp & 0x1000) /* Check for a high bit to get random boolean */
          inst = tmp & INSTRUCTION_MASK;
        else reg = tmp & INSTRUCTION_MASK;
      }
      
      /* Each instruction processed converts one unit of energy into energy
       * of the corresponding valence. */
      if (!pptr->energy[val])
        break;
      else --pptr->energy[val];
      ++pptr->energy[correspondingVal];
      
      /* Execute the instruction */
      if (falseLoopDepth) {
        /* Skip forward to matching REP if we're in a false loop. */
        if (inst == 0xa) /* Increment false LOOP depth */
          ++falseLoopDepth;
        else if (inst == 0xb) /* Decrement on REP */
          --falseLoopDepth;
      } else {
        /* If we're not in a false LOOP/REP, execute normally */
        
        /* Keep track of execution frequencies for each instruction */
        statCounters.instructionExecutions[inst] += 1.0;
        
        switch(inst) {
          case 0x0: /* ZERO: Zero VM state registers */
            reg = 0;
            ptr = 0;
            facing = 0;
            break;
          case 0x1: /* FWD: Increment the pointer (wrap at end), set reg==0 on wrap */
            if (++ptr >= POND_DEPTH) {
              ptr = 0;
              reg = 0;
            } else reg = 1;
            break;
          case 0x2: /* BACK: Decrement the pointer (wrap at beginning), set reg==0 on wrap */
            if (ptr) {
              --ptr;
              reg = 1;
            } else {
              ptr = POND_DEPTH - 1;
              reg = 0;
            }
            break;
          case 0x3: /* INC: Increment the register */
            reg = (reg + 1) & INSTRUCTION_MASK;
            break;
          case 0x4: /* DEC: Decrement the register */
            reg = (reg - 1) & INSTRUCTION_MASK;
            break;
          case 0x5: /* MASK: XOR register with valence */
            reg ^= val;
            break;
          case 0x6: /* READG: Read into the register from genome */
            reg = genomeAt(pptr->genome,ptr);
            break;
          case 0x7: /* WRITEG: Write out from the register to genome */
            genomeSet(pptr->genome,ptr,reg);
            break;
          case 0x8: /* READB: Read into the register from buffer */
            if (facingAllowed[facing])
              reg = genomeAt(neighbor[facing]->genome,ptr);
            else reg = 0;
            break;
          case 0x9: /* WRITEB: Write out from the register to buffer */
            if (facingAllowed[facing]) {
              if (++neighborWriteCounts[facing] >= 4) {
                /* Writes of more than 4 values are considered an "offspring" */
                neighbor[facing]->ID = cellIdCounter++;
                neighbor[facing]->generation = pptr->generation + 1;
              }
              genomeSet(neighbor[facing]->genome,ptr,reg);
            }
            break;
          case 0xa: /* LOOP: Jump forward to matching REP if register is zero */
            if (reg) {
              if (loopStackPtr >= POND_DEPTH)
                stop = 1; /* Stack overflow ends execution */
              else loopStack[loopStackPtr++] = ip;
            } else falseLoopDepth = 1;
            break;
          case 0xb: /* REP: Jump back to matching LOOP if register is nonzero */
            if (loopStackPtr) {
              if (reg) {
                ip = loopStack[--loopStackPtr];
                /* This ensures that the LOOP is rerun */
                continue;
              } else --loopStackPtr;
            }
            break;
          case 0xc: /* TURN: Turn in the direction specified by register */
            facing = reg & 3; /* Four cardinal directions */
            break;
          case 0xd: /* XCHG: Skip next instruction and exchange value of register with it */
            if (++ip < POND_DEPTH) {
              tmp = reg;
              reg = genomeAt(pptr->genome,ip);
              genomeSet(pptr->genome,ip,tmp);
            }
            break;
          case 0xe: /* SHARE: Equalize energy between self and neighbor if allowed */
            if (facingAllowed[facing]) {
              if (isViableReplicator(neighbor[facing]->generation))
                ++statCounters.viableCellShares;

              tmp = pptr->energy[val] + neighbor[facing]->energy[val];
              neighbor[facing]->energy[val] = tmp / 2;
              pptr->energy[val] = tmp - neighbor[facing]->energy[val];
            }
            break;
          case 0xf: /* STOP: End execution */
            stop = 1;
            break;
        }
      }
      
      if (++ip >= POND_DEPTH)
        break;
    }
    
    /* If the cell runs out of energy of it's valence, it "dies." */
    if (!pptr->energy[val]) {
      pptr->ID = 0ULL;
      pptr->generation = 0;
      for(i=0;i<(POND_DEPTH / INSTRUCTIONS_PER_UINTPTR);++i)
        pptr->genome[i] = 0;
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
  
  return 0;
}
