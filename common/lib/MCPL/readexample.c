#include "mcpl.h"
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

int main(int argc,char**argv) {
  if (argc!=2) {
    printf("Please supply input filename\n");
    return 1;
  }
  const char * filename = argv[1];

  //Open the file:
  mcpl_file_t f = mcpl_open_file(filename);

  //For fun, access and print a bit of the info found in the header (see mcpl.h for more):
  printf("Opened MCPL file produced with %s\n",mcpl_hdr_srcname(f));
  for (unsigned i = 0; i < mcpl_hdr_ncomments(f); ++i)
    printf("file had comment: '%s'\n",mcpl_hdr_comment(f,i));
  printf("File containts %lu particles\n",mcpl_hdr_nparticles(f));

  //Now, loop over particles and print some info:
  const mcpl_particle_t* p;
  while ( ( p = mcpl_read(f) ) ) {
    //print some info (see the mcpl_particle_t struct in mcpl.h for more fields):
    printf("  found particle with pdgcode %i and time-stamp %g ms with weight %g\n",
           p->pdgcode, p->time, p->weight);

  }

  mcpl_close_file(f);
}
