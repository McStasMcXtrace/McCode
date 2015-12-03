#include "mcpl.h"
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h>

int main(int argc,char**argv) {
  if (argc!=2) {
    printf("Please supply output filename\n");
    return 1;
  }
  const char * filename = argv[1];

  // Initialisation, create output file handle, embed name of source application
  // in the header and declare that this is a neutron-only file. Note that an
  // ".mcpl" extension will be added to the filename if it doesn't have it
  // already:

  mcpl_outfile_t f = mcpl_create_outfile(filename);
  mcpl_hdr_set_srcname(f,"FakeMcStas-v2.2a");
  mcpl_enable_universal_pdgcode(f,2112);//all particles are neutrons

  // By default, floating point numbers will be stored in single precision and
  // neither polarisation nor user-flags will be stored in the file. These
  // defaults can be modified by one or more of the following calls (perhaps
  // they could be options to your McStas component):
  //
  //    mcpl_enable_userflags(f);
  //    mcpl_enable_polarisation(f);
  //    mcpl_enable_doubleprec(f);

  //We can add comments (strings) to the header. It would be great if we could
  //automatically have comments explaining things like coordinate system,
  //contents of user-flags (if any), and what the values in the "weight" field
  //means exactly (is there some generic mcstas meaning, like "bla per
  //second"?). I also imagine that it might be useful to embed the name and
  //version of the McStas component producing the .mcpl file, if easily
  //available:
  mcpl_hdr_add_comment(f,"Some comment.");
  mcpl_hdr_add_comment(f,"Another comment.");

  //It is also possible to add binary data with mcpl_hdr_add_data, if needed
  //(can be indexed by strings... so one could embed an integer representing the
  //McStas version directly as binary data).

  //Allocate the particle structure we will use during the simulation loop
  //to register particle data in the output file:
  mcpl_particle_t * particle = (mcpl_particle_t*)calloc(sizeof(mcpl_particle_t),1);

  //Simulation loop, modify the particle struct and add to the file as many
  //times as needed (here everything will simply be filled with some stupid
  //random numbers):
  for (int i = 0; i < 1000; ++i) {
    //position in centimeters:
    particle->position[0] = rand();
    particle->position[1] = rand();
    particle->position[2] = rand();
    //velocity in meters/seconds:
    double tmp0,tmp1,tmp2, nrm;
    tmp0 = rand();
    tmp1 = rand();
    tmp2 = rand();
    nrm =sqrt(tmp0*tmp0 + tmp1*tmp1 + tmp2*tmp2);
    particle->ekin = 1e3*rand();
    particle->direction[0] = tmp0/nrm;
    particle->direction[1] = tmp1/nrm;
    particle->direction[2] = tmp2/nrm;
    //time in milliseconds:
    particle->time = rand();
    //weight in unspecified units:
    particle->weight = rand();
    //modify userflags and polarisation (what units?) as well, if enabled.

    //Finally, add the particle to the file:
    mcpl_add_particle(f,particle);
  }

  //At the end, remember to properly close the output file:
  mcpl_close_outfile(f);

  //And, if you wish, you can run "gzip <filename>" as a post-processing step,
  //and get a smaller <filename>.mcpl.gz file. Such files can be read seemlessly
  //if mcpl is compiled with zlib support (like it will be in our Geant4
  //framework).
}
