
/////////////////////////////////////////////////////////////////////////////////////
//                                                                                 //
//  Monte Carlo Particle Lists : MCPL                                              //
//                                                                                 //
//  Utilities for reading and writing .mcpl files: A binary format with lists of   //
//  particle state information, for interchanging and reshooting events between    //
//  various Monte Carlo simulation applications.                                   //
//                                                                                 //
//  Written by Thomas Kittelmann, 2015.                                            //
//                                                                                 //
/////////////////////////////////////////////////////////////////////////////////////

// Compiles with C99 and C11 (not C89, mainly due to "//" comments).

#include "mcpl.h"

#if MCPL_HASZLIB
#include "zlib.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>

// TODO:
//  * Implement --ignore flag for --merge.

//Possible future developments:
//
//  1) Support for particle groups ("events"). The file writing interface would
//     get:
//
//       //If your particles should be grouped (into "events" typically),
//       //mcpl_add_particle calls must come between calls to:
//       void mcpl_begin_particle_group(mcpl_outfile_t);
//       void mcpl_end_particle_group(mcpl_outfile_t);
//
//    And the file reading interface might get a special method as well. If we
//    want to keep constant-size particles, we might need a flag on all
//    particles with internal group-id, and a special read method which can be
//    used to loop over particles in a group. Challenge: support empty events
//    (fake particle with negative group id?).
//
// 2) Support chaining of input files (,;: separated + globable wildcards). This
//    will likely need a pointer to next and previous files and to keep track of
//    global event position. Should optionally test for file merge-ability (if
//    not, what should the mcpl_hdr_... methods return? Always the info
//    associated with the current event? Should there be a way to detect file
//    transitions?).
//
// 3) Checksum to verify data integrity.
//
// 4) More complete endianness check (catch mixed-endian as well and test
//    different types, not just uint32_t). Or perhaps simply write/read data
//    with appropriate wrappers.
//
// 5) Speed up merges of large files by using sys/sendfile.h on linux (once all
//    kernels are >= 2.6.33, which unfortunately excludes slc6), and equivalent
//    on osx. Even better would be a "move operator version".
//
// 6) merge(file1,file2) should support file2 being gzipped.


#define MCPL_NPARTICLES_POS 8

int mcpl_platform_is_little_endian() {
  // return 0 for big endian, 1 for little endian.
  volatile uint32_t i=0x01234567;
  return (*((uint8_t*)(&i))) == 0x67;
}

void mcpl_default_error_handler(const char * msg) {
  printf("MCPL ERROR: %s\n",msg);
  exit(1);
}

static void (*mcpl_error_handler)(const char *) = &mcpl_default_error_handler;

void mcpl_error(const char * msg) {
  mcpl_error_handler(msg);
}

void mcpl_set_error_handler(void (*handler)(const char *))
{
  mcpl_error_handler = handler;
}

void mcpl_store_string(char** dest, const char * src)
{
  size_t n = strlen(src);
  if (n>65535) n = 65535;
  if (*dest)
    free(*dest);
  *dest = (char*)calloc(n+1,1);
  assert(*dest);
  strncpy( *dest,src,n );
  (*dest)[n] = '\0';
  return;
}

void mcpl_write_buffer(FILE* f, uint32_t n, const char * data, const char * errmsg)
{
  size_t nb = fwrite(&n, 1, sizeof(n), f);
  if (nb!=sizeof(n))
    mcpl_error(errmsg);
  nb = fwrite(data, 1, n, f);
  if (nb!=n)
    mcpl_error(errmsg);
}
void mcpl_write_string(FILE* f, const char * str, const char * errmsg)
{
  size_t n = strlen(str);
  mcpl_write_buffer(f,n,str,errmsg);//nb: we don't write the terminating null-char
}

#pragma pack (push, 1)
// single-precision version of particle struct
// (without energy which is anyway stored in direction[2]):
typedef struct {
  float polarisation[3];
  float position[3];
  float direction[3];
  float time;
  float weight;
  int32_t pdgcode;
  int32_t userflags;
} mcpl_particlesingleprec_t;

#pragma pack (pop)

typedef struct {
  char * filename;
  FILE * file;
  char * hdr_srcprogname;
  uint32_t ncomments;
  char ** comments;
  uint32_t nblobs;
  char ** blobkeys;
  uint32_t * bloblengths;
  char ** blobs;
  int opt_userflags;
  int opt_polarisation;
  int opt_singleprec;
  int opt_universalpdgcode;
  int header_notwritten;
  uint64_t nparticles;
  unsigned particle_size;
  int particle_offset;
  mcpl_particlesingleprec_t * psp;
} mcpl_outfileinternal_t;

#define MCPL_OUTFILEDECODE mcpl_outfileinternal_t * f = (mcpl_outfileinternal_t *)of.internal; assert(f)

void mcpl_recalc_sizeoffset(mcpl_outfile_t of)
{
  MCPL_OUTFILEDECODE;
  unsigned fp = f->opt_singleprec ? sizeof(float) : sizeof(double);
  f->particle_offset = 3*fp;
  f->particle_size = 8*fp+1*sizeof(int32_t);
  if (f->opt_userflags)
    f->particle_size += sizeof(uint32_t);
  if (f->opt_polarisation) {
    f->particle_size += 3*fp;
    f->particle_offset = 0;
  }
  if (f->opt_universalpdgcode)
    f->particle_size -= sizeof(int32_t);
  if (!f->opt_singleprec)
    f->particle_offset += fp;//skip past initial energy field
}

void mcpl_platform_compatibility_check() {
  static int first = 1;
  if (!first)
    return;
  first = 0;

  if (sizeof(float)!=4)
    mcpl_error("Platform compatibility check failed (float is not 4 bytes)");

  if (sizeof(double)!=8)
    mcpl_error("Platform compatibility check failed (double is not 8 bytes)");

  mcpl_particle_t pd;
  if ( (char*)&(pd.userflags)-(char*)&(pd) != 12*sizeof(double)+sizeof(uint32_t) )
    mcpl_error("Platform compatibility check failed (unexpected padding in mcpl_particle_t)");

  mcpl_particlesingleprec_t ps;
  if ( (char*)&(ps.userflags)-(char*)&(ps) != 11*sizeof(float)+sizeof(uint32_t) )
    mcpl_error("Platform compatibility check failed (unexpected padding in mcpl_particlesingleprec_t)");
}

mcpl_outfile_t mcpl_create_outfile(const char * filename)
{
  //Sanity check chosen filename and append ".mcpl" if missing to help people
  //who forgot to add the extension (in the hope of higher consistency).
  if (!filename)
    mcpl_error("mcpl_create_outfile called with null string.");
  size_t n = strlen(filename);
  if (!n)
    mcpl_error("mcpl_create_outfile called with empty string.");
  if (n>4096)
    mcpl_error("mcpl_create_outfile called with too long string.");
  char * lastdot = strrchr(filename, '.');
  if (lastdot==filename && n==5)
    mcpl_error("mcpl_create_outfile called with string with no basename part (\".mcpl\").");

  //Initialise data structures and open file:
  mcpl_platform_compatibility_check();

  mcpl_outfile_t out;
  out.internal = 0;

  mcpl_outfileinternal_t * f = (mcpl_outfileinternal_t*)calloc(sizeof(mcpl_outfileinternal_t),1);
  assert(f);

  if (!lastdot || strcmp(lastdot, ".mcpl") != 0) {
    f->filename = malloc(n+6);
    f->filename[0] = '\0';
    strcat(f->filename,filename);
    strcat(f->filename,".mcpl");
  } else {
    f->filename = malloc(n+1);
    f->filename[0] = '\0';
    strcat(f->filename,filename);
  }

  f->hdr_srcprogname = 0;
  f->psp = (mcpl_particlesingleprec_t*)calloc(sizeof(mcpl_particlesingleprec_t),1);
  f->ncomments = 0;
  f->comments = 0;
  f->nblobs = 0;
  f->blobkeys = 0;
  f->bloblengths = 0;
  f->blobs = 0;
  f->opt_userflags = 0;
  f->opt_polarisation = 0;
  f->opt_singleprec = 1;
  f->opt_universalpdgcode = 0;
  f->header_notwritten = 1;
  f->nparticles = 0;
  f->file = fopen(f->filename,"wb");
  if (!f->file)
    mcpl_error("Unable to open output file!");

  out.internal = f;
  mcpl_recalc_sizeoffset(out);
  return out;
}

const char * mcpl_outfile_filename(mcpl_outfile_t of) {
  MCPL_OUTFILEDECODE;
  return f->filename;
}

void mcpl_hdr_set_srcname(mcpl_outfile_t of,const char * spn)
{
  MCPL_OUTFILEDECODE;
  if (!f->header_notwritten)
    mcpl_error("mcpl_hdr_set_srcname.");
  mcpl_store_string(&(f->hdr_srcprogname),spn);
}

void mcpl_hdr_add_comment(mcpl_outfile_t of,const char *comment)
{
  MCPL_OUTFILEDECODE;
  if (!f->header_notwritten)
    mcpl_error("mcpl_hdr_add_comment called too late.");
  size_t oldn = f->ncomments;
  f->ncomments += 1;
  if (oldn)
    f->comments = (char **)realloc(f->comments,f->ncomments * sizeof(char*) );
  else
    f->comments = (char **)calloc(f->ncomments,sizeof(char*));
  f->comments[oldn] = 0;
  mcpl_store_string(&(f->comments[oldn]),comment);
}

void mcpl_hdr_add_data(mcpl_outfile_t of, const char * key,
                       unsigned ldata, const char * data)
{
  MCPL_OUTFILEDECODE;
  if (!f->header_notwritten)
    mcpl_error("mcpl_hdr_add_data called too late.");
  size_t oldn = f->nblobs;
  f->nblobs += 1;
  //Check that key is unique
  for (unsigned i =0; i<oldn; ++i) {
    if (strcmp(f->blobkeys[i],key)==0)
      mcpl_error("mcpl_hdr_add_data got duplicate key");
  }
  //store key:
  if (oldn)
    f->blobkeys = (char **)realloc(f->blobkeys,f->nblobs * sizeof(char*) );
  else
    f->blobkeys = (char **)calloc(f->nblobs,sizeof(char*));
  f->blobkeys[oldn] = 0;
  mcpl_store_string(&(f->blobkeys[oldn]),key);
  //store blob-lengths:
  if (oldn)
    f->bloblengths = (uint32_t*)realloc(f->bloblengths,f->nblobs * sizeof(uint32_t) );
  else
    f->bloblengths = (uint32_t *)calloc(f->nblobs,sizeof(uint32_t));
  f->bloblengths[oldn] = ldata;

  //store data:
  if (oldn)
    f->blobs = (char **)realloc(f->blobs,f->nblobs * sizeof(char*) );
  else
    f->blobs = (char **)calloc(f->nblobs,sizeof(char*));
  f->blobs[oldn] = (char *)malloc(ldata);
  memcpy(f->blobs[oldn],data,ldata);
}

void mcpl_enable_userflags(mcpl_outfile_t of)
{
  MCPL_OUTFILEDECODE;
  if (f->opt_userflags)
    return;
  if (!f->header_notwritten)
    mcpl_error("mcpl_enable_userflags called too late.");
  f->opt_userflags = 1;
  mcpl_recalc_sizeoffset(of);
}

void mcpl_enable_polarisation(mcpl_outfile_t of)
{
  MCPL_OUTFILEDECODE;
  if (f->opt_polarisation)
    return;
  if (!f->header_notwritten)
    mcpl_error("mcpl_enable_polarisation called too late.");
  f->opt_polarisation = 1;
  mcpl_recalc_sizeoffset(of);
}

void mcpl_enable_doubleprec(mcpl_outfile_t of)
{
  MCPL_OUTFILEDECODE;
  if (!f->opt_singleprec)
    return;
  if (!f->header_notwritten)
    mcpl_error("mcpl_enable_doubleprec called too late.");
  f->opt_singleprec = 0;
  if (f->psp) {
    free(f->psp);
    f->psp = 0;
  }
  mcpl_recalc_sizeoffset(of);
}

void mcpl_enable_universal_pdgcode(mcpl_outfile_t of, int pdgcode)
{
  MCPL_OUTFILEDECODE;
  if (pdgcode==0)
    mcpl_error("mcpl_enable_universal_pdgcode must be called with non-zero pdgcode.");
  if (f->opt_universalpdgcode) {
    if (f->opt_universalpdgcode!=pdgcode)
      mcpl_error("mcpl_enable_universal_pdgcode called multiple times");
    return;
  }
  if (!f->header_notwritten)
    mcpl_error("mcpl_enable_universal_pdgcode called too late.");
  f->opt_universalpdgcode = pdgcode;
  mcpl_recalc_sizeoffset(of);

}

void mcpl_write_header(mcpl_outfile_t of)
{
  MCPL_OUTFILEDECODE;
  if (!f->header_notwritten)
    mcpl_error("Logical error!");

  const char * errmsg="Errors encountered while attempting to write file header.";
  //Always start the file with an unsigned char-array (for endian agnosticity)
  //containing magic word (MCPL), file format version ('001'-'999') and
  //endianness used in the file ('L' or 'B'):
  unsigned char start[8] = {'M','C','P','L','0','0','0','L'};
  start[4] = (MCPL_FORMATVERSION/100)%10 + '0';
  start[5] = (MCPL_FORMATVERSION/10)%10 + '0';
  start[6] = MCPL_FORMATVERSION%10 + '0';
  if (!mcpl_platform_is_little_endian())
    start[7] = 'B';
  size_t nb = fwrite(start, 1, sizeof(start), f->file);
  if (nb!=sizeof(start))
    mcpl_error(errmsg);

  //Right after the initial 8 bytes, we put the number of particles (0 for now,
  //but important that position is fixed so we can seek and update it later).:
  long int nparticles_pos = ftell(f->file);
  if (nparticles_pos!=MCPL_NPARTICLES_POS)
    mcpl_error(errmsg);
  nb = fwrite(&f->nparticles, 1, sizeof(f->nparticles), f->file);
  if (nb!=sizeof(f->nparticles))
    mcpl_error(errmsg);

  //Then a bunch of numbers:
  uint32_t arr[8];
  arr[0] = f->ncomments;
  arr[1] = f->nblobs;
  arr[2] = f->opt_userflags;
  arr[3] = f->opt_polarisation;
  arr[4] = f->opt_singleprec;
  arr[5] = f->opt_universalpdgcode;
  arr[6] = f->particle_size;
  arr[7] = 0;//reserved to implement particle groups
  assert(sizeof(arr)==32);
  nb = fwrite(arr, 1, sizeof(arr), f->file);
  if (nb!=sizeof(arr))
    mcpl_error(errmsg);

  //strings:
  mcpl_write_string(f->file,f->hdr_srcprogname?f->hdr_srcprogname:"unknown",errmsg);
  for (uint32_t i = 0; i < f->ncomments; ++i)
    mcpl_write_string(f->file,f->comments[i],errmsg);

  //blob keys:
  for (uint32_t i = 0; i < f->nblobs; ++i)
    mcpl_write_string(f->file,f->blobkeys[i],errmsg);

  //blobs:
  for (uint32_t i = 0; i < f->nblobs; ++i)
    mcpl_write_buffer(f->file, f->bloblengths[i], f->blobs[i],errmsg);

  //Free up acquired memory only needed for header writing:
  free(f->hdr_srcprogname);
  f->hdr_srcprogname = 0;
  if (f->ncomments) {
    for (uint32_t i = 0; i < f->ncomments; ++i)
      free(f->comments[i]);
    free(f->comments);
    f->comments=0;
    f->ncomments=0;
  }
  if (f->nblobs) {
    for (uint32_t i = 0; i < f->nblobs; ++i)
      free(f->blobkeys[i]);
    free(f->blobkeys);
    for (uint32_t i = 0; i < f->nblobs; ++i)
      free(f->blobs[i]);
    free(f->blobs);
    free(f->bloblengths);
  }
  f->header_notwritten = 0;
}

void mcpl_transfer_fields_d2s(const mcpl_particle_t* pd,mcpl_particlesingleprec_t* ps)
{
  //transfers all fields except energy
  ps->polarisation[0] = pd->polarisation[0];
  ps->polarisation[1] = pd->polarisation[1];
  ps->polarisation[2] = pd->polarisation[2];
  ps->position[0] = pd->position[0];
  ps->position[1] = pd->position[1];
  ps->position[2] = pd->position[2];
  ps->direction[0] = pd->direction[0];
  ps->direction[1] = pd->direction[1];
  ps->direction[2] = pd->direction[2];
  ps->time = pd->time;
  ps->weight = pd->weight;
  ps->pdgcode = pd->pdgcode;
  ps->userflags = pd->userflags;
}

void mcpl_transfer_fields_s2d(const mcpl_particlesingleprec_t* ps, mcpl_particle_t* pd)
{
  //transfers all fields except energy
  pd->polarisation[0] = ps->polarisation[0];
  pd->polarisation[1] = ps->polarisation[1];
  pd->polarisation[2] = ps->polarisation[2];
  pd->position[0] = ps->position[0];
  pd->position[1] = ps->position[1];
  pd->position[2] = ps->position[2];
  pd->direction[0] = ps->direction[0];
  pd->direction[1] = ps->direction[1];
  pd->direction[2] = ps->direction[2];
  pd->time = ps->time;
  pd->weight = ps->weight;
  pd->pdgcode = ps->pdgcode;
  pd->userflags = ps->userflags;
}

void mcpl_unitvect_pack(const double* in, double* out) {
  //Octahedral packing inspired by http://jcgt.org/published/0003/02/01/
  //
  //and:
  //
  //Octahedron Environment Maps, T. Engelhardt & C. Dachsbacher, Conference:
  //Proceedings of the Vision, Modeling, and Visualization Conference 2008, VMV
  //2008, Konstanz, Germany, October 8-10, 2008
  //
  //Note that we might consider using the hemi-oct storage suggested in the
  //first of the two above references, since we can store the sign of the z
  //component elsewhere (like in the signbit of the kinetic energy).

  //project unit sphere to octahedron and store x and y coords:
  double n = 1.0 / (fabs(in[0]) + fabs(in[1]) + fabs(in[2]));
  out[0] = in[0] * n;  out[1] = in[1] * n;
  if (in[2] > 0.0)
    return;
  //For the lower hemisphere, we reflect the folds over the diagonals:
  double tmp = out[0];
  out[0] = ( 1.0-fabs(out[1]) ) * ( out[0]>=0.0? 1.0 : -1.0 );
  out[1] = ( 1.0-fabs( tmp  ) ) * ( out[1]>=0.0? 1.0 : -1.0 );
}

void mcpl_unitvect_unpack(const double* in, double* out) {
  //restore z-coord of octahedron:
  out[2] = 1.0 - fabs(in[0]) - fabs(in[1]);
  if (out[2]<0) {
    //lower hemisphere
    out[0] = ( 1.0 - fabs( in[1] ) ) * ( in[0] >= 0.0 ? 1.0 : -1.0 );
    out[1] = ( 1.0 - fabs( in[0] ) ) * ( in[1] >= 0.0 ? 1.0 : -1.0 );
  } else {
    //upper hemisphere
    out[0] = in[0];
    out[1] = in[1];
  }
  //project from octahedron to unit sphere:
  double n = 1.0 / sqrt(out[0]*out[0]+out[1]*out[1]+out[2]*out[2]);
  out[0] *= n; out[1] *= n; out[2] *= n;
}

void mcpl_add_particle(mcpl_outfile_t of,const mcpl_particle_t* particle)
{
  MCPL_OUTFILEDECODE;
  if (f->header_notwritten)
    mcpl_write_header(of);
  f->nparticles += 1;

  //Sanity check (add more??):
  double dirsq = particle->direction[0] * particle->direction[0]
    + particle->direction[1] * particle->direction[1]
    + particle->direction[2] * particle->direction[2];
  if (fabs(dirsq-1.0)>1.0e-5)
    mcpl_error("attempting to add particle with non-unit direction vector");
  if (particle->ekin<0.0)
    mcpl_error("attempting to add particle with negative kinetic energy");

  //shuffle both ekin and unit vector into the three directional doubles
  //(essentially loss-less packing!):
  mcpl_particle_t * parnc = ((mcpl_particle_t*)particle);
  double dir[3];
  dir[0] = particle->direction[0];
  dir[1] = particle->direction[1];
  dir[2] = particle->direction[2];
  mcpl_unitvect_pack(dir,parnc->direction);
  //sometimes the unit vector packing->unpacking transforms z=0 into
  //z=somethingverysmall. We (ab)use the sign bit on the kinetic energy storage
  //to remember when z is strictly 0.
  parnc->direction[2] = particle->ekin;
  if (!dir[2]) {
    parnc->direction[2] = copysign(particle->ekin,-1.0);//copysign to be sure the signbit is set also when ekin=0
    assert(signbit(parnc->direction[2]));
  }

  if (f->opt_userflags&&f->opt_universalpdgcode)
    ((mcpl_particle_t*)particle)->pdgcode = particle->userflags;//shuffle userflags into pdgcode field
  size_t nb;
  if (!f->opt_singleprec) {
    nb = fwrite((char*)particle + f->particle_offset, 1, f->particle_size, f->file);
  } else {
    assert(f->psp);
    mcpl_transfer_fields_d2s(particle,f->psp);
    nb=fwrite((char*)(f->psp) + f->particle_offset, 1, f->particle_size, f->file);
  }
  //shuffle back:
  if (f->opt_userflags&&f->opt_universalpdgcode)
    parnc->userflags = particle->pdgcode;
  parnc->direction[0] = dir[0];
  parnc->direction[1] = dir[1];
  parnc->direction[2] = dir[2];
  if (nb!=f->particle_size)
    mcpl_error("Errors encountered while attempting to write particle data.");
}

void mcpl_update_nparticles(FILE* f, uint64_t n)
{
  //Seek and update nparticles at correct location in header:
  const char * errmsg = "Errors encountered while attempting to update number of particles in file.";
  long int savedpos = ftell(f);
  if (savedpos<0)
    mcpl_error(errmsg);
  if (fseek( f, MCPL_NPARTICLES_POS, SEEK_SET ))
    mcpl_error(errmsg);
  size_t nb = fwrite(&n, 1, sizeof(n), f);
  if (nb != sizeof(n))
    mcpl_error(errmsg);
  if (fseek( f, savedpos, SEEK_SET ))
    mcpl_error(errmsg);
}

void mcpl_close_outfile(mcpl_outfile_t of)
{
  MCPL_OUTFILEDECODE;
  if (f->header_notwritten)
    mcpl_write_header(of);
  if (f->nparticles)
    mcpl_update_nparticles(f->file,f->nparticles);
  fclose(f->file);
  free(f->filename);
  free(f->psp);
  free(f);
}

void mcpl_closeandgzip_outfile(mcpl_outfile_t of)
{
  MCPL_OUTFILEDECODE;
  char * filename = f->filename;
  f->filename = 0;//prevent free in mcpl_close_file:
  mcpl_close_outfile(of);
  mcpl_gzip_file(filename);
  free(filename);
}

typedef struct {
  FILE * file;
#if MCPL_HASZLIB
  gzFile filegz;
#else
  void * filegz;
#endif
  char * hdr_srcprogname;
  unsigned format_version;
  int opt_userflags;
  int opt_polarisation;
  int opt_singleprec;
  int opt_universalpdgcode;
  int is_little_endian;
  unsigned long nparticles;
  uint32_t ncomments;
  char ** comments;
  uint32_t nblobs;
  char ** blobkeys;
  uint32_t * bloblengths;
  char ** blobs;
  unsigned particle_size;
  int particle_offset;
  long int first_particle_pos;
  unsigned long current_particle_idx;
  mcpl_particle_t* particle;
  mcpl_particlesingleprec_t * psp;
} mcpl_fileinternal_t;

#define MCPL_FILEDECODE mcpl_fileinternal_t * f = (mcpl_fileinternal_t *)ff.internal; assert(f)

void mcpl_read_buffer(mcpl_fileinternal_t* f, unsigned* n, char ** buf, const char * errmsg)
{
  size_t nb;
#if MCPL_HASZLIB
  if (f->filegz)
    nb = gzread(f->filegz, n, sizeof(*n));
  else
#endif
    nb = fread(n, 1, sizeof(*n), f->file);
  if (nb!=sizeof(*n))
    mcpl_error(errmsg);
  *buf = (char*)calloc(*n,1);
#if MCPL_HASZLIB
  if (f->filegz)
    nb = gzread(f->filegz, *buf, *n);
  else
#endif
    nb = fread(*buf, 1, *n, f->file);
  if (nb!=*n)
    mcpl_error(errmsg);
}

void mcpl_read_string(mcpl_fileinternal_t* f, char ** dest, const char* errmsg)
{
  size_t nb;
  uint32_t n;
#if MCPL_HASZLIB
  if (f->filegz)
    nb = gzread(f->filegz, &n, sizeof(n));
  else
#endif
    nb = fread(&n, 1, sizeof(n), f->file);
  if (nb!=sizeof(n))
    mcpl_error(errmsg);
  char * s = (char*)calloc(n+1,1);
#if MCPL_HASZLIB
  if (f->filegz)
    nb = gzread(f->filegz, s, n);
  else
#endif
    nb = fread(s, 1, n, f->file);
  if (nb!=n)
    mcpl_error(errmsg);
  s[n] = '\0';
  *dest = s;
}

mcpl_file_t mcpl_actual_open_file(const char * filename, int * repair_status)
{
  int caller_is_mcpl_repair = *repair_status;
  *repair_status = 0;//file not broken

  if (!filename)
    mcpl_error("mcpl_open_file called with null string");

  mcpl_platform_compatibility_check();

  mcpl_file_t out;
  out.internal = 0;

  mcpl_fileinternal_t * f = (mcpl_fileinternal_t*)calloc(sizeof(mcpl_fileinternal_t),1);
  assert(f);

  //open file (with gzopen if filename ends with .gz):
  f->file = 0;
  f->filegz = 0;
  char * lastdot = strrchr(filename, '.');
  if (lastdot && strcmp(lastdot, ".gz") == 0) {
#if MCPL_HASZLIB
    f->filegz = gzopen(filename,"rb");
    if (!f->filegz)
      mcpl_error("Unable to open file!");
#else
    mcpl_error("This installation of MCPL was not built with zlib support and can not read compressed (.gz) files directly.");
#endif
  } else {
    f->file = fopen(filename,"rb");
    if (!f->file)
      mcpl_error("Unable to open file!");
  }

  //First read and check magic word, format version and endianess.
  unsigned char start[8];// = {'M','C','P','L','0','0','0','L'};
  size_t nb;
#if MCPL_HASZLIB
  if (f->filegz)
    nb = gzread(f->filegz, start, sizeof(start));
  else
#endif
    nb = fread(start, 1, sizeof(start), f->file);
  if (nb>=4&&(start[0]!='M'||start[1]!='C'||start[2]!='P'||start[3]!='L'))
    mcpl_error("File is not an MCPL file!");
  if (nb!=sizeof(start))
    mcpl_error("Error while reading first bytes of file!");
  f->format_version = (start[4]-'0')*100 + (start[5]-'0')*10 + (start[6]-'0');
  if (f->format_version!=2)
    mcpl_error("File is in an unsupported MCPL version!");
  f->is_little_endian = mcpl_platform_is_little_endian();
  if (start[7]!=(f->is_little_endian?'L':'B'))
    mcpl_error("Endian-ness of current platform is different than the one used to write the file.");

  //proceed reading header, knowing we have a consistent version and endian-ness.
  const char * errmsg = "Errors encountered while attempting to read header";

  uint64_t np;
#if MCPL_HASZLIB
  if (f->filegz)
    nb = gzread(f->filegz, &np, sizeof(np));
  else
#endif
    nb = fread(&np, 1, sizeof(np), f->file);
  if (nb!=sizeof(np))
    mcpl_error(errmsg);
  f->nparticles = np;

  uint32_t arr[8];
  assert(sizeof(arr)==32);
#if MCPL_HASZLIB
  if (f->filegz)
    nb = gzread(f->filegz, arr, sizeof(arr));
  else
#endif
    nb=fread(arr, 1, sizeof(arr), f->file);
  assert(nb==sizeof(arr));
  if (nb!=sizeof(arr))
    mcpl_error(errmsg);

  f->ncomments = arr[0];
  f->nblobs = arr[1];
  f->opt_userflags = arr[2];
  f->opt_polarisation = arr[3];
  f->opt_singleprec = arr[4];
  f->opt_universalpdgcode = arr[5];
  f->particle_size = arr[6];
  if (arr[7]!=0)
    mcpl_error("Reading error: reserved word is mysteriously non-zero.");

  f->particle_offset = f->opt_polarisation ? 0 : 3*(f->opt_singleprec?sizeof(float):sizeof(double));
  if (!f->opt_singleprec)
    f->particle_offset += sizeof(double);//skip past initial energy field

  //Then some strings:
  mcpl_read_string(f,&f->hdr_srcprogname,errmsg);
  f->comments = f->ncomments ? (char **)calloc(f->ncomments,sizeof(char*)) : 0;
  for (uint32_t i = 0; i < f->ncomments; ++i)
    mcpl_read_string(f,&(f->comments[i]),errmsg);

  f->blobkeys = 0;
  f->bloblengths = 0;
  f->blobs = 0;
  if (f->nblobs) {
    f->blobs = (char **)calloc(f->nblobs,sizeof(char*));
    f->blobkeys = (char **)calloc(f->nblobs,sizeof(char*));
    f->bloblengths = (uint32_t *)calloc(f->nblobs,sizeof(uint32_t));
    for (uint32_t i =0; i < f->nblobs; ++i)
      mcpl_read_string(f,&(f->blobkeys[i]),errmsg);
    for (uint32_t i =0; i < f->nblobs; ++i)
      mcpl_read_buffer(f, &(f->bloblengths[i]), &(f->blobs[i]), errmsg);
  }
  f->particle = (mcpl_particle_t*)calloc(sizeof(mcpl_particle_t),1);
  f->psp = f->opt_singleprec ? (mcpl_particlesingleprec_t*)calloc(sizeof(mcpl_particlesingleprec_t),1) : 0;

  //At first event now:
  f->current_particle_idx = 0;
#if MCPL_HASZLIB
  if (f->filegz)
    f->first_particle_pos = gztell(f->filegz);
  else
#endif
    f->first_particle_pos = ftell(f->file);
  if (f->first_particle_pos<0)
    mcpl_error(errmsg);

  if (f->nparticles==0) {
    //Although empty files are permitted, it is possible that the file was never
    //closed properly (maybe the writing program ended prematurely). Let us
    //check to possibly recover usage of the file.
    if (f->filegz) {
      //SEEK_END is not supported by zlib, and there is no reliable way to get
      //the input size. Thus, all we can do is to uncompress the whole thing,
      //which we won't since it might stall operations for a long time. But we
      //can at least try to check whether the file is indeed empty or not, and
      //give an error in the latter case:
#if MCPL_HASZLIB
      char testbuf[4];
      nb = gzread(f->filegz, testbuf, sizeof(testbuf));
      if (nb>0) {
        if (caller_is_mcpl_repair) {
          *repair_status = 1;//file broken but can't recover since gzip.
        } else {
          mcpl_error("Input file appears to not have been closed properly and data recovery is disabled for gzipped files.");
        }
      }
      gzseek( f->filegz, f->first_particle_pos, SEEK_SET );
#endif
    } else {
      if (f->file && !fseek( f->file, 0, SEEK_END )) {//SEEK_END is not guaranteed to always work, so we fail our recovery attempt silently.
        long int endpos = ftell(f->file);
        if (endpos>f->first_particle_pos && endpos != f->first_particle_pos) {
          f->nparticles = ( endpos - f->first_particle_pos ) / f->particle_size;
          if (caller_is_mcpl_repair) {
            *repair_status = 2;//file broken and should be able to repair
          } else {
            printf("MCPL WARNING: Input file appears to not have been closed properly. Recovered %lu particles.\n",f->nparticles);
          }
        }
      }
      fseek( f->file, f->first_particle_pos, SEEK_SET );//if this fseek failed, it might just be that we are at EOF with no particles.
    }
  }

  out.internal = f;
  return out;
}

mcpl_file_t mcpl_open_file(const char * filename)
{
  int repair_status = 0;
  return mcpl_actual_open_file(filename,&repair_status);
}

void mcpl_repair(const char * filename)
{
  int repair_status = 1;
  mcpl_file_t f = mcpl_actual_open_file(filename,&repair_status);
  unsigned long nparticles = mcpl_hdr_nparticles(f);
  mcpl_close_file(f);
  if (repair_status==0) {
    mcpl_error("Asked to repair file which does not appear to be broken.");
  } else if (repair_status==1) {
    mcpl_error("Input file is indeed broken, but must be gunzipped before it can be repaired.");
  }
  //Ok, we should repair the file by updating nparticles in the header:
  FILE * fh = fopen(filename,"rb+");
  if (!fh)
    mcpl_error("Unable to open file in update mode!");
  mcpl_update_nparticles(fh, nparticles);
  fclose(fh);
  //Verify that we fixed it:
  repair_status = 1;
  f = mcpl_actual_open_file(filename,&repair_status);
  unsigned long nparticles2 = mcpl_hdr_nparticles(f);
  mcpl_close_file(f);
  if (repair_status==0&&nparticles==nparticles2) {
    printf("Succesfully repaired file with %lu particles.\n",nparticles);
  } else {
    mcpl_error("Something went wrong while attempting to repair file.");
  }
}

void mcpl_close_file(mcpl_file_t ff)
{
  MCPL_FILEDECODE;

  free(f->hdr_srcprogname);
  for (uint32_t i = 0; i < f->ncomments; ++i)
    free(f->comments[i]);
  free(f->comments);
  for (uint32_t i = 0; i < f->nblobs; ++i)
    free(f->blobkeys[i]);
  for (uint32_t i = 0; i < f->nblobs; ++i)
    free(f->blobs[i]);
  free(f->blobkeys);
  free(f->blobs);
  free(f->bloblengths);
  free(f->particle);
  free(f->psp);
#if MCPL_HASZLIB
  if (f->filegz)
    gzclose(f->filegz);
#endif
  if (f->file)
    fclose(f->file);
  free(f);
}


unsigned mcpl_hdr_version(mcpl_file_t ff)
{
  MCPL_FILEDECODE;
  return f->format_version;
}

unsigned long mcpl_hdr_nparticles(mcpl_file_t ff)
{
  MCPL_FILEDECODE;
  return f->nparticles;
}

unsigned mcpl_hdr_ncomments(mcpl_file_t ff)
{
  MCPL_FILEDECODE;
  return f->ncomments;
}

const char * mcpl_hdr_comment(mcpl_file_t ff, unsigned i)
{
  MCPL_FILEDECODE;
  if (i>=f->ncomments)
    mcpl_error("Invalid comment requested (index out of bounds)");
  return f->comments[i];
}

int mcpl_hdr_nblobs(mcpl_file_t ff)
{
  MCPL_FILEDECODE;
  return f->nblobs;
}

const char** mcpl_hdr_blobkeys(mcpl_file_t ff)
{
  MCPL_FILEDECODE;
  return (const char**)f->blobkeys;
}

int mcpl_hdr_blob(mcpl_file_t ff, const char* key,
                  unsigned* ldata, const char ** data)
{
  MCPL_FILEDECODE;
  for (uint32_t i = 0; i < f->nblobs; ++i) {
    if (strcmp(f->blobkeys[i],key)==0) {
      *data = f->blobs[i];
      *ldata = f->bloblengths[i];
      return 1;
    }
  }
  *data = 0;
  *ldata = 0;
  return 0;
}

const char* mcpl_hdr_srcname(mcpl_file_t ff)
{
  MCPL_FILEDECODE;
  return f->hdr_srcprogname;
}

int mcpl_hdr_has_userflags(mcpl_file_t ff)
{
  MCPL_FILEDECODE;
  return f->opt_userflags;
}

int mcpl_hdr_has_polarisation(mcpl_file_t ff)
{
  MCPL_FILEDECODE;
  return f->opt_polarisation;
}

int mcpl_hdr_has_doubleprec(mcpl_file_t ff)
{
  MCPL_FILEDECODE;
  return !f->opt_singleprec;
}

const mcpl_particle_t* mcpl_read(mcpl_file_t ff)
{
  MCPL_FILEDECODE;
  f->current_particle_idx += 1;
  if (f->current_particle_idx >= f->nparticles+1)
    return 0;
  //read particle data (note due to size and offset, some ends of the struct
  //might be ignored, but they were nulled out at initialisation):

  size_t nb;
  unsigned lbuf = f->particle_size;
  if (!f->opt_singleprec) {
    char * buf = (char*)(f->particle) + f->particle_offset;
#if MCPL_HASZLIB
    if (f->filegz)
      nb = gzread(f->filegz, buf, lbuf);
    else
#endif
      nb = fread(buf, 1, lbuf, f->file);
  } else {
    char * buf = (char*)(f->psp) + f->particle_offset;
#if MCPL_HASZLIB
    if (f->filegz)
      nb = gzread(f->filegz, buf, lbuf);
    else
#endif
      nb = fread(buf, 1, lbuf, f->file);
    mcpl_transfer_fields_s2d(f->psp,f->particle);
  }
  if (nb!=f->particle_size)
    mcpl_error("Errors encountered while attempting to read particle data.");
  if (f->opt_universalpdgcode) {
    if (f->opt_userflags)
      f->particle->userflags=f->particle->pdgcode;//shuffle fields
    f->particle->pdgcode=f->opt_universalpdgcode;
  }

  f->particle->ekin = f->particle->direction[2];
  double packdir[2];
  packdir[0] = f->particle->direction[0];
  packdir[1] = f->particle->direction[1];
  mcpl_unitvect_unpack(packdir,f->particle->direction);
  if (signbit(f->particle->ekin)) {
    f->particle->ekin = - f->particle->ekin;
    f->particle->direction[2] = 0.0;
  }
  return f->particle;
}

int mcpl_skipforward(mcpl_file_t ff,unsigned long n)
{
  MCPL_FILEDECODE;
  f->current_particle_idx += n;
  int notEOF = f->current_particle_idx<f->nparticles;
  if (n==0)
    return notEOF;
  if (notEOF) {
    int error;
#if MCPL_HASZLIB
    if (f->filegz) {
      long targetpos = f->current_particle_idx*f->particle_size+f->first_particle_pos;
      error = gzseek( f->filegz, targetpos, SEEK_SET )!=targetpos;
    } else
#endif
      error = fseek( f->file, f->particle_size * n, SEEK_CUR )!=0;
    if (error)
      mcpl_error("Errors encountered while skipping in particle list");
  }
  return notEOF;
}

int mcpl_rewind(mcpl_file_t ff)
{
  MCPL_FILEDECODE;
  int already_there = (f->current_particle_idx==0);
  f->current_particle_idx = 0;
  int notEOF = f->current_particle_idx<f->nparticles;
  if (notEOF&&!already_there) {
    int error;
#if MCPL_HASZLIB
    if (f->filegz) {
      error = gzseek( f->filegz, f->first_particle_pos, SEEK_SET )!=f->first_particle_pos;
    } else
#endif
      error = fseek( f->file, f->first_particle_pos, SEEK_SET )!=0;
    if (error)
      mcpl_error("Errors encountered while rewinding particle list");
  }
  return notEOF;
}

int mcpl_seek(mcpl_file_t ff,unsigned long ipos)
{
  MCPL_FILEDECODE;
  int already_there = (f->current_particle_idx==ipos);
  f->current_particle_idx = ipos;
  int notEOF = f->current_particle_idx<f->nparticles;
  if (notEOF&&!already_there) {
    int error;
#if MCPL_HASZLIB
    if (f->filegz) {
      long targetpos = f->current_particle_idx*f->particle_size+f->first_particle_pos;
      error = gzseek( f->filegz, targetpos, SEEK_SET )!=targetpos;
    } else
#endif
      error = fseek( f->file, f->first_particle_pos + f->particle_size * ipos, SEEK_SET )!=0;
    if (error)
      mcpl_error("Errors encountered while seeking in particle list");
  }
  return notEOF;
}

unsigned long mcpl_currentposition(mcpl_file_t ff)
{
  MCPL_FILEDECODE;
  return f->current_particle_idx;
}

const char * mcpl_basename(const char * filename)
{
  //portable "basename" which doesn't modify it's argument:
  const char * bn = strrchr(filename, '/');
  return bn ? bn + 1 : filename;
}

int mcpl_hdr_particle_size(mcpl_file_t ff)
{
  MCPL_FILEDECODE;
  return f->particle_size;
}

int mcpl_hdr_header_size(mcpl_file_t ff)
{
  MCPL_FILEDECODE;
  return f->first_particle_pos;
}

int mcpl_hdr_universel_pdgcode(mcpl_file_t ff)
{
  MCPL_FILEDECODE;
  return f->opt_universalpdgcode;
}

int mcpl_hdr_little_endian(mcpl_file_t ff)
{
  MCPL_FILEDECODE;
  return f->is_little_endian;
}

void mcpl_dump_header(mcpl_file_t f)
{
  printf("\n  Basic info\n");
  printf("    Format             : MCPL-%i\n",mcpl_hdr_version(f));
  printf("    No. of particles   : %lu\n",mcpl_hdr_nparticles(f));
  printf("    Header storage     : %i bytes\n",mcpl_hdr_header_size(f));
  printf("    Data storage       : %lu bytes\n",
         mcpl_hdr_nparticles(f)*mcpl_hdr_particle_size(f));
  printf("\n  Custom meta data\n");
  printf("    Source             : \"%s\"\n",mcpl_hdr_srcname(f));
  unsigned nc=mcpl_hdr_ncomments(f);
  printf("    Number of comments : %i\n",nc);
  for (unsigned ic = 0; ic < nc; ++ic)
    printf("          -> comment %i : \"%s\"\n",ic,mcpl_hdr_comment(f,ic));
  unsigned nb = mcpl_hdr_nblobs(f);
  printf("    Number of blobs    : %i\n",nb);
  const char** blobkeys = mcpl_hdr_blobkeys(f);
  for (uint32_t ib = 0; ib < nb; ++ib) {
    const char * data;
    unsigned ldata;
    int ok = mcpl_hdr_blob(f, blobkeys[ib], &ldata, &data);
    if (!ok)
      mcpl_error("Unexpected blob access error");
    printf("          -> %i bytes of data with key \"%s\"\n",ldata,blobkeys[ib]);
  }

  printf("\n  Particle data format\n");
  printf("    User flags         : %s\n",(mcpl_hdr_has_userflags(f)?"yes":"no"));
  printf("    Polarisation info  : %s\n",(mcpl_hdr_has_polarisation(f)?"yes":"no"));
  printf("    Fixed part. type   : ");
  int updg = mcpl_hdr_universel_pdgcode(f);
  if (updg)
    printf("yes (pdgcode %i)\n",updg);
  else
    printf("no\n");
  printf("    FP precision       : %s\n",(mcpl_hdr_has_doubleprec(f)?"double":"single"));
  printf("    Endianness         : %s\n",(mcpl_hdr_little_endian(f)?"little":"big"));
  printf("    Storage            : %i bytes/particle\n",mcpl_hdr_particle_size(f));

  printf("\n");
}

void mcpl_dump_particles(mcpl_file_t f, unsigned nskip, unsigned nlimit)
{
  int has_uf = mcpl_hdr_has_userflags(f);
  int has_pol = mcpl_hdr_has_polarisation(f);
  printf("index     pdgcode   ekin[MeV]       x[cm]       y[cm]       z[cm]          ux          uy          uz    time[ms]      weight");
  if (has_pol)
    printf("       pol-x       pol-y       pol-z");
  if (has_uf)
    printf("  userflags");
  printf("\n");
  mcpl_skipforward(f,nskip);
  unsigned count = nlimit;
  const mcpl_particle_t* p;
  while((nlimit==0||count--)&&(p=mcpl_read(f))) {
    unsigned long idx = mcpl_currentposition(f)-1;//-1 since mcpl_read skipped ahead
    printf("%5lu %11i %11.5g %11.5g %11.5g %11.5g %11.5g %11.5g %11.5g %11.5g %11.5g",
           idx,
           p->pdgcode,
           p->ekin,
           p->position[0],
           p->position[1],
           p->position[2],
           p->direction[0],
           p->direction[1],
           p->direction[2],
           p->time,
           p->weight
           );
    if (has_pol)
      printf(" %11.5g %11.5g %11.5g",p->polarisation[0],p->polarisation[1],p->polarisation[2]);
    if (has_uf)
      printf(" 0x%08x",p->userflags);
    printf("\n");
  }
}

void mcpl_dump(const char * filename, int parts, unsigned nskip, unsigned nlimit)
{
  if (parts<0||parts>2)
    mcpl_error("mcpl_dump got forbidden value for argument parts");
  mcpl_file_t f = mcpl_open_file(filename);
  printf("Opened MCPL file %s:\n",mcpl_basename(filename));
  if (parts==0||parts==1)
    mcpl_dump_header(f);
  if (parts==0||parts==2)
    mcpl_dump_particles(f,nskip,nlimit);
  mcpl_close_file(f);
}

int mcpl_actual_can_merge(mcpl_file_t ff1, mcpl_file_t ff2)
{
  mcpl_fileinternal_t * f1 = (mcpl_fileinternal_t *)ff1.internal;
  mcpl_fileinternal_t * f2 = (mcpl_fileinternal_t *)ff2.internal;
  assert(f1&&f2);
  if (f1->first_particle_pos!=f2->first_particle_pos)
    return 0;//different header

  //Very strict checking of everything except nparticles. Even order of blobs
  //and comments must be preserved (could possibly be relaxed a bit):
  if (strcmp(f1->hdr_srcprogname,f2->hdr_srcprogname)!=0) return 0;
  if (f1->format_version!=f2->format_version) return 0;
  if (f1->opt_userflags!=f2->opt_userflags) return 0;
  if (f1->opt_polarisation!=f2->opt_polarisation) return 0;
  if (f1->opt_singleprec!=f2->opt_singleprec) return 0;
  if (f1->opt_universalpdgcode!=f2->opt_universalpdgcode) return 0;
  if (f1->is_little_endian!=f2->is_little_endian) return 0;
  if (f1->particle_size!=f2->particle_size) return 0;
  if (f1->particle_offset!=f2->particle_offset) return 0;
  if (f1->ncomments!=f2->ncomments) return 0;
  if (f1->nblobs!=f2->nblobs) return 0;
  for (uint32_t i = 0; i<f1->ncomments; ++i) {
    if (strcmp(f1->comments[i],f2->comments[i])!=0) return 0;
  }
  for (uint32_t i = 0; i<f1->nblobs; ++i) {
    if (f1->bloblengths[i]!=f2->bloblengths[i]) return 0;
    if (strcmp(f1->blobkeys[i],f2->blobkeys[i])!=0) return 0;
    if (memcmp(f1->blobs[i],f2->blobs[i],f1->bloblengths[i])!=0) return 0;
  }
  return 1;
}


int mcpl_can_merge(const char * file1, const char* file2)
{
  mcpl_file_t f1 = mcpl_open_file(file1);
  mcpl_file_t f2 = mcpl_open_file(file2);
  int can_merge = mcpl_actual_can_merge(f1,f2);
  mcpl_close_file(f1);
  mcpl_close_file(f2);
  return can_merge;
}

void mcpl_merge(const char * file1, const char* file2)
{
  mcpl_file_t ff1 = mcpl_open_file(file1);
  mcpl_file_t ff2 = mcpl_open_file(file2);
  int can_merge = mcpl_actual_can_merge(ff1,ff2);
  if (!can_merge) {
    mcpl_close_file(ff1);
    mcpl_close_file(ff2);
    mcpl_error("Attempting to merge incompatible files");
  }

  mcpl_fileinternal_t * f1 = (mcpl_fileinternal_t *)ff1.internal;
  mcpl_fileinternal_t * f2 = (mcpl_fileinternal_t *)ff2.internal;
  assert(f1&&f2);
  unsigned long np1 = f1->nparticles;
  unsigned long np2 = f2->nparticles;
  if (!np2)
    return;//nothing to take from file 2.

  if (f1->filegz)
    mcpl_error("direct modification of gzipped files is not supported.");

  if (f2->filegz)
    mcpl_error("merging of gzipped files is not supported.");//TODO: file2 can be gzipped!

  unsigned particle_size = f1->particle_size;
  long int first_particle_pos = f1->first_particle_pos;

  //Should be same since can_merge:
  assert(particle_size==f2->particle_size);
  assert(first_particle_pos==f2->first_particle_pos);

  //Now, close file1 and reopen in append mode:
  mcpl_close_file(ff1);

  FILE * f1a = fopen(file1,"rb+");
  if (!f1a)
    mcpl_error("Unable to open file1 in update mode!");
  if (fseek( f1a, first_particle_pos + particle_size*np1, SEEK_SET ))
    mcpl_error("Unable to seek to end of file1 in update mode");

  //f2->file is already at the position for the first particle.

  //buffer for transferring up to 1000 particles at a time:
  char * buf = malloc(1000*particle_size);
  unsigned long np_remaining = np2;

  while(np_remaining) {
    //NB: On linux > 2.6.33 we could use sendfile for more efficient in-kernel
    //transfer of data between two files!
    unsigned toread = np_remaining >= 1000 ? 1000 : np_remaining;
    np_remaining -= toread;
    size_t nb = fread(buf,1,toread*particle_size,f2->file);
    if (nb!=toread*particle_size)
      mcpl_error("Unexpected read-error while merging");
    nb = fwrite(buf,1,toread*particle_size,f1a);
    if (nb!=toread*particle_size)
      mcpl_error("Unexpected write-error while merging");
  }

  //update number of particles in file1 + cleanup:
  free(buf);
  mcpl_close_file(ff2);
  mcpl_update_nparticles(f1a,np1+np2);
  fclose(f1a);
}

#define MCPL_TOOL_DEFAULT_NLIMIT 10
#define MCPL_TOOL_DEFAULT_NSKIP 0

int mcpl_tool_usage( char** argv, char * errmsg ) {
  if (errmsg) {
    printf("ERROR: %s\n\n",errmsg);
    printf("Run with -h or --help for usage information\n");
    return 1;
  }

  const char * progname = mcpl_basename(argv[0]);

  printf("Tool for inspecting or modifying Monte Carlo Particle List (.mcpl) files.\n");
  printf("\n");
  printf("The default behaviour is to display the contents of the FILE in human readable\n");
  printf("format (see Dump Options below for how to modify what is displayed).\n");
  printf("\n");
#if MCPL_HASZLIB
  printf("This installation supports direct reading of gzipped files (.mcpl.gz).\n");
  printf("\n");
#endif
  printf("Usage:\n");
  printf("  %s [dump-options] FILE\n",progname);
  printf("  %s --merge [merge-options] FILE1 FILE2\n",progname);
  printf("  %s --repair FILE\n",progname);
  printf("  %s --version\n",progname);
  printf("  %s --help\n",progname);
  printf("\n");
  printf("Dump Options:\n");
  printf("  By default include the info in the FILE header plus the first ten contained\n");
  printf("  particles. Modify with the following options:\n");
  assert(MCPL_TOOL_DEFAULT_NLIMIT==10);
  printf("  -j, --justhead  : Dump just header info and no particle info.\n");
  printf("  -n, --nohead    : Dump just particle info and no header info.\n");
  printf("  -lN             : Dump up to N particles from the file (default %i). You\n",MCPL_TOOL_DEFAULT_NLIMIT);
  printf("                    can specify -l0 to disable this limit.\n");
  printf("  -sN             : Skip past the first N particles in the file (default %i).\n",MCPL_TOOL_DEFAULT_NSKIP);
  printf("\n");
  printf("Merge Options:\n");
  printf("  -m, --merge FILE1 FILE2\n");
  printf("                    Appends the particle contents in FILE2 to the end of FILE1.\n");
  printf("                    Note that this will fail unless FILE1 and FILE2 have iden-\n");
  printf("                    tical headers (but see option --ignore below).\n");
  printf("  -i, --ignore      Ignore comments and binary blobs in FILE2. This allows some\n");
  printf("                    otherwise forbidden merges, but some info might get lost.\n");
  printf("\n");
  printf("Other options:\n");
  printf("  -r, --repair FILE\n");
  printf("                    Attempt to repair FILE which was not properly closed, by up-\n");
  printf("                    dating the file header with the correct number of particles.\n");
  printf("  -v, --version   : Display version of MCPL installation.\n");
  printf("  -h, --help      : Display this usage information (ignores all other options).\n");

  return 0;
}

int mcpl_tool(int argc,char** argv) {
  const char * filename1 = 0;
  const char * filename2 = 0;
  int opt_justhead = 0;
  int opt_nohead = 0;
  int opt_num_limit = -1;
  int opt_num_skip = -1;
  int opt_merge = 0;
  int opt_ignore = 0;
  int opt_repair = 0;
  int opt_version = 0;

  for (int i = 1; i<argc; ++i) {
    char * a = argv[i];
    size_t n=strlen(a);
    if (!n)
      continue;
    if (n>=2&&a[0]=='-'&&a[1]!='-') {
      //short options:
      int * consume_digit = 0;
      for (size_t j=1; j<n; ++j) {
        if (consume_digit) {
          if (a[j]<'0'||a[j]>'9')
            return mcpl_tool_usage(argv,"Bad option: expected number");
          *consume_digit *= 10;
          *consume_digit += a[j] - '0';
          continue;
        }
        switch(a[j]) {
          case 'h': return mcpl_tool_usage(argv,0);
          case 'j': opt_justhead = 1; break;
          case 'n': opt_nohead = 1; break;
          case 'm': opt_merge = 1; break;
          case 'i': opt_ignore = 1; break;
          case 'r': opt_repair = 1; break;
          case 'v': opt_version = 1; break;
          case 'l': consume_digit = &opt_num_limit; break;
          case 's': consume_digit = &opt_num_skip; break;
          default:
            return mcpl_tool_usage(argv,"Unrecognised option");
        }
        if (consume_digit) {
          *consume_digit = 0;
          if (j+1==n)
            return mcpl_tool_usage(argv,"Bad option: missing number");
        }
      }
    } else if (n>=3&&a[0]=='-'&&a[1]=='-') {
      a+=2;
      //long options:
      const char * lo_help = "help";
      const char * lo_justhead = "justhead";
      const char * lo_nohead = "nohead";
      const char * lo_merge = "merge";
      const char * lo_ignore = "ignore";
      const char * lo_repair = "repair";
      const char * lo_version = "version";
      //Use strstr instead of "strcmp(a,"--help")==0" to support shortened
      //versions (works since all our long-opts start with unique char).
      if (strstr(lo_help,a)==lo_help) return mcpl_tool_usage(argv,0);
      else if (strstr(lo_justhead,a)==lo_justhead) opt_justhead = 1;
      else if (strstr(lo_nohead,a)==lo_nohead) opt_nohead = 1;
      else if (strstr(lo_merge,a)==lo_merge) opt_merge = 1;
      else if (strstr(lo_ignore,a)==lo_ignore) opt_ignore = 1;
      else if (strstr(lo_repair,a)==lo_repair) opt_repair = 1;
      else if (strstr(lo_version,a)==lo_version) opt_version = 1;
      else return mcpl_tool_usage(argv,"Unrecognised option");
    } else if (n>=1&&a[0]!='-') {
      //input file
      if (filename2)
        return mcpl_tool_usage(argv,"Too many arguments.");
      if (filename1) filename2 = a;
      else filename1 = a;
    } else {
      return mcpl_tool_usage(argv,"Bad arguments");
    }
  }

  int any_dumpopts = (opt_justhead + opt_nohead + (opt_num_limit!=-1) + (opt_num_skip!=-1))!=0;
  int any_mergeopts = (opt_merge + opt_ignore)!=0;
  if (any_dumpopts+any_mergeopts+opt_repair+opt_version>1)
    return mcpl_tool_usage(argv,"Conflicting options specified.");

  if (opt_version) {
    if (filename1)
      return mcpl_tool_usage(argv,"Unrecognised arguments for --version.");
    printf("MCPL version %i.%i\n",MCPL_VERSION/100,MCPL_VERSION%100);
    return 0;
  }

  if (opt_ignore&&!opt_merge)
    return mcpl_tool_usage(argv,"Use --ignore only with --merge.");

  if (opt_merge) {
    if (!filename2)
      return mcpl_tool_usage(argv,"Must specify two input files with --merge.");
    if (opt_ignore)
      return mcpl_tool_usage(argv,"--ignore is not implemented yet.");

    if (!mcpl_can_merge(filename1,filename2))
      return mcpl_tool_usage(argv,"Requested files are incompatible for merge as they have different header info.");

    mcpl_merge(filename1,filename2);
    return 0;
  }

  if (filename2)
    return mcpl_tool_usage(argv,"Too many arguments.");

  if (!filename1)
    return mcpl_tool_usage(argv,"No input file specified");

  if (opt_repair) {
    mcpl_repair(filename1);
    return 0;
  }

  //Dump mode:

  if (opt_justhead&&(opt_num_limit!=-1||opt_num_skip!=-1))
    return mcpl_tool_usage(argv,"Do not specify -l or -s with --justhead");

  if (opt_num_limit<0) opt_num_limit = MCPL_TOOL_DEFAULT_NLIMIT;
  if (opt_num_skip<0) opt_num_skip = MCPL_TOOL_DEFAULT_NSKIP;

  if (opt_justhead&&opt_nohead)
    return mcpl_tool_usage(argv,"Do not supply both --justhead and --nohead.");

  int parts = 0;
  if (opt_nohead) parts=2;
  else if (opt_justhead) parts=1;
  mcpl_dump(filename1,parts,opt_num_skip,opt_num_limit);
  return 0;
}

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <errno.h>

void mcpl_gzip_file(const char * filename)
{
  const char * bn = strrchr(filename, '/');
  bn = bn ? bn + 1 : filename;

  //spawn process in which to perform gzip:
  printf("MCPL: Attempting to compress file %s with gzip\n",bn);
  fflush(0);
  pid_t gzip_pid = fork();
  if (gzip_pid) {
    //main proc
    int chld_state = 0;
    pid_t ret = waitpid(gzip_pid,&chld_state,0);
    if (ret!=gzip_pid||chld_state!=0)
      mcpl_error("ERROR encountered while attempting to invoke gzip\n");
    else
      printf("MCPL: Succesfully compressed file into %s.gz\n",bn);
  } else {
    //spawned proc in which to invoke gzip
    execlp("gzip", "gzip", "-f",filename, 0, 0);
    printf("MCPL: execlp/gzip error: %s\n",strerror(errno));
    exit(1);
  }
}
