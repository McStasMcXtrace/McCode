#ifndef MCPL_H
#define MCPL_H

#include <stdint.h>

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

#define MCPL_VERSION 4       //Code version (100*MAJOR+MINOR)
#define MCPL_FORMATVERSION 2 //Format version of written files

#ifndef MCPL_HASZLIB
#  define MCPL_HASZLIB 0     //Set to 1 if compiling and linking with zlib,
#endif                       //to allow transparent reading of .mcpl.gz files.

#ifdef __cplusplus
extern "C" {
#endif

  ///////////
  // Types //
  ///////////

#pragma pack (push, 1)

  //The data structure representing a particle (note that persistification of
  //polarisation and userflags must be explicitly enabled when writing .mcpl
  //files, or they will simply contain zeroes when the file is read):

  typedef struct {
    double ekin;            // kinetic energy [MeV]
    double polarisation[3]; // polarisation vector
    double position[3];     // position [cm]
    double direction[3];    // momentum direction (unit vector)
    double time;            // time-stamp [millisecond]
    double weight;          // weight or intensity
    int32_t pdgcode;   //MC particle number from the Particle Data Group (2112=neutron, 22=gamma, ...)
    int32_t userflags; //User flags (if used, the file header should probably contain information about how).
  } mcpl_particle_t;

#pragma pack (pop)

  typedef struct { void * internal; } mcpl_file_t;    //file-object used while reading .mcpl
  typedef struct { void * internal; } mcpl_outfile_t; //file-object used while writing .mcpl

  //////////////////////////////
  // Creating new .mcpl files //
  //////////////////////////////

  //Instantiate new file object (will also open and override specified file)
  mcpl_outfile_t mcpl_create_outfile(const char * filename);

  const char * mcpl_outfile_filename(mcpl_outfile_t);//filename being written too (might have had .mcpl appended)

  //Optionally optionally set global options or add info to the header:
  void mcpl_hdr_set_srcname(mcpl_outfile_t,const char *);//Name of the generating application
  void mcpl_hdr_add_comment(mcpl_outfile_t,const char *);//Add one or more human-readable comments
  void mcpl_hdr_add_data(mcpl_outfile_t, const char * key,
                         unsigned ldata, const char * data);//add binary blobs by key
  void mcpl_enable_userflags(mcpl_outfile_t);//to write the "userflags" info
  void mcpl_enable_polarisation(mcpl_outfile_t);//to write the "polarisation" info
  void mcpl_enable_doubleprec(mcpl_outfile_t);//use double precision FP numbers in storage
  void mcpl_enable_universal_pdgcode(mcpl_outfile_t, int pdgcode);//All particles are of the same type

  //Optionally (but rarely skipped) add particles, by updating the info in
  //and then passing in a pointer to an mcpl_particle_t instance:
  void mcpl_add_particle(mcpl_outfile_t,const mcpl_particle_t*);

  //Finally, always remember to close the file:
  void mcpl_close_outfile(mcpl_outfile_t);

  //Alternatively close with (will call mcpl_gzip_file after close):
  void mcpl_closeandgzip_outfile(mcpl_outfile_t);

  /////////////////////////
  // Reading .mcpl files //
  /////////////////////////

  //Open file and load header information into memory, skip to the first (if
  //any) particle in the list:
  mcpl_file_t mcpl_open_file(const char * filename);

  //Access header data:
  unsigned mcpl_hdr_version(mcpl_file_t);//file format version (not the same as MCPL_VERSION)
  unsigned long mcpl_hdr_nparticles(mcpl_file_t);//number of particles stored in file
  const char* mcpl_hdr_srcname(mcpl_file_t);//Name of the generating application
  unsigned mcpl_hdr_ncomments(mcpl_file_t);//number of comments stored in file
  const char * mcpl_hdr_comment(mcpl_file_t, unsigned icomment);//access i'th comment
  int mcpl_hdr_nblobs(mcpl_file_t);
  const char** mcpl_hdr_blobkeys(mcpl_file_t);//returns 0 if there are no keys
  int mcpl_hdr_blob(mcpl_file_t, const char* key,
                    unsigned* ldata, const char ** data);//access data (returns 0 if key doesn't exist)
  int mcpl_hdr_has_userflags(mcpl_file_t);
  int mcpl_hdr_has_polarisation(mcpl_file_t);
  int mcpl_hdr_has_doubleprec(mcpl_file_t);
  int mcpl_hdr_header_size(mcpl_file_t);//bytes consumed by header
  int mcpl_hdr_particle_size(mcpl_file_t);//bytes per particle
  int mcpl_hdr_universel_pdgcode(mcpl_file_t);
  int mcpl_hdr_little_endian(mcpl_file_t);

  //Request pointer to particle at current location and skip forward to the next
  //particle. Return value will be null in case there was no particle at the
  //current location (normally due to end-of-file):
  const mcpl_particle_t* mcpl_read(mcpl_file_t);

  //Seek and skip in particles (returns 0 when there is no particle at the new position):
  int mcpl_skipforward(mcpl_file_t,unsigned long n);
  int mcpl_rewind(mcpl_file_t);
  int mcpl_seek(mcpl_file_t,unsigned long ipos);
  unsigned long mcpl_currentposition(mcpl_file_t);

  //Deallocate memory and release file-handle with:
  void mcpl_close_file(mcpl_file_t);

  /////////////////////////////////////
  // Other operations on .mcpl files //
  /////////////////////////////////////

  //Dump information about the file to std-output:
  //   parts : 0 -> header+particle list, 1 -> just header, 2 -> just particle list.
  //   nlimit: maximum number of particles to list (0 for unlimited)
  //   nskip : index of first particle in the file to list.
  void mcpl_dump(const char * file, int parts, unsigned nskip, unsigned nlimit);

  //Merge contents of two files by appending all particles in file2 to the list
  //in file1 (thus file1 grows while file2 stays untouched). This results in an
  //error unless all of their meta-data and settings are identical:
  void mcpl_merge(const char * file1, const char* file2);

  //Test if files could be merged:
  int mcpl_can_merge(const char * file1, const char* file2);

  //Attempt to fix number of particles in the header of a file which was never
  //properly closed:
  void mcpl_repair(const char * file1);

  //For easily creating a standard mcpl-tool cmdline application:
  int mcpl_tool(int argc,char** argv);

  //Attempt to run gzip on a file (does not require MCPL_HASZLIB):
  void mcpl_gzip_file(const char * filename);

  ////////////////////
  // Error handling //
  ////////////////////

  //Override the error handler which will get called with the error
  //description. If no handler is set, errors will get printed to stdout and the
  //process terminated. An error handler should not return to the calling code.
  void mcpl_set_error_handler(void (*handler)(const char *));

#ifdef __cplusplus
}
#endif

#endif
