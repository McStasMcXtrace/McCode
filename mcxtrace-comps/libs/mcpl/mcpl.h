#ifndef MCPL_H
#define MCPL_H

#include <stdint.h>

/***********************************************************************************/
/*                                                                                 */
/*  Monte Carlo Particle Lists : MCPL                                              */
/*                                                                                 */
/*  Utilities for reading and writing .mcpl files: A binary format with lists of   */
/*  particle state information, for interchanging and reshooting events between    */
/*  various Monte Carlo simulation applications.                                   */
/*                                                                                 */
/*  Find more information and updates at https://mctools.github.io/mcpl/           */
/*                                                                                 */
/*  This file can be freely used as per the terms in the LICENSE file.             */
/*                                                                                 */
/*  Written by Thomas Kittelmann, 2015-2017.                                       */
/*                                                                                 */
/***********************************************************************************/

#define MCPL_VERSION_MAJOR 1
#define MCPL_VERSION_MINOR 1
#define MCPL_VERSION_PATCH 0
#define MCPL_VERSION   10100 /* (10000*MAJOR+100*MINOR+PATCH)   */
#define MCPL_VERSION_STR "1.1.0"
#define MCPL_FORMATVERSION 3 /* Format version of written files */

#ifdef __cplusplus
extern "C" {
#endif

  /*********/
  /* Types */
  /*********/

#pragma pack (push, 1)

  /* The data structure representing a particle (note that persistification of */
  /* polarisation and userflags must be explicitly enabled when writing .mcpl  */
  /* files, or they will simply contain zeroes when the file is read):         */

  typedef struct {
    double ekin;            /* kinetic energy [MeV]             */
    double polarisation[3]; /* polarisation vector              */
    double position[3];     /* position [cm]                    */
    double direction[3];    /* momentum direction (unit vector) */
    double time;            /* time-stamp [millisecond]         */
    double weight;          /* weight or intensity              */
    int32_t pdgcode;    /* MC particle number from the Particle Data Group (2112=neutron, 22=gamma, ...)        */
    uint32_t userflags; /* User flags (if used, the file header should probably contain information about how). */
  } mcpl_particle_t;

#pragma pack (pop)

  typedef struct { void * internal; } mcpl_file_t;    /* file-object used while reading .mcpl */
  typedef struct { void * internal; } mcpl_outfile_t; /* file-object used while writing .mcpl */

  /****************************/
  /* Creating new .mcpl files */
  /****************************/

  /* Instantiate new file object (will also open and override specified file) */
  mcpl_outfile_t mcpl_create_outfile(const char * filename);

  const char * mcpl_outfile_filename(mcpl_outfile_t);/* filename being written to (might have had .mcpl appended) */

  /* Optionally set global options or add info to the header: */
  void mcpl_hdr_set_srcname(mcpl_outfile_t, const char *);/* Name of the generating application         */
  void mcpl_hdr_add_comment(mcpl_outfile_t, const char *);/* Add one or more human-readable comments    */
  void mcpl_hdr_add_data(mcpl_outfile_t, const char * key,
                         uint32_t ldata, const char * data);/* add binary blobs by key                  */
  void mcpl_enable_userflags(mcpl_outfile_t);/* to write the "userflags" info                           */
  void mcpl_enable_polarisation(mcpl_outfile_t);/* to write the "polarisation" info                     */
  void mcpl_enable_doubleprec(mcpl_outfile_t);/* use double precision FP numbers in storage             */
  void mcpl_enable_universal_pdgcode(mcpl_outfile_t, int32_t pdgcode);/* All particles are of the same type */
  void mcpl_enable_universal_weight(mcpl_outfile_t, double w);/* All particles have the same weight */

  /* Optionally (but rarely skipped) add particles, by updating the info in */
  /* and then passing in a pointer to an mcpl_particle_t instance:          */
  void mcpl_add_particle(mcpl_outfile_t,const mcpl_particle_t*);

  /* Finally, always remember to close the file: */
  void mcpl_close_outfile(mcpl_outfile_t);

  /* Alternatively close with (will call mcpl_gzip_file after close). */
  /* Returns non-zero if gzipping was succesful:                      */
  int mcpl_closeandgzip_outfile(mcpl_outfile_t);

  /* Convenience function which returns a pointer to a nulled-out particle
     struct which can be used to edit and pass to mcpl_add_particle. It can be
     reused and will be automatically free'd when the file is closed: */
  mcpl_particle_t* mcpl_get_empty_particle(mcpl_outfile_t);

  /***********************/
  /* Reading .mcpl files */
  /***********************/

  /* Open file and load header information into memory, skip to the first (if */
  /* any) particle in the list:                                               */
  mcpl_file_t mcpl_open_file(const char * filename);

  /* Access header data: */
  unsigned mcpl_hdr_version(mcpl_file_t);/* file format version (not the same as MCPL_VERSION) */
  uint64_t mcpl_hdr_nparticles(mcpl_file_t);/* number of particles stored in file              */
  const char* mcpl_hdr_srcname(mcpl_file_t);/* Name of the generating application              */
  unsigned mcpl_hdr_ncomments(mcpl_file_t);/* number of comments stored in file                */
  const char * mcpl_hdr_comment(mcpl_file_t, unsigned icomment);/* access i'th comment         */
  int mcpl_hdr_nblobs(mcpl_file_t);
  const char** mcpl_hdr_blobkeys(mcpl_file_t);/* returns 0 if there are no keys */
  int mcpl_hdr_blob(mcpl_file_t, const char* key,
                    uint32_t* ldata, const char ** data);/* access data (returns 0 if key doesn't exist) */
  int mcpl_hdr_has_userflags(mcpl_file_t);
  int mcpl_hdr_has_polarisation(mcpl_file_t);
  int mcpl_hdr_has_doubleprec(mcpl_file_t);
  uint64_t mcpl_hdr_header_size(mcpl_file_t);/* bytes consumed by header (uncompressed) */
  int mcpl_hdr_particle_size(mcpl_file_t);/* bytes per particle (uncompressed)     */
  int32_t mcpl_hdr_universal_pdgcode(mcpl_file_t);/* returns 0 in case of per-particle pdgcode */
  double mcpl_hdr_universal_weight(mcpl_file_t);/* returns 0.0 in case of per-particle weights */
  int mcpl_hdr_little_endian(mcpl_file_t);

  /* Request pointer to particle at current location and skip forward to the next */
  /* particle. Return value will be null in case there was no particle at the     */
  /* current location (normally due to end-of-file):                              */
  const mcpl_particle_t* mcpl_read(mcpl_file_t);

  /* Seek and skip in particles (returns 0 when there is no particle at the new position): */
  int mcpl_skipforward(mcpl_file_t,uint64_t n);
  int mcpl_rewind(mcpl_file_t);
  int mcpl_seek(mcpl_file_t,uint64_t ipos);
  uint64_t mcpl_currentposition(mcpl_file_t);

  /* Deallocate memory and release file-handle with: */
  void mcpl_close_file(mcpl_file_t);

  /***********************************/
  /* Other operations on .mcpl files */
  /***********************************/

  /* Dump information about the file to std-output:                                  */
  /*   parts : 0 -> header+particle list, 1 -> just header, 2 -> just particle list. */
  /*   nlimit: maximum number of particles to list (0 for unlimited)                 */
  /*   nskip : index of first particle in the file to list.                          */
  void mcpl_dump(const char * file, int parts, uint64_t nskip, uint64_t nlimit);

  /* Merge contents of a list of files by concatenating all particle contents into a   */
  /* new file, file_output. This results in an error unless all meta-data and settings */
  /* in the files are identical. Also fails if file_output already exists. Note that   */
  /* the return value is a handle to the output file which has not yet been closed:    */
  mcpl_outfile_t mcpl_merge_files( const char* file_output,
                                   unsigned nfiles, const char ** files);

  /* Test if files could be merged by mcpl_merge_files: */
  int mcpl_can_merge(const char * file1, const char* file2);

  /* Similar to mcpl_merge_files, but merges two files by appending all particles in */
  /* file2 to the list in file1 (thus file1 grows while file2 stays untouched).      */
  /* Note that this requires similar version of the MCPL format of the two files, in */
  /* addition to the other checks in mcpl_can_merge().                               */
  /* Careful usage of this function can be more efficient than mcpl_merge_files.     */
  void mcpl_merge_inplace(const char * file1, const char* file2);

  /* Attempt to fix number of particles in the header of a file which was never */
  /* properly closed:                                                           */
  void mcpl_repair(const char * file1);

  /* For easily creating a standard mcpl-tool cmdline application: */
  int mcpl_tool(int argc, char** argv);

  /* Attempt to run gzip on a file (does not require MCPL_HASZLIB on unix) */
  /* Returns non-zero if gzipping was succesful.                           */
  int mcpl_gzip_file(const char * filename);

  /* Convenience function which transfers all settings, blobs and comments to */
  /* target. Intended to make it easy to filter files via custom C code.      */
  void mcpl_transfer_metadata(mcpl_file_t source, mcpl_outfile_t target);

  /******************/
  /* Error handling */
  /******************/

  /* Override the error handler which will get called with the error              */
  /* description. If no handler is set, errors will get printed to stdout and the */
  /* process terminated. An error handler should not return to the calling code.  */
  void mcpl_set_error_handler(void (*handler)(const char *));

  /**********************/
  /* Obsolete functions */
  /**********************/

  /* Functions kept for backwards compatibility. They keep working for now, but  */
  /* usage will result in a warning printed to stdout, notifying users to update */
  /* their code.                                                                 */

  void mcpl_merge(const char *, const char*);/* Obsolete name for mcpl_merge_inplace */
  int mcpl_gzip_file_rc(const char * filename);/* Obsolete name for mcpl_gzip_file */
  int mcpl_closeandgzip_outfile_rc(mcpl_outfile_t);/* Obsolete name for mcpl_closeandgzip_outfile_rc */
  int32_t mcpl_hdr_universel_pdgcode(mcpl_file_t);/* Obsolete name for mcpl_hdr_universal_pdgcode */

#ifdef __cplusplus
}
#endif

#endif
