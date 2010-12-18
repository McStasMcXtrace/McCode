/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright (C) 1997-2010, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Runtime: share/mcstas-r.h
*
* %Identification
* Written by: KN
* Date:    Aug 29, 1997
* Release: McStas X.Y
* Version: $Revision: 1.101 $
*
* Runtime system header for McStas.
*
* In order to use this library as an external library, the following variables
* and macros must be declared (see details in the code)
*
*   struct mcinputtable_struct mcinputtable[];
*   int mcnumipar;
*   char mcinstrument_name[], mcinstrument_source[];
*   int mctraceenabled, mcdefaultmain;
*   extern MCNUM  mccomp_storein[];
*   extern MCNUM  mcAbsorbProp[];
*   extern MCNUM  mcScattered;
*   #define MCSTAS_VERSION "the McStas version"
*
* Usage: Automatically embbeded in the c code.
*
* $Id$
*
*       $Log: mcstas-r.h,v $
*       Revision 1.101  2009-04-02 09:47:46  pkwi
*       Updated runtime and interoff from dev branch (bugfixes etc.)
*
*       Proceeding to test before release
*
*       Revision 1.108  2009/01/23 14:01:12  farhi
*       Back to smaller buffer size for MPI exchange, to ensure that it works on
*       *most* machines.
*
*       Revision 1.107  2009/01/23 10:51:30  farhi
*       Minor speedup: Identity rotation matrices are now checked for and
*       caculations reduced.
*       It seems this McSatsStable commit did not got through for McStas 1.12b
*
*       Revision 1.106  2009/01/15 15:42:44  farhi
*       Saving lists using MPI: must use MPI_Ssend to avoid the buffer max size
*       in MPI1
*
*       Revision 1.105  2008/10/21 15:19:18  farhi
*       use common CHAR_BUFFER_LENGTH = 1024
*
*       Revision 1.104  2008/09/02 08:36:17  farhi
*       MPI support: block size defined in mcstas-r.h as 1e5. Correct bug when
*       p0, p1 or p2 are NULL, and re-enable S(q,w) save in Isotropic_Sqw with
*       MPI.
*
*       Revision 1.103  2008/08/26 13:32:05  farhi
*       Remove Threading support which is poor efficiency and may give wrong
*       results
*       Add quotes around string instrument parameters from mcgui simulation
*       dialog
*
*       Revision 1.102  2008/08/25 14:13:28  farhi
*       changed neutron-mc to mcstas-users
*
*       Revision 1.101  2008/07/17 12:50:18  farhi
*       MAJOR commit to McStas 2.x
*       uniformized parameter naming in components
*       uniformized SITE for instruments
*       all compile OK
*
*       Revision 1.99  2008/04/25 08:26:33  erkn
*       added utility functions/macros for intersecting with a plane and mirroring a vector in a plane
*
*       Revision 1.98  2008/04/21 15:50:19  pkwi
*       Name change randvec_target_rect -> randvec_target_rect_real .
*
*       The renamed routine takes local emmission coordinate into account, correcting for the
*       effects mentioned by George Apostolopoulus <gapost@ipta.demokritos.gr> to the
*       mcstas-users list (parameter list extended by four parms).
*
*       For backward-compatibility, a define has been added that maps randvec_target_rect
*       to the new routine, defaulting to the "old" behaviour.
*
*       To make any use of these modifications, we need to correct all (or all relevant) comps
*       that have calls to randvec_target_rect.
*
*       Will supply a small doc with plots showing that we now correct for the effect pointed
*       out by George.
*
*       Similar change should in principle happen to the _sphere focusing routine.
*
*       Revision 1.97  2008/02/10 20:55:53  farhi
*       OpenMP number of nodes now set properly from either --threads=NB or
*       --threads which sets the computer core nb.
*
*       Revision 1.96  2008/02/10 15:12:56  farhi
*       mcgui: save log when File/Quit
*       mcrun/mcgui: OpenMP now uses the specified number of nodes
*       mcstas-r: number of OpenMP nodes can be set by user. If left at default
*       (--threads), then use omp_get_num_threads. This may be inaccurate on some systems..
*
*       Revision 1.95  2008/02/09 22:26:27  farhi
*       Major contrib for clusters/multi-core: OpenMP support
*       	try ./configure --with-cc=gcc4.2 or icc
*       then mcrun --threads ...
*       Also tidy-up configure. Made relevant changes to mcrun/mcgui to enable OpenMP
*       Updated install-doc accordingly
*
*       Revision 1.94  2007/08/09 16:47:34  farhi
*       Solved old gcc compilation issue when using macros in macros.
*       Solved MPI issuie when exiting in the middle of a simulation. Now use MPI_Abort.
*
*       Revision 1.93  2007/05/29 14:57:56  farhi
*       New rand function to shoot on a triangular distribution. Useful to simulate chopper time spread.
*
*       Revision 1.92  2007/02/01 15:49:45  pkwi
*       For some instruments (e.g. h8) , it seems that <sys/stat.h> is needed to compile on Mac OS X (like FreeBSD)
*
*       Added define to include this.
*
*       Revision 1.91  2007/01/29 15:51:56  farhi
*       mcstas-r: avoid undef of USE_NEXUS as napi is importer afterwards
*
*       Revision 1.90  2007/01/25 14:57:36  farhi
*       NeXus output now supports MPI. Each node writes a data set in the NXdata
*       group. Uses compression LZW (may be unactivated with the
*       -DUSE_NEXUS_FLAT).
*
*       Revision 1.89  2007/01/23 00:41:05  pkwi
*       Edits by Jiao Lin (linjao@caltech.edu) for embedding McStas in the DANSE project. Define -DDANSE during compile will enable these edits.
*
*       Have tested that McStas works properly without the -DDANSE.
*
*       Jiao: Could you please test if all is now OK?
*       (After 15 minutes) Get current CVS tarball from http://www.mcstas.org/cvs
*
*       Revision 1.88  2007/01/22 01:38:25  farhi
*       Improved NeXus/NXdata support. Attributes may not be at the right place
*       yet.
*
*       Revision 1.87  2007/01/21 15:43:08  farhi
*       NeXus support. Draft version (functional). To be tuned.
*
*       Revision 1.86  2006/08/28 10:12:25  pchr
*       Basic infrastructure for spin propagation in magnetic fields.
*
*       Revision 1.85  2006/08/15 12:09:35  pkwi
*       Global define GRAVITY=9.81, used in PROP_ routines and Guide_gravity. Will add handeling of
*
*       -g xx / --gravitation==xx
*
*       in mcstas-r.c at a later time.
*
*       Revision 1.84  2006/08/03 13:11:18  pchr
*       Added additional functions for handling vectors.
*
*       Revision 1.83  2006/07/25 08:49:13  pchr
*       Inserted missing end brackets in routines PROP_X0 and PROP_Y0.
*
*       Revision 1.82  2006/07/06 08:59:21  pchr
*       Added new draw methods for rectangle and box.
*
*       Revision 1.81  2006/05/19 14:17:40  farhi
*       Added support for multi threading with --threads=NB option for mcrun or instr.out
*       Requires new option in mcgui run dialog: a popup menu to select run mode ?
*
*       Revision 1.80  2006/04/05 11:45:05  pkwi
*       Need to include <sys/stat.h> on FreeBSD 6.0 / PC-BSD (maybe also other bsd's?!) for prototype declaration of mkdir call...
*
*       Revision 1.79  2006/03/15 16:00:42  farhi
*       minor modifications (position of FLT_MAX in code)
*
*       Revision 1.78  2005/08/31 08:35:53  farhi
*       MCdisplay now prints component name and position when building view (bug/request 44 closed)
*
*       Revision 1.77  2005/08/24 11:55:12  pkwi
*       Usage of mcallowbackprop flag in all PROP routines. Use in component by e.g.
*
*       ALLOWBACKPROP;
*       PROP_Z0;
*
*       Prop routines disallow backpropagation on exit.
*
*       Revision 1.76  2005/08/24 09:51:31  pkwi
*       Beamstop and runtime modified according to Emmanuels remarks.
*
*       To allow backpropagation in a specific component, use
*
*       ALLOW_BACKPROP;
*
*       before calling
*
*       PROP_Z0;
*
*       (One could consider making the backpropagation flag common to all propagation routines, should we do so?)
*
*       Revision 1.75  2005/08/12 11:23:19  pkwi
*       Special Z0 backpropagation macro defined to allow backpropagation without absorbtion. Needed in Beamstop.comp. We foresee usage elsewhere. Problematic: Duplication of code - can we think of a better way to handle this problem?
*
*       Revision 1.74  2005/07/25 14:55:08  farhi
*       DOC update:
*       checked all parameter [unit] + text to be OK
*       set all versions to CVS Revision
*
*       Revision 1.73  2005/07/18 14:43:05  farhi
*       Now gives a warning message per component for 'computational absorbs'
*
*       Revision 1.72  2005/06/20 08:09:07  farhi
*       Changed all ABSORB by adding mcAbsorbProp incrementation
*       in PROP macros
*
*       Revision 1.71  2005/05/29 09:50:32  pkwi
*       t=0 now allowed in PROP_X0, PROP_Y0, PROP_Z0. As far as I can see, there are no other occurancies of this problem in the propagation routines.
*
*       Fixes bug #43 on BugZilla
*
*       Revision 1.70  2005/02/24 15:57:20  farhi
*       FIXED gravity bug (probably OK). Gravity is not handled properly in other Guide elements. Will adapt so that it works better...
*       The n.v was not computed using the actual 'v' values when reaching the guide side, but before propagation. So the velocity was not reflected, but scattered depending on the previous neutron position/velocity, bringing strange divergence effects.
*       On other guide elements, should update the n.v term just before reflection, not computing it before propagation... This probably holds for some other components (monochromators ???) to be checked !
*
*       Revision 1.69  2005/02/23 12:36:53  farhi
*       Added gravitation support in PROP_X0 and PROP_Y0
*
*       Revision 1.66  2005/02/16 12:21:39  farhi
*       Removed left spaces at end of lines
*
*       Revision 1.65  2005/01/26 14:41:16  farhi
*       Updated constant values from CODATA 2002
*
*       Revision 1.64  2005/01/18 10:32:28  farhi
*       Clarify a macro for MPI
*
*       Revision 1.63  2004/11/30 16:14:47  farhi
*       Uses NOSIGNALS and put back PROP_X0 and Y0 for some contrib comps
*
*       Revision 1.62  2004/09/21 12:25:03  farhi
*       Reorganised code so that I/O functions are includable easely (for mcformat.c)
*
*       Revision 1.59  2004/09/03 14:19:14  farhi
*       Correct invertion in mcformat specs structure
*
*       Revision 1.58  2004/07/30 14:49:15  farhi
*       MPI update for usage with mcrun.
*       Still done by Christophe Taton. CC=mpicc and CFLAGS = -DUSE_MPI.
*       Execute (using mpich) with:
*                 mpirun -np NumNodes -machinefile <file> instr.out parameters...
*            where <file> is text file that lists the machines to use
*
*       Revision 1.57  2004/07/16 14:59:03  farhi
*       MPI support. Requires to have mpi installed, and compile with
*          CC=mpicc and CFLAGS = -DUSE_MPI.
*       Work done by Christophe Taton from ENSIMAG/Grenoble
*       Execute (using mpich) with:
*          mpirun -np NumNodes -machinefile <file> instr.out parameters...
*       where <file> is text file that lists the machines to use
*
*       Revision 1.56  2004/06/30 12:11:29  farhi
*       Updated obsolete MCDETECTOR_OUT #define -> mcdetector_out_0d
*
*       Revision 1.55  2003/10/21 14:08:12  pkwi
*       Rectangular focusing improved: Renamed randvec_target_rect to randvec_target_rect_angular. Wrote new randvec_target_rect routine, w/h in metres. Both routines use use component orientation (ROT_A_CURRENT_COMP) as input.
*
*       Modifications to Res_sample and V_sample to match new features of the runtime.
*
*       Revision 1.54  2003/09/05 08:59:18  farhi
*       added INSTRUMENT parameter default value grammar
*       mcinputtable now has also default values
*       mcreadpar now uses default values if parameter not given
*       extended instr_formal parameter struct
*       extended mcinputtable structure type
*
*       Revision 1.53  2003/04/07 11:50:51  farhi
*       Extended the way mcplot:plotter is assigned. Set --portable ok
*       Handle Scilab:Tk and ~GTk menu (shifted)
*       Updated help in mcrun and mcstas-r.c
*
*       Revision 1.52  2003/04/04 18:20:21  farhi
*       remove some warnings (duplicated decl) for --no-runtime on Dec OSF
*
*       Revision 1.51  2003/04/04 14:27:19  farhi
*       Moved format definitions to mcstas-r.c for --no-runtime to work
*
*       Revision 1.50  2003/02/11 12:28:46  farhi
*       Variouxs bug fixes after tests in the lib directory
*       mcstas_r  : disable output with --no-out.. flag. Fix 1D McStas output
*       read_table:corrected MC_SYS_DIR -> MCSTAS define
*       monitor_nd-lib: fix Log(signal) log(coord)
*       HOPG.trm: reduce 4000 points -> 400 which is enough and faster to resample
*       Progress_bar: precent -> percent parameter
*       CS: ----------------------------------------------------------------------
*
* Revision 1.5 2002/10/19 22:46:21 ef
*        gravitation for all with -g. Various output formats.
*
* Revision 1.4 2002/09/17 12:01:21 ef
*       removed unused macros (PROP_Y0, X0), changed randvec_target_sphere to circle
* added randvec_target_rect
*
* Revision 1.3 2002/08/28 11:36:37 ef
*       Changed to lib/share/c code
*
* Revision 1.2 2001/10/10 11:36:37 ef
*       added signal handler
*
* Revision 1.1 1998/08/29 11:36:37 kn
*       Initial revision
*
*******************************************************************************/

#ifndef MCSTAS_R_H
#define MCSTAS_R_H "$Revision: 1.101 $"

#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <limits.h>
#include <errno.h>
#include <time.h>
#include <float.h>
#include <ctype.h>
#include <sys/stat.h>
#include <unistd.h>

/* If the runtime is embedded in the simulation program, some definitions can
   be made static. */

#ifdef MC_EMBEDDED_RUNTIME
#define mcstatic static
#else
#define mcstatic
#endif

#ifdef __dest_os
#if (__dest_os == __mac_os)
#define MAC
#endif
#endif

#ifdef __FreeBSD__
#define NEED_STAT_H
#endif

#if defined(__APPLE__) && defined(__GNUC__)
#define NEED_STAT_H
#endif

#ifdef NEED_STAT_H
#include <sys/stat.h>
#endif

#ifndef MC_PATHSEP_C
#ifdef WIN32
#define MC_PATHSEP_C '\\'
#define MC_PATHSEP_S "\\"
#else  /* !WIN32 */
#ifdef MAC
#define MC_PATHSEP_C ':'
#define MC_PATHSEP_S ":"
#else  /* !MAC */
#define MC_PATHSEP_C '/'
#define MC_PATHSEP_S "/"
#endif /* !MAC */
#endif /* !WIN32 */
#endif /* MC_PATHSEP_C */

#ifndef MCSTAS_VERSION
#define MCSTAS_VERSION "External Run-time"
#endif

#ifdef MC_PORTABLE
#ifndef NOSIGNALS
#define NOSIGNALS
#endif
#endif

#ifdef MAC
#ifndef NOSIGNALS
#define NOSIGNALS
#endif
#endif

#ifdef USE_MPI
#ifndef NOSIGNALS
#define NOSIGNALS
#endif
#endif

#if (USE_NEXUS == 0)
#undef USE_NEXUS
#endif

/* I/O section part ========================================================= */

/* Note: the enum instr_formal_types definition MUST be kept
   synchronized with the one in mcstas.h and with the
   instr_formal_type_names array in cogen.c. */
enum instr_formal_types
  {
    instr_type_double, instr_type_int, instr_type_string
  };
struct mcinputtable_struct {
  char *name; /* name of parameter */
  void *par;  /* pointer to instrument parameter (variable) */
  enum instr_formal_types type;
  char *val;  /* default value */
};

typedef double MCNUM;
typedef struct {MCNUM x, y, z;} Coords;
typedef MCNUM Rotation[3][3];

/* the following variables are defined in the McStas generated C code
   but should be defined externally in case of independent library usage */
#ifndef DANSE
extern struct mcinputtable_struct mcinputtable[];
extern int    mcnumipar;
extern char   mcinstrument_name[], mcinstrument_source[];
extern MCNUM  mccomp_storein[]; /* 11 coords * number of components in instrument */
extern MCNUM  mcAbsorbProp[];
extern MCNUM  mcScattered;
#ifndef MC_ANCIENT_COMPATIBILITY
extern int mctraceenabled, mcdefaultmain;
#endif
#endif

/* file I/O definitions and function prototypes */

struct mcformats_struct {
  char *Name;  /* may also specify: append, partial(hidden), binary */
  char *Extension;
  char *Header;
  char *Footer;
  char *BeginSection;
  char *EndSection;
  char *AssignTag;
  char *BeginData;
  char *EndData;
  char *BeginErrors;
  char *EndErrors;
  char *BeginNcount;
  char *EndNcount;
  };

#ifndef MC_EMBEDDED_RUNTIME /* the mcstatic variables (from mcstas-r.c) */
extern FILE * mcsiminfo_file;
extern int    mcgravitation;
extern int    mcdotrace;
extern struct mcformats_struct mcformats[];
extern struct mcformats_struct mcformat;
extern struct mcformats_struct mcformat_data;
#else
mcstatic FILE *mcsiminfo_file        = NULL;
#endif

/* Useful macros ============================================================ */

#define DETECTOR_OUT(p0,p1,p2) mcdetector_out_0D(NAME_CURRENT_COMP,p0,p1,p2,NAME_CURRENT_COMP,POS_A_CURRENT_COMP)
#define DETECTOR_OUT_0D(t,p0,p1,p2) mcdetector_out_0D(t,p0,p1,p2,NAME_CURRENT_COMP,POS_A_CURRENT_COMP)
#define DETECTOR_OUT_1D(t,xl,yl,xvar,x1,x2,n,p0,p1,p2,f) \
     mcdetector_out_1D(t,xl,yl,xvar,x1,x2,n,p0,p1,p2,f,NAME_CURRENT_COMP,POS_A_CURRENT_COMP)
#define DETECTOR_OUT_2D(t,xl,yl,x1,x2,y1,y2,m,n,p0,p1,p2,f) \
     mcdetector_out_2D(t,xl,yl,x1,x2,y1,y2,m,n,p0,p1,p2,f,NAME_CURRENT_COMP,POS_A_CURRENT_COMP)
#define DETECTOR_OUT_3D(t,xl,yl,zl,xv,yv,zv,x1,x2,y1,y2,z1,z2,m,n,p,p0,p1,p2,f) \
     mcdetector_out_3D(t,xl,yl,zl,xv,yv,zv,x1,x2,y1,y2,z1,z2,m,n,p,p0,p1,p2,f,NAME_CURRENT_COMP,POS_A_CURRENT_COMP)
#define DETECTOR_CUSTOM_HEADER(t)  if (t && strlen(t)) { \
     mcDetectorCustomHeader=malloc(strlen(t)); \
     if (mcDetectorCustomHeader) strcpy(mcDetectorCustomHeader, t); }

#define randvec_target_rect(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9)  randvec_target_rect_real(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,0,0,0,1)

/* MPI stuff ================================================================ */

#ifdef USE_MPI
#include "mpi.h"

/*
 * MPI_MASTER(i):
 * execution of i only on master node
 */
#define MPI_MASTER(statement) { \
  if(mpi_node_rank == mpi_node_root)\
  { statement; } \
}

#ifndef MPI_REDUCE_BLOCKSIZE
#define MPI_REDUCE_BLOCKSIZE 10000
#endif

int mc_MPI_Reduce(void* sbuf, void* rbuf,
                  int count, MPI_Datatype dtype,
                  MPI_Op op, int root, MPI_Comm comm);

#define exit(code) MPI_Abort(MPI_COMM_WORLD, code)

#else /* !USE_MPI */
#define MPI_MASTER(instr) instr
#endif /* USE_MPI */

#ifdef USE_MPI
static int mpi_node_count;
#endif

#ifdef USE_THREADS  /* user want threads */
#error Threading (USE_THREADS) support has been removed for very poor efficiency. Use MPI/SSH grid instead.
#endif

/* I/O function prototypes ================================================== */

/* The mcformat.Name may contain additional keywords:
 *  no header: omit the format header
 *  no footer: omit the format footer
 */

void   mcset_ncount(double count);
double mcget_ncount(void);
double mcget_run_num(void);
double mcdetector_out(char *cname, double p0, double p1, double p2, char *filename);
double mcdetector_out_0D(char *t, double p0, double p1, double p2, char *c, Coords pos);
double mcdetector_out_1D(char *t, char *xl, char *yl,
                  char *xvar, double x1, double x2, int n,
                  double *p0, double *p1, double *p2, char *f, char *c, Coords pos);
double mcdetector_out_2D(char *t, char *xl, char *yl,
                  double x1, double x2, double y1, double y2, int m,
                  int n, double *p0, double *p1, double *p2, char *f,
                  char *c, Coords pos);
double mcdetector_out_3D(char *t, char *xl, char *yl, char *zl,
      char *xvar, char *yvar, char *zvar,
                  double x1, double x2, double y1, double y2, double z1, double z2, int m,
                  int n, int p, double *p0, double *p1, double *p2, char *f,
                  char *c, Coords pos);
void   mcinfo_simulation(FILE *f, struct mcformats_struct format,
  char *pre, char *name); /* used to add sim parameters (e.g. in Res_monitor) */
void   mcsiminfo_init(FILE *f);
void   mcsiminfo_close(void);
char *mcfull_file(char *name, char *ext);

#ifndef FLT_MAX
#define FLT_MAX         3.40282347E+38F /* max decimal value of a "float" */
#endif

#ifndef CHAR_BUF_LENGTH
#define CHAR_BUF_LENGTH 1024
#endif

/* Following part is only embedded when not redundent with mcstas.h ========= */

#ifndef MCSTAS_H

#ifndef NOSIGNALS
#include <signal.h>
#define SIG_MESSAGE(msg) strcpy(mcsig_message, msg);
#else
#define SIG_MESSAGE(msg)
#endif /* !NOSIGNALS */



/* Useful macros ============================================================ */

#define RAD2MIN  ((180*60)/PI)
#define MIN2RAD  (PI/(180*60))
#define DEG2RAD  (PI/180)
#define RAD2DEG  (180/PI)
#define AA2MS    629.622368        /* Convert k[1/AA] to v[m/s] */
#define MS2AA    1.58825361e-3     /* Convert v[m/s] to k[1/AA] */
#define K2V      AA2MS
#define V2K      MS2AA
#define Q2V      AA2MS
#define V2Q      MS2AA
#define SE2V     437.393377        /* Convert sqrt(E)[meV] to v[m/s] */
#define VS2E     5.22703725e-6     /* Convert (v[m/s])**2 to E[meV] */
#define FWHM2RMS 0.424660900144    /* Convert between full-width-half-max and */
#define RMS2FWHM 2.35482004503     /* root-mean-square (standard deviation) */
#define HBAR     1.05457168e-34    /* [Js] h bar Planck constant CODATA 2002 */
#define MNEUTRON 1.67492728e-27    /* [kg] mass of neutron CODATA 2002 */
#define GRAVITY  9.81              /* [m/s^2] gravitational acceleration */

#ifndef PI
# ifdef M_PI
#  define PI M_PI
# else
#  define PI 3.14159265358979323846
# endif
#endif

/* mccomp_posa and mccomp_posr are defined in McStas generated C code */
#define POS_A_COMP_INDEX(index) \
    (mccomp_posa[index])
#define POS_R_COMP_INDEX(index) \
    (mccomp_posr[index])
/* mcScattered defined in McStas generated C code */
#define SCATTERED mcScattered

/* Retrieve component information from the kernel */
/* Name, position and orientation (both absolute and relative)  */
/* Any component: For "redundancy", see comment by KN */
#define tmp_name_comp(comp) #comp
#define NAME_COMP(comp) tmp_name_comp(comp)
#define tmp_pos_a_comp(comp) (mcposa ## comp)
#define POS_A_COMP(comp) tmp_pos_a_comp(comp)
#define tmp_pos_r_comp(comp) (mcposr ## comp)
#define POS_R_COMP(comp) tmp_pos_r_comp(comp)
#define tmp_rot_a_comp(comp) (mcrota ## comp)
#define ROT_A_COMP(comp) tmp_rot_a_comp(comp)
#define tmp_rot_r_comp(comp) (mcrotr ## comp)
#define ROT_R_COMP(comp) tmp_rot_r_comp(comp)

/* Current component */
#define NAME_CURRENT_COMP  NAME_COMP(mccompcurname)
#define INDEX_CURRENT_COMP mccompcurindex
#define POS_A_CURRENT_COMP POS_A_COMP(mccompcurname)
#define POS_R_CURRENT_COMP POS_R_COMP(mccompcurname)
#define ROT_A_CURRENT_COMP ROT_A_COMP(mccompcurname)
#define ROT_R_CURRENT_COMP ROT_R_COMP(mccompcurname)



#define SCATTER do {mcDEBUG_SCATTER(mcnlx, mcnly, mcnlz, mcnlvx, mcnlvy, mcnlvz, \
        mcnlt,mcnlsx,mcnlsy, mcnlp); mcScattered++;} while(0)
#define ABSORB do {mcDEBUG_STATE(mcnlx, mcnly, mcnlz, mcnlvx, mcnlvy, mcnlvz, \
        mcnlt,mcnlsx,mcnlsy, mcnlp); mcDEBUG_ABSORB(); MAGNET_OFF; goto mcabsorb;} while(0)
/* Note: The two-stage approach to MC_GETPAR is NOT redundant; without it,
* after #define C sample, MC_GETPAR(C,x) would refer to component C, not to
* component sample. Such are the joys of ANSI C.

* Anyway the usage of MCGETPAR requires that we use sometimes bare names...
*/
#define MC_GETPAR2(comp, par) (mcc ## comp ## _ ## par)
#define MC_GETPAR(comp, par) MC_GETPAR2(comp,par)

#define STORE_NEUTRON(index, x, y, z, vx, vy, vz, t, sx, sy, sz, p) \
  mcstore_neutron(mccomp_storein,index, x, y, z, vx, vy, vz, t, sx, sy, sz, p);
#define RESTORE_NEUTRON(index, x, y, z, vx, vy, vz, t, sx, sy, sz, p) \
  mcrestore_neutron(mccomp_storein,index, &x, &y, &z, &vx, &vy, &vz, &t, &sx, &sy, &sz, &p);

#define MAGNET_ON \
  do { \
    mcMagnet = 1; \
  } while(0)

#define MAGNET_OFF \
  do { \
    mcMagnet = 0; \
  } while(0)

#define ALLOW_BACKPROP \
  do { \
    mcallowbackprop = 1; \
  } while(0)

#define DISALLOW_BACKPROP \
  do { \
    mcallowbackprop = 0; \
  } while(0)

#define PROP_MAGNET(dt) \
  do { \
    /* change coordinates from local system to magnet system */ \
    Rotation rotLM, rotTemp; \
    Coords   posLM = coords_sub(POS_A_CURRENT_COMP, mcMagnetPos); \
    rot_transpose(ROT_A_CURRENT_COMP, rotTemp); \
    rot_mul(rotTemp, mcMagnetRot, rotLM); \
    mcMagnetPrecession(mcnlx, mcnly, mcnlz, mcnlt, mcnlvx, mcnlvy, mcnlvz, \
	   	       &mcnlsx, &mcnlsy, &mcnlsz, dt, posLM, rotLM); \
  } while(0)

#define mcPROP_DT(dt) \
  do { \
    if (mcMagnet && dt > 0) PROP_MAGNET(dt);\
    mcnlx += mcnlvx*(dt); \
    mcnly += mcnlvy*(dt); \
    mcnlz += mcnlvz*(dt); \
    mcnlt += (dt); \
  } while(0)

/* ADD: E. Farhi, Aug 6th, 2001 PROP_GRAV_DT propagation with acceleration */
#define PROP_GRAV_DT(dt, Ax, Ay, Az) \
  do { \
    if(dt < 0 && mcallowbackprop == 0) { mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }\
    if (mcMagnet) printf("Spin precession gravity\n"); \
    mcnlx  += mcnlvx*(dt) + (Ax)*(dt)*(dt)/2; \
    mcnly  += mcnlvy*(dt) + (Ay)*(dt)*(dt)/2; \
    mcnlz  += mcnlvz*(dt) + (Az)*(dt)*(dt)/2; \
    mcnlvx += (Ax)*(dt); \
    mcnlvy += (Ay)*(dt); \
    mcnlvz += (Az)*(dt); \
    mcnlt  += (dt); \
    DISALLOW_BACKPROP;\
  } while(0)

#define PROP_DT(dt) \
  do { \
    if(dt < 0 && mcallowbackprop == 0) { mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }; \
    if (mcgravitation) { Coords mcLocG; double mc_gx, mc_gy, mc_gz; \
    mcLocG = rot_apply(ROT_A_CURRENT_COMP, coords_set(0,-GRAVITY,0)); \
    coords_get(mcLocG, &mc_gx, &mc_gy, &mc_gz); \
    PROP_GRAV_DT(dt, mc_gx, mc_gy, mc_gz); } \
    else mcPROP_DT(dt); \
    DISALLOW_BACKPROP;\
  } while(0)


#define PROP_Z0 \
  do { \
    if (mcgravitation) { Coords mcLocG; int mc_ret; \
    double mc_dt, mc_gx, mc_gy, mc_gz; \
    mcLocG = rot_apply(ROT_A_CURRENT_COMP, coords_set(0,-GRAVITY,0)); \
    coords_get(mcLocG, &mc_gx, &mc_gy, &mc_gz); \
    mc_ret = solve_2nd_order(&mc_dt, -mc_gz/2, -mcnlvz, -mcnlz); \
    if (mc_ret && mc_dt>=0) PROP_GRAV_DT(mc_dt, mc_gx, mc_gy, mc_gz); \
    else { if (mcallowbackprop ==0) {mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }}; }\
    else mcPROP_Z0; \
    DISALLOW_BACKPROP;\
  } while(0)

#define mcPROP_Z0 \
  do { \
    double mc_dt; \
    if(mcnlvz == 0) { mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }; \
    mc_dt = -mcnlz/mcnlvz; \
    if(mc_dt < 0 && mcallowbackprop == 0) { mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }; \
    mcPROP_DT(mc_dt); \
    mcnlz = 0; \
    DISALLOW_BACKPROP;\
  } while(0)

#define PROP_X0 \
  do { \
    if (mcgravitation) { Coords mcLocG; int mc_ret; \
    double mc_dt, mc_gx, mc_gy, mc_gz; \
    mcLocG = rot_apply(ROT_A_CURRENT_COMP, coords_set(0,-GRAVITY,0)); \
    coords_get(mcLocG, &mc_gx, &mc_gy, &mc_gz); \
    mc_ret = solve_2nd_order(&mc_dt, -mc_gx/2, -mcnlvx, -mcnlx); \
    if (mc_ret && mc_dt>=0) PROP_GRAV_DT(mc_dt, mc_gx, mc_gy, mc_gz); \
    else { if (mcallowbackprop ==0) {mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }}; }\
    else mcPROP_X0; \
    DISALLOW_BACKPROP;\
  } while(0)

#define mcPROP_X0 \
  do { \
    double mc_dt; \
    if(mcnlvx == 0) { mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }; \
    mc_dt = -mcnlx/mcnlvx; \
    if(mc_dt < 0 && mcallowbackprop == 0) { mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }; \
    mcPROP_DT(mc_dt); \
    mcnlx = 0; \
    DISALLOW_BACKPROP;\
  } while(0)

#define PROP_Y0 \
  do { \
    if (mcgravitation) { Coords mcLocG; int mc_ret; \
    double mc_dt, mc_gx, mc_gy, mc_gz; \
    mcLocG = rot_apply(ROT_A_CURRENT_COMP, coords_set(0,-GRAVITY,0)); \
    coords_get(mcLocG, &mc_gx, &mc_gy, &mc_gz); \
    mc_ret = solve_2nd_order(&mc_dt, -mc_gy/2, -mcnlvy, -mcnly); \
    if (mc_ret && mc_dt>=0) PROP_GRAV_DT(mc_dt, mc_gx, mc_gy, mc_gz); \
    else { if (mcallowbackprop ==0) {mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }}; }\
    else mcPROP_Y0; \
    DISALLOW_BACKPROP;\
  } while(0)


#define mcPROP_Y0 \
  do { \
    double mc_dt; \
    if(mcnlvy == 0) { mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }; \
    mc_dt = -mcnly/mcnlvy; \
    if(mc_dt < 0 && mcallowbackprop == 0) { mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }; \
    mcPROP_DT(mc_dt); \
    mcnly = 0; \
    DISALLOW_BACKPROP; \
  } while(0)

#define vec_prod(x, y, z, x1, y1, z1, x2, y2, z2) \
  do { \
    double mcvp_tmpx, mcvp_tmpy, mcvp_tmpz; \
    mcvp_tmpx = (y1)*(z2) - (y2)*(z1); \
    mcvp_tmpy = (z1)*(x2) - (z2)*(x1); \
    mcvp_tmpz = (x1)*(y2) - (x2)*(y1); \
    (x) = mcvp_tmpx; (y) = mcvp_tmpy; (z) = mcvp_tmpz; \
  } while(0)

#define scalar_prod(x1, y1, z1, x2, y2, z2) \
  ((x1)*(x2) + (y1)*(y2) + (z1)*(z2))

#define NORM(x,y,z) \
  do { \
    double mcnm_tmp = sqrt((x)*(x) + (y)*(y) + (z)*(z)); \
    if(mcnm_tmp != 0.0) \
    { \
      (x) /= mcnm_tmp; \
      (y) /= mcnm_tmp; \
      (z) /= mcnm_tmp; \
    } \
  } while(0)

#define rotate(x, y, z, vx, vy, vz, phi, ax, ay, az) \
  do { \
    double mcrt_tmpx = (ax), mcrt_tmpy = (ay), mcrt_tmpz = (az); \
    double mcrt_vp, mcrt_vpx, mcrt_vpy, mcrt_vpz; \
    double mcrt_vnx, mcrt_vny, mcrt_vnz, mcrt_vn1x, mcrt_vn1y, mcrt_vn1z; \
    double mcrt_bx, mcrt_by, mcrt_bz; \
    double mcrt_cos, mcrt_sin; \
    NORM(mcrt_tmpx, mcrt_tmpy, mcrt_tmpz); \
    mcrt_vp = scalar_prod((vx), (vy), (vz), mcrt_tmpx, mcrt_tmpy, mcrt_tmpz); \
    mcrt_vpx = mcrt_vp*mcrt_tmpx; \
    mcrt_vpy = mcrt_vp*mcrt_tmpy; \
    mcrt_vpz = mcrt_vp*mcrt_tmpz; \
    mcrt_vnx = (vx) - mcrt_vpx; \
    mcrt_vny = (vy) - mcrt_vpy; \
    mcrt_vnz = (vz) - mcrt_vpz; \
    vec_prod(mcrt_bx, mcrt_by, mcrt_bz, \
             mcrt_tmpx, mcrt_tmpy, mcrt_tmpz, mcrt_vnx, mcrt_vny, mcrt_vnz); \
    mcrt_cos = cos((phi)); mcrt_sin = sin((phi)); \
    mcrt_vn1x = mcrt_vnx*mcrt_cos + mcrt_bx*mcrt_sin; \
    mcrt_vn1y = mcrt_vny*mcrt_cos + mcrt_by*mcrt_sin; \
    mcrt_vn1z = mcrt_vnz*mcrt_cos + mcrt_bz*mcrt_sin; \
    (x) = mcrt_vpx + mcrt_vn1x; \
    (y) = mcrt_vpy + mcrt_vn1y; \
    (z) = mcrt_vpz + mcrt_vn1z; \
  } while(0)

#define mirror(x,y,z,rx,ry,rz,nx,ny,nz) \
  do { \
    double mcrt_tmpx= (nx), mcrt_tmpy = (ny), mcrt_tmpz = (nz); \
    double mcrt_tmpt; \
    NORM(mcrt_tmpx, mcrt_tmpy, mcrt_tmpz); \
    mcrt_tmpt=scalar_prod((rx),(ry),(rz),mcrt_tmpx,mcrt_tmpy,mcrt_tmpz); \
    (x) = rx -2 * mcrt_tmpt*mcrt_rmpx; \
    (y) = ry -2 * mcrt_tmpt*mcrt_rmpy; \
    (z) = rz -2 * mcrt_tmpt*mcrt_rmpz; \
  } while (0)

#ifdef MC_TRACE_ENABLED
#define DEBUG
#endif

#ifdef DEBUG
#define mcDEBUG_INSTR() if(!mcdotrace); else { printf("INSTRUMENT:\n"); printf("Instrument '%s' (%s)\n", mcinstrument_name, mcinstrument_source); }
#define mcDEBUG_COMPONENT(name,c,t) if(!mcdotrace); else {\
  printf("COMPONENT: \"%s\"\n" \
         "POS: %g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g\n", \
         name, c.x, c.y, c.z, t[0][0], t[0][1], t[0][2], \
         t[1][0], t[1][1], t[1][2], t[2][0], t[2][1], t[2][2]); \
  printf("Component %30s AT (%g,%g,%g)\n", name, c.x, c.y, c.z); \
  }
#define mcDEBUG_INSTR_END() if(!mcdotrace); else printf("INSTRUMENT END:\n");
#define mcDEBUG_ENTER() if(!mcdotrace); else printf("ENTER:\n");
#define mcDEBUG_COMP(c) if(!mcdotrace); else printf("COMP: \"%s\"\n", c);
#define mcDEBUG_STATE(x,y,z,vx,vy,vz,t,s1,s2,p) if(!mcdotrace); else \
  printf("STATE: %g, %g, %g, %g, %g, %g, %g, %g, %g, %g\n", \
         x,y,z,vx,vy,vz,t,s1,s2,p);
#define mcDEBUG_SCATTER(x,y,z,vx,vy,vz,t,s1,s2,p) if(!mcdotrace); else \
  printf("SCATTER: %g, %g, %g, %g, %g, %g, %g, %g, %g, %g\n", \
         x,y,z,vx,vy,vz,t,s1,s2,p);
#define mcDEBUG_LEAVE() if(!mcdotrace); else printf("LEAVE:\n");
#define mcDEBUG_ABSORB() if(!mcdotrace); else printf("ABSORB:\n");
#else
#define mcDEBUG_INSTR()
#define mcDEBUG_COMPONENT(name,c,t)
#define mcDEBUG_INSTR_END()
#define mcDEBUG_ENTER()
#define mcDEBUG_COMP(c)
#define mcDEBUG_STATE(x,y,z,vx,vy,vz,t,s1,s2,p)
#define mcDEBUG_SCATTER(x,y,z,vx,vy,vz,t,s1,s2,p)
#define mcDEBUG_LEAVE()
#define mcDEBUG_ABSORB()
#endif

#ifdef TEST
#define test_printf printf
#else
#define test_printf while(0) printf
#endif

#ifndef MC_RAND_ALG
#define MC_RAND_ALG 1
#endif

#if MC_RAND_ALG == 0
   /* Use system random() (not recommended). */
#  define MC_RAND_MAX RAND_MAX
#elif MC_RAND_ALG == 1
   /* "Mersenne Twister", by Makoto Matsumoto and Takuji Nishimura. */
#  define MC_RAND_MAX ((unsigned long)0xffffffff)
#  define random mt_random
#  define srandom mt_srandom
#elif MC_RAND_ALG == 2
   /* Algorithm used in McStas CVS-080208 and earlier (not recommended). */
#  define MC_RAND_MAX 0x7fffffff
#  define random mc_random
#  define srandom mc_srandom
#else
#  error "Bad value for random number generator choice."
#endif

#define rand01() ( ((double)random())/((double)MC_RAND_MAX+1) )
#define randpm1() ( ((double)random()) / (((double)MC_RAND_MAX+1)/2) - 1 )
#define rand0max(max) ( ((double)random()) / (((double)MC_RAND_MAX+1)/(max)) )
#define randminmax(min,max) ( rand0max((max)-(min)) + (min) )

#ifndef DANSE
void mcinit(void);
void mcraytrace(void);
void mcsave(FILE *);
void mcfinally(void);
void mcdisplay(void);
#endif

void mcdis_magnify(char *);
void mcdis_line(double, double, double, double, double, double);
void mcdis_dashed_line(double, double, double, double, double, double, int);
void mcdis_multiline(int, ...);
void mcdis_rectangle(char *, double, double, double, double, double);
void mcdis_box(double, double, double, double, double, double);
void mcdis_circle(char *, double, double, double, double);


typedef int mc_int32_t;
mc_int32_t mc_random(void);
void mc_srandom (unsigned int x);
unsigned long mt_random(void);
void mt_srandom (unsigned long x);

Coords coords_set(MCNUM x, MCNUM y, MCNUM z);
Coords coords_get(Coords a, MCNUM *x, MCNUM *y, MCNUM *z);
Coords coords_add(Coords a, Coords b);
Coords coords_sub(Coords a, Coords b);
Coords coords_neg(Coords a);
Coords coords_scale(Coords b, double scale);
double coords_sp(Coords a, Coords b);
Coords coords_xp(Coords b, Coords c);
void   coords_print(Coords a);

void rot_set_rotation(Rotation t, double phx, double phy, double phz);
int  rot_test_identity(Rotation t);
void rot_mul(Rotation t1, Rotation t2, Rotation t3);
void rot_copy(Rotation dest, Rotation src);
void rot_transpose(Rotation src, Rotation dst);
Coords rot_apply(Rotation t, Coords a);
void mccoordschange(Coords a, Rotation t, double *x, double *y, double *z,
    double *vx, double *vy, double *vz, double *time,
    double *s1, double *s2);
void mccoordschange_polarisation(Rotation t,
    double *sx, double *sy, double *sz);
double mcestimate_error(double N, double p1, double p2);
void mcreadparams(void);

void mcsetstate(double x, double y, double z, double vx, double vy, double vz,
                double t, double sx, double sy, double sz, double p);
void mcgenstate(void);
double randnorm(void);
double randtriangle(void);
void normal_vec(double *nx, double *ny, double *nz,
    double x, double y, double z);
int inside_rectangle(double, double, double, double);
int box_intersect(double *dt_in, double *dt_out, double x, double y, double z,
    double vx, double vy, double vz, double dx, double dy, double dz);
int cylinder_intersect(double *t0, double *t1, double x, double y, double z,
    double vx, double vy, double vz, double r, double h);
int sphere_intersect(double *t0, double *t1, double x, double y, double z,
                 double vx, double vy, double vz, double r);
/* ADD: E. Farhi, Aug 6th, 2001 solve_2nd_order */
int solve_2nd_order(double *Idt,
    double A,  double B,  double C);
void randvec_target_circle(double *xo, double *yo, double *zo,
    double *solid_angle, double xi, double yi, double zi, double radius);
#define randvec_target_sphere randvec_target_circle
#define plane_intersect_Gfast solve_2nd_order
void randvec_target_rect_angular(double *xo, double *yo, double *zo,
    double *solid_angle,
               double xi, double yi, double zi, double height, double width, Rotation A);
void randvec_target_rect_real(double *xo, double *yo, double *zo,
    double *solid_angle,
	       double xi, double yi, double zi, double height, double width, Rotation A,
			 double lx, double ly, double lz, int order);
void extend_list(int count, void **list, int *size, size_t elemsize);

int mcstas_main(int argc, char *argv[]);


#endif /* !MCSTAS_H */

#endif /* MCSTAS_R_H */
