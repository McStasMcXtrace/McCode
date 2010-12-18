/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright (C) 1997-2010, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Runtime: share/mcstas-r.c
*
* %Identification
* Written by: KN
* Date:    Aug 29, 1997
* Release: McStas X.Y
* Version: $Revision: 1.194 $
*
* Runtime system for McStas.
* Embedded within instrument in runtime mode.
*
* Usage: Automatically embbeded in the c code whenever required.
*
* $Id$
*
* $Log: mcstas-r.c,v $
* Revision 1.194  2009-04-02 09:47:46  pkwi
* Updated runtime and interoff from dev branch (bugfixes etc.)
*
* Proceeding to test before release
*
* Revision 1.217  2009/03/26 13:41:36  erkn
* fixed bug in mcestimate_error. Missing factor 1/N in quadratic (1st) term of square sum.
*
* Revision 1.216  2009/02/20 16:17:55  farhi
* Fixed warnings and a few bugs detected with GCC 4.3.
*
* Revision 1.215  2009/02/13 14:03:20  farhi
* Fixed GCC 4.3 warnings. More will come in components.
*
* Revision 1.214  2009/02/12 10:43:48  erkn
* check abs. value to protect for rounding errors - not signed value.
*
* Revision 1.213  2009/02/11 15:11:05  farhi
* printf format fixes revealed with gcc 4.3
*
* Revision 1.212  2009/01/23 10:51:30  farhi
* Minor speedup: Identity rotation matrices are now checked for and
* caculations reduced.
* It seems this McSatsStable commit did not got through for McStas 1.12b
*
* Revision 1.211  2009/01/18 14:43:13  farhi
* Fixed MPI event list output (broken and reported first by A. Percival).
* This required to split lists in small blocks not to overflow the MPI
* buffer.
*
* Revision 1.207  2008/10/21 15:19:18  farhi
* use common CHAR_BUFFER_LENGTH = 1024
*
* Revision 1.206  2008/10/14 14:29:50  farhi
* sans sample expanded with cylinder and sphere. cosmetics and updated
* todo.
*
* Revision 1.205  2008/10/09 14:47:53  farhi
* cosmetics for SIGNAL displaying starting date
*
* Revision 1.204  2008/09/08 10:08:21  farhi
* in save sessions, filename was not registered in mcopenedfiles,
* but was searching for simfile (always opened) leading
* to always catenated files. This happen when e.g. sending USR2 or using
* intermediate saves.
*
* Revision 1.203  2008/09/05 10:04:20  farhi
* sorry, my mistake...
*
* Revision 1.202  2008/09/02 14:50:42  farhi
* cosmetics
*
* Revision 1.201  2008/09/02 08:36:17  farhi
* MPI support: block size defined in mcstas-r.h as 1e5. Correct bug when
* p0, p1 or p2 are NULL, and re-enable S(q,w) save in Isotropic_Sqw with
* MPI.
*
* Revision 1.200  2008/08/29 15:35:08  farhi
* Split MPI_Reduce into 1e5 bits to avoid de-sync of nodes.. This was done
* in fact in last commit.
*
* Revision 1.199  2008/08/29 15:32:28  farhi
* Indicate memory allocation size when reporting error.
*
* Revision 1.198  2008/08/26 13:32:05  farhi
* Remove Threading support which is poor efficiency and may give wrong
* results
* Add quotes around string instrument parameters from mcgui simulation
* dialog
*
* Revision 1.197  2008/08/25 14:13:28  farhi
* changed neutron-mc to mcstas-users
*
* Revision 1.196  2008/08/19 11:25:52  farhi
* make sure the opened file list is reset when calling mcsave (same save
* session). already opened files are catenated, just as with the catenate
* word in mcformat.Name
*
* Revision 1.195  2008/08/07 21:52:10  farhi
* Second major commit for v2: fixed sources, and most instruments for
* automatic testing. A few instruments need more work still.
*
* Revision 1.194  2008/07/17 12:50:18  farhi
* MAJOR commit to McStas 2.x
* uniformized parameter naming in components
* uniformized SITE for instruments
* all compile OK
*
* Revision 1.192  2008/04/25 08:26:33  erkn
* added utility functions/macros for intersecting with a plane and mirroring a vector in a plane
*
* Revision 1.191  2008/04/21 16:08:05  pkwi
* OpenMPI mpicc dislikes declaration of the counter var in the for(   ) (C99 extension)
*
* Revision 1.190  2008/04/21 15:50:19  pkwi
* Name change randvec_target_rect -> randvec_target_rect_real .
*
* The renamed routine takes local emmission coordinate into account, correcting for the
* effects mentioned by George Apostolopoulus <gapost@ipta.demokritos.gr> to the
* mcstas-users list (parameter list extended by four parms).
*
* For backward-compatibility, a define has been added that maps randvec_target_rect
* to the new routine, defaulting to the "old" behaviour.
*
* To make any use of these modifications, we need to correct all (or all relevant) comps
* that have calls to randvec_target_rect.
*
* Will supply a small doc with plots showing that we now correct for the effect pointed
* out by George.
*
* Similar change should in principle happen to the _sphere focusing routine.
*
* Revision 1.189  2008/04/02 13:20:20  pkwi
* Minor correction: && -> || , otherwise we still stop at the cmdline/default ncount...
*
* Revision 1.188  2008/04/02 12:32:38  farhi
* Add explicit condition for node raytrace loop end with ncount value,
* instead of using local copy of ncount. Makes mcset_ncount work again...
*
* Revision 1.187  2008/03/27 12:47:26  farhi
* Fixed unwanted additional NL chars when using mcformat on PGPLOT 1D
*
* Revision 1.186  2008/03/25 14:34:49  pkwi
* Restoring Revision 1.184 since last commit breaks mcplot 1-D plots.
*
* (Emmanuel will fix 'locally' for mcformat)
*
* Revision 1.184  2008/03/11 16:13:08  farhi
* Infrastructure for running mcrun/mcgui on a grid. --force-compile spans
* over nodes. Local data files are sent to slaves for proper execution of
* complex components.
*
* Revision 1.183  2008/02/14 08:56:35  farhi
* McRun/McGUI SSH grid now operates on each simulation. This emulates completely
* MPI without installing it. Simulation steps for scans may also be distributed,
* and single runs can be sent to execute on other machines transparently.
* mcrun code has reduced in size. mcformat is used to merge nodes.
*
* Revision 1.182  2008/02/10 20:55:53  farhi
* OpenMP number of nodes now set properly from either --threads=NB or
* --threads which sets the computer core nb.
*
* Revision 1.180  2008/02/09 22:26:27  farhi
* Major contrib for clusters/multi-core: OpenMP support
* 	try ./configure --with-cc=gcc4.2 or icc
* then mcrun --threads ...
* Also tidy-up configure. Made relevant changes to mcrun/mcgui to enable OpenMP
* Updated install-doc accordingly
*
* Revision 1.179  2008/01/18 15:39:08  farhi
* mcformat merge mode now takes into account individual Ncount so that addition
* is a weighted sum. Event lists (when 'list' is found in Format) are catenated
* un-weighted.
* Option --format_data aliased to --format-data (internal usage)
*
* Revision 1.178  2007/12/12 08:48:58  pkwi
* Fix for wrong ncount in monitor 'ratios' in case of MPI
*
* Revision 1.177  2007/11/21 09:16:55  farhi
* Added MPI_Barrier to easy synchronization of nodes before Reduce (hey Dude !)
* (Windows) Fixed mcformat catenation of path containing disk label.
*
* Revision 1.176  2007/11/20 20:48:44  pkwi
* Fixes for MPI and input from virtual sources.
*
* When using a virtual source (with N neutron rays), the ncount is now
* restricted to exactly
*
* Ncnt = N * ceil(k/m)
*
* where k is a requested number of repetitions and m is the number of mpi hosts.
*
* Revision 1.175  2007/11/20 14:58:23  farhi
* Fixed change of ncount when using MPI (e.g. Virtual sources)
*
* Revision 1.174  2007/10/18 10:01:22  farhi
* mcdetector_out: fflush(NULL) to flush all opened streams, not only for MPI.
*
* Revision 1.173  2007/10/17 13:05:04  farhi
* MPI run: solve output scrambling when using MPI: force fflush when saving.
*
* Revision 1.172  2007/10/02 09:59:00  farhi
* Fixed exit call for instr --help and --info with or without MPI.
*
* Revision 1.171  2007/09/14 14:46:48  farhi
* mcstas-r: instr.out with MPI may now exit without error code in case of 'usage' and 'info'.
*
* Revision 1.170  2007/08/10 11:30:44  pkwi
* Compilation of mcformat warned about missing newline at end of mcstas-r.c. Added.
*
* Revision 1.169  2007/08/09 16:47:34  farhi
* Solved old gcc compilation issue when using macros in macros.
* Solved MPI issuie when exiting in the middle of a simulation. Now use MPI_Abort.
*
* Revision 1.168  2007/06/30 10:05:11  farhi
* Focus: new TOF-angle detector, so tht it looks like real data
* mcstas-r.c: fixed 2D monitor calls that are in fact 1D.
* Plus cosmetics
*
* Revision 1.167  2007/06/13 08:46:08  pkwi
* A couple of bugfixes...
*
* Virtual_input got the n+1'th weight for the n'th neutron.
*
* mcstas-r.c base adress for store/restore pointers fixed.
*
* These bugs had effect on use of virtual sources plus the new SPLIT keyword...
*
* Will double-check if this is a problem with current stable relase 1.10 and
* report to mcstas-users.
*
* Thanks to Kim Lefmann / Linda Udby for noticing a subtile energy-widening
* effect when using a virtual source!
*
* Revision 1.166  2007/06/11 09:05:33  pkwi
* We need to also check on filename_orig here, otherwise free() is run on a descriptive string in case of Monitor.comp.
*
* Revision 1.165  2007/06/06 12:30:07  pkwi
* Re-introducing --help item for MPI enabled instruments. Was removed on last cvs commit... Please remember to cvs update before committing.
*
* Revision 1.164  2007/05/29 14:57:56  farhi
* New rand function to shoot on a triangular distribution. Useful to simulate chopper time spread.
*
* Revision 1.162  2007/05/18 13:34:54  farhi
* mcformat: warning when using --scan to non McStas/PGPLOT format
* new instrument with sample container and single environment sheild
* removed OpenGENIE format (never used)
*
* Revision 1.161  2007/05/11 10:17:27  farhi
* fix field naming when generating/converting data files.
*
* Revision 1.160  2007/04/20 12:25:25  farhi
* Field names should not exceed e.g. 32 (for Matlab/scilab, etc compatibility).
* Now using VALID_NAME_LENGTH define.
*
* Revision 1.159  2007/04/03 13:29:49  farhi
* store/restore neutron now uses incremented pointer.
* Might improve slightly performances
*
* Revision 1.158  2007/03/12 14:06:35  farhi
* Warning 'Low Stat' when >25 % error in detector results
*
* Revision 1.157  2007/03/05 19:02:55  farhi
* NEXUS support now works as MPI. NEXUS keyword is optional and only -DUSE_NEXUS is required. All instruments may then export in NEXUS if McStas
* has been installed with --with-nexus
*
* Revision 1.156  2007/02/24 16:44:41  farhi
* nexus support adapted partially for SNS. File name can be specified with -f option of instr.exe or mcrun or follow NEXUS keyword. The NULL filename will set 'instr_timestamp'.
*
* Revision 1.155  2007/02/17 13:37:50  farhi
* cogen: display tip when no NEXUS keyword but user wants NeXus output.
* mcstas-r.c: fixed pb when using MPI, that gave 0 detector values.
*
* Revision 1.154  2007/02/06 14:07:40  vel
* Axes limits for 3rd axis using  DETECTOR_OUT_3D are corrected
*
* Revision 1.153  2007/02/05 10:16:08  pkwi
* Mac OS, MPI related: Disable use of sighandler in case of NOSIGNALS
*
* Revision 1.152  2007/01/29 15:51:56  farhi
* mcstas-r: avoid undef of USE_NEXUS as napi is importer afterwards
*
* Revision 1.151  2007/01/29 15:16:07  farhi
* Output file customization in header, through the DETECTOR_CUSTOM_HEADER macro.
* Small adds-on in install doc.
*
* Revision 1.150  2007/01/26 16:23:25  farhi
* NeXus final integration (mcplot, mcgui, mcrun).
* Only mcgui initiate mcstas.nxs as default output file, whereas
* simulation may use instr_time.nxs
*
* Revision 1.149  2007/01/25 14:57:36  farhi
* NeXus output now supports MPI. Each node writes a data set in the NXdata
* group. Uses compression LZW (may be unactivated with the
* -DUSE_NEXUS_FLAT).
*
* Revision 1.148  2007/01/23 00:41:05  pkwi
* Edits by Jiao Lin (linjao@caltech.edu) for embedding McStas in the DANSE project. Define -DDANSE during compile will enable these edits.
*
* Have tested that McStas works properly without the -DDANSE.
*
* Jiao: Could you please test if all is now OK?
* (After 15 minutes) Get current CVS tarball from http://www.mcstas.org/cvs
*
* Revision 1.147  2007/01/22 18:22:43  farhi
* NeXus support for lists and Virtual_output

* Revision 1.146  2007/01/22 15:13:42  farhi
* Fully functional NeXus output format.
* Works also for lists, but as catenation is not working in NAPI, one
* has to store all in memory (e.g. with large Monitor_nD bufsize), so that
* its written in one go at the end of sim.
*
* Revision 1.145  2007/01/22 01:38:25  farhi
* Improved NeXus/NXdata support. Attributes may not be at the right place
* yet.
*
* Revision 1.144  2007/01/21 15:43:08  farhi
* NeXus support. Draft version (functional). To be tuned.
*
* Revision 1.143  2006/12/19 18:51:52  farhi
* Trace disables MPI and Threads only in multicpu mode...
*
* Revision 1.142  2006/12/19 15:11:57  farhi
* Restored basic threading support without mutexes. All is now in mcstas-r.c
*
* Revision 1.141  2006/10/12 12:09:11  farhi
* mcformat can now handle scans, but only works with PGPLOT output format now.
* Input format is any, compatible with --merge as well.
*
* Revision 1.140  2006/10/09 11:31:35  farhi
* Added blue/white sky to VRML output files. Prefer Octagaplayer.
*
* Revision 1.139  2006/10/03 22:14:24  farhi
* Added octaga VRML player in install
*
* Revision 1.138  2006/09/05 15:26:18  farhi
* Update of mcformat
*
* Revision 1.137  2006/08/30 12:13:41  farhi
* Define mutexes for mcstas-r parts.
*
* Revision 1.136  2006/08/28 10:12:25  pchr
* Basic infrastructure for spin propagation in magnetic fields.
*
* Revision 1.135  2006/08/03 13:11:18  pchr
* Added additional functions for handling vectors.
*
* Revision 1.134  2006/07/11 12:21:17  pchr
* Changed polarization default value to be (0, 0, 0) (old was: sy=1)
*
* Revision 1.133  2006/07/06 08:59:21  pchr
* Added new draw methods for rectangle and box.
*
* Revision 1.132  2006/06/01 09:12:45  farhi
* Correct bug related to event for run_num > ncount
* Now forces simulation to finish both in Virtual_input and mcraytrace()
*
* Revision 1.131  2006/05/29 11:51:02  farhi
* Fixed thread joining that caused SEGV when using many threads
*
* Revision 1.130  2006/05/19 19:01:15  farhi
* rum_num now regularly incremented and display warning when pthread requested but not compiled
*
* Revision 1.129  2006/05/19 14:17:40  farhi
* Added support for multi threading with --threads=NB option for mcrun or instr.out
* Requires new option in mcgui run dialog: a popup menu to select run mode ?
*
* Revision 1.128  2006/03/22 14:54:13  farhi
* Added EOL chars (\n) for all matrix output to all formats except IDL
* (which has limitations in the way matrix are entered).
* Will generate data sets to be handled by mcformat/mcconvert
*
* Revision 1.127  2006/03/15 15:59:37  farhi
* output format function more robust (uses default args if called with NULL args)
*
* Revision 1.126  2006/03/02 12:39:33  pkwi
* Corrected typo in last commit:
*
* tout should have been t_out - resulted in:
*
* lp-07151:~> mcrun -c vanadium_example.instr
* Translating instrument definition 'vanadium_example.instr' into C ...
* mcstas -t -o vanadium_example.c vanadium_example.instr
* Warning: 'Source_flat' is an obsolete component (not maintained).
* Compiling C source 'vanadium_example.c' ...
* gcc -g -O2 -o vanadium_example.out vanadium_example.c -lm
* mcstas-r.c: In function `cylinder_intersect':
* mcstas-r.c:3713: error: `tout' undeclared (first use in this function)
* mcstas-r.c:3713: error: (Each undeclared identifier is reported only once
* mcstas-r.c:3713: error: for each function it appears in.)
* ** Error exit **
* lp-07151:~>
*
* Please make simple tests of compilation etc. before committing...
*
* Revision 1.125  2006/03/01 16:06:25  farhi
* Fixed error in cylinder_intersect when trajectory is parallel to the cylinder axis (raised by T. Vanvuure).
*
* Revision 1.124  2005/12/12 13:43:14  farhi
* remove gridding on Matlab in-line plots
*
* Revision 1.123  2005/11/08 14:20:33  farhi
* misprint
*
* Revision 1.122  2005/11/08 13:37:49  farhi
* Warnings for formats are now easier to read
*
* Revision 1.121  2005/09/16 08:43:19  farhi
* Removed floor+0.5 in Monitor_nD
* Take care of ploting with bin centers in mcplot stuff (inline+matlab+scilab+octave...)
*
* Revision 1.120  2005/08/24 09:51:31  pkwi
* Beamstop and runtime modified according to Emmanuels remarks.
*
* To allow backpropagation in a specific component, use
*
* ALLOW_BACKPROP;
*
* before calling
*
* PROP_Z0;
*
* (One could consider making the backpropagation flag common to all propagation routines, should we do so?)
*
* Revision 1.119  2005/07/25 14:55:08  farhi
* DOC update:
* checked all parameter [unit] + text to be OK
* set all versions to CVS Revision
*
* Revision 1.118  2005/07/21 10:19:24  farhi
* Corrected big bug in randvec_*_rect routines when shooting 4PI
* (when one of the params is 0)
* 'circle' routine was OK.
*
* Revision 1.117  2005/07/05 12:04:22  farhi
* Solve bug with default values and non optional parameters
*
* Revision 1.116  2005/07/04 09:06:42  farhi
* test for scilab not bianry and large matrix -> warning more often...
*
* Revision 1.115  2005/06/29 15:08:49  lieutenant
* x values centred (for 1-dim PGPLOT plots) Bug 39
*
* Revision 1.114  2005/06/22 08:56:23  farhi
* Adding 'b' flag to fopen (new files) for binary support on Win32
*
* Revision 1.113  2005/06/20 08:04:18  farhi
* More cautious message for Low Stat
* Add rounding error check in coords_sub
*
* Revision 1.112  2005/05/07 14:29:01  lieutenant
* function coords_add: z=0 if abs(z)<1e-14 to prevent loss of neutrons by numerical rounding errors
*
* Revision 1.111  2005/03/30 21:37:21  farhi
* Corrected gravity bug at last after left test modification (A was replaced by 0 for comp testing, and not put back). Thanks Klaus ! Small time values replaced by 0 in 2nd order solve (Klaus).
*
* Revision 1.110  2005/03/23 14:41:11  farhi
* Added test not to overwrite/delete a temp file by itself
*
* Revision 1.109  2005/03/02 10:40:27  farhi
* Now displays warning for Low Statistics and large matrices in text mode for Matlab/Scilab
*
* Revision 1.108  2005/02/24 15:57:20  farhi
* FIXED gravity bug (probably OK). Gravity is not handled properly in other Guide elements. Will adapt so that it works better...
* The n.v was not computed using the actual 'v' values when reaching the guide side, but before propagation. So the velocity was not reflected, but scattered depending on the previous neutron position/velocity, bringing strange divergence effects.
* On other guide elements, should update the n.v term just before reflection, not computing it before propagation... This probably holds for some other components (monochromators ???) to be checked !
*
* Revision 1.107  2005/02/23 12:29:55  farhi
* FIXED GRAVITATION BUG: was in the choice of the intersection time (2nd order
* equation result) of trajectory with plane
*
* Revision 1.105  2005/02/17 15:54:56  farhi
* Added 'per bin' in labels if more that 1 bin. Requested by R. Cubitt
*
* Revision 1.104  2005/02/16 12:20:36  farhi
* Removed left space chars at end of lines
*
* Revision 1.103  2004/11/30 16:13:22  farhi
* Put back PROP_X0 and Y0 that are used in some contrib components
* Uses NOSIGNALS and set signal handling from that
*
* Revision 1.102  2004/11/29 14:29:02  farhi
* Show title as filename in 'Detector: ... "filename"' line if no file name given
*
* Revision 1.101  2004/11/16 13:35:47  farhi
* Correct HTML -> VRML data format pre selection. May be overridden when using the --format_data option (currently undocumented)
*
* Revision 1.100  2004/09/30 08:23:41  farhi
* Correct pointer mismatch in 'xlimits' for PGPLOT data files
*
* Revision 1.99  2004/09/21 12:25:02  farhi
* Reorganised code so that I/O functions are includable easely (for mcformat.c)
*
* Revision 1.97  2004/09/09 13:46:52  farhi
* Code clean-up
*
* Revision 1.96  2004/09/07 12:28:21  farhi
* Correct allocation bug SEGV in multi-format handling
*
* Revision 1.95  2004/09/03 13:51:07  farhi
* add extension automatically in data/sim files
* may use a format for sim files, and an oher for data, e.g. HTML/VRML.
* added --data_format option to handle 2nd file format.
*
* Revision 1.94  2004/09/01 14:03:41  farhi
* 1 new VRML format for single data files. requires more work for the 'sim' file
* 2 add more info in output file name headers about how to view data
* 3 re-arranged format structure fields in more logical way
* 4 checked all formats for valid export
* 5 compute and update y/z min/max for correct values in data block of files
* 6 correct bug in dynamic format fields alloction when replacing aliases
* 7 adding more field aliases
* 8 use more dynamic allocations to avoid local const variables
*
* Revision 1.93  2004/08/25 09:45:41  farhi
* Main change in the format definition specifications. Aliases are now available to ease maintenance and writing of new formats, e.g. %FIL instead of %2$s !!
*
* Revision 1.92  2004/08/04 10:38:08  farhi
* Added 'raw' data set support (N,p,sigma) -> (N,p,p2) in data files, so that this is additive (for better grid support)
*
* Revision 1.91  2004/07/30 14:49:15  farhi
* MPI update for usage with mcrun.
* Still done by Christophe Taton. CC=mpicc and CFLAGS = -DUSE_MPI.
* Execute (using mpich) with:
*           mpirun -np NumNodes -machinefile <file> instr.out parameters...
*      where <file> is text file that lists the machines to use
*
* Revision 1.90  2004/07/16 14:59:03  farhi
* MPI support. Requires to have mpi installed, and compile with
*    CC=mpicc and CFLAGS = -DUSE_MPI.
* Work done by Christophe Taton from ENSIMAG/Grenoble
* Execute (using mpich) with:
*    mpirun -np NumNodes -machinefile <file> instr.out parameters...
* where <file> is text file that lists the machines to use
*
* Revision 1.88  2004/06/30 15:06:06  farhi
* Solved 'pre' SEGV occuring when indenting/unindenting a Parameter block
* in a data file. Removed Date field in mcinfo_simulation, as this is now included
* in all data files.
*
* Revision 1.86  2004/06/16 14:03:07  farhi
* Corrected misprint
*
* Revision 1.85  2004/03/05 17:43:47  farhi
* Default instr parameters are now correctly handled in all instrument usage cases.
*
* Revision 1.84  2004/03/03 13:41:23  pkwi
* Corrected error in relation to instrument default values: 0's were used in all cases.
*
* Revision 1.83  2004/02/26 12:53:27  farhi
* Scilab format now enables more than one monitor file for a single component
* (e.g. Monitor_nD with multiple detectors).
*
* Revision 1.82  2004/02/23 12:48:42  farhi
* Additional check for default value and unset parameters
*
* Revision 1.81  2004/02/19 14:42:52  farhi
* Experimental Octave/OpenGENIE output format (for ISIS)
*
* Revision 1.80  2004/01/23 16:14:12  pkwi
* Updated version of Mersenne Twister algorithm. make test'ed ok on my machine.
*
* Revision 1.79  2003/11/28 18:08:32  farhi
* Corrected error for IDL import
*
* Revision 1.77  2003/10/22 15:51:26  farhi
* <instr> -i also displays default parameter values (if any), which may be
* read by mcgui for init of Run Simulation dialog
*
* Revision 1.76  2003/10/22 09:18:00  farhi
* Solved name conflict problem for Matlab/Scilab by adding 'mc_' prefix
* to all component/file field names. Works ok for both, and also in binary.
*
* Revision 1.75  2003/10/21 14:08:12  pkwi
* Rectangular focusing improved: Renamed randvec_target_rect to randvec_target_rect_angular. Wrote new randvec_target_rect routine, w/h in metres. Both routines use use component orientation (ROT_A_CURRENT_COMP) as input.
*
* Modifications to Res_sample and V_sample to match new features of the runtime.
*
* Revision 1.74  2003/10/21 11:54:48  farhi
* instrument default parameter value handling now works better
* either from args or from mcreadparam (prompt)
*
* Revision 1.73  2003/09/05 08:59:17  farhi
* added INSTRUMENT parameter default value grammar
* mcinputtable now has also default values
* mcreadpar now uses default values if parameter not given
* extended instr_formal parameter struct
* extended mcinputtable structure type
*
* Revision 1.72  2003/08/26 12:32:43  farhi
* Corrected 4PI random vector generation to retain initial vector length
*
* Revision 1.71  2003/08/20 09:25:00  farhi
* Add the instrument Source tag in scan files (origin of data !)
*
* Revision 1.70  2003/08/12 13:35:52  farhi
* displays known signals list in instrument help (-h)
*
* Revision 1.68  2003/06/17 14:21:54  farhi
* removed 'clear %4$s' in Scilab/Matlab 'end of section' format which
* caused pb when comp_name == file_name
*
* Revision 1.67  2003/06/12 10:22:00  farhi
* -i show info as McStas format, --info use MCSTAS_FORMAT or --format setting
*
* Revision 1.66  2003/06/10 11:29:58  pkwi
* Corrected multiple parse errors: Added two missing sets of curly brackets { } in parameter parsing function.
*
* Revision 1.65  2003/06/05 09:25:59  farhi
* restore header support in data files when --format option found
*
* Revision 1.64  2003/05/26 10:21:00  farhi
* Correct core dump for instrument STRING parameters in 'string printer'
*
* Revision 1.63  2003/05/20 11:54:38  farhi
* make sighandler not restart SAVE when already saving (USR2)
*
* Revision 1.62  2003/05/16 12:13:03  farhi
* added path rehash for Matlab mcload_inline
*
* Revision 1.61  2003/04/25 16:24:44  farhi
* corrected 4PI scattering from randvec_* functions causing mcdisplay to crash
* when using (0,0,0) vector for coordinate transformations
*
* Revision 1.60  2003/04/16 14:55:47  farhi
* Major change in saving data so that it's shown just like PGPLOT
* and axes+labels should follow data orientation (if transposed)
* when in binary mode, sets -a as default. Use +a to force text header
*
* Revision 1.59  2003/04/09 15:51:33  farhi
* Moved MCSTAS_FORMAT define
*
* Revision 1.58  2003/04/08 18:55:56  farhi
* Made XML format more NeXus compliant
*
* Revision 1.57  2003/04/07 11:50:50  farhi
* Extended the way mcplot:plotter is assigned. Set --portable ok
* Handle Scilab:Tk and ~GTk menu (shifted)
* Updated help in mcrun and mcstas-r.c
*
* Revision 1.56  2003/04/04 18:36:12  farhi
* Corrected $ and % chars for IDL format, conflicting with pfprintf (Dec/SGI)
*
* Revision 1.55  2003/04/04 15:11:08  farhi
* Use MCSTAS_FORMAT env var for default plotter, or use mcstas_config
* Corrected strlen(NULL pointer) for getenv(MCSTAS_FORMAT)==NULL
*
* Revision 1.54  2003/04/04 14:26:25  farhi
* Managed --no-runtime to work. Use MCSTAS_FORMAT env/define for default format
* Make --no-output-files still print out the integrated counts
*
* Revision 1.53  2003/02/18 09:10:52  farhi
* Just changed a message (warning for -a flag binary)
*
* Revision 1.51  2003/02/11 12:28:46  farhi
* Variouxs bug fixes after tests in the lib directory
* mcstas_r  : disable output with --no-out.. flag. Fix 1D McStas output
* read_table:corrected MC_SYS_DIR -> MCSTAS define
* monitor_nd-lib: fix Log(signal) log(coord)
* HOPG.trm: reduce 4000 points -> 400 which is enough and faster to resample
* Progress_bar: precent -> percent parameter
* CS: ----------------------------------------------------------------------
*
* Revision 1.50  2003/02/06 14:25:05  farhi
* Made --no-output-files work again and 1D McStas data 4 columns again
*
* : ----------------------------------------------------------------------
*
* Revision 1.7 2002/10/19 22:46:21 ef
*        gravitation for all with -g. Various output formats.
*
* Revision 1.6 2002/09/17 12:01:21 ef
*        changed randvec_target_sphere to circle
* added randvec_target_rect
*
* Revision 1.5 2002/09/03 19:48:01 ef
*        corrected randvec_target_sphere. created target_rect.
*
* Revision 1.4 2002/09/02 18:59:05 ef
*        moved adapt_tree functions to independent lib. Updated sighandler.
*
* Revision 1.3 2002/08/28 11:36:37 ef
*        Changed to lib/share/c code
*
* Revision 1.2 2001/10/10 11:36:37 ef
*        added signal handler
*
* Revision 1.1 1998/08/29 11:36:37 kn
*        Initial revision
*
*******************************************************************************/

#ifndef MCSTAS_R_H
#include "mcstas-r.h"
#endif
#ifdef DANSE
#include "mcstas-globals.h"
#endif

/*******************************************************************************
* The I/O format definitions and functions
*******************************************************************************/

#ifndef DANSE
#ifdef MC_ANCIENT_COMPATIBILITY
int mctraceenabled = 0;
int mcdefaultmain  = 0;
#endif
/* else defined directly in the McStas generated C code */

static   long mcseed                 = 0;
static   int  mcascii_only           = 0;
static   int  mcsingle_file          = 0;
static   long mcstartdate            = 0;
static   int  mcdisable_output_files = 0;
mcstatic int  mcgravitation          = 0;
mcstatic int  mcdotrace              = 0;
/* mcstatic FILE *mcsiminfo_file        = NULL; */
static   char *mcdirname             = NULL;
static   char *mcsiminfo_name        = "mcstas";
int      mcallowbackprop             = 0;
int      mcMagnet                    = 0;
/*the magnet stack*/
double*  mcMagnetData                = NULL;
Coords   mcMagnetPos;
Rotation mcMagnetRot;
char*    mcDetectorCustomHeader      = NULL;
char*    mcopenedfiles               = "";
long     mcopenedfiles_size          = 0;
#endif

/* mcMagneticField(x, y, z, t, Bx, By, Bz) */
void (*mcMagneticField) (double, double, double, double,
			 double*, double*, double*) = NULL;
void (*mcMagnetPrecession) (double, double, double, double, double, double,
			    double, double*, double*, double*, double, Coords, Rotation) = NULL;

/* Number of neutron histories to simulate. */
#ifndef DANSE
mcstatic double mcncount             = 1e6;
mcstatic double mcrun_num            = 0;
#endif

/* parameters handling ====================================================== */

/* Instrument input parameter type handling. */
/* mcparm_double: extract double value from 's' into 'vptr' */
static int
mcparm_double(char *s, void *vptr)
{
  char *p;
  double *v = (double *)vptr;

  if (!s) { *v = 0; return(1); }
  *v = strtod(s, &p);
  if(*s == '\0' || (p != NULL && *p != '\0') || errno == ERANGE)
    return 0;                        /* Failed */
  else
    return 1;                        /* Success */
}

/* mcparminfo_double: display parameter type double */
static char *
mcparminfo_double(char *parmname)
{
  return "double";
}

/* mcparmerror_double: display error message when failed extract double */
static void
mcparmerror_double(char *parm, char *val)
{
  fprintf(stderr, "Error: Invalid value '%s' for floating point parameter %s (mcparmerror_double)\n",
          val, parm);
}

/* mcparmprinter_double: convert double to string */
static void
mcparmprinter_double(char *f, void *vptr)
{
  double *v = (double *)vptr;
  sprintf(f, "%g", *v);
}

/* mcparm_int: extract int value from 's' into 'vptr' */
static int
mcparm_int(char *s, void *vptr)
{
  char *p;
  int *v = (int *)vptr;
  long x;

  if (!s) { *v = 0; return(1); }
  *v = 0;
  x = strtol(s, &p, 10);
  if(x < INT_MIN || x > INT_MAX)
    return 0;                        /* Under/overflow */
  *v = x;
  if(*s == '\0' || (p != NULL && *p != '\0') || errno == ERANGE)
    return 0;                        /* Failed */
  else
    return 1;                        /* Success */
}

/* mcparminfo_int: display parameter type int */
static char *
mcparminfo_int(char *parmname)
{
  return "int";
}

/* mcparmerror_int: display error message when failed extract int */
static void
mcparmerror_int(char *parm, char *val)
{
  fprintf(stderr, "Error: Invalid value '%s' for integer parameter %s (mcparmerror_int)\n",
          val, parm);
}

/* mcparmprinter_int: convert int to string */
static void
mcparmprinter_int(char *f, void *vptr)
{
  int *v = (int *)vptr;
  sprintf(f, "%d", *v);
}

/* mcparm_string: extract char* value from 's' into 'vptr' (copy) */
static int
mcparm_string(char *s, void *vptr)
{
  char **v = (char **)vptr;
  if (!s) { *v = NULL; return(1); }
  *v = (char *)malloc(strlen(s) + 1);
  if(*v == NULL)
  {
    exit(fprintf(stderr, "Error: Out of memory %li (mcparm_string).\n", (long)strlen(s) + 1));
  }
  strcpy(*v, s);
  return 1;                        /* Success */
}

/* mcparminfo_string: display parameter type string */
static char *
mcparminfo_string(char *parmname)
{
  return "string";
}

/* mcparmerror_string: display error message when failed extract string */
static void
mcparmerror_string(char *parm, char *val)
{
  fprintf(stderr, "Error: Invalid value '%s' for string parameter %s (mcparmerror_string)\n",
          val, parm);
}

/* mcparmprinter_string: convert string to string (including esc chars) */
static void
mcparmprinter_string(char *f, void *vptr)
{
  char **v = (char **)vptr;
  char *p;

  if (!*v) { *f='\0'; return; }
  strcpy(f, "");
  for(p = *v; *p != '\0'; p++)
  {
    switch(*p)
    {
      case '\n':
        strcat(f, "\\n");
        break;
      case '\r':
        strcat(f, "\\r");
        break;
      case '"':
        strcat(f, "\\\"");
        break;
      case '\\':
        strcat(f, "\\\\");
        break;
      default:
        strncat(f, p, 1);
    }
  }
  /* strcat(f, "\""); */
}

/* now we may define the parameter structure, using previous functions */
static struct
  {
    int (*getparm)(char *, void *);
    char * (*parminfo)(char *);
    void (*error)(char *, char *);
    void (*printer)(char *, void *);
  } mcinputtypes[] =
      {
        mcparm_double, mcparminfo_double, mcparmerror_double,
                mcparmprinter_double,
        mcparm_int, mcparminfo_int, mcparmerror_int,
                mcparmprinter_int,
        mcparm_string, mcparminfo_string, mcparmerror_string,
                mcparmprinter_string
      };

/* mcestimate_error: compute sigma from N,p,p2 in Gaussian large numbers approx */
double mcestimate_error(double N, double p1, double p2)
{
  double pmean, n1;
  if(N <= 1)
    return p1;
  pmean = p1 / N;
  n1 = N - 1;
  /* Note: underflow may cause p2 to become zero; the fabs() below guards
     against this. */
  return sqrt((N/n1)*fabs(p2 - pmean*pmean));
}

/* mcset_ncount: set total number of neutrons to generate */
void mcset_ncount(double count)
{
  mcncount = count;
}

/* mcget_ncount: get total number of neutrons to generate */
double mcget_ncount(void)
{
  return mcncount;
}

/* mcget_run_num: get curent number of neutrons in TRACE */
double mcget_run_num(void)
{
  return mcrun_num;
}

#ifdef USE_MPI
/* MPI rank */
static int mpi_node_rank;
static int mpi_node_root = 0;

/*******************************************************************************
* mc_MPI_Reduce: Gathers arrays from MPI nodes using Reduce function.
*******************************************************************************/
int mc_MPI_Reduce(void *sbuf, void *rbuf,
                  int count, MPI_Datatype dtype,
                  MPI_Op op, int root, MPI_Comm comm)
{
  void *lrbuf;
  int dsize;
  int res= MPI_SUCCESS;
  
  if (!sbuf || count <= 0) return(-1);

  MPI_Type_size(dtype, &dsize);
  lrbuf = malloc(count*dsize);
  if (lrbuf == NULL)
    exit(fprintf(stderr, "Error: Out of memory %li (mc_MPI_Reduce).\n", (long)count*dsize));
  /* we must cut the buffer into blocks not exceeding the MPI max buffer size of 32000 */
  long offset=0;
  int  length=MPI_REDUCE_BLOCKSIZE; /* defined in mcstas.h */
  while (offset < count && res == MPI_SUCCESS) {
    if (!length || offset+length > count-1) length=count-offset; else length=MPI_REDUCE_BLOCKSIZE;
    res = MPI_Reduce((void*)(sbuf+offset*dsize), (void*)(lrbuf+offset*dsize), length, dtype, op, root, comm);
    offset += length;
  }

  if(res != MPI_SUCCESS)
    fprintf(stderr, "Warning: node %i: MPI_Reduce error (mc_MPI_Reduce) at offset=%li, count=%i\n", mpi_node_rank, offset, count);

  if(mpi_node_rank == root)
    memcpy(rbuf, lrbuf, count*dsize);

  free(lrbuf);
  return res;
}

/*******************************************************************************
* mc_MPI_Send: Send array to MPI node by blocks to avoid buffer limit
*******************************************************************************/
int mc_MPI_Send(void *sbuf, 
                  int count, MPI_Datatype dtype,
                  int dest, MPI_Comm comm)
{
  int dsize;
  int res= MPI_SUCCESS;
  
  if (!sbuf || count <= 0) return(-1);

  MPI_Type_size(dtype, &dsize);

  long offset=0;
  int  tag=1;
  int  length=MPI_REDUCE_BLOCKSIZE; /* defined in mcstas.h */
  while (offset < count && res == MPI_SUCCESS) {
    if (offset+length > count-1) length=count-offset; else length=MPI_REDUCE_BLOCKSIZE;
    res = MPI_Send((void*)(sbuf+offset*dsize), length, dtype, dest, tag++, comm);
    offset += length;
  }

  if(res != MPI_SUCCESS)
    fprintf(stderr, "Warning: node %i: MPI_Send error (mc_MPI_Send) at offset=%li, count=%i tag=%i\n", mpi_node_rank, offset, count, tag);

  return res;
}

/*******************************************************************************
* mc_MPI_Recv: Receives arrays from MPI nodes by blocks to avoid buffer limit
*             the buffer must have been allocated previously.
*******************************************************************************/
int mc_MPI_Recv(void *sbuf, 
                  int count, MPI_Datatype dtype,
                  int source, MPI_Comm comm)
{
  int dsize;
  int res= MPI_SUCCESS;
  
  if (!sbuf || count <= 0) return(-1);

  MPI_Type_size(dtype, &dsize);

  long offset=0;
  int  tag=1;
  int  length=MPI_REDUCE_BLOCKSIZE; /* defined in mcstas.h */
  while (offset < count && res == MPI_SUCCESS) {
    if (offset+length > count-1) length=count-offset; else length=MPI_REDUCE_BLOCKSIZE;
    res = MPI_Recv((void*)(sbuf+offset*dsize), length, dtype, source, tag++, comm, MPI_STATUS_IGNORE);
    offset += length;
  }

  if(res != MPI_SUCCESS)
    fprintf(stderr, "Warning: node %i: MPI_Send error (mc_MPI_Send) at offset=%li, count=%i tag=%i\n", mpi_node_rank, offset, count, tag);

  return res;
}

#endif /* USE_MPI */

/* Multiple output format support. ========================================== */
#ifdef USE_NEXUS
#define mcNUMFORMATS 9
#else
#define mcNUMFORMATS 8
#endif
#ifndef MCSTAS_FORMAT
#define MCSTAS_FORMAT "McStas"  /* default format */
#endif

#ifndef DANSE
mcstatic struct mcformats_struct mcformat;
mcstatic struct mcformats_struct mcformat_data;
#endif

/*******************************************************************************
* Definition of output formats. structure defined in mcstas-r.h
* Name aliases are defined in mcuse_format_* functions (below)
*******************************************************************************/

mcstatic struct mcformats_struct mcformats[mcNUMFORMATS] = {
  { "McStas", "sim",
    "%PREFormat: %FMT file. Use mcplot/PGPLOT to view.\n"
      "%PREURL:    http://www.mcstas.org/\n"
      "%PREEditor: %USR\n"
      "%PRECreator:%SRC simulation (McStas " MCSTAS_VERSION ")\n"
      "%PREDate:   Simulation started (%DATL) %DAT\n"
      "%PREFile:   %FIL\n",
    "%PREEndDate:%DAT\n",
    "%PREbegin %TYP\n",
    "%PREend %TYP\n",
    "%PRE%NAM: %VAL\n",
    "", "",
    "%PREErrors [%PAR/%FIL]: \n", "",
    "%PREEvents [%PAR/%FIL]: \n", "" },
  { "Scilab", "sci",
    "function mc_%VPA = get_%VPA(p)\n"
      "// %FMT function issued from McStas on %DAT\n"
      "// McStas simulation %SRC: %FIL %FMT\n"
      "// Import data using scilab> exec('%VPA.sci',-1); s=get_%VPA(); and s=get_%VPA('plot'); to plot\n"
      "mode(-1); //silent execution\n"
      "if argn(2) > 0, p=1; else p=0; end\n"
      "mc_%VPA = struct();\n"
      "mc_%VPA.Format ='%FMT';\n"
      "mc_%VPA.URL    ='http://www.mcstas.org';\n"
      "mc_%VPA.Editor ='%USR';\n"
      "mc_%VPA.Creator='%SRC McStas " MCSTAS_VERSION " simulation';\n"
      "mc_%VPA.Date   =%DATL; // for getdate\n"
      "mc_%VPA.File   ='%FIL';\n",
    "mc_%VPA.EndDate=%DATL; // for getdate\nendfunction\n"
    "function d=mcload_inline(d)\n"
      "// local inline func to load data\n"
      "execstr(['S=['+part(d.type,10:(length(d.type)-1))+'];']);\n"
      "if ~length(d.data)\n"
      " if ~length(strindex(d.format, 'binary'))\n"
      "  exec(d.filename,-1);p=d.parent;\n"
      "  if ~execstr('d2='+d.func+'();','errcatch'),d=d2; d.parent=p;end\n"
      " else\n"
      "  if length(strindex(d.format, 'float')), t='f';\n"
      "  elseif length(strindex(d.format, 'double')), t='d';\n"
      "  else return; end\n"
      "  fid=mopen(d.filename, 'rb');\n"
      "  pS = prod(S);\n"
      "  x = mget(3*pS, t, fid);\n"
      "  d.data  =matrix(x(1:pS), S);\n"
      "  if length(x) >= 3*pS,\n"
      "  d.errors=matrix(x((pS+1):(2*pS)), S);\n"
      "  d.events=matrix(x((2*pS+1):(3*pS)), S);end\n"
      "  mclose(fid);\n"
      "  return\n"
      " end\n"
      "end\n"
      "endfunction\n"
      "function d=mcplot_inline(d,p)\n"
      "// local inline func to plot data\n"
      "if ~length(strindex(d.type,'0d')), d=mcload_inline(d); end\n"
      "if ~p, return; end;\n"
      "execstr(['l=[',d.xylimits,'];']); S=size(d.data);\n"
      "t1=['['+d.parent+'] '+d.filename+': '+d.title];t = [t1;['  '+d.variables+'=['+d.values+']'];['  '+d.signal];['  '+d.statistics]];\n"
      "mprintf('%%s\\n',t(:));\n"
      "if length(strindex(d.type,'0d')),return; end\n"
      "w=winsid();if length(w),w=w($)+1; else w=0; end\n"
      "xbasr(w); xset('window',w);\n"
      "if length(strindex(d.type,'2d'))\n"
      " if S(2) > 1, d.stepx=abs(l(1)-l(2))/(S(2)-1); else d.stepx=0; end\n"
      " if S(1) > 1, d.stepy=abs(l(3)-l(4))/(S(1)-1); else d.stepy=0; end\n"
      " d.x=linspace(l(1)+d.stepx/2,l(2)-d.stepx/2,S(2));\n"
      " d.y=linspace(l(3)+d.stepy/2,l(4)-d.stepy/2,S(1)); z=d.data;\n"
      " xlab=d.xlabel; ylab=d.ylabel; x=d.x; y=d.y;\n"
      " fz=max(abs(z));fx=max(abs(d.x));fy=max(abs(d.y));\n"
      " if fx>0,fx=round(log10(fx)); x=x/10^fx; xlab=xlab+' [*10^'+string(fx)+']'; end\n"
      " if fy>0,fy=round(log10(fy)); y=y/10^fy; ylab=ylab+' [*10^'+string(fy)+']'; end\n"
      " if fz>0,fz=round(log10(fz)); z=z/10^fz; t1=t1+' [*10^'+string(fz)+']'; end\n"
      " xset('colormap',hotcolormap(64));\n"
      " plot3d1(x,y,z',90,0,xlab+'@'+ylab+'@'+d.zlabel,[-1,2,4]); xtitle(t);\n"
      "else\n"
      " if max(S) > 1, d.stepx=abs(l(1)-l(2))/(max(S)-1); else d.stepx=0; end\n"
      " d.x=linspace(l(1)+d.stepx/2,l(2)-d.stepx/2,max(S));\n"
      " plot2d(d.x,d.data);xtitle(t,d.xlabel,d.ylabel);\n"
      "end\n"
      "xname(t1);\nendfunction\n"
    "mc_%VPA=get_%VPA();\n",
    "// Section %TYP [%NAM] (level %LVL)\n"
      "%PREt=[]; execstr('t=mc_%VNA.class','errcatch'); if ~length(t), mc_%VNA = struct(); end; mc_%VNA.class = '%TYP';",
    "%PREmc_%VPA.mc_%VNA = 0; mc_%VPA.mc_%VNA = mc_%VNA;\n",
    "%PREmc_%SEC.%NAM = '%VAL';\n",
    "%PREmc_%VPA.func='get_%VPA';\n%PREmc_%VPA.data = [ \n",
    " ]; // end of data\n%PREif length(mc_%VPA.data) == 0, single_file=0; else single_file=1; end\n%PREmc_%VPA=mcplot_inline(mc_%VPA,p);\n",
    "%PREerrors = [ \n",
    " ]; // end of errors\n%PREif single_file == 1, mc_%VPA.errors=errors; end\n",
    "%PREevents = [ \n",
    " ]; // end of events\n%PREif single_file == 1, mc_%VPA.events=events; end\n"},
  { "Matlab", "m",
    "function mc_%VPA = get_%VPA(p)\n"
      "%% %FMT function issued from McStas on %DAT\n"
      "%% McStas simulation %SRC: %FIL\n"
      "%% Import data using matlab> s=%VPA; and s=%VPA('plot'); to plot\n"
      "if nargout == 0 | nargin > 0, p=1; else p=0; end\n"
      "mc_%VPA.Format ='%FMT';\n"
      "mc_%VPA.URL    ='http://www.mcstas.org';\n"
      "mc_%VPA.Editor ='%USR';\n"
      "mc_%VPA.Creator='%SRC McStas " MCSTAS_VERSION " simulation';\n"
      "mc_%VPA.Date   =%DATL; %% for datestr\n"
      "mc_%VPA.File   ='%FIL';\n",
    "mc_%VPA.EndDate=%DATL; %% for datestr\n"
      "function d=mcload_inline(d)\n"
      "%% local inline function to load data\n"
      "S=d.type; eval(['S=[ ' S(10:(length(S)-1)) ' ];']);\n"
      "if isempty(d.data)\n"
      " if ~length(findstr(d.format, 'binary'))\n"
      "  if ~strcmp(d.filename,[d.func,'.m']) copyfile(d.filename,[d.func,'.m']); end\n"
      "  p=d.parent;path(path);\n"
      "  eval(['d=',d.func,';']);d.parent=p;\n"
      "  if ~strcmp(d.filename,[d.func,'.m']) delete([d.func,'.m']); end\n"
      " else\n"
      "  if length(findstr(d.format, 'float')), t='single';\n"
      "  elseif length(findstr(d.format, 'double')), t='double';\n"
      "  else return; end\n"
      "  if length(S) == 1, S=[S 1]; end\n"
      "  fid=fopen(d.filename, 'r');\n"
      "  pS = prod(S);\n"
      "  x = fread(fid, 3*pS, t);\n"
      "  d.data  =reshape(x(1:pS), S);\n"
      "  if prod(size(x)) >= 3*pS,\n"
      "  d.errors=reshape(x((pS+1):(2*pS)), S);\n"
      "  d.events=reshape(x((2*pS+1):(3*pS)), S);end\n"
      "  fclose(fid);\n"
      "  return\n"
      " end\n"
      "end\n"
      "return;\n"
      "function d=mcplot_inline(d,p)\n"
      "%% local inline function to plot data\n"
      "if isempty(findstr(d.type,'0d')), d=mcload_inline(d); end\nif ~p, return; end;\n"
      "eval(['l=[',d.xylimits,'];']); S=size(d.data);\n"
      "t1=['[',d.parent,'] ',d.filename,': ',d.title];t = strvcat(t1,['  ',d.variables,'=[',d.values,']'],['  ',d.signal],['  ',d.statistics]);\n"
      "disp(t);\n"
      "if ~isempty(findstr(d.type,'0d')), return; end\n"
      "figure; if ~isempty(findstr(d.type,'2d'))\n"
      " if S(2) > 1, d.stepx=abs(l(1)-l(2))/(S(2)-1); else d.stepx=0; end\n"
      " if S(1) > 1, d.stepy=abs(l(3)-l(4))/(S(1)-1); else d.stepy=0; end\n"
      " d.x=linspace(l(1)+d.stepx/2,l(2)-d.stepx/2,S(2));\n"
      " d.y=linspace(l(3)+d.stepy/2,l(4)-d.stepy/2,S(1));\n"
      " surface(d.x,d.y,d.data); xlim([l(1) l(2)]); ylim([l(3) l(4)]); shading flat;\n"
      "else\n"
      " if max(S) > 1, d.stepx=abs(l(1)-l(2))/(max(S)-1); else d.stepx=0; end\n"
      " d.x=linspace(l(1)+d.stepx/2,l(2)-d.stepx/2,max(S));\n"
      " plot(d.x,d.data); xlim([l(1) l(2)]);\n"
      "end\n"
      "xlabel(d.xlabel); ylabel(d.ylabel); title(t); \n"
      "set(gca,'position',[.18,.18,.7,.65]); set(gcf,'name',t1);grid on;\n"
      "if ~isempty(findstr(d.type,'2d')), colorbar; end\n",
    "%% Section %TYP [%NAM] (level %LVL)\n"
      "mc_%VNA.class = '%TYP';",
    "mc_%VPA.mc_%VNA = mc_%VNA;\n",
    "%PREmc_%SEC.%NAM = '%VAL';\n",
    "%PREmc_%VPA.func='%VPA';\n%PREmc_%VPA.data = [ \n",
    " ]; %% end of data\nif length(mc_%VPA.data) == 0, single_file=0; else single_file=1; end\nmc_%VPA=mcplot_inline(mc_%VPA,p);\n",
    "%PREerrors = [ \n",
    " ]; %% end of errors\nif single_file, mc_%VPA.errors=errors; end\n",
    "%PREevents = [ \n",
    " ]; %% end of events\nif single_file, mc_%VPA.events=events; end\n"},
  { "IDL", "pro",
    "; McStas/IDL file. Import using idl> s=%VPA() and s=%VPA(/plot) to plot\n"
      "function mcload_inline,d\n"
      "; local inline function to load external data\n"
      "S=d.type & a=execute('S=long(['+strmid(S,9,strlen(S)-10)+'])')\n"
      "if strpos(d.format, 'binary') lt 0 then begin\n"
      " p=d.parent\n"
      " x=read_binary(d.filename)\n"
      " get_lun, lun\n"
      " openw,lun,d.func+'.pro'\n"
      " writeu, lun,x\n"
      " free_lun,lun\n"
      " resolve_routine, d.func, /is_func, /no\n"
      " d=call_function(d.func)\n"
      "endif else begin\n"
      " if strpos(d.format, 'float') ge 0 then t=4 $\n"
      " else if strpos(d.format, 'double') ge 0 then t=5 $\n"
      " else return,d\n"
      " x=read_binary(d.filename, data_type=t)\n"
      " pS=n_elements(S)\nif pS eq 1 then pS=long(S) $\n"
      " else if pS eq 2 then pS=long(S(0)*S(1)) $\n"
      " else pS=long(S(0)*S(1)*S(2))\n"
      " pS=pS(0)\nstv,d,'data',reform(x(0:(pS-1)),S)\n"
      " d.data=transpose(d.data)\n"
      " if n_elements(x) ge long(3*pS) then begin\n"
      "  stv,d,'errors',reform(x(pS:(2*pS-1)),S)\n"
      "  stv,d,'events',reform(x((2*pS):(3*pS-1)),S)\n"
      "  d.errors=transpose(d.errors)\n"
      "  d.events=transpose(d.events)\n"
      " endif\n"
      "endelse\n"
      "return,d\nend ; FUN load\n"
    "function mcplot_inline,d,p\n"
      "; local inline function to plot data\n"
      "if size(d.data,/typ) eq 7 and strpos(d.type,'0d') lt 0 then d=mcload_inline(d)\n"
      "if p eq 0 or strpos(d.type,'0d') ge 0 then return, d\n"
      "S=d.type & a=execute('S=long(['+strmid(S,9,strlen(S)-10)+'])')\n"
      "stv,d,'data',reform(d.data,S,/over)\n"
      "if total(strpos(tag_names(d),'ERRORS')+1) gt 0 then begin\n"
      " stv,d,'errors',reform(d.errors,S,/over)\n"
      " stv,d,'events',reform(d.events,S,/over)\n"
      "endif\n"
      "d.xylimits=strjoin(strsplit(d.xylimits,' ',/extract),',') & a=execute('l=['+d.xylimits+']')\n"
      "t1='['+d.parent+'] '+d.filename+': '+d.title\n"
      "t=[t1,'  '+d.variables+'=['+d.values+']','  '+d.signal,'  '+d.statistics]\n"
      "print,t\n"
      "if strpos(d.type,'0d') ge 0 then return,d\n"
      "d.xlabel=strjoin(strsplit(d.xlabel,'`!\"^&*()-+=|\\,.<>/?@''~#{[}]',/extract),'_')\n"
      "d.ylabel=strjoin(strsplit(d.ylabel,'`!\"^&*()-+=|\\,.<>/?@''~#{[}]',/extract),'_')\n"
      "stv,d,'x',l(0)+indgen(S(0))*(l(1)-l(0))/S(0)\n"
      "if strpos(d.type,'2d') ge 0 then begin\n"
      "  name={DATA:d.func,IX:d.xlabel,IY:d.ylabel}\n"
      "  stv,d,'y',l(2)+indgen(S(1))*(l(3)-l(2))/S(1)\n"
      "  live_surface,d.data,xindependent=d.x,yindependent=d.y,name=name,reference_out=Win\n"
      "endif else begin\n"
      "  name={DATA:d.func,I:d.xlabel}\n"
      "  live_plot,d.data,independent=d.x,name=name,reference_out=Win\n"
      "endelse\n"
      "live_text,t,Window_In=Win.Win,location=[0.3,0.9]\n"
      "return,d\nend ; FUN plot\n"
    "pro stv,S,T,V\n"
      "; procedure set-tag-value that does S.T=V\n"
      "sv=size(V)\n"
      "T=strupcase(T)\n"
      "TL=strupcase(tag_names(S))\n"
      "id=where(TL eq T)\n"
      "sz=[0,0,0]\n"
      "vd=n_elements(sv)-2\n"
      "type=sv[vd]\n"
      "if id(0) ge 0 then d=execute('sz=SIZE(S.'+T+')')\n"
      "if (sz(sz(0)+1) ne sv(sv(0)+1)) or (sz(0) ne sv(0)) $\n"
      "  or (sz(sz(0)+2) ne sv(sv(0)+2)) $\n"
      "  or type eq 8 then begin\n"
      " ES = ''\n"
      " for k=0,n_elements(TL)-1 do begin\n"
      "  case TL(k) of\n"
      "   T:\n"
      "   else: ES=ES+','+TL(k)+':S.'+TL(k)\n"
      "  endcase\n"
      " endfor\n"
      " d=execute('S={'+T+':V'+ES+'}')\n"
      "endif else d=execute('S.'+T+'=V')\n"
      "end ; PRO stv\n"
    "function %VPA,plot=plot\n"
      "; %FMT function issued from McStas on %DAT\n"
      "; McStas simulation %SRC: %FIL\n"
      "; import using s=%VPA() and s=%VPA(/plot) to plot\n"
      "if keyword_set(plot) then p=1 else p=0\n"
      "%7$s={Format:'%FMT',URL:'http://www.mcstas.org',"
      "Editor:'%USR',$\n"
      "Creator:'%SRC McStas " MCSTAS_VERSION " simulation',$\n"
      "Date:%DATL,"
      "File:'%FIL'}\n",
    "stv,%VPA,'EndDate',%DATL ; for systime\nreturn, %VPA\nend\n",
    "; Section %TYP [%NAM] (level %LVL)\n"
      "%PRE%VNA={class:'%TYP'}\n",
    "%PREstv,%VPA,'%VNA',%VNA\n",
    "%PREstv,%SEC,'%NAM','%VAL'\n",
    "%PREstv,%VPA,'func','%VPA' & data=[ $\n",
    " ]\n%PREif size(data,/type) eq 7 then single_file=0 else single_file=1\n"
    "%PREstv,%VPA,'data',data & data=0 & %VPA=mcplot_inline(%VPA,p)\n",
    "%PREif single_file ne 0 then begin errors=[ ",
    " ]\n%PREstv,%VPA,'errors',reform(errors,%MDIM,%NDIM,/over) & errors=0\n%PREendif\n",
    "%PREif single_file ne 0 then begin events=[ ",
    " ]\n%PREstv,%VPA,'events',reform(events,%MDIM,%NDIM,/over) & events=0\n%PREendif\n\n"},
  { "XML", "xml",
    "<?xml version=\"1.0\" ?>\n<!--\n"
      "URL:    http://www.nexusformat.org/\n"
      "Editor: %USR\n"
      "Creator:%SRC McStas " MCSTAS_VERSION " [www.mcstas.org].\n"
      "Date:   Simulation started (%DATL) %DAT\n"
      "File:   %FIL\n"
      "View with Mozilla, InternetExplorer, gxmlviewer, kxmleditor\n-->\n"
      "<NX%PAR file_name=\"%FIL\" file_time=\"%DAT\" user=\"%USR\">\n"
        "<NXentry name=\"McStas " MCSTAS_VERSION "\"><start_time>%DAT</start_time>\n",
    "<end_time>%DAT</end_time></NXentry></NX%PAR>\n<!-- EndDate:%DAT -->\n",
    "%PRE<NX%TYP name=\"%NAM\">\n",
    "%PRE</NX%TYP>\n",
    "%PRE<%NAM>%VAL</%NAM>\n",
    "%PRE<%XVL long_name=\"%XLA\" axis=\"1\" primary=\"1\" min=\"%XMIN\""
        " max=\"%XMAX\" dims=\"%MDIM\" range=\"1\"></%XVL>\n"
      "%PRE<%YVL long_name=\"%YLA\" axis=\"2\" primary=\"1\" min=\"%YMIN\""
        " max=\"%YMAX\" dims=\"%NDIM\" range=\"1\"></%YVL>\n"
      "%PRE<%ZVL long_name=\"%ZLA\" axis=\"3\" primary=\"1\" min=\"%ZMIN\""
        " max=\"%ZMAX\" dims=\"%PDIM\" range=\"1\"></%ZVL>\n"
      "%PRE<data long_name=\"%TITL\" signal=\"1\"  axis=\"[%XVL,%YVL,%ZVL]\" file_name=\"%FIL\">\n",
    "%PRE</data>\n",
    "%PRE<errors>\n", "%PRE</errors>\n",
    "%PRE<monitor>\n", "%PRE</monitor>\n"},
  { "HTML", "html",
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD %DAT//EN\"\n"
      "\"http://www.w3.org/TR/html4/strict.dtd\">\n"
      "<HTML><HEAD><META name=\"Author\" content=\"%PAR\">\n"
      "<META name=\"Creator\" content=\"%PAR (%SRC) McStas " MCSTAS_VERSION " [www.mcstas.org] simulation\">\n"
      "<META name=\"Date\" content=\"%DAT\">\n"
      "<TITLE>[McStas %PAR (%SRC)]%FIL</TITLE></HEAD>\n"
      "<BODY><center><h1><a name=\"%PAR\">"
        "McStas simulation %SRC (%SRC): Result file %FIL.html</a></h1></center><br>\n"
        "This simulation report was automatically created by"
        " <a href=\"http://www.mcstas.org/\"><i>McStas " MCSTAS_VERSION "</i></a><br>\n"
        "<pre>User:   %USR<br>\n"
        "%PRECreator: <a href=\"%SRC\">%SRC</a> %PAR McStas simulation<br>\n"
        "%PREFormat:  %FMT<br>\n"
        "%PREDate:    (%DATL) %DAT<br></pre>\n"
        "VRML viewers may be obtained at <a href=\"http://cic.nist.gov/vrml/vbdetect.html\">http://cic.nist.gov/vrml/vbdetect.html</a>\n",
    "<b>EndDate: </b>(%DATL) %DAT<br></BODY></HTML>\n",
    "%PRE<h%LVL><a name=\"%NAM\">%TYP %NAM</a></h%LVL> "
      "[child of <a href=\"#%PAR\">%PAR</a>]<br>\n",
    "[end of <a href=\"#%NAM\">%TYP %NAM</a>]<br>\n",
    "%PRE<b>%NAM: </b>%VAL<br>\n",
    "%PRE<b>DATA</b><br><center><embed src=\"%FIL\" type=\"model/vrml\" width=\"75%%\" height=\"50%%\"></embed><br>File <a href=\"%FIL\">%FIL [VRML format]</a></center><br>\n", "%PREEnd of DATA<br>\n",
    "%PRE<b>ERRORS</b><br>\n","%PREEnd of ERRORS<br>\n",
    "%PRE<b>EVENTS</b><br>\n", "%PREEnd of EVENTS<br>\n"},
  { "Octave", "m",
    "function mc_%VPA = get_%VPA(p)\n"
      "%% %FMT function issued from McStas on %DAT\n"
      "%% McStas simulation %SRC: %FIL\n"
      "%% Import data using octave> s=%VPA(); and plot with s=%VPA('plot');\n"
      "if nargin > 0, p=1; else p=0; end\n"
      "mc_%VPA.Format ='%FMT';\n"
      "mc_%VPA.URL    ='http://www.mcstas.org';\n"
      "mc_%VPA.Editor ='%USR';\n"
      "mc_%VPA.Creator='%SRC McStas " MCSTAS_VERSION " simulation';\n"
      "mc_%VPA.Date   =%DATL; %% for datestr\n"
      "mc_%VPA.File   ='%FIL';\n",
    "mc_%VPA.EndDate=%DATL; %% for datestr\nendfunction\n"
      "if exist('mcload_inline'), return; end\n"
      "function d=mcload_inline(d)\n"
      "%% local inline function to load data\n"
      "S=d.type; eval(['S=[ ' S(10:(length(S)-1)) ' ];']);\n"
      "if isempty(d.data)\n"
      " if ~length(findstr(d.format, 'binary'))\n"
      "  source(d.filename);p=d.parent;\n"
      "  eval(['d=get_',d.func,';']);d.parent=p;\n"
      " else\n"
      "  if length(findstr(d.format, 'float')), t='float';\n"
      "  elseif length(findstr(d.format, 'double')), t='double';\n"
      "  else return; end\n"
      "  if length(S) == 1, S=[S 1]; end\n"
      "  fid=fopen(d.filename, 'r');\n"
      "  pS = prod(S);\n"
      "  x = fread(fid, 3*pS, t);\n"
      "  d.data  =reshape(x(1:pS), S);\n"
      "  if prod(size(x)) >= 3*pS,\n"
      "  d.errors=reshape(x((pS+1):(2*pS)), S);\n"
      "  d.events=reshape(x((2*pS+1):(3*pS)), S);end\n"
      "  fclose(fid);\n"
      "  return\n"
      " end\n"
      "end\n"
      "return;\nendfunction\n\n"
      "function d=mcplot_inline(d,p)\n"
      "%% local inline function to plot data\n"
      "if isempty(findstr(d.type,'0d')), d=mcload_inline(d); end\nif ~p, return; end;\n"
      "eval(['l=[',d.xylimits,'];']); S=size(d.data);\n"
      "t1=['[',d.parent,'] ',d.filename,': ',d.title];t = strcat(t1,['  ',d.variables,'=[',d.values,']'],['  ',d.signal],['  ',d.statistics]);\n"
      "disp(t);\n"
      "if ~isempty(findstr(d.type,'0d')), return; end\n"
      "xlabel(d.xlabel); ylabel(d.ylabel); title(t);"
      "figure; if ~isempty(findstr(d.type,'2d'))\n"
      " if S(2) > 1, d.stepx=abs(l(1)-l(2))/(S(2)-1); else d.stepx=0; end\n"
      " if S(1) > 1, d.stepy=abs(l(3)-l(4))/(S(1)-1); else d.stepy=0; end\n"
      " d.x=linspace(l(1)+d.stepx/2,l(2)-d.stepx/2,S(2));\n"
      " d.y=linspace(l(3)+d.stepy/2,l(4)-d.stepy/2,S(1));\n"
      " mesh(d.x,d.y,d.data);\n"
      "else\n"
      " if max(S) > 1, d.stepx=abs(l(1)-l(2))/(max(S)-1); else d.stepx=0; end\n"
      " d.x=linspace(l(1)+d.stepx/2,l(2)-d.stepx/2,max(S));\n"
      " plot(d.x,d.data);\n"
      "end\nendfunction\n",
    "%% Section %TYP [%NAM] (level %LVL)\n"
      "mc_%VNA.class = '%TYP';",
    "mc_%VPA.mc_%VNA = mc_%VNA;\n",
    "%PREmc_%SEC.%NAM = '%VAL';\n",
    "%PREmc_%VPA.func='%VPA';\n%PREmc_%VPA.data = [ \n",
    " ]; %% end of data\nif length(mc_%VPA.data) == 0, single_file=0; else single_file=1; end\nmc_%VPA=mcplot_inline(mc_%VPA,p);\n",
    "%PREerrors = [ \n",
    " ]; %% end of errors\nif single_file, mc_%VPA.errors=errors; end\n",
    "%PREevents = [ \n",
    " ]; %% end of events\nif single_file, mc_%VPA.events=events; end\n"},
  { "VRML", "wrl",
    "#VRML V2.0 utf8\n%PREFormat: %FMT file\n"
      "%PREuse freeWRL, openvrml, vrmlview, CosmoPlayer, Cortona, Octaga... to view file\n"
      "WorldInfo {\n"
      "title \"%SRC/%FIL simulation Data\"\n"
      "info [ \"URL:    http://www.mcstas.org/\"\n"
      "       \"Editor: %USR\"\n"
      "       \"Creator:%SRC simulation (McStas)\"\n"
      "       \"Date:   Simulation started (%DATL) %DAT\"\n"
      "       \"File:   %FIL\" ]\n}\n"
      "Background { skyAngle [ 1.57 1.57] skyColor [0 0 1, 1 1 1, 0.1 0 0] }\n",
    "%PREEndDate:%DAT\n",
    "%PREbegin %TYP %PAR\n",
    "%PREend %TYP %PAR\n",
    "%PRE%SEC.%NAM= '%VAL'\n",
    "%PREThe Proto that contains data values and objects to plot these\n"
      "PROTO I_ERR_N_%VPA [\n"
      "%PREthe PROTO parameters\n"
      "  field MFFloat Data [ ]\n"
      "  field MFFloat Errors [ ]\n"
      "  field MFFloat Ncounts [ ]\n"
      "] { %PREThe plotting objects/methods in the Proto\n"
      "  %PREdraw a small sphere at the origin\n"
      "  DEF Data_%VPA Group {\n"
      "  children [\n"
      "    DEF CoordinateOrigin Group {\n"
      "      children [\n"
      "        Transform { translation  0 0 0 }\n"
      "        Shape { \n"
      "          appearance Appearance { \n"
      "            material Material {\n"
      "              diffuseColor 1.0 1.0 0.0\n"
      "              transparency 0.5 } }\n"
      "          geometry Sphere { radius .01 } \n"
      "    } ] }\n"
      "    %PREdefintion of the arrow allong Y axis\n"
      "    DEF ARROW Group {\n"
      "      children [\n"
      "        Transform {\n"
      "          translation 0 0.5 0\n"
      "          children [\n"
      "            Shape {\n"
      "              appearance DEF ARROW_APPEARANCE Appearance {\n"
      "                material Material {\n"
      "                  diffuseColor .3 .3 1\n"
      "                  emissiveColor .1 .1 .33\n"
      "                }\n"
      "              }\n"
      "              geometry Cylinder {\n"
      "                bottom FALSE\n"
      "                radius .005\n"
      "                height 1\n"
      "                top FALSE\n"
      "        } } ] }\n"
      "        Transform {\n"
      "          translation 0 1 0\n"
      "          children [\n"
      "            DEF ARROW_POINTER Shape {\n"
      "              geometry Cone {\n"
      "                bottomRadius .05\n"
      "                height .1\n"
      "              }\n"
      "              appearance USE ARROW_APPEARANCE\n"
      "    } ] } ] }\n"
      "    %PREthe arrow along X axis\n"
      "    Transform {\n"
      "      translation 0 0 0\n"
      "      rotation 1 0 0 1.57\n"
      "      children [\n"
      "        Group {\n"
      "          children [ \n"
      "            USE ARROW\n"
      "    ] } ] }\n"
      "    %PREthe arrow along Z axis\n"
      "    Transform {\n"
      "      translation 0 0 0\n"
      "      rotation 0 0 1 -1.57\n"
      "      children [\n"
      "        Group {\n"
      "          children [ \n"
      "            USE ARROW\n"
      "    ] } ] }\n"
      "    %PREthe Y label (which is vertical)\n"
      "    DEF Y_Label Group {\n"
      "      children [\n"
      "        Transform {\n"
      "          translation 0 1 0\n"
      "          children [\n"
      "            Billboard {\n"
      "              children [\n"
      "                Shape {\n"
      "                  appearance DEF LABEL_APPEARANCE Appearance {\n"
      "                    material Material {\n"
      "                      diffuseColor 1 1 .3\n"
      "                      emissiveColor .33 .33 .1\n"
      "                    } }\n"
      "                  geometry Text { \n"
      "                    string [ \"%ZVAR: %ZLA\", \"%ZMIN:%ZMAX - %PDIM points\" ] \n"
      "                    fontStyle FontStyle {  size .2 }\n"
      "    } } ] } ] } ] }\n"
      "    %PREthe X label\n"
      "    DEF X_Label Group {\n"
      "      children [\n"
      "        Transform {\n"
      "          translation 1 0 0\n"
      "          children [\n"
      "            Billboard {\n"
      "              children [\n"
      "                Shape {\n"
      "                  appearance DEF LABEL_APPEARANCE Appearance {\n"
      "                    material Material {\n"
      "                      diffuseColor 1 1 .3\n"
      "                      emissiveColor .33 .33 .1\n"
      "                    } }\n"
      "                  geometry Text { \n"
      "                    string [ \"%XVAR: %XLA\", \"%XMIN:%XMAX - %MDIM points\" ] \n"
      "                    fontStyle FontStyle {  size .2 }\n"
      "    } } ] } ] } ] }\n"
      "    %PREthe Z label\n"
      "    DEF Z_Label Group {\n"
      "      children [\n"
      "        Transform {\n"
      "          translation 0 0 1\n"
      "          children [\n"
      "            Billboard {\n"
      "              children [\n"
      "                Shape {\n"
      "                  appearance DEF LABEL_APPEARANCE Appearance {\n"
      "                    material Material {\n"
      "                      diffuseColor 1 1 .3\n"
      "                      emissiveColor .33 .33 .1\n"
      "                    } }\n"
      "                  geometry Text { \n"
      "                    string [ \"%YVAR: %YLA\", \"%YMIN:%YMAX - %NDIM points\" ] \n"
      "                    fontStyle FontStyle {  size .2 }\n"
      "    } } ] } ] } ] }\n"
      "    %PREThe text information (header data )\n"
      "    DEF Header Group {\n"
      "      children [\n"
      "        Transform {\n"
      "          translation 0 2 0\n"
      "          children [\n"
      "            Billboard {\n"
      "              children [\n"
      "                Shape {\n"
      "                  appearance Appearance {\n"
      "                    material Material { \n"
      "                      diffuseColor .9 0 0\n"
      "                      emissiveColor .9 0 0 }\n"
      "                  }\n"
      "                  geometry Text {\n"
      "                    string [ \"%PAR/%FIL\",\"%TITL\" ]\n"
      "                    fontStyle FontStyle {\n"
      "                        style \"BOLD\"\n"
      "                        size .2\n"
      "    } } } ] } ] } ] }\n"
      "    %PREThe Data plot\n"
      "    DEF MonitorData Group {\n"
      "      children [\n"
      "        DEF TransformData Transform {\n"
      "          children [\n"
      "            Shape {\n"
      "              appearance Appearance {\n"
      "                material Material { emissiveColor 0 0.2 0 }\n"
      "              }\n"
      "              geometry ElevationGrid {\n"
      "                xDimension  %MDIM\n"
      "                zDimension  %NDIM\n"
      "                xSpacing    1\n"
      "                zSpacing    1\n"
      "                solid       FALSE\n"
      "                height IS Data\n"
      "    } } ] } ] }\n"
      "    %PREThe VRMLScript that redimension x and z axis within 0:1\n"
      "    %PREand re-scale data within 0:1\n"
      "    DEF GetScale Script {\n"
      "      eventOut SFVec3f scale_vect\n"
      "      url \"javascript: \n"
      "        function initialize( ) {\n"
      "          scale_vect = new SFVec3f(1.0/%MDIM, 1.0/Math.abs(%ZMAX-%ZMIN), 1.0/%NDIM); }\" }\n"
      "  ] }\n"
      "ROUTE GetScale.scale_vect TO TransformData.scale\n} # end of PROTO\n"
      "%PREnow we call the proto with Data values\n"
      "I_ERR_N_%VPA {\nData [\n",
    "] # End of Data\n",
    "Errors [\n",
    "] # End of Errors\n",
    "Ncounts [\n",
    "] # End of Ncounts\n}" }
#ifdef USE_NEXUS
    ,
    { "NeXus", "nxs",
    "%PREFormat: %FMT file. Use hdfview to view.\n"
      "%PREURL:    http://www.mcstas.org/\n"
      "%PREEditor: %USR\n"
      "%PRECreator:%SRC simulation (McStas " MCSTAS_VERSION ")\n"
      "%PREDate:   Simulation started (%DATL) %DAT\n"
      "%PREFile:   %FIL\n",
    "%PREEndDate:%DAT\n",
    "%PREbegin %TYP\n",
    "%PREend %TYP\n",
    "%PRE%NAM: %VAL\n",
    "", "",
    "%PREErrors [%PAR/%FIL]: \n", "",
    "%PREEvents [%PAR/%FIL]: \n", "" }
#endif
};

/* file i/o handling ======================================================== */

/*******************************************************************************
* mcfull_file: allocates a full file name=mcdirname+file
*******************************************************************************/
char *mcfull_file(char *name, char *ext)
{
  int dirlen;
  char *mem;
  dirlen = mcdirname ? strlen(mcdirname) : 0;
  mem = (char *)malloc(dirlen + strlen(name) + 256);
  if(!mem)
  {
    exit(fprintf(stderr, "Error: Out of memory %li (mcfull_file)\n", (long)(dirlen + strlen(name) + 256)));
  }
  strcpy(mem, "");
  if(dirlen)
  {
    strcat(mem, mcdirname);
    if(mcdirname[dirlen - 1] != MC_PATHSEP_C &&
       name[0] != MC_PATHSEP_C)
      strcat(mem, MC_PATHSEP_S);
  }
  strcat(mem, name);
  if (!strchr(name, '.') && ext)
  { /* add extension if not in file name already */
    strcat(mem, ".");
    strcat(mem, ext);
  }
  return(mem);
}

/*******************************************************************************
* mcnew_file: opens a new file within mcdirname if non NULL
*             if mode is non 0, then mode is used, else mode is 'w'
*******************************************************************************/
FILE *mcnew_file(char *name, char *ext, char *mode)
{
  char *mem;
  FILE *file;

  if (!name || strlen(name) == 0) return(NULL);

  mem = mcfull_file(name, ext);
  file = fopen(mem, (mode ? mode : "w"));
  if(!file)
    fprintf(stderr, "Warning: could not open output file '%s' in mode '%s' (mcnew_file)\n", mem, mode);
  else {
    if (!mcopenedfiles || 
        (mcopenedfiles && mcopenedfiles_size <= strlen(mcopenedfiles)+strlen(mem))) {
      mcopenedfiles_size+=CHAR_BUF_LENGTH;
      if (!mcopenedfiles || !strlen(mcopenedfiles))
        mcopenedfiles = (char *)calloc(1, mcopenedfiles_size);
      else
        mcopenedfiles = (char *)realloc(mcopenedfiles, mcopenedfiles_size);
    } 
    strcat(mcopenedfiles, " ");
    strcat(mcopenedfiles, mem);
  }
  free(mem);
  
  return file;
} /* mcnew_file */

/*******************************************************************************
* str_rep: Replaces a token by an other (of SAME length) in a string
* This function modifies 'string'
*******************************************************************************/
char *str_rep(char *string, char *from, char *to)
{
  char *p;

  if (!string || !strlen(string)) return(string);
  if (strlen(from) != strlen(to)) return(string);

  p   = string;

  while (( p = strstr(p, from) ) != NULL) {
    long index;
    for (index=0; index<strlen(to); index++) p[index]=to[index];
  }
  return(string);
}

#define VALID_NAME_LENGTH 64
/*******************************************************************************
* mcvalid_name: makes a valid string for variable names.
*   copy 'original' into 'valid', replacing invalid characters by '_'
*   char arrays must be pre-allocated. n can be 0, or the maximum number of
*   chars to be copied/checked
*******************************************************************************/
static char *mcvalid_name(char *valid, char *original, int n)
{
  long i;


  if (original == NULL || strlen(original) == 0)
  { strcpy(valid, "noname"); return(valid); }
  if (n <= 0) n = strlen(valid);

  if (n > strlen(original)) n = strlen(original);
  else original += strlen(original)-n;
  strncpy(valid, original, n);

  for (i=0; i < n; i++)
  {
    if ( (valid[i] > 122)
      || (valid[i] < 32)
      || (strchr("!\"#$%&'()*+,-.:;<=>?@[\\]^`/ \n\r\t", valid[i]) != NULL) )
    {
      if (i) valid[i] = '_'; else valid[i] = 'm';
    }
  }
  valid[i] = '\0';

  return(valid);
} /* mcvalid_name */

/*******************************************************************************
* pfprintf: just as fprintf with positional arguments %N$t, 
*   but with (char *)fmt_args being the list of arg type 't'.
*   Needed as the vfprintf is not correctly handled on some platforms.
*   1- look for the maximum %d$ field in fmt
*   2- look for all %d$ fields up to max in fmt and set their type (next alpha)
*   3- retrieve va_arg up to max, and save pointer to arg in local arg array
*   4- use strchr to split around '%' chars, until all pieces are written
*   returns number of arguments written.
* Warning: this function is restricted to only handles types t=s,g,i,li
*          without additional field formating, e.g. %N$t
*******************************************************************************/
static int pfprintf(FILE *f, char *fmt, char *fmt_args, ...)
{
  #define MyNL_ARGMAX 50
  char  *fmt_pos;

  char *arg_char[MyNL_ARGMAX];
  int   arg_int[MyNL_ARGMAX];
  long  arg_long[MyNL_ARGMAX];
  double arg_double[MyNL_ARGMAX];

  char *arg_posB[MyNL_ARGMAX];  /* position of '%' */
  char *arg_posE[MyNL_ARGMAX];  /* position of '$' */
  char *arg_posT[MyNL_ARGMAX];  /* position of type */

  int   arg_num[MyNL_ARGMAX];   /* number of argument (between % and $) */
  int   this_arg=0;
  int   arg_max=0;
  va_list ap;

  if (!f || !fmt_args || !fmt) return(-1);
  for (this_arg=0; this_arg<MyNL_ARGMAX;  arg_num[this_arg++] =0); this_arg = 0;
  fmt_pos = fmt;
  while(1)  /* analyse the format string 'fmt' */
  {
    char *tmp;

    arg_posB[this_arg] = (char *)strchr(fmt_pos, '%');
    tmp = arg_posB[this_arg];
    if (tmp)	/* found a percent */
    {
      char  printf_formats[]="dliouxXeEfgGcs\0";
      arg_posE[this_arg] = (char *)strchr(tmp, '$');
      if (arg_posE[this_arg] && isdigit(tmp[1]))
      { /* found a dollar following a percent  and a digit after percent */
        char  this_arg_chr[10];
        

        /* extract positional argument index %*$ in fmt */
        strncpy(this_arg_chr, arg_posB[this_arg]+1, arg_posE[this_arg]-arg_posB[this_arg]-1 < 10 ? arg_posE[this_arg]-arg_posB[this_arg]-1 : 9);
        this_arg_chr[arg_posE[this_arg]-arg_posB[this_arg]-1] = '\0';
        arg_num[this_arg] = atoi(this_arg_chr);
        if (arg_num[this_arg] <=0 || arg_num[this_arg] >= MyNL_ARGMAX)
          return(-fprintf(stderr,"pfprintf: invalid positional argument number (%i is <=0 or >=%i) from '%s'.\n", arg_num[this_arg], MyNL_ARGMAX, this_arg_chr));
        /* get type of positional argument: follows '%' -> arg_posE[this_arg]+1 */
        fmt_pos = arg_posE[this_arg]+1;
        fmt_pos[0] = tolower(fmt_pos[0]);
        if (!strchr(printf_formats, fmt_pos[0]))
          return(-fprintf(stderr,"pfprintf: invalid positional argument type (%c != expected %c).\n", fmt_pos[0], fmt_args[arg_num[this_arg]-1]));
        if (fmt_pos[0] == 'l' && (fmt_pos[1] == 'i' || fmt_pos[1] == 'd')) fmt_pos++;
        arg_posT[this_arg] = fmt_pos;
        /* get next argument... */
        this_arg++;
      }
      else
      { /* no dollar or no digit */
        if  (tmp[1] == '%') {
          fmt_pos = arg_posB[this_arg]+2;  /* found %% */
        } else if (strchr(printf_formats,tmp[1])) {
          fmt_pos = arg_posB[this_arg]+1;  /* found %s */
        } else { 
          return(-fprintf(stderr,"pfprintf: must use only positional arguments (%s).\n", arg_posB[this_arg]));
        }
      }
    } else
      break;  /* no more % argument */
  }
  arg_max = this_arg;
  /* get arguments from va_arg list, according to their type */
  va_start(ap, fmt_args);
  for (this_arg=0; this_arg<strlen(fmt_args); this_arg++)
  {

    switch(tolower(fmt_args[this_arg]))
    {
      case 's':                       /* string */
              arg_char[this_arg] = va_arg(ap, char *);
              break;
      case 'd':
      case 'i':
      case 'c':                      /* int */
              arg_int[this_arg] = va_arg(ap, int);
              break;
      case 'l':                       /* long int */
              arg_long[this_arg] = va_arg(ap, long int);
              break;
      case 'f':
      case 'g':
      case 'e':                      /* double */
              arg_double[this_arg] = va_arg(ap, double);
              break;
      default: fprintf(stderr,"pfprintf: argument type is not implemented (arg %%%i$ type %c).\n", this_arg+1, fmt_args[this_arg]);
    }
  }
  va_end(ap);
  /* split fmt string into bits containing only 1 argument */
  fmt_pos = fmt;
  for (this_arg=0; this_arg<arg_max; this_arg++)
  {
    char *fmt_bit;
    int   arg_n;

    if (arg_posB[this_arg]-fmt_pos>0)
    {
      fmt_bit = (char*)malloc(arg_posB[this_arg]-fmt_pos+10);
      if (!fmt_bit) return(-fprintf(stderr,"pfprintf: not enough memory.\n"));
      strncpy(fmt_bit, fmt_pos, arg_posB[this_arg]-fmt_pos);
      fmt_bit[arg_posB[this_arg]-fmt_pos] = '\0';
      fprintf(f, "%s", fmt_bit); /* fmt part without argument */
    } else
    {
      fmt_bit = (char*)malloc(10);
      if (!fmt_bit) return(-fprintf(stderr,"pfprintf: not enough memory.\n"));
    }
    arg_n = arg_num[this_arg]-1; /* must be >= 0 */
    strcpy(fmt_bit, "%");
    strncat(fmt_bit, arg_posE[this_arg]+1, arg_posT[this_arg]-arg_posE[this_arg]);
    fmt_bit[arg_posT[this_arg]-arg_posE[this_arg]+1] = '\0';

    switch(tolower(fmt_args[arg_n]))
    {
      case 's': fprintf(f, fmt_bit, arg_char[arg_n]);
                break;
      case 'd':
      case 'i':
      case 'c':                      /* int */
              fprintf(f, fmt_bit, arg_int[arg_n]);
              break;
      case 'l':                       /* long */
              fprintf(f, fmt_bit, arg_long[arg_n]);
              break;
      case 'f':
      case 'g':
      case 'e':                       /* double */
              fprintf(f, fmt_bit, arg_double[arg_n]);
              break;
    }
    fmt_pos = arg_posT[this_arg]+1;
    if (this_arg == arg_max-1)
    { /* add eventual leading characters for last parameter */
      if (fmt_pos < fmt+strlen(fmt))
        fprintf(f, "%s", fmt_pos);
    }
    if (fmt_bit) free(fmt_bit);

  }
  return(this_arg);
} /* pfprintf */

/*******************************************************************************
* mcfile_header: output header/footer using specific file format.
*   outputs, in file 'name' having preallocated 'f' handle, the format Header
*   'part' may be 'header' or 'footer' depending on part to write
*   if name == NULL, ignore function (no header/footer output)
*******************************************************************************/
static int mcfile_header(FILE *f, struct mcformats_struct format, char *part, char *pre, char *name, char *parent)
{
  char user[64];
  char date[64];
  char *HeadFoot;
  long date_l; /* date as a long number */
  time_t t;
  char valid_parent[256];
  char instrname[256];
  char file[256];

  if(!f)
    return (-1);

  time(&t);

  if (part && !strcmp(part,"footer"))
  {
    HeadFoot = format.Footer;
    date_l = (long)t;
  }
  else
  {
    HeadFoot = format.Header;
    date_l = mcstartdate;
  }
  t = (time_t)date_l;

  if (!strlen(HeadFoot) || (!name)) return (-1);

  sprintf(file,"%s",name);
  sprintf(user,"%s on %s",
        getenv("USER") ? getenv("USER") : "mcstas",
        getenv("HOST") ? getenv("HOST") : "localhost");
  if (strstr(format.Name, "HTML")) {
    sprintf(instrname,"%s", mcinstrument_source);
    mcvalid_name(valid_parent, mcinstrument_name, VALID_NAME_LENGTH);
  } else {
    sprintf(instrname,"%s (%s)", mcinstrument_name, mcinstrument_source);
    if (parent && strlen(parent)) mcvalid_name(valid_parent, parent, VALID_NAME_LENGTH);
    else strcpy(valid_parent, "root");
  }
  strncpy(date, ctime(&t), 64);
  if (strlen(date)) date[strlen(date)-1] = '\0';

#ifdef USE_NEXUS
  if (strstr(format.Name, "NeXus")) {
    if(mcnxfile_header(mcnxHandle, part,
    pre,                  /* %1$s  PRE  */
    instrname,            /* %2$s  SRC  */
    file,                 /* %3$s  FIL  */
    format.Name,          /* %4$s  FMT  */
    date,                 /* %5$s  DAT  */
    user,                 /* %6$s  USR  */
    valid_parent,         /* %7$s  PAR  */
    date_l) == NX_ERROR) {
      fprintf(stderr, "Error: writing NeXus header file %s (mcfile_header)\n", file);
      return(-1);
    } else return(1); }
  else
#endif
  return(pfprintf(f, HeadFoot, "sssssssl",
    pre,                  /* %1$s  PRE  */
    instrname,            /* %2$s  SRC  */
    file,                 /* %3$s  FIL  */
    format.Name,          /* %4$s  FMT  */
    date,                 /* %5$s  DAT  */
    user,                 /* %6$s  USR  */
    valid_parent,         /* %7$s  PAR  */
    date_l));             /* %8$li DATL */
} /* mcfile_header */

/*******************************************************************************
* mcfile_tag: output tag/value using specific file format.
*   outputs, in file with 'f' handle, a tag/value pair.
*   if name == NULL, ignore function (no section definition)
*******************************************************************************/
static int mcfile_tag(FILE *f, struct mcformats_struct format, char *pre, char *section, char *name, char *value)
{
  char valid_section[256];
  char valid_name[256];
  int i;

  if (!strlen(format.AssignTag) || (!name) || (!f)) return(-1);

  mcvalid_name(valid_section, section, VALID_NAME_LENGTH);
  mcvalid_name(valid_name, name, VALID_NAME_LENGTH);

  /* remove quote chars in values */
  if (strstr(format.Name, "Scilab") || strstr(format.Name, "Matlab") || strstr(format.Name, "IDL"))
    for(i = 0; i < strlen(value); i++) {
      if (value[i] == '"' || value[i] == '\'')   value[i] = ' ';
      if (value[i] == '\n'  || value[i] == '\r') value[i] = ';';
    }
#ifdef USE_NEXUS
  if (strstr(format.Name, "NeXus")) {
    if(mcnxfile_tag(mcnxHandle, pre, valid_section, name, value) == NX_ERROR) {
      fprintf(stderr, "Error: writing NeXus tag file %s/%s=%s (mcfile_tag)\n", section, name, value);
      return(-1);
    } else return(1); }
  else
#endif
  return(pfprintf(f, format.AssignTag, "ssss",
    pre,          /* %1$s PRE */
    valid_section,/* %2$s SEC */
    valid_name,   /* %3$s NAM */
    value));      /* %4$s VAL */
} /* mcfile_tag */

/*******************************************************************************
* mcfile_section: output section start/end using specific file format.
*   outputs, in file 'name' having preallocated 'f' handle, the format Section.
*   'part' may be 'begin' or 'end' depending on section part to write
*   'type' may be e.g. 'instrument','simulation','component','data'
*   if name == NULL, ignore function (no section definition)
*   the prefix 'pre' is automatically indented/un-indented (pre-allocated !)
*******************************************************************************/
static int mcfile_section(FILE *f, struct mcformats_struct format, char *part, char *pre, char *name, char *type, char *parent, int level)
{
  char *Section;
  char valid_name[256];
  char valid_parent[256];
  int  ret;

  if(!f && !strstr(format.Name, "NeXus")) return (-1);

  if (part && !strcmp(part,"end")) Section = format.EndSection;
  else Section = format.BeginSection;

  if (!strlen(Section) || (!name)) return (-1);

  if (strcmp(part,"header") && strstr(format.Name, "no header")) return (-1);
  if (strcmp(part,"footer") && strstr(format.Name, "no footer")) return (-1);

  mcvalid_name(valid_name, name, VALID_NAME_LENGTH);
  if (parent && strlen(parent)) mcvalid_name(valid_parent, parent, VALID_NAME_LENGTH);
  else strcpy(valid_parent, "root");

  if (!strcmp(part,"end") && pre)       /* un-indent */
  {
    if (strlen(pre) > 2) pre[strlen(pre)-2]='\0';
  }
#ifdef USE_NEXUS
  if (strstr(format.Name, "NeXus")) {
    if (mcnxfile_section(mcnxHandle,part,
      pre, type, name, valid_name, parent, valid_parent, level) == NX_ERROR) {
      fprintf(stderr, "Error: writing NeXus section %s/%s=NX%s (mcfile_section)\n", parent, name, type);
      ret=-1;
    } else ret=1; }
  else
#endif
  ret = pfprintf(f, Section, "ssssssi",
    pre,          /* %1$s  PRE  */
    type,         /* %2$s  TYP  */
    name,         /* %3$s  NAM  */
    valid_name,   /* %4$s  VNA  */
    parent,       /* %5$s  PAR  */
    valid_parent, /* %6$s  VPA  */
    level);       /* %7$i  LVL */

  if (!strcmp(part,"begin"))
  {
    if (pre) strcat(pre,"  ");  /* indent */
    if (name && strlen(name))
      mcfile_tag(f, format, pre, name, "name", name);
    if (parent && strlen(parent))
      mcfile_tag(f, format, pre, name, "parent", parent);
  }

  return(ret);
} /* mcfile_section */


/*******************************************************************************
* mcinfo_instrument: output instrument info
*******************************************************************************/
static void mcinfo_instrument(FILE *f, struct mcformats_struct format,
  char *pre, char *name)
{
  char Value[1300] = "";
  int  i;

  if (!f) return;

  for(i = 0; i < mcnumipar; i++)
  {
    char ThisParam[256];
    if (strlen(mcinputtable[i].name) > 200) break;
    sprintf(ThisParam, " %s(%s)", mcinputtable[i].name,
            (*mcinputtypes[mcinputtable[i].type].parminfo)
                (mcinputtable[i].name));
    strcat(Value, ThisParam);
    if (strlen(Value) > CHAR_BUF_LENGTH) break;
  }

  mcfile_tag(f, format, pre, name, "Parameters", Value);
  mcfile_tag(f, format, pre, name, "Source", mcinstrument_source);
  mcfile_tag(f, format, pre, name, "Trace_enabled", (char *)(mctraceenabled ? "yes" : "no"));
  mcfile_tag(f, format, pre, name, "Default_main", (char *)(mcdefaultmain ? "yes" : "no"));
  mcfile_tag(f, format, pre, name, "Embedded_runtime",
#ifdef MC_EMBEDDED_RUNTIME
         "yes"
#else
         "no"
#endif
         );
} /* mcinfo_instrument */

/*******************************************************************************
* mcinfo_instrument: output simulation info
*******************************************************************************/
void mcinfo_simulation(FILE *f, struct mcformats_struct format,
  char *pre, char *name)
{
  int i;
  double run_num, ncount;
  char Value[256];

  if (!f) return;

  run_num = mcget_run_num();
  ncount  = 
#ifdef USE_MPI
    mpi_node_count * 
#endif
    mcget_ncount();
  
  if (run_num == 0 || run_num == ncount) sprintf(Value, "%g", ncount);
  else sprintf(Value, "%g/%g", run_num, ncount);
  mcfile_tag(f, format, pre, name, "Ncount", Value);
  mcfile_tag(f, format, pre, name, "Trace", (char *)(mcdotrace ? "yes" : "no"));
  mcfile_tag(f, format, pre, name, "Gravitation", (char *)(mcgravitation ? "yes" : "no"));
  if(mcseed)
  {
    sprintf(Value, "%ld", mcseed);
    mcfile_tag(f, format, pre, name, "Seed", Value);
  }
  if (strstr(format.Name, "McStas"))
  {
    for(i = 0; i < mcnumipar; i++)
    {
      if (mcrun_num || (mcinputtable[i].val && strlen(mcinputtable[i].val))) {
        if (mcinputtable[i].par == NULL) {
          strncpy(Value, (mcinputtable[i].val ? mcinputtable[i].val : ""), 256);
        } else (*mcinputtypes[mcinputtable[i].type].printer)(Value, mcinputtable[i].par);
        fprintf(f, "%sParam: %s=%s", pre, mcinputtable[i].name, Value);
        fprintf(f, "\n");
      }
    }
  }
  else
  {
    mcfile_section(f, format, "begin", pre, "parameters", "parameters", name, 3);
    for(i = 0; i < mcnumipar; i++)
    {
      if (mcinputtable[i].par == NULL)
        strncpy(Value, (mcinputtable[i].val ? mcinputtable[i].val : ""), 256);
      else (*mcinputtypes[mcinputtable[i].type].printer)(Value, mcinputtable[i].par);
      mcfile_tag(f, format, pre, "parameters", mcinputtable[i].name, Value);
    }
    mcfile_section(f, format, "end", pre, "parameters", "parameters", name, 3);
  }
} /* mcinfo_simulation */

/*******************************************************************************
* mcinfo_data: output data info, computes basic stats.
*******************************************************************************/
static void mcinfo_data(FILE *f, struct mcformats_struct format,
  char *pre, char *parent, char *title,
  int m, int n, int p,
  char *xlabel, char *ylabel, char *zlabel,
  char *xvar, char *yvar, char *zvar,
  double *x1, double *x2, double *y1, double *y2, double *z1, double *z2,
  char *filename,
  double *p0, double *p1, double *p2, char istransposed, Coords posa)
{
  char type[256];
  char stats[256];
  char vars[256];
  char signal[256];
  char values[256];
  char limits[256];
  char lim_field[10];
  char c[32];
  double run_num, ncount;
  char ratio[256];
  char pos[256];

  double sum_xz  = 0;
  double sum_yz  = 0;
  double sum_z   = 0;
  double sum_y   = 0;
  double sum_x   = 0;
  double sum_x2z = 0;
  double sum_y2z = 0;
  double min_z   = 0;
  double max_z   = 0;
  double fmon_x=0, smon_x=0, fmon_y=0, smon_y=0, mean_z=0;
  double Nsum=0;
  double P2sum=0;

  int    i,j;

  if (!f || m*n*p == 0) return;

  if (p1)
  {
    min_z   = p1[0];
    max_z   = min_z;
    for(j = 0; j < n*p; j++)
    {
      for(i = 0; i < m; i++)
      {
        double x,y,z;
        double N, E;
        long index;

        if (!istransposed) index = i*n*p + j;
        else index = i+j*m;
        if (p0) N = p0[index];
        if (p2) E = p2[index];

        if (m) x = *x1 + (i + 0.5)/m*(*x2 - *x1); else x = 0;
        if (n && p) y = *y1 + (j + 0.5)/n/p*(*y2 - *y1); else y = 0;
        z = p1[index];
        sum_xz += x*z;
        sum_yz += y*z;
        sum_x += x;
        sum_y += y;
        sum_z += z;
        sum_x2z += x*x*z;
        sum_y2z += y*y*z;
        if (z > max_z) max_z = z;
        if (z < min_z) min_z = z;

        Nsum += p0 ? N : 1;
        P2sum += p2 ? E : z*z;
      }
    }
    if (sum_z && n*m*p)
    {
      fmon_x = sum_xz/sum_z;
      fmon_y = sum_yz/sum_z;
      smon_x = sqrt(sum_x2z/sum_z-fmon_x*fmon_x);
      smon_y = sqrt(sum_y2z/sum_z-fmon_y*fmon_y);
      mean_z = sum_z/n/m/p;
    }
  }

  if (abs(m*n*p) == 1)
  { strcpy(type, "array_0d"); strcpy(stats, ""); }
  else if (n == 1 || m == 1)
  { if (m == 1) {m = n; n = 1; }
    sprintf(type, "array_1d(%d)", m);
    sprintf(stats, "X0=%g; dX=%g;", fmon_x, smon_x); }
  else
  { if (strstr(format.Name," scan ")) sprintf(type, "multiarray_1d(%d)", n);
    else if (p == 1) sprintf(type, "array_2d(%d, %d)", m, n);
    else sprintf(type, "array_3d(%d, %d, %d)", m, n, p);
    sprintf(stats, "X0=%g; dX=%g; Y0=%g; dY=%g;", fmon_x, smon_x, fmon_y, smon_y); }
  strcpy(c, "I ");
  if (zvar && strlen(zvar)) strncpy(c, zvar,32);
  else if (yvar && strlen(yvar)) strncpy(c, yvar,32);
  else if (xvar && strlen(xvar)) strncpy(c, xvar,32);
  else strncpy(c, xvar,32);
  if (m == 1 || n == 1) sprintf(vars, "%s %s %s_err N", xvar, c, c);
  else sprintf(vars, "%s %s_err N", c, c);

  run_num = mcget_run_num();
  ncount  = 
#ifdef USE_MPI
    mpi_node_count * 
#endif
    mcget_ncount();
  sprintf(ratio, "%g/%g", run_num, ncount);

  mcfile_tag(f, format, pre, parent, "type", type);
  mcfile_tag(f, format, pre, parent, "Source", mcinstrument_source);
  if (parent) mcfile_tag(f, format, pre, parent, (char *)(strstr(format.Name,"McStas") ? "component" : "parent"), parent);
  sprintf(pos, "%g %g %g", posa.x, posa.y, posa.z);
  mcfile_tag(f, format, pre, parent, "position", pos);

  if (title) mcfile_tag(f, format, pre, parent, "title", title);
  mcfile_tag(f, format, pre, parent, "ratio", ratio);
  if (filename) {
    mcfile_tag(f, format, pre, parent, "filename", filename);
    mcfile_tag(f, format, pre, parent, "format", format.Name);
  } else mcfile_tag(f, format, pre, parent, "filename", "");

  if (p1)
  {
    if (n*m*p > 1)
    {
      sprintf(signal, "Min=%g; Max=%g; Mean= %g;", min_z, max_z, mean_z);
      if (m > 1 && n == 1 && p == 1) { *y1 = min_z; *y2 = max_z; *z1=*y1; *z2=*y2; }
      else if (m > 1 && n > 1 && p == 1) { *z1 = min_z; *z2 = max_z;}
    } else strcpy(signal, "");

    mcfile_tag(f, format, pre, parent, "statistics", stats);
    mcfile_tag(f, format, pre, parent,
      (char *)(strstr(format.Name, "NeXus") ? "Signal" : "signal"), signal);

    sprintf(values, "%g %g %g", sum_z, mcestimate_error(Nsum, sum_z, P2sum), Nsum);
    mcfile_tag(f, format, pre, parent, "values", values);
  }
  strcpy(lim_field, "xylimits");
  if (n*m > 1)
  {
    mcfile_tag(f, format, pre, parent, (char *)(strstr(format.Name," scan ") ? "xvars" : "xvar"), xvar);
    mcfile_tag(f, format, pre, parent, (char *)(strstr(format.Name," scan ") ? "yvars" : "yvar"), yvar);
    mcfile_tag(f, format, pre, parent, "xlabel", xlabel);
    mcfile_tag(f, format, pre, parent, "ylabel", ylabel);
    if ((n == 1 || m == 1 || strstr(format.Name," scan ")) && strstr(format.Name, "McStas"))
    {
      sprintf(limits, "%g %g", *x1, *x2);
      strcpy(lim_field, "xlimits");
    }
    else
    {
      if (!strstr(format.Name," scan ")) {
        mcfile_tag(f, format, pre, parent, "zvar", zvar);
        mcfile_tag(f, format, pre, parent, "zlabel", zlabel);
      }
      sprintf(limits, "%g %g %g %g %g %g", *x1, *x2, *y1, *y2, *z1, *z2);
    }
  } else strcpy(limits, "0 0 0 0 0 0");
  mcfile_tag(f, format, pre, parent, lim_field, limits);
  mcfile_tag(f, format, pre, parent, "variables", strstr(format.Name," scan ") ? zvar : vars);
  /* add warning in case of low statistics or large number of bins in text format mode */
  if (mcestimate_error(Nsum, sum_z, P2sum) > sum_z/4) fprintf(stderr,
    "Warning: file '%s': Low Statistics\n",
    filename);
  else
  if (n*m*p > 1000 && Nsum < n*m*p && Nsum) fprintf(stderr,
    "Warning: file '%s':\n"
    "         Low Statistics (%g events in %dx%dx%d bins).\n",
    filename, Nsum, m,n,p);
  if ( !strstr(format.Name, "binary")
    && (strstr(format.Name, "Scilab") || strstr(format.Name, "Matlab"))
    && (n*m*p > 10000 || m > 1000) ) fprintf(stderr,
      "Warning: file '%s' (%s format)\n"
      "         Large matrices (%dx%dx%d) in text mode may be\n"
      "         slow or fail at import. Prefer binary mode.\n",
      filename, format.Name, m,n,p);
   if (mcDetectorCustomHeader && strlen(mcDetectorCustomHeader)) {
     mcfile_tag(f, format, pre, parent, "custom", mcDetectorCustomHeader);
   }
} /* mcinfo_data */

/*******************************************************************************
* mcsiminfo_init: writes simulation structured description file (mcstas.sim)
*******************************************************************************/
void mcsiminfo_init(FILE *f)
{
#ifdef USE_MPI
  if(mpi_node_rank != mpi_node_root) return;
#endif
  if (mcdisable_output_files) return;
  if (!f && (!mcsiminfo_name || !strlen(mcsiminfo_name))) return;
  /* clear list of opened files to start new save session */
  if (mcopenedfiles && strlen(mcopenedfiles) > 0) strcpy(mcopenedfiles, "");
#ifdef USE_NEXUS
  /* NeXus sim info is the NeXus root file */
  if(strstr(mcformat.Name, "NeXus")) {
    if (mcnxfile_init(mcsiminfo_name, mcformat.Extension,
      strstr(mcformat.Name, "append") || strstr(mcformat.Name, "catenate") ? "a":"w",
      &mcnxHandle) == NX_ERROR) {
      exit(fprintf(stderr, "Error: opening NeXus file %s (mcsiminfo_init)\n", mcsiminfo_name));
    } else mcsiminfo_file = (FILE *)mcsiminfo_name;
  } else
#endif
  if (!f) mcsiminfo_file = mcnew_file(mcsiminfo_name, mcformat.Extension,
    (char *)(strstr(mcformat.Name, "append")
      || strstr(mcformat.Name, "catenate")  
      || (char *)(strstr(mcopenedfiles, mcsiminfo_name) ? "a":"w")));
  else mcsiminfo_file = f;
  if(!mcsiminfo_file)
    fprintf(stderr,
            "Warning: could not open simulation description file '%s' (mcsiminfo_init)\n",
            mcsiminfo_name);
  else
  {
    char *pre; /* allocate enough space for indentations */
    int  ismcstas_nx;
    char simname[CHAR_BUF_LENGTH];
    char root[10];

    pre = (char *)malloc(20);
    if (!pre) exit(fprintf(stderr, "Error: insufficient memory (mcsiminfo_init)\n"));
    strcpy(pre, (char *)(strstr(mcformat.Name, "VRML")
               || (char *)(strstr(mcformat.Name, "OpenGENIE") ? "# " : "  ")));


    ismcstas_nx = (strstr(mcformat.Name, "McStas") || strstr(mcformat.Name, "NeXus"));
    strcpy(root, strstr(mcformat.Name, "XML") ? "root" : "mcstas");
    sprintf(simname, "%s%s%s",
      mcdirname ? mcdirname : ".", MC_PATHSEP_S, mcsiminfo_name);

#ifdef USE_NEXUS
    if (strstr(mcformat.Name, "NeXus")) {
      /* NXentry class */
      char file_time[CHAR_BUF_LENGTH];
      sprintf(file_time, "%s_%li", mcinstrument_name, mcstartdate);
      mcfile_section(mcsiminfo_file, mcformat, "begin", pre, file_time, "entry", root, 1);
    }
#endif

    mcfile_header(mcsiminfo_file, mcformat, "header", pre, simname, root);
#ifdef USE_NEXUS
    if (strstr(mcformat.Name, "NeXus"))
    mcnxfile_section(mcnxHandle,"end_data", NULL, NULL, NULL, NULL, NULL, NULL, 0);
#endif
    /* begin-end instrument */
    mcfile_section(mcsiminfo_file, mcformat, "begin", pre, mcinstrument_name, "instrument", root, 1);
    mcinfo_instrument(mcsiminfo_file, mcformat, pre, mcinstrument_name);
#ifdef USE_NEXUS
    if (strstr(mcformat.Name, "NeXus")) {
      mcnxfile_section(mcnxHandle,"end_data", NULL, NULL, NULL, NULL, NULL, NULL, 0);
      mcnxfile_section(mcnxHandle,"instr_code",
        pre, "instrument", mcinstrument_source, NULL, mcinstrument_name, NULL, 0);
    }
#endif
    if (ismcstas_nx) mcfile_section(mcsiminfo_file, mcformat, "end", pre, mcinstrument_name, "instrument", root, 1);

    /* begin-end simulation */
    mcfile_section(mcsiminfo_file, mcformat, "begin", pre, simname, "simulation", mcinstrument_name, 2);
    mcinfo_simulation(mcsiminfo_file, mcformat, pre, simname);
#ifdef USE_NEXUS
    if (strstr(mcformat.Name, "NeXus"))
    mcnxfile_section(mcnxHandle,"end_data", NULL, NULL, NULL, NULL, NULL, NULL, 0);
#endif
    if (ismcstas_nx) mcfile_section(mcsiminfo_file, mcformat, "end", pre, simname, "simulation", mcinstrument_name, 2);

    free(pre);
  }
} /* mcsiminfo_init */

/*******************************************************************************
* mcsiminfo_close: close simulation file (mcstas.sim)
*******************************************************************************/
void mcsiminfo_close(void)
{
#ifdef USE_MPI
  if(mpi_node_rank != mpi_node_root) return;
#endif
  if (mcdisable_output_files) return;
  if(mcsiminfo_file)
  {
    int  ismcstas_nx;
    char simname[CHAR_BUF_LENGTH];
    char root[10];
    char *pre;

    pre = (char *)malloc(20);
    if (!pre) exit(fprintf(stderr, "Error: insufficient memory (mcsiminfo_close)\n"));
    strcpy(pre, strstr(mcformat.Name, "VRML")
               || strstr(mcformat.Name, "OpenGENIE") ? "# " : "  ");

    ismcstas_nx = (strstr(mcformat.Name, "McStas") || strstr(mcformat.Name, "NeXus"));
    strcpy(root, strstr(mcformat.Name, "XML") ? "root" : "mcstas");
    sprintf(simname, "%s%s%s",
      mcdirname ? mcdirname : ".", MC_PATHSEP_S, mcsiminfo_name);

    if (!ismcstas_nx)
    {
      mcfile_section(mcsiminfo_file, mcformat, "end", pre, simname, "simulation", mcinstrument_name, 2);
      mcfile_section(mcsiminfo_file, mcformat, "end", pre, mcinstrument_name, "instrument", root, 1);
    }
#ifdef USE_NEXUS
    if (strstr(mcformat.Name, "NeXus")) mcfile_section(mcsiminfo_file, mcformat, "end", pre, mcinstrument_name, "entry", root, 1);
#endif
    mcfile_header(mcsiminfo_file, mcformat, "footer", pre, simname, root);
#ifdef USE_NEXUS
    if (strstr(mcformat.Name, "NeXus")) mcnxfile_close(&mcnxHandle);
#endif
    if (mcsiminfo_file != stdout && mcsiminfo_file && !strstr(mcformat.Name, "NeXus")) fclose(mcsiminfo_file);
    mcsiminfo_file = NULL;

    free(pre);
  }
} /* mcsiminfo_close */

/*******************************************************************************
* mcfile_datablock: output a single data block using specific file format.
*   'part' can be 'data','errors','ncount'
*   if y1 == y2 == 0 and McStas format, then stores as a 1D array with [I,E,N]
*   return value: 0=0d/2d, 1=1d
*   when !single_file, create independent data files, with header and data tags
*   if one of the dimensions m,n,p is negative, the data matrix will be written
*   after transposition of m/x and n/y dimensions
*******************************************************************************/
static int mcfile_datablock(FILE *f, struct mcformats_struct format,
  char *pre, char *parent, char *part,
  double *p0, double *p1, double *p2, int m, int n, int p,
  char *xlabel, char *ylabel, char *zlabel, char *title,
  char *xvar, char *yvar, char *zvar,
  double *x1, double *x2, double *y1, double *y2, double *z1, double *z2,
  char *filename, char istransposed, Coords posa)
{
  char *Begin;
  char *End;
  char valid_xlabel[64];
  char valid_ylabel[64];
  char valid_zlabel[64];
  char valid_parent[64];
  FILE *datafile= NULL;
  int  isdata=0;
  int  just_header=0;
  int  i,j, is1d;
  double Nsum=0, Psum=0, P2sum=0;
  char sec[256];
  char isdata_present;
  char israw_data=0; /* raw data=(N,p,p2) instead of (N,P,sigma) */
  struct mcformats_struct dataformat;

  if (strstr(part,"data"))
  { isdata = 1; Begin = format.BeginData; End = format.EndData; }
  if (strstr(part,"errors"))
  { isdata = 2; Begin = format.BeginErrors; End = format.EndErrors; }
  if (strstr(part,"ncount"))
  { isdata = 0; Begin = format.BeginNcount; End = format.EndNcount; }
  if (strstr(part, "begin")) just_header = 1;
  if (strstr(part, "end"))   just_header = 2;

  isdata_present=((isdata==1 && p1) || (isdata==2 && p2) || (isdata==0 && p0));

  is1d = ((m==1 || n==1) && strstr(format.Name,"McStas"));
  mcvalid_name(valid_xlabel, xlabel, 64);
  mcvalid_name(valid_ylabel, ylabel, 64);
  mcvalid_name(valid_zlabel, zlabel, 64);

  if (strstr(format.Name, "McStas") || !filename || strlen(filename) == 0)
    mcvalid_name(valid_parent, parent, VALID_NAME_LENGTH);
  else mcvalid_name(valid_parent, filename, VALID_NAME_LENGTH);

#ifdef USE_NEXUS
  if (strstr(format.Name, "NeXus")) {
    /* istransposed==1 : end NeXus data only with last output slab */
    if (strstr(part,"data") && !strstr(format.Name,"no header")) { /* writes attributes in information SDS */
      mcinfo_data(mcsiminfo_file, format, pre, valid_parent, title, m, n, p,
                  xlabel, ylabel, zlabel, xvar, yvar, zvar,
                  x1, x2, y1, y2, z1, z2, filename, p0, p1, p2,
                  0, posa);
      mcnxfile_section(mcnxHandle,"end_data", NULL, filename, NULL, NULL, NULL, NULL, 0);
    }
    if(mcnxfile_datablock(mcnxHandle, part,
      format.Name, valid_parent, filename, xlabel, valid_xlabel, ylabel, valid_ylabel, zlabel, valid_zlabel, title, xvar, yvar, zvar, abs(m), abs(n), abs(p), *x1, *x2, *y1, *y2, *z1, *z2, p0, p1, p2) == NX_ERROR) {
      fprintf(stderr, "Error: writing NeXus data %s/%s (mcfile_datablock)\n", parent, filename);
    }
    return(0); }
#endif

  if (strstr(format.Name, " raw")) israw_data = 1;

  /* if normal or begin and part == data: output info_data (sim/data_file) */
  if (isdata == 1 && just_header != 2 && f)
  {
    if(!strstr(format.Name, "no header")) {
      mcinfo_data(f, format, pre, valid_parent, title, m, n, p,
                  xlabel, ylabel, zlabel, xvar, yvar, zvar,
                  x1, x2, y1, y2, z1, z2, filename, p0, p1, p2,
                  istransposed, posa);
    }
  }

  /* if normal or begin: begin part (sim/data file) */
  if (strlen(Begin) && just_header != 2 && f) {
    pfprintf(f, Begin, "sssssssssssssiiigggggg",
      pre,          /* %1$s   PRE  */
      valid_parent, /* %2$s   PAR  */
      filename,     /* %3$s   FIL  */
      xlabel,       /* %4$s   XLA  */
      valid_xlabel, /* %5$s   XVL  */
      ylabel,       /* %6$s   YLA  */
      valid_ylabel, /* %7$s   YVL  */
      zlabel,       /* %8$s   ZLA  */
      valid_zlabel, /* %9$s   ZVL  */
      title,        /* %10$s  TITL */
      xvar,         /* %11$s  XVAR */
      yvar,         /* %12$s  YVAR */
      zvar,         /* %13$s  ZVAR */
      m,            /* %14$i  MDIM */
      n,            /* %15$i  NDIM */
      p,            /* %16$i  PDIM */
      *x1,           /* %17$g  XMIN */
      *x2,           /* %18$g  XMAX */
      *y1,           /* %19$g  YMIN */
      *y2,           /* %20$g  YMAX */
      *z1,           /* %21$g  ZMIN */
      *z2);          /* %22$g  ZMAX */
  }
 /* if normal, and !single:
  *   open datafile,
  *   if !ascii_only
  *     if data: write file header,
  *     call datablock part+header(begin)
  * else data file = f
  */
  dataformat=format;
  if (!mcsingle_file && just_header == 0)
  {
    /* if data: open new file for data else append for error/ncount */
    if (filename) {
      char mode[10];

      strcpy(mode,
             (isdata != 1 || strstr(format.Name, "no header")
              || strstr(format.Name, "append") 
              || strstr(format.Name, "catenate") 
              || strstr(mcopenedfiles, filename) ?
             "a" : "w"));
      if (strstr(format.Name, "binary")) strcat(mode, "b");
      if (mcformat_data.Name) dataformat = mcformat_data;
      datafile = mcnew_file(filename, dataformat.Extension, mode);
    } else datafile = NULL;
    /* special case of IDL: can not have empty vectors. Init to 'external' */
    if (strstr(format.Name, "IDL") && f) fprintf(f, "'external'");
    /* if data, start with root header plus tags of parent data */
    if (datafile && !mcascii_only)
    {
      char *new_pre;
      char *mode;
      new_pre = (char *)malloc(20);
      mode    = (char *)malloc(20);
      if (!new_pre || !mode) exit(fprintf(stderr, "Error: insufficient memory (mcfile_datablock)\n"));
      strcpy(new_pre, (strstr(dataformat.Name, "McStas")
               || strstr(dataformat.Name, "VRML")
               || strstr(dataformat.Name, "OpenGENIE") ? "# " : ""));

      if (isdata == 1) {
        if(!strstr(format.Name, "no header"))
          {
            mcfile_header(datafile, dataformat, "header", new_pre,
                          filename, valid_parent);
            mcinfo_simulation(datafile, dataformat,
                              new_pre, valid_parent);
          }
      }
      sprintf(mode, "%s begin", part);
      /* write header+data block begin tags into datafile */
      mcfile_datablock(datafile, dataformat, new_pre,
          valid_parent, mode,
          p0, p1, p2, m, n, p,
          xlabel,  ylabel, zlabel, title,
          xvar, yvar, zvar,
          x1, x2, y1, y2, z1, z2, filename, istransposed, posa);
      free(mode); free(new_pre);
    }
  }
  else if (just_header == 0)
  {
    if (strstr(format.Name, "McStas") && abs(m*n*p)>1 && f)
    {
      if (is1d) sprintf(sec,"array_1d(%d)", m);
      else if (p==1) sprintf(sec,"array_2d(%d,%d)", m,n);
      else sprintf(sec,"array_3d(%d,%d,%d)", m,n,p);
      fprintf(f,"%sbegin %s\n", pre, sec);
      datafile = f; dataformat=format;
    }
    if (mcsingle_file) { datafile = f; dataformat=format; }
  }

  /* if normal: [data] in data file */
  /* do loops: 2 loops on m,n. */
  if (just_header == 0)
  {
    char eol_char[3];
    int  isIDL, isPython;
    int  isBinary=0;

    if (strstr(format.Name, "binary")) isBinary=1;
    if (strstr(format.Name, "float"))  isBinary=1;
    else if (strstr(format.Name, "double")) isBinary=2;
    isIDL    = (strstr(format.Name, "IDL") != NULL);
    isPython = (strstr(format.Name, "Python") != NULL);
    if (isIDL) strcpy(eol_char,"$\n"); else strcpy(eol_char,"\n");

    if (datafile && !isBinary)
    for(j = 0; j < n*p; j++)  /* loop on rows(y) */
    {
      for(i = 0; i < m; i++)  /* write all columns (x) */
      {
        double I=0, E=0, N=0;
        double value=0;
        long index;

        if (!istransposed) index = i*n*p + j;
        else index = i+j*m;
        if (p0) N = p0[index];
        if (p1) I = p1[index];
        if (p2) E = p2[index];

        Nsum += p0 ? N : 1;
        Psum += I;
        P2sum += p2 ? E : I*I;

        if (p0 && p1 && p2 && !israw_data) E = mcestimate_error(N,I,E);
        if(isdata_present)
        {
          if (is1d)
          {
            double x;
            x = *x1+(*x2-*x1)*(index+0.5)/(abs(m*n*p));
            if (abs(m*n*p) > 1) fprintf(datafile, "%g %g %g %g\n", x, I, E, N);
          }
          else
          {
            if (isdata == 1) value = I;
            else if (isdata == 0) value = N;
            else if (isdata == 2) value = E;
            fprintf(datafile, "%g", value);
            if ((isIDL || isPython) && ((i+1)*(j+1) < abs(m*n*p))) fprintf(datafile, ",");
            else fprintf(datafile, " ");
          }
        }
      }
      if (isdata_present && !is1d) fprintf(datafile, "%s", eol_char);
    } /* end 2 loops if not Binary */
    if (datafile && isBinary)
    {
      double *d=NULL;
      if (isdata==1) d=p1;
      else if (isdata==2) d=p2;
      else if (isdata==0) d=p0;

      if (d && isBinary == 1)  /* float */
      {
        float *s;
        s = (float*)malloc(abs(m*n*p)*sizeof(float));
        if (s)
        {
          long    i, count;
          for (i=0; i<abs(m*n*p); i++)
            { if (isdata != 2 || israw_data) s[i] = (float)d[i];
              else s[i] = (float)mcestimate_error(p0[i],p1[i],p2[i]); }
            count = fwrite(s, sizeof(float), abs(m*n*p), datafile);
          if (count != abs(m*n*p)) fprintf(stderr, "McStas: error writing float binary file '%s' (%li instead of %li, mcfile_datablock)\n", filename,count, (long)abs(m*n*p));
          free(s);
        } else fprintf(stderr, "McStas: Out of memory for writing %li float binary file '%s' (mcfile_datablock)\n", (long)abs(m*n*p)*sizeof(float), filename);
      }
      else if (d && isBinary == 2)  /* double */
      {
        long count;
        double *s=NULL;
        if (isdata == 2 && !israw_data)
        {
          s = (double*)malloc(abs(m*n*p)*sizeof(double));
          if (s) { long i;
            for (i=0; i<abs(m*n*p); i++)
              s[i] = (double)mcestimate_error(p0[i],p1[i],p2[i]);
            d = s;
          }
          else fprintf(stderr, "McStas: Out of memory for writing %li 'errors' part of double binary file '%s' (mcfile_datablock)\n", (long)abs(m*n*p)*sizeof(double), filename);
        }
        count = fwrite(d, sizeof(double), abs(m*n*p), datafile);
        if (isdata == 2 && s) free(s);
        if (count != abs(m*n*p)) fprintf(stderr, "McStas: error writing double binary file '%s' (%li instead of %li, mcfile_datablock)\n", filename,count, (long)abs(m*n*p));
      }
    } /* end if Binary */
  }
  if (strstr(format.Name, "McStas") || !filename || strlen(filename) == 0)
    mcvalid_name(valid_parent, parent, VALID_NAME_LENGTH);
  else mcvalid_name(valid_parent, filename, VALID_NAME_LENGTH);
  /* if normal or end: end_data */
  if (strlen(End) && just_header != 1 && f) {
    pfprintf(f, End, "sssssssssssssiiigggggg",
      pre,          /* %1$s   PRE  */
      valid_parent, /* %2$s   PAR  */
      filename,     /* %3$s   FIL  */
      xlabel,       /* %4$s   XLA  */
      valid_xlabel, /* %5$s   XVL  */
      ylabel,       /* %6$s   YLA  */
      valid_ylabel, /* %7$s   YVL  */
      zlabel,       /* %8$s   ZLA  */
      valid_zlabel, /* %9$s   ZVL  */
      title,        /* %10$s  TITL */
      xvar,         /* %11$s  XVAR */
      yvar,         /* %12$s  YVAR */
      zvar,         /* %13$s  ZVAR */
      m,            /* %14$i  MDIM */
      n,            /* %15$i  NDIM */
      p,            /* %16$i  PDIM */
      *x1,           /* %17$g  XMIN */
      *x2,           /* %18$g  XMAX */
      *y1,           /* %19$g  YMIN */
      *y2,           /* %20$g  YMAX */
      *z1,           /* %21$g  ZMIN */
      *z2);          /* %22$g  ZMAX */
  }

 /* if normal and !single and datafile:
  *   datablock part+footer
  *   write file footer
  *   close datafile
  */
  if (!mcsingle_file && just_header == 0)
  {
    char *mode;
    char *new_pre;

    new_pre = (char *)malloc(20);
    mode    = (char *)malloc(20);
    if (!new_pre || !mode) exit(fprintf(stderr, "Error: insufficient memory (mcfile_datablock)\n"));

    strcpy(new_pre, (strstr(dataformat.Name, "McStas")
               || strstr(dataformat.Name, "VRML")
               || strstr(dataformat.Name, "OpenGENIE") ? "# " : ""));

    if (datafile && datafile != f && !mcascii_only)
    {
      sprintf(mode, "%s end", part);
      /* write header+data block end tags into datafile */
      mcfile_datablock(datafile, dataformat, new_pre,
          valid_parent, mode,
          p0, p1, p2, m, n, p,
          xlabel,  ylabel, zlabel, title,
          xvar, yvar, zvar,
          x1, x2, y1, y2, z1, z2, filename, istransposed, posa);
      if ((isdata == 1 && is1d) || strstr(part,"ncount") || !p0 || !p2) /* either ncount, or 1d */
        if(!strstr(format.Name, "no footer"))
          mcfile_header(datafile, dataformat, "footer", new_pre,
                        filename, valid_parent);
    }
    if (datafile && datafile != f) fclose(datafile);
    free(mode); free(new_pre);
  }
  else
  {
    if (strstr(format.Name, "McStas") && just_header == 0 && abs(m*n*p) > 1)
      fprintf(f,"%send %s\n", pre, sec);
  }

  /* set return value */
  return(is1d);
} /* mcfile_datablock */

/*******************************************************************************
* mcfile_data: output data/errors/ncounts using specified file format.
*   if McStas 1D then data is stored. f is the simfile handle or NULL.
*   as a long 1D array [p0, p1, p2] to reorder -> don't output err/ncount again.
*   if p1 or p2 is NULL then skip that part.
*******************************************************************************/
static int mcfile_data(FILE *f, struct mcformats_struct format,
  char *pre, char *parent,
  double *p0, double *p1, double *p2, int m, int n, int p,
  char *xlabel, char *ylabel, char *zlabel, char *title,
  char *xvar, char *yvar, char *zvar,
  double ox1, double ox2, double oy1, double oy2, double oz1, double oz2,
  char *filename, char istransposed, Coords posa)
{
  int is1d;
  double x2, x1, y2, y1, z2, z1;

  x1=ox1; y1=oy1; z1=oz1;
  x2=ox2; y2=oy2; z2=oz2;

  /* return if f,n,m,p1 NULL */
  if ((m*n*p == 0) || !p1) return (-1);
  /* output data block */
  is1d = mcfile_datablock(f, format, pre, parent, "data",
    p0, p1, p2, m, n, p,
    xlabel,  ylabel, zlabel, title,
    xvar, yvar, zvar,
    &x1, &x2, &y1, &y2, &z1, &z2, filename, istransposed, posa);
  /* return if 1D data */
  if (is1d) return(is1d);
  /* output error block and p2 non NULL */
  if (p0 && p2) mcfile_datablock(f, format, pre, parent, "errors",
    p0, p1, p2, m, n, p,
    xlabel,  ylabel, zlabel, title,
    xvar, yvar, zvar,
    &x1, &x2, &y1, &y2, &z1, &z2, filename, istransposed, posa);
  /* output ncount block and p0 non NULL */
  if (p0 && p2) mcfile_datablock(f, format, pre, parent, "ncount",
    p0, p1, p2, m, n, p,
    xlabel,  ylabel, zlabel, title,
    xvar, yvar, zvar,
    &x1, &x2, &y1, &y2, &z1, &z2, filename, istransposed, posa);

  return(is1d);
} /* mcfile_data */

double mcdetector_out(char *cname, double p0, double p1, double p2, char *filename)
{
  printf("Detector: %s_I=%g %s_ERR=%g %s_N=%g",
         cname, p1, cname, mcestimate_error(p0,p1,p2), cname, p0);
  if(filename && strlen(filename))
    printf(" \"%s\"", filename);
  printf("\n");
  return(p0);
}

/*******************************************************************************
* mcdetector_out_012D: main output function, works for 0d, 1d, 2d data
*   parent is the component name. Handles MPI stuff.
*******************************************************************************/
static double mcdetector_out_012D(struct mcformats_struct format,
  char *parent, char *title,
  int m, int n,  int p,
  char *xlabel, char *ylabel, char *zlabel,
  char *xvar, char *yvar, char *zvar,
  double x1, double x2, double y1, double y2, double z1, double z2,
  char *filename_orig,
  double *p0, double *p1, double *p2,
  Coords posa)
{
  char simname[512];
  int i,j;
  double Nsum=0, Psum=0, P2sum=0;
  FILE *simfile_f=NULL;
  char istransposed=0;
  char *pre;
  char *filename=NULL;

#ifdef USE_MPI
  int mpi_event_list;
#endif /* !USE_MPI */

  if (!p1 || (p1 && abs(m*n*p) > 1 && (!filename_orig || !strlen(filename_orig)))) return(0);

  pre = (char *)malloc(20);
  if (!pre) exit(fprintf(stderr, "Error: insufficient memory (mcdetector_out_012D)\n"));
  strcpy(pre, strstr(format.Name, "VRML")
           || strstr(format.Name, "OpenGENIE") ? "# " : "");
  if (filename_orig && abs(m*n*p) > 1) {
    filename = (char *)malloc(CHAR_BUF_LENGTH);
    if (!filename) exit(fprintf(stderr, "Error: insufficient memory (mcdetector_out_012D)\n"));
    strcpy(filename, filename_orig);
    if (!strchr(filename, '.') && !strstr(format.Name, "NeXus"))
    { /* add extension to file name if it is missing and not NeXus  */
      strcat(filename,".");
      if (mcformat_data.Extension) strcat(filename,mcformat_data.Extension);
      else strcat(filename,mcformat.Extension);
    }
  }
  fflush(NULL);

#ifdef USE_MPI
  mpi_event_list = (strstr(format.Name," list ") != NULL);

  if (!mpi_event_list && mpi_node_count > 1) {
    /* we save additive data: reduce everything */
    if (p0) mc_MPI_Reduce(p0, p0, abs(m*n*p), MPI_DOUBLE, MPI_SUM, mpi_node_root, MPI_COMM_WORLD);
    if (p1) mc_MPI_Reduce(p1, p1, abs(m*n*p), MPI_DOUBLE, MPI_SUM, mpi_node_root, MPI_COMM_WORLD);
    if (p2) mc_MPI_Reduce(p2, p2, abs(m*n*p), MPI_DOUBLE, MPI_SUM, mpi_node_root, MPI_COMM_WORLD);

    /* slaves are done */
    if(mpi_node_rank != mpi_node_root) return 0;
      
    if (!p0) {  /* additive signal must be then divided by the number of nodes */ 
      for (i=0; i<abs(m*n*p); i++) { 
        p1[i] /= mpi_node_count; 
        if (p2) p2[i] /= mpi_node_count; 
      } 
    } 
  }
#endif /* USE_MPI */

  if (!strstr(format.Name, "NeXus")) {
    if (m<0 || n<0 || p<0)                istransposed = !istransposed;
    if (strstr(format.Name, "binary"))    istransposed = !istransposed;
    if (strstr(format.Name, "transpose")) istransposed = !istransposed;
    if (istransposed)
    { /* do the swap once for all */
      i=m; m=abs(n); n=abs(i); p=abs(p);
    }
  } else m=abs(m); n=abs(n); p=abs(p);

  if (!strstr(format.Name," list ")) simfile_f = mcsiminfo_file; /* use sim file */
  if (mcdirname)
    sprintf(simname, "%s%s%s", mcdirname, MC_PATHSEP_S, mcsiminfo_name);
  else
    sprintf(simname, "%s%s%s", ".", MC_PATHSEP_S, mcsiminfo_name);

  if (!mcdisable_output_files) {
    MPI_MASTER (
      if (!strstr(format.Name,"NeXus"))
      mcfile_section(simfile_f, format, "begin", pre, parent, "component", simname, 3);
      mcfile_section(simfile_f, format, "begin", pre, filename, "data", parent, 4);
      );
  }

  if (mcDetectorCustomHeader && strlen(mcDetectorCustomHeader)) {
     if (strstr(format.Name, "Octave") || strstr(format.Name, "Matlab"))
       str_rep(mcDetectorCustomHeader, "%PRE", "%   ");
     else if (strstr(format.Name, "IDL"))    str_rep(mcDetectorCustomHeader, "%PRE", ";   ");
     else if (strstr(format.Name, "Scilab")) str_rep(mcDetectorCustomHeader, "%PRE", "//  ");
     else if (strstr(format.Name, "McStas")) str_rep(mcDetectorCustomHeader, "%PRE", "#   ");
     else str_rep(mcDetectorCustomHeader, "%PRE", "    ");
   }

#ifdef USE_MPI
  if (mpi_event_list && mpi_node_count > 1) {
    if (mpi_node_rank != mpi_node_root) {
      /* we save an event list: all slaves send their data to master */
      /* m, n, p must be sent too, since all slaves do not have the same number of events */
      int mnp[3];
      mnp[0] = m; mnp[1] = n; mnp[2] = p;
        
      if (MPI_Send(mnp, 3, MPI_INT, mpi_node_root, 0, MPI_COMM_WORLD)!= MPI_SUCCESS)
        fprintf(stderr, "Warning: node %i to master: MPI_Send mnp list error (mcdetector_out_012D)", mpi_node_rank);
      if (!p1 || mc_MPI_Send(p1, abs(mnp[0]*mnp[1]*mnp[2]), MPI_DOUBLE, mpi_node_root, MPI_COMM_WORLD)!= MPI_SUCCESS)
        fprintf(stderr, "Warning: node %i to master: MPI_Send p1 list error (mcdetector_out_012D)", mpi_node_rank);
      /* slaves are done */
      return 0;
    } else { /* master node list */
      int node_i;
      /* get, then save master and slaves event lists */
      for(node_i=0; node_i<mpi_node_count; node_i++) {
        double *this_p1=NULL; /* buffer to hold the list to save */
        int    mnp[3]={0,0,0};        /* size of this buffer */
        if (node_i != mpi_node_root) { /* get data from slaves */
          if (MPI_Recv(mnp, 3, MPI_INT, node_i, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE)!= MPI_SUCCESS)
            fprintf(stderr, "Warning: master from node %i: MPI_Recv mnp list error (mcdetector_out_012D)", node_i);
          this_p1 = (double *)calloc(abs(mnp[0]*mnp[1]*mnp[2]), sizeof(double));
          if (!this_p1 || mc_MPI_Recv(this_p1, abs(mnp[0]*mnp[1]*mnp[2]), MPI_DOUBLE, node_i, MPI_COMM_WORLD)!= MPI_SUCCESS)
            fprintf(stderr, "Warning: master from node %i: MPI_Recv p1 list error (mcdetector_out_012D)", node_i);
        } else {
          this_p1 = p1; 
          mnp[0] = m; mnp[1] = n; mnp[2] = p;
        }
        if (!strstr(format.Name, "NeXus")) { /* not MPI+NeXus format */
          char *formatName_orig = mcformat.Name;  /* copy the pointer position */
          char  formatName[256];
          strcpy(formatName, mcformat.Name);
          if (!strstr(formatName, "append")) strcat(formatName, " append ");
          if (node_i == 1) { /* first slave */
            /* disables header: it has been written by master */
            if (!strstr(formatName, "no header")) strcat(formatName, " no header ");
          }
          char *no_footer = strstr(formatName, "no footer");
          if (node_i == mpi_node_count-1) { /* last node */
            /* we write the last data block: request a footer */
            if (no_footer) strncpy(no_footer, "         ", 9);
          } else if (node_i == mpi_node_root) {
            /* master does not need footer (followed by slaves) */
            if (!no_footer) strcat(formatName, " no footer "); /* slaves do not need footer */
          }
          if (!mcdisable_output_files && this_p1) {
            mcformat.Name = formatName; /* use special customized format for list MPI */
            mcfile_data(simfile_f, format,
                        pre, parent,
                        NULL, this_p1, NULL, mnp[0], mnp[1], mnp[2],
                        xlabel, ylabel, zlabel, title,
                        xvar, yvar, zvar,
                        x1, x2, y1, y2, z1, z2, filename, istransposed, posa);
            mcformat.Name= formatName_orig; /* restore original format */
          }
        }
#ifdef USE_NEXUS
        else {
          /* MPI+NeXus: write one SDS per node list */
          char part[256];
          sprintf(part, "data_node_%i", node_i);
          if(mcnxfile_datablock(mcnxHandle, part,
              format.Name, parent, filename, xlabel, xlabel, ylabel, ylabel, zlabel, zlabel, title,
              xvar, yvar, zvar, abs(mnp[0]), abs(mnp[1]), abs(mnp[2]), x1, x2, y1, y2, z1, z2, NULL, this_p1, NULL)
              == NX_ERROR) {
            fprintf(stderr, "Error: writing NeXus data %s/%s (mcfile_datablock)\n", parent, filename);
          }
        }
#endif /* USE_NEXUS */
        if (node_i != mpi_node_root && this_p1) free(this_p1);
      } /* end for node_i */
    } /* end list for master node */
  } else
#endif /* USE_MPI */
  if (!mcdisable_output_files) { /* normal output */
    mcfile_data(simfile_f, format,
                pre, parent,
                p0, p1, p2, m, n, p,
                xlabel, ylabel, zlabel, title,
                xvar, yvar, zvar,
                x1, x2, y1, y2, z1, z2, filename, istransposed, posa);
  }

  if (!mcdisable_output_files) {
    mcfile_section(simfile_f, format, "end", pre, filename, "data", parent, 4);
    if (!strstr(format.Name,"NeXus"))
    mcfile_section(simfile_f, format, "end", pre, parent, "component", simname, 3);
  }

  if (simfile_f || mcdisable_output_files) {
    for(j = 0; j < n*p; j++) {
      for(i = 0; i < m; i++) {
        double N,I,E;
        int index;
        if (!istransposed) index = i*n*p + j;
        else index = i+j*m;
        if (p0) N = p0[index];
        if (p1) I = p1[index];
        if (p2) E = p2[index];

        Nsum += p0 ? N : 1;
        Psum += I;
        P2sum += p2 ? E : I*I;
      }
    }
    /* give 0D detector output. */
    if ((!filename || !strlen(filename)) && title && strlen(title)) filename = title;
    mcdetector_out(parent, Nsum, Psum, P2sum, filename);
  }
  free(pre); if (filename && filename_orig) free(filename);
  if (mcDetectorCustomHeader && strlen(mcDetectorCustomHeader)) {
     free(mcDetectorCustomHeader); mcDetectorCustomHeader=NULL;
  }
  fflush(NULL);
  return(Psum);
} /* mcdetector_out_012D */

/*******************************************************************************
* mcdetector_out_0D: wrapper to mcdetector_out_012D for 0D (single value).
*******************************************************************************/
double mcdetector_out_0D(char *t, double p0, double p1, double p2,
                         char *c, Coords posa)
{
  return(mcdetector_out_012D(mcformat,
    (char *)(c ? c : "McStas component"), (char *)(t ? t : "McStas data"),
    1, 1, 1,
    "I", "", "",
    "I", "", "",
    0, 0, 0, 0, 0, 0, NULL,
    &p0, &p1, &p2, posa));
}

/*******************************************************************************
* mcdetector_out_1D: wrapper to mcdetector_out_012D for 1D.
*******************************************************************************/
double mcdetector_out_1D(char *t, char *xl, char *yl,
                  char *xvar, double x1, double x2, int n,
                  double *p0, double *p1, double *p2, char *f,
                  char *c, Coords posa)
{
  return(mcdetector_out_012D(mcformat,
    (char *)(c ? c : "McStas component"), (char *)(t ? t : "McStas 1D data"),
    n, 1, 1,
    (char *)(xl ? xl : "X"), (char *)(yl ? yl : "Y"), (char *)(n > 1 ? "Signal per bin" : " Signal"),
    (char *)(xvar ? xvar : "x"), "(I,I_err)", "I",
    x1, x2, 0, 0, 0, 0, f,
    p0, p1, p2, posa));
}

/*******************************************************************************
* mcdetector_out_2D: wrapper to mcdetector_out_012D for 2D.
*******************************************************************************/
double mcdetector_out_2D(char *t, char *xl, char *yl,
                  double x1, double x2, double y1, double y2, int m,
                  int n, double *p0, double *p1, double *p2, char *f,
                  char *c, Coords posa)
{
  char xvar[3];
  char yvar[3];

  strcpy(xvar, "x "); strcpy(yvar, "y ");
  if (xl && strlen(xl)) strncpy(xvar, xl, 2);
  if (yl && strlen(yl)) strncpy(yvar, yl, 2);

  /* is it in fact a 1D call ? */
  if (m == 1)      return(mcdetector_out_1D(
                    t, yl, "I", yvar, y1, y2, n, p0, p1, p2, f, c, posa));
  else if (n == 1) return(mcdetector_out_1D(
                    t, xl, "I", xvar, x1, x2, m, p0, p1, p2, f, c, posa));

  return(mcdetector_out_012D(mcformat,
    (char *)(c ? c : "McStas component"), (char *)(t ? t : "McStas 2D data"),
    m, n, 1,
    (char *)(xl ? xl : "X"), (char *)(yl ? yl : "Y"), (char *)(n*m > 1 ? "Signal per bin" : " Signal"),
    xvar, yvar, "I",
    x1, x2, y1, y2, 0, 0, f,
    p0, p1, p2, posa));
}

/*******************************************************************************
* mcdetector_out_3D: wrapper to mcdetector_out_012D for 3D.
*   exported as a large 2D array, but the " dims are given in the header
*******************************************************************************/
double mcdetector_out_3D(char *t, char *xl, char *yl, char *zl,
      char *xvar, char *yvar, char *zvar,
                  double x1, double x2, double y1, double y2, double z1, double z2, int m,
                  int n, int p, double *p0, double *p1, double *p2, char *f,
                  char *c, Coords posa)
{
  return(mcdetector_out_012D(mcformat,
    (char *)(c ? c : "McStas component"), (char *)(t ? t : "McStas 3D data"),
    m, n, p,
    (char *)(xl ? xl : "X"), (char *)(yl ? yl : "Y"), (char *)(zl ? zl : "Z"),
    (char *)(xvar ? xvar : "x"), (char *)(yvar ? yvar : "y"), (char *)(zvar ? zvar : "z"),
    x1, x2, y1, y2, z1, z2, f,
    p0, p1, p2, posa));
}

/* end of file i/o functions ================================================ */

/* mcuse_format_header: Replaces aliases names in format fields (header part) */
char *mcuse_format_header(char *format_const)
{ /* Header Footer */
  char *format=NULL;

  if (!format_const) return(NULL);
  format = (char *)malloc(strlen(format_const)+1);
  if (!format) exit(fprintf(stderr, "Error: insufficient memory (mcuse_format_header)\n"));
  strcpy(format, format_const);
  str_rep(format, "%PRE", "%1$s"); /* prefix */
  str_rep(format, "%SRC", "%2$s"); /* name of instrument source file */
  str_rep(format, "%FIL", "%3$s"); /* output file name (data) */
  str_rep(format, "%FMT", "%4$s"); /* format name */
  str_rep(format, "%DATL","%8$li");/* Time elapsed since Jan 1st 1970, in seconds */
  str_rep(format, "%DAT", "%5$s"); /* Date as a string */
  str_rep(format, "%USR", "%6$s"); /* User/machine name */
  str_rep(format, "%PAR", "%7$s"); /* Parent name (root/mcstas) valid_parent */
  str_rep(format, "%VPA", "%7$s");
  return(format);
}

/* mcuse_format_tag: Replaces aliases names in format fields (tag part) */
char *mcuse_format_tag(char *format_const)
{ /* AssignTag */
  char *format=NULL;

  if (!format_const) return(NULL);
  format = (char *)malloc(strlen(format_const)+1);
  if (!format) exit(fprintf(stderr, "Error: insufficient memory (mcuse_format_tag)\n"));
  strcpy(format, format_const);
  str_rep(format, "%PRE", "%1$s"); /* prefix */
  str_rep(format, "%SEC", "%2$s"); /* section/parent name valid_parent/section */
  str_rep(format, "%PAR", "%2$s");
  str_rep(format, "%VPA", "%2$s");
  str_rep(format, "%NAM", "%3$s"); /* name of field valid_name */
  str_rep(format, "%VNA", "%3$s");
  str_rep(format, "%VAL", "%4$s"); /* value of field */
  return(format);
}

/* mcuse_format_section: Replaces aliases names in format fields (section part) */
char *mcuse_format_section(char *format_const)
{ /* BeginSection EndSection */
  char *format=NULL;

  if (!format_const) return(NULL);
  format = (char *)malloc(strlen(format_const)+1);
  if (!format) exit(fprintf(stderr, "Error: insufficient memory (mcuse_format_section)\n"));
  strcpy(format, format_const);
  str_rep(format, "%PRE", "%1$s"); /* prefix */
  str_rep(format, "%TYP", "%2$s"); /* type/class of section */
  str_rep(format, "%NAM", "%3$s"); /* name of section */
  str_rep(format, "%SEC", "%3$s");
  str_rep(format, "%VNA", "%4$s"); /* valid name (letters/digits) of section */
  str_rep(format, "%PAR", "%5$s"); /* parent name (simulation) */
  str_rep(format, "%VPA", "%6$s"); /* valid parent name (letters/digits) */
  str_rep(format, "%LVL", "%7$i"); /* level index */
  return(format);
}

/* mcuse_format_data: Replaces aliases names in format fields (data part) */
char *mcuse_format_data(char *format_const)
{ /* BeginData EndData BeginErrors EndErrors BeginNcount EndNcount */
  char *format=NULL;

  if (!format_const) return(NULL);
  format = (char *)malloc(strlen(format_const)+1);
  if (!format) exit(fprintf(stderr, "Error: insufficient memory (mcuse_format_data)\n"));
  strcpy(format, format_const);
  str_rep(format, "%PRE", "%1$s"); /* prefix */
  str_rep(format, "%PAR", "%2$s"); /* parent name (component instance name) valid_parent */
  str_rep(format, "%VPA", "%2$s");
  str_rep(format, "%FIL", "%3$s"); /* output file name (data) */
  str_rep(format, "%XLA", "%4$s"); /* x axis label */
  str_rep(format, "%XVL", "%5$s"); /* x axis valid label (letters/digits) */
  str_rep(format, "%YLA", "%6$s"); /* y axis label */
  str_rep(format, "%YVL", "%7$s"); /* y axis valid label (letters/digits) */
  str_rep(format, "%ZLA", "%8$s"); /* z axis label */
  str_rep(format, "%ZVL", "%9$s"); /* z axis valid label (letters/digits) */
  str_rep(format, "%TITL", "%10$s"); /* data title */
  str_rep(format, "%XVAR", "%11$s"); /* x variables */
  str_rep(format, "%YVAR", "%12$s"); /* y variables */
  str_rep(format, "%ZVAR", "%13$s"); /* z variables */
  str_rep(format, "%MDIM", "%14$i"); /* m dimension of x axis */
  str_rep(format, "%NDIM", "%15$i"); /* n dimension of y axis */
  str_rep(format, "%PDIM", "%16$i"); /* p dimension of z axis */
  str_rep(format, "%XMIN", "%17$g"); /* x min axis value (m bins) */
  str_rep(format, "%XMAX", "%18$g"); /* x max axis value (m bins) */
  str_rep(format, "%YMIN", "%19$g"); /* y min axis value (n bins) */
  str_rep(format, "%YMAX", "%20$g"); /* y max axis value (n bins) */
  str_rep(format, "%ZMIN", "%21$g"); /* z min axis value (usually min of signal, p bins) */
  str_rep(format, "%ZMAX", "%22$g"); /* z max axis value (usually max of signal, p bins) */
  return(format);
}

/*******************************************************************************
* mcuse_format: selects an output format for sim and data files. returns format
*******************************************************************************/
struct mcformats_struct mcuse_format(char *format)
{
  int i,j;
  int i_format=-1;
  char *tmp;
  char low_format[256];
  struct mcformats_struct usedformat;

  usedformat.Name = NULL;
  /* get the format to lower case */
  if (!format) exit(fprintf(stderr, "Error: Invalid NULL format. Exiting (mcuse_format)\n"));
  strcpy(low_format, format);
  for (i=0; i<strlen(low_format); i++) low_format[i]=tolower(format[i]);
  /* handle format aliases */
  if (!strcmp(low_format, "pgplot")) strcpy(low_format, "mcstas");
  if (!strcmp(low_format, "hdf"))    strcpy(low_format, "nexus");
#ifndef USE_NEXUS
  if (!strcmp(low_format, "nexus"))
    fprintf(stderr, "WARNING: to enable NeXus format you must have the NeXus libs installed.\n"
                    "         You should then re-compile with the -DUSE_NEXUS define.\n");
#endif

  tmp = (char *)malloc(256);
  if(!tmp) exit(fprintf(stderr, "Error: insufficient memory (mcuse_format)\n"));

  /* look for a specific format in mcformats.Name table */
  for (i=0; i < mcNUMFORMATS; i++)
  {
    strcpy(tmp, mcformats[i].Name);
    for (j=0; j<strlen(tmp); j++) tmp[j] = tolower(tmp[j]);
    if (strstr(low_format, tmp))  i_format = i;
  }
  if (i_format < 0)
  {
    i_format = 0; /* default format is #0 McStas */
    fprintf(stderr, "Warning: unknown output format '%s'. Using default (%s, mcuse_format).\n", format, mcformats[i_format].Name);
  }

  usedformat = mcformats[i_format];
  strcpy(tmp, usedformat.Name);
  usedformat.Name = tmp;
  if (strstr(low_format,"raw")) strcat(usedformat.Name," raw");
  if (strstr(low_format,"binary"))
  {
    if (strstr(low_format,"double")) strcat(usedformat.Name," binary double data");
    else strcat(usedformat.Name," binary float data");
    mcascii_only = 1;
  }

  /* Replaces vfprintf parameter name aliases */
  /* Header Footer */
  usedformat.Header       = mcuse_format_header(usedformat.Header);
  usedformat.Footer       = mcuse_format_header(usedformat.Footer);
  /* AssignTag */
  usedformat.AssignTag    = mcuse_format_tag(usedformat.AssignTag);
  /* BeginSection EndSection */
  usedformat.BeginSection = mcuse_format_section(usedformat.BeginSection);
  usedformat.EndSection   = mcuse_format_section(usedformat.EndSection);
  /*  BeginData  EndData  BeginErrors  EndErrors  BeginNcount  EndNcount  */
  usedformat.BeginData    = mcuse_format_data(usedformat.BeginData  );
  usedformat.EndData      = mcuse_format_data(usedformat.EndData    );
  usedformat.BeginErrors  = mcuse_format_data(usedformat.BeginErrors);
  usedformat.EndErrors    = mcuse_format_data(usedformat.EndErrors  );
  usedformat.BeginNcount  = mcuse_format_data(usedformat.BeginNcount);
  usedformat.EndNcount    = mcuse_format_data(usedformat.EndNcount  );

  return(usedformat);
} /* mcuse_format */

/* mcclear_format: free format structure */
void mcclear_format(struct mcformats_struct usedformat)
{
/* free format specification strings */
  if (usedformat.Name        ) free(usedformat.Name        );
  else return;
  if (usedformat.Header      ) free(usedformat.Header      );
  if (usedformat.Footer      ) free(usedformat.Footer      );
  if (usedformat.AssignTag   ) free(usedformat.AssignTag   );
  if (usedformat.BeginSection) free(usedformat.BeginSection);
  if (usedformat.EndSection  ) free(usedformat.EndSection  );
  if (usedformat.BeginData   ) free(usedformat.BeginData   );
  if (usedformat.EndData     ) free(usedformat.EndData     );
  if (usedformat.BeginErrors ) free(usedformat.BeginErrors );
  if (usedformat.EndErrors   ) free(usedformat.EndErrors   );
  if (usedformat.BeginNcount ) free(usedformat.BeginNcount );
  if (usedformat.EndNcount   ) free(usedformat.EndNcount   );
} /* mcclear_format */

/* mcuse_file: will save data/sim files */
static void mcuse_file(char *file)
{
  if (file && strcmp(file, "NULL"))
    mcsiminfo_name = file;
  else {
    char *filename=(char*)malloc(CHAR_BUF_LENGTH);
    sprintf(filename, "%s_%li", mcinstrument_name, mcstartdate);
    mcsiminfo_name = filename;
  }
  mcsingle_file  = 1;
}

/* Following part is only embedded when not redundent with mcstas.h ========= */

#ifndef MCSTAS_H

/* MCDISPLAY support. ======================================================= */

/*******************************************************************************
* Just output MCDISPLAY keywords to be caught by an external plotter client.
*******************************************************************************/

void mcdis_magnify(char *what){
  printf("MCDISPLAY: magnify('%s')\n", what);
}

void mcdis_line(double x1, double y1, double z1,
                double x2, double y2, double z2){
  printf("MCDISPLAY: multiline(2,%g,%g,%g,%g,%g,%g)\n",
         x1,y1,z1,x2,y2,z2);
}

void mcdis_dashed_line(double x1, double y1, double z1,
		       double x2, double y2, double z2, int n){
  int i;
  const double dx = (x2-x1)/(2*n+1);
  const double dy = (y2-y1)/(2*n+1);
  const double dz = (z2-z1)/(2*n+1);

  for(i = 0; i < n+1; i++)
    mcdis_line(x1 + 2*i*dx,     y1 + 2*i*dy,     z1 + 2*i*dz,
	       x1 + (2*i+1)*dx, y1 + (2*i+1)*dy, z1 + (2*i+1)*dz);
}

void mcdis_multiline(int count, ...){
  va_list ap;
  double x,y,z;

  printf("MCDISPLAY: multiline(%d", count);
  va_start(ap, count);
  while(count--)
    {
    x = va_arg(ap, double);
    y = va_arg(ap, double);
    z = va_arg(ap, double);
    printf(",%g,%g,%g", x, y, z);
    }
  va_end(ap);
  printf(")\n");
}

void mcdis_rectangle(char* plane, double x, double y, double z,
		     double width, double height){
  /* draws a rectangle in the plane           */
  /* x is ALWAYS width and y is ALWAYS height */
  if (strcmp("xy", plane)==0) {
    mcdis_multiline(5,
		    x - width/2, y - height/2, z,
		    x + width/2, y - height/2, z,
		    x + width/2, y + height/2, z,
		    x - width/2, y + height/2, z,
		    x - width/2, y - height/2, z);
  } else if (strcmp("xz", plane)==0) {
    mcdis_multiline(5,
		    x - width/2, y, z - height/2,
		    x + width/2, y, z - height/2,
		    x + width/2, y, z + height/2,
		    x - width/2, y, z + height/2,
		    x - width/2, y, z - height/2);
  } else if (strcmp("yz", plane)==0) {
    mcdis_multiline(5,
		    x, y - height/2, z - width/2,
		    x, y - height/2, z + width/2,
		    x, y + height/2, z + width/2,
		    x, y + height/2, z - width/2,
		    x, y - height/2, z - width/2);
  } else {

    fprintf(stderr, "Error: Definition of plane %s unknown\n", plane);
    exit(1);
  }
}

/*  draws a box with center at (x, y, z) and
    width (deltax), height (deltay), length (deltaz) */
void mcdis_box(double x, double y, double z,
	       double width, double height, double length){

  mcdis_rectangle("xy", x, y, z-length/2, width, height);
  mcdis_rectangle("xy", x, y, z+length/2, width, height);
  mcdis_line(x-width/2, y-height/2, z-length/2,
	     x-width/2, y-height/2, z+length/2);
  mcdis_line(x-width/2, y+height/2, z-length/2,
	     x-width/2, y+height/2, z+length/2);
  mcdis_line(x+width/2, y-height/2, z-length/2,
	     x+width/2, y-height/2, z+length/2);
  mcdis_line(x+width/2, y+height/2, z-length/2,
	     x+width/2, y+height/2, z+length/2);
}

void mcdis_circle(char *plane, double x, double y, double z, double r){
  printf("MCDISPLAY: circle('%s',%g,%g,%g,%g)\n", plane, x, y, z, r);
}

/* coordinates handling ===================================================== */

/*******************************************************************************
* Since we use a lot of geometric calculations using Cartesian coordinates,
* we collect some useful routines here. However, it is also permissible to
* work directly on the underlying struct coords whenever that is most
* convenient (that is, the type Coords is not abstract).
*
* Coordinates are also used to store rotation angles around x/y/z axis.
*
* Since coordinates are used much like a basic type (such as double), the
* structure itself is passed and returned, rather than a pointer.
*
* At compile-time, the values of the coordinates may be unknown (for example
* a motor position). Hence coordinates are general expressions and not simple
* numbers. For this we used the type Coords_exp which has three CExp
* fields. For runtime (or calculations possible at compile time), we use
* Coords which contains three double fields.
*******************************************************************************/

/* coords_set: Assign coordinates. */
Coords
coords_set(MCNUM x, MCNUM y, MCNUM z)
{
  Coords a;

  a.x = x;
  a.y = y;
  a.z = z;
  return a;
}

/* coords_get: get coordinates. Required when 'x','y','z' are #defined as neutron pars */
Coords
coords_get(Coords a, MCNUM *x, MCNUM *y, MCNUM *z)
{
  *x = a.x;
  *y = a.y;
  *z = a.z;
  return a;
}

/* coords_add: Add two coordinates. */
Coords
coords_add(Coords a, Coords b)
{
  Coords c;

  c.x = a.x + b.x;
  c.y = a.y + b.y;
  c.z = a.z + b.z;
  if (fabs(c.z) < 1e-14) c.z=0.0;
  return c;
}

/* coords_sub: Subtract two coordinates. */
Coords
coords_sub(Coords a, Coords b)
{
  Coords c;

  c.x = a.x - b.x;
  c.y = a.y - b.y;
  c.z = a.z - b.z;
  if (fabs(c.z) < 1e-14) c.z=0.0;
  return c;
}

/* coords_neg: Negate coordinates. */
Coords
coords_neg(Coords a)
{
  Coords b;

  b.x = -a.x;
  b.y = -a.y;
  b.z = -a.z;
  return b;
}

/* coords_scale: Scale a vector. */
Coords coords_scale(Coords b, double scale) {
  Coords a;

  a.x = b.x*scale;
  a.y = b.y*scale;
  a.z = b.z*scale;
  return a;
}

/* coords_sp: Scalar product: a . b */
double coords_sp(Coords a, Coords b) {
  double value;

  value = a.x*b.x + a.y*b.y + a.z*b.z;
  return value;
}

/* coords_xp: Cross product: a = b x c. */
Coords coords_xp(Coords b, Coords c) {
  Coords a;

  a.x = b.y*c.z - c.y*b.z;
  a.y = b.z*c.x - c.z*b.x;
  a.z = b.x*c.y - c.x*b.y;
  return a;
}

/* coords_mirror: Mirror a in plane (through the origin) defined by normal n*/
Coords coords_mirror(Coords a, Coords n) {
  double t = scalar_prod(n.x, n.y, n.z, n.x, n.y, n.z);
  Coords b;
  if (t!=1) {
    t = sqrt(t);
    n.x /= t;
    n.y /= t;
    n.z /= t;
  }
  t=scalar_prod(a.x, a.y, a.z, n.x, n.y, n.z);
  b.x = a.x-2*t*n.x;
  b.y = a.y-2*t*n.y;
  b.z = a.z-2*t*n.z;
  return b;
}

/* coords_print: Print out vector values. */
void coords_print(Coords a) {

  fprintf(stdout, "(%f, %f, %f)\n", a.x, a.y, a.z);
  return;
}

/*******************************************************************************
* The Rotation type implements a rotation transformation of a coordinate
* system in the form of a double[3][3] matrix.
*
* Contrary to the Coords type in coords.c, rotations are passed by
* reference. Functions that yield new rotations do so by writing to an
* explicit result parameter; rotations are not returned from functions. The
* reason for this is that arrays cannot by returned from functions (though
* structures can; thus an alternative would have been to wrap the
* double[3][3] array up in a struct). Such are the ways of C programming.
*
* A rotation represents the tranformation of the coordinates of a vector when
* changing between coordinate systems that are rotated with respect to each
* other. For example, suppose that coordinate system Q is rotated 45 degrees
* around the Z axis with respect to coordinate system P. Let T be the
* rotation transformation representing a 45 degree rotation around Z. Then to
* get the coordinates of a vector r in system Q, apply T to the coordinates
* of r in P. If r=(1,0,0) in P, it will be (sqrt(1/2),-sqrt(1/2),0) in
* Q. Thus we should be careful when interpreting the sign of rotation angles:
* they represent the rotation of the coordinate systems, not of the
* coordinates (which has opposite sign).
*******************************************************************************/

/*******************************************************************************
* rot_set_rotation: Get transformation for rotation first phx around x axis,
* then phy around y, then phz around z.
*******************************************************************************/
void
rot_set_rotation(Rotation t, double phx, double phy, double phz)
{
  if ((phx == 0) && (phy == 0) && (phz == 0)) {
    t[0][0] = 1.0;
    t[0][1] = 0.0;
    t[0][2] = 0.0;
    t[1][0] = 0.0;
    t[1][1] = 1.0;
    t[1][2] = 0.0;
    t[2][0] = 0.0;
    t[2][1] = 0.0;
    t[2][2] = 1.0;
  } else {
    double cx = cos(phx);
    double sx = sin(phx);
    double cy = cos(phy);
    double sy = sin(phy);
    double cz = cos(phz);
    double sz = sin(phz);
    
    t[0][0] = cy*cz;
    t[0][1] = sx*sy*cz + cx*sz;
    t[0][2] = sx*sz - cx*sy*cz;
    t[1][0] = -cy*sz;
    t[1][1] = cx*cz - sx*sy*sz;
    t[1][2] = sx*cz + cx*sy*sz;
    t[2][0] = sy;
    t[2][1] = -sx*cy;
    t[2][2] = cx*cy;
  } 
}

/*******************************************************************************
* rot_test_identity: Test if rotation is identity
*******************************************************************************/
int 
rot_test_identity(Rotation t)
{
  return (t[0][0] + t[1][1] + t[2][2] == 3);
}

/*******************************************************************************
* rot_mul: Matrix multiplication of transformations (this corresponds to
* combining transformations). After rot_mul(T1, T2, T3), doing T3 is
* equal to doing first T2, then T1.
* Note that T3 must not alias (use the same array as) T1 or T2.
*******************************************************************************/
void
rot_mul(Rotation t1, Rotation t2, Rotation t3)
{
  int i,j;
  if (rot_test_identity(t1)) {
    memcpy(t3, t2, 9 * sizeof (double));
  } else if (rot_test_identity(t2)) {
    memcpy(t3, t1, 9 * sizeof (double));
  } else {
    for(i = 0; i < 3; i++)
      for(j = 0; j < 3; j++)
	t3[i][j] = t1[i][0]*t2[0][j] + t1[i][1]*t2[1][j] + t1[i][2]*t2[2][j];
  }
}

/*******************************************************************************
* rot_copy: Copy a rotation transformation (arrays cannot be assigned in C).
*******************************************************************************/
void
rot_copy(Rotation dest, Rotation src)
{
	memcpy(dest, src, 9 * sizeof (double));
}

/*******************************************************************************
* rot_transpose: Matrix transposition, which is inversion for Rotation matrices
*******************************************************************************/
void
rot_transpose(Rotation src, Rotation dst)
{
  dst[0][0] = src[0][0];
  dst[0][1] = src[1][0];
  dst[0][2] = src[2][0];
  dst[1][0] = src[0][1];
  dst[1][1] = src[1][1];
  dst[1][2] = src[2][1];
  dst[2][0] = src[0][2];
  dst[2][1] = src[1][2];
  dst[2][2] = src[2][2];
}

/*******************************************************************************
* rot_apply: returns t*a
*******************************************************************************/
Coords
rot_apply(Rotation t, Coords a)
{
  Coords b;
  if (rot_test_identity(t)) { 
    return a;
  } else {
    b.x = t[0][0]*a.x + t[0][1]*a.y + t[0][2]*a.z;
    b.y = t[1][0]*a.x + t[1][1]*a.y + t[1][2]*a.z;
    b.z = t[2][0]*a.x + t[2][1]*a.y + t[2][2]*a.z;
    return b;
  }
}

/*******************************************************************************
* mccoordschange: applies rotation to (x y z) and (vx vy vz). Spin unchanged
*******************************************************************************/
void
mccoordschange(Coords a, Rotation t, double *x, double *y, double *z,
               double *vx, double *vy, double *vz, double *time,
               double *s1, double *s2)
{
  Coords b, c;

  b.x = *x;
  b.y = *y;
  b.z = *z;
  c = rot_apply(t, b);
  b = coords_add(c, a);
  *x = b.x;
  *y = b.y;
  *z = b.z;

  b.x = *vx;
  b.y = *vy;
  b.z = *vz;
  c = rot_apply(t, b);
  *vx = c.x;
  *vy = c.y;
  *vz = c.z;
  /* spin handled with mccoordschange_polarisation */
}

/*******************************************************************************
* mccoordschange_polarisation: applies rotation to (sx sy sz)
*******************************************************************************/
void
mccoordschange_polarisation(Rotation t, double *sx, double *sy, double *sz)
{
  Coords b, c;

  b.x = *sx;
  b.y = *sy;
  b.z = *sz;
  c = rot_apply(t, b);
  *sx = c.x;
  *sy = c.y;
  *sz = c.z;
}

/*******************************************************************************
* mcstore_neutron: stores neutron coodinates into global array (per component)
*******************************************************************************/
void
mcstore_neutron(MCNUM *s, int index, double x, double y, double z,
               double vx, double vy, double vz, double t,
               double sx, double sy, double sz, double p)
{
    double *dptr = &s[11*index];
    *dptr++  = x;
    *dptr++  = y ;
    *dptr++  = z ;
    *dptr++  = vx;
    *dptr++  = vy;
    *dptr++  = vz;
    *dptr++  = t ;
    *dptr++  = sx;
    *dptr++  = sy;
    *dptr++  = sz;
    *dptr    = p ;
}

/*******************************************************************************
* mcrestore_neutron: restores neutron coodinates from global array
*******************************************************************************/
void
mcrestore_neutron(MCNUM *s, int index, double *x, double *y, double *z,
               double *vx, double *vy, double *vz, double *t,
               double *sx, double *sy, double *sz, double *p)
{
    double *dptr = &s[11*index];
    *x  =  *dptr++;
    *y  =  *dptr++;
    *z  =  *dptr++;
    *vx =  *dptr++;
    *vy =  *dptr++;
    *vz =  *dptr++;
    *t  =  *dptr++;
    *sx =  *dptr++;
    *sy =  *dptr++;
    *sz =  *dptr++;
    *p  =  *dptr;
}

/* init/run/rand handling =================================================== */

/* mcreadparams: request parameters from the prompt (or use default) */
void
mcreadparams(void)
{
  int i,j,status;
  static char buf[CHAR_BUF_LENGTH];
  char *p;
  int len;

  MPI_MASTER(printf("Instrument parameters for %s (%s)\n",
                    mcinstrument_name, mcinstrument_source));

  for(i = 0; mcinputtable[i].name != 0; i++)
  {
    do
    {
      MPI_MASTER(
                 if (mcinputtable[i].val && strlen(mcinputtable[i].val))
                   printf("Set value of instrument parameter %s (%s) [default='%s']:\n",
                          mcinputtable[i].name,
                          (*mcinputtypes[mcinputtable[i].type].parminfo)
                          (mcinputtable[i].name), mcinputtable[i].val);
                 else
                   printf("Set value of instrument parameter %s (%s):\n",
                          mcinputtable[i].name,
                          (*mcinputtypes[mcinputtable[i].type].parminfo)
                          (mcinputtable[i].name));
                 fflush(stdout);
                 );
#ifdef USE_MPI
      if(mpi_node_rank == mpi_node_root)
        {
          p = fgets(buf, CHAR_BUF_LENGTH, stdin);
          if(p == NULL)
            {
              fprintf(stderr, "Error: empty input for paramater %s (mcreadparams)\n", mcinputtable[i].name);
              exit(1);
            }
        }
      else
        p = buf;
      MPI_Bcast(buf, CHAR_BUF_LENGTH, MPI_CHAR, mpi_node_root, MPI_COMM_WORLD);
#else /* !USE_MPI */
      p = fgets(buf, CHAR_BUF_LENGTH, stdin);
      if(p == NULL)
        {
          fprintf(stderr, "Error: empty input for paramater %s (mcreadparams)\n", mcinputtable[i].name);
          exit(1);
        }
#endif /* USE_MPI */
      len = strlen(buf);
      if (!len || (len == 1 && (buf[0] == '\n' || buf[0] == '\r')))
      {
        if (mcinputtable[i].val && strlen(mcinputtable[i].val)) {
          strncpy(buf, mcinputtable[i].val, CHAR_BUF_LENGTH);  /* use default value */
          len = strlen(buf);
        }
      }
      for(j = 0; j < 2; j++)
      {
        if(len > 0 && (buf[len - 1] == '\n' || buf[len - 1] == '\r'))
        {
          len--;
          buf[len] = '\0';
        }
      }

      status = (*mcinputtypes[mcinputtable[i].type].getparm)
                   (buf, mcinputtable[i].par);
      if(!status)
      {
        (*mcinputtypes[mcinputtable[i].type].error)(mcinputtable[i].name, buf);
        if (!mcinputtable[i].val || strlen(mcinputtable[i].val)) {
          fprintf(stderr, "       Change %s default value in instrument definition.\n", mcinputtable[i].name);
          exit(1);
        }
      }
    } while(!status);
  }
}

/* mcsetstate: transfert parameters into global McStas variables */
void
mcsetstate(double x, double y, double z, double vx, double vy, double vz,
           double t, double sx, double sy, double sz, double p)
{
  extern double mcnx, mcny, mcnz, mcnvx, mcnvy, mcnvz;
  extern double mcnt, mcnsx, mcnsy, mcnsz, mcnp;

  mcnx = x;
  mcny = y;
  mcnz = z;
  mcnvx = vx;
  mcnvy = vy;
  mcnvz = vz;
  mcnt = t;
  mcnsx = sx;
  mcnsy = sy;
  mcnsz = sz;
  mcnp = p;
}

/* mcgenstate: set default neutron parameters */
void
mcgenstate(void)
{
  mcsetstate(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1);
  /* old initialisation: mcsetstate(0, 0, 0, 0, 0, 1, 0, sx=0, sy=1, sz=0, 1); */
}

/* McStas random number routine. */

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/*
 * This is derived from the Berkeley source:
 *        @(#)random.c        5.5 (Berkeley) 7/6/88
 * It was reworked for the GNU C Library by Roland McGrath.
 * Rewritten to use reentrant functions by Ulrich Drepper, 1995.
 */

/*******************************************************************************
* Modified for McStas from glibc 2.0.7pre1 stdlib/random.c and
* stdlib/random_r.c.
*
* This way random() is more than four times faster compared to calling
* standard glibc random() on ix86 Linux, probably due to multithread support,
* ELF shared library overhead, etc. It also makes McStas generated
* simulations more portable (more likely to behave identically across
* platforms, important for parrallel computations).
*******************************************************************************/


#define        TYPE_3                3
#define        BREAK_3                128
#define        DEG_3                31
#define        SEP_3                3

static mc_int32_t randtbl[DEG_3 + 1] =
  {
    TYPE_3,

    -1726662223, 379960547, 1735697613, 1040273694, 1313901226,
    1627687941, -179304937, -2073333483, 1780058412, -1989503057,
    -615974602, 344556628, 939512070, -1249116260, 1507946756,
    -812545463, 154635395, 1388815473, -1926676823, 525320961,
    -1009028674, 968117788, -123449607, 1284210865, 435012392,
    -2017506339, -911064859, -370259173, 1132637927, 1398500161,
    -205601318,
  };

static mc_int32_t *fptr = &randtbl[SEP_3 + 1];
static mc_int32_t *rptr = &randtbl[1];
static mc_int32_t *state = &randtbl[1];
#define rand_deg DEG_3
#define rand_sep SEP_3
static mc_int32_t *end_ptr = &randtbl[sizeof (randtbl) / sizeof (randtbl[0])];

mc_int32_t
mc_random (void)
{
  mc_int32_t result;

  *fptr += *rptr;
  /* Chucking least random bit.  */
  result = (*fptr >> 1) & 0x7fffffff;
  ++fptr;
  if (fptr >= end_ptr)
  {
    fptr = state;
    ++rptr;
  }
  else
  {
    ++rptr;
    if (rptr >= end_ptr)
      rptr = state;
  }
  return result;
}

void
mc_srandom (unsigned int x)
{
  /* We must make sure the seed is not 0.  Take arbitrarily 1 in this case.  */
  state[0] = x ? x : 1;
  {
    long int i;
    for (i = 1; i < rand_deg; ++i)
    {
      /* This does:
         state[i] = (16807 * state[i - 1]) % 2147483647;
         but avoids overflowing 31 bits.  */
      long int hi = state[i - 1] / 127773;
      long int lo = state[i - 1] % 127773;
      long int test = 16807 * lo - 2836 * hi;
      state[i] = test + (test < 0 ? 2147483647 : 0);
    }
    fptr = &state[rand_sep];
    rptr = &state[0];
    for (i = 0; i < 10 * rand_deg; ++i)
      random ();
  }
}

/* "Mersenne Twister", by Makoto Matsumoto and Takuji Nishimura. */
/* See http://www.math.keio.ac.jp/~matumoto/emt.html for original source. */


/*
   A C-program for MT19937, with initialization improved 2002/1/26.
   Coded by Takuji Nishimura and Makoto Matsumoto.

   Before using, initialize the state by using mt_srandom(seed)
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
   http://www.math.keio.ac.jp/matumoto/emt.html
   email: matumoto@math.keio.ac.jp
*/

#include <stdio.h>

/* Period parameters */
#define N 624
#define M 397
#define MATRIX_A 0x9908b0dfUL   /* constant vector a */
#define UPPER_MASK 0x80000000UL /* most significant w-r bits */
#define LOWER_MASK 0x7fffffffUL /* least significant r bits */

static unsigned long mt[N]; /* the array for the state vector  */
static int mti=N+1; /* mti==N+1 means mt[N] is not initialized */

/* initializes mt[N] with a seed */
void mt_srandom(unsigned long s)
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

/* initialize by an array with array-length */
/* init_key is the array for initializing keys */
/* key_length is its length */
void init_by_array(unsigned long init_key[], unsigned long key_length)
{
    //unsigned long init_key[], key_length;

    int i, j, k;
    mt_srandom(19650218UL);
    i=1; j=0;
    k = (N>key_length ? N : key_length);
    for (; k; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1664525UL))
          + init_key[j] + j; /* non linear */
        mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
        i++; j++;
        if (i>=N) { mt[0] = mt[N-1]; i=1; }
        if (j>=key_length) j=0;
    }
    for (k=N-1; k; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1566083941UL))
          - i; /* non linear */
        mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
        i++;
        if (i>=N) { mt[0] = mt[N-1]; i=1; }
    }

    mt[0] = 0x80000000UL; /* MSB is 1; assuring non-zero initial array */
}

/* generates a random number on [0,0xffffffff]-interval */
unsigned long mt_random(void)
{
    unsigned long y;
    static unsigned long mag01[2]={0x0UL, MATRIX_A};
    /* mag01[x] = x * MATRIX_A  for x=0,1 */

    if (mti >= N) { /* generate N words at one time */
        int kk;

        if (mti == N+1)   /* if mt_srandom() has not been called, */
            mt_srandom(5489UL); /* a default initial seed is used */

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

#undef N
#undef M
#undef MATRIX_A
#undef UPPER_MASK
#undef LOWER_MASK

/* End of "Mersenne Twister". */

/* End of McStas random number routine. */

/* randnorm: generate a random number from normal law */
double
randnorm(void)
{
  static double v1, v2, s;
  static int phase = 0;
  double X, u1, u2;

  if(phase == 0)
  {
    do
    {
      u1 = rand01();
      u2 = rand01();
      v1 = 2*u1 - 1;
      v2 = 2*u2 - 1;
      s = v1*v1 + v2*v2;
    } while(s >= 1 || s == 0);

    X = v1*sqrt(-2*log(s)/s);
  }
  else
  {
    X = v2*sqrt(-2*log(s)/s);
  }

  phase = 1 - phase;
  return X;
}

/* generate a random number from -1 to 1 with triangle distribution */
double randtriangle(void) {
  double randnum=rand01();
  if (randnum>0.5) return(1-sqrt(2*(randnum-0.5)));
  else return(sqrt(2*randnum)-1);
}

/* intersect handling ======================================================= */

/* normal_vec: Compute normal vector to (x,y,z). */
void normal_vec(double *nx, double *ny, double *nz,
                double x, double y, double z)
{
  double ax = fabs(x);
  double ay = fabs(y);
  double az = fabs(z);
  double l;
  if(x == 0 && y == 0 && z == 0)
  {
    *nx = 0;
    *ny = 0;
    *nz = 0;
    return;
  }
  if(ax < ay)
  {
    if(ax < az)
    {                           /* Use X axis */
      l = sqrt(z*z + y*y);
      *nx = 0;
      *ny = z/l;
      *nz = -y/l;
      return;
    }
  }
  else
  {
    if(ay < az)
    {                           /* Use Y axis */
      l = sqrt(z*z + x*x);
      *nx = z/l;
      *ny = 0;
      *nz = -x/l;
      return;
    }
  }
  /* Use Z axis */
  l = sqrt(y*y + x*x);
  *nx = y/l;
  *ny = -x/l;
  *nz = 0;
}

/* inside_rectangle: Check if (x,y) is inside rectangle (xwidth, yheight) */
/* return 0 if outside and 1 if inside */
int inside_rectangle(double x, double y, double xwidth, double yheight)
{
  if (x>-xwidth/2 && x<xwidth/2 && y>-yheight/2 && y<yheight/2)
    return 1;
  else
    return 0;
}

/* box_intersect: compute time intersection with a box
 * returns 0 when no intersection is found
 *      or 1 in case of intersection with resulting times dt_in and dt_out
 * This function written by Stine Nyborg, 1999. */
int box_intersect(double *dt_in, double *dt_out,
                  double x, double y, double z,
                  double vx, double vy, double vz,
                  double dx, double dy, double dz)
{
  double x_in, y_in, z_in, tt, t[6], a, b;
  int i, count, s;

      /* Calculate intersection time for each of the six box surface planes
       *  If the box surface plane is not hit, the result is zero.*/

  if(vx != 0)
   {
    tt = -(dx/2 + x)/vx;
    y_in = y + tt*vy;
    z_in = z + tt*vz;
    if( y_in > -dy/2 && y_in < dy/2 && z_in > -dz/2 && z_in < dz/2)
      t[0] = tt;
    else
      t[0] = 0;

    tt = (dx/2 - x)/vx;
    y_in = y + tt*vy;
    z_in = z + tt*vz;
    if( y_in > -dy/2 && y_in < dy/2 && z_in > -dz/2 && z_in < dz/2)
      t[1] = tt;
    else
      t[1] = 0;
   }
  else
    t[0] = t[1] = 0;

  if(vy != 0)
   {
    tt = -(dy/2 + y)/vy;
    x_in = x + tt*vx;
    z_in = z + tt*vz;
    if( x_in > -dx/2 && x_in < dx/2 && z_in > -dz/2 && z_in < dz/2)
      t[2] = tt;
    else
      t[2] = 0;

    tt = (dy/2 - y)/vy;
    x_in = x + tt*vx;
    z_in = z + tt*vz;
    if( x_in > -dx/2 && x_in < dx/2 && z_in > -dz/2 && z_in < dz/2)
      t[3] = tt;
    else
      t[3] = 0;
   }
  else
    t[2] = t[3] = 0;

  if(vz != 0)
   {
    tt = -(dz/2 + z)/vz;
    x_in = x + tt*vx;
    y_in = y + tt*vy;
    if( x_in > -dx/2 && x_in < dx/2 && y_in > -dy/2 && y_in < dy/2)
      t[4] = tt;
    else
      t[4] = 0;

    tt = (dz/2 - z)/vz;
    x_in = x + tt*vx;
    y_in = y + tt*vy;
    if( x_in > -dx/2 && x_in < dx/2 && y_in > -dy/2 && y_in < dy/2)
      t[5] = tt;
    else
      t[5] = 0;
   }
  else
    t[4] = t[5] = 0;

  /* The intersection is evaluated and *dt_in and *dt_out are assigned */

  a = b = s = 0;
  count = 0;

  for( i = 0; i < 6; i = i + 1 )
    if( t[i] == 0 )
      s = s+1;
    else if( count == 0 )
    {
      a = t[i];
      count = 1;
    }
    else
    {
      b = t[i];
      count = 2;
    }

  if ( a == 0 && b == 0 )
    return 0;
  else if( a < b )
  {
    *dt_in = a;
    *dt_out = b;
    return 1;
  }
  else
  {
    *dt_in = b;
    *dt_out = a;
    return 1;
  }

}

/* cylinder_intersect: compute intersection with a cylinder
 * returns 0 when no intersection is found
 *      or 2/4/8/16 bits depending on intersection,
 *     and resulting times t0 and t1
 * Written by: EM,NB,ABA 4.2.98 */
int
cylinder_intersect(double *t0, double *t1, double x, double y, double z,
                   double vx, double vy, double vz, double r, double h)
{
  double D, t_in, t_out, y_in, y_out;
  int ret=1;

  D = (2*vx*x + 2*vz*z)*(2*vx*x + 2*vz*z)
    - 4*(vx*vx + vz*vz)*(x*x + z*z - r*r);

  if (D>=0)
  {
    if (vz*vz + vx*vx) {
      t_in  = (-(2*vz*z + 2*vx*x) - sqrt(D))/(2*(vz*vz + vx*vx));
      t_out = (-(2*vz*z + 2*vx*x) + sqrt(D))/(2*(vz*vz + vx*vx));
    } else if (vy) { /* trajectory parallel to cylinder axis */
      t_in = (y + h/2)/vy;
      t_out = (y - h/2)/vy;
      if (t_in>t_out){ 
	        double tmp=t_in; 
	        t_in=t_out;t_out=tmp; 
	    } 
    } else return 0;
    y_in = vy*t_in + y;
    y_out =vy*t_out + y;

    if ( (y_in > h/2 && y_out > h/2) || (y_in < -h/2 && y_out < -h/2) )
      return 0;
    else
    {
      if (y_in > h/2)
        { t_in = ((h/2)-y)/vy; ret += 2; }
      else if (y_in < -h/2)
        { t_in = ((-h/2)-y)/vy; ret += 4; }
      if (y_out > h/2)
        { t_out = ((h/2)-y)/vy; ret += 8; }
      else if (y_out < -h/2)
        { t_out = ((-h/2)-y)/vy; ret += 16; }
    }
    *t0 = t_in;
    *t1 = t_out;
    return ret;
  }
  else
  {
    *t0 = *t1 = 0;
    return 0;
  }
}


/* sphere_intersect: Calculate intersection between a line and a sphere.
 * returns 0 when no intersection is found
 *      or 1 in case of intersection with resulting times t0 and t1 */
int
sphere_intersect(double *t0, double *t1, double x, double y, double z,
                 double vx, double vy, double vz, double r)
{
  double A, B, C, D, v;

  v = sqrt(vx*vx + vy*vy + vz*vz);
  A = v*v;
  B = 2*(x*vx + y*vy + z*vz);
  C = x*x + y*y + z*z - r*r;
  D = B*B - 4*A*C;
  if(D < 0)
    return 0;
  D = sqrt(D);
  *t0 = (-B - D) / (2*A);
  *t1 = (-B + D) / (2*A);
  return 1;
}

/* solve_2nd_order: second order equation solve: A*t^2 + B*t + C = 0
 * returns 0 if no solution was found, or set 't' to the smallest positive
 * solution.
 * EXAMPLE usage for intersection of a trajectory with a plane in gravitation
 * field (gx,gy,gz):
 * The neutron starts at point r=(x,y,z) with velocityv=(vx vy vz). The plane
 * has a normal vector n=(nx,ny,nz) and contains the point W=(wx,wy,wz).
 * The problem consists in solving the 2nd order equation:
 *      1/2.n.g.t^2 + n.v.t + n.(r-W) = 0
 * so that A = 0.5 n.g; B = n.v; C = n.(r-W);
 * Without acceleration, t=-n.(r-W)/n.v
 */
int solve_2nd_order(double *Idt,
                  double A,  double B,  double C)
{
  int ret=0;

  *Idt = 0;

  if (fabs(A) < 1E-10) /* this plane is parallel to the acceleration: A ~ 0 */
  {
    if (B) {  *Idt = -C/B; ret=3; }
    /* else the speed is parallel to the plane, no intersection: A=B=0 ret=0 */
  }
  else
  {
    double D;
    D = B*B - 4*A*C;
    if (D >= 0) /* Delta > 0: neutron trajectory hits the mirror */
    {
      double sD, dt1, dt2;
      sD = sqrt(D);
      dt1 = (-B + sD)/2/A;
      dt2 = (-B - sD)/2/A;
      /* we identify very small values with zero */
      if (fabs(dt1) < 1e-10) dt1=0.0;
      if (fabs(dt2) < 1e-10) dt2=0.0;

      /* now we choose the smallest positive solution */
      if      (dt1<=0.0 && dt2>0.0) ret=2; /* dt2 positive */
      else if (dt2<=0.0 && dt1>0.0) ret=1; /* dt1 positive */
      else if (dt1> 0.0 && dt2>0.0)
      {  if (dt1 < dt2) ret=1; else ret=2; } /* all positive: min(dt1,dt2) */
      /* else two solutions are negative. ret=0 */
      if (ret==1) *Idt = dt1; else if (ret==2) *Idt=dt2;
    } /* else Delta <0: no intersection.  ret=0 */
  }
  return(ret);
}

/* plane_intersect: Calculate intersection between a plane and a line.
 * returns 0 when no intersection is found (i.e. line is parallel to the plane)
 * returns 1 or -1 when intersection time is positive and negative respectively
 */  
int
plane_intersect(double *t, double x, double y, double z,
                 double vx, double vy, double vz, double nx, double ny, double nz, double wx, double wy, double wz)
{
  double s;
  if (fabs(s=scalar_prod(nx,ny,nz,vx,vy,vz))<FLT_EPSILON) return 0;
  *t = - scalar_prod(nx,ny,nz,x-wx,y-wy,z-wz)/s;
  if (*t<0) return -1;
  else return 1;
}

/* randvec_target_circle: Choose random direction towards target at (x,y,z)
 * with given radius.
 * If radius is zero, choose random direction in full 4PI, no target. */
void
randvec_target_circle(double *xo, double *yo, double *zo, double *solid_angle,
               double xi, double yi, double zi, double radius)
{
  double l2, phi, theta, nx, ny, nz, xt, yt, zt, xu, yu, zu;

  if(radius == 0.0)
  {
    /* No target, choose uniformly a direction in full 4PI solid angle. */
    theta = acos (1 - rand0max(2));
    phi = rand0max(2 * PI);
    if(solid_angle)
      *solid_angle = 4*PI;
    nx = 1;
    ny = 0;
    nz = 0;
    yi = sqrt(xi*xi+yi*yi+zi*zi);
    zi = 0;
    xi = 0;
  }
  else
  {
    double costheta0;
    l2 = xi*xi + yi*yi + zi*zi; /* sqr Distance to target. */
    costheta0 = sqrt(l2/(radius*radius+l2));
    if (radius < 0) costheta0 *= -1;
    if(solid_angle)
    {
      /* Compute solid angle of target as seen from origin. */
        *solid_angle = 2*PI*(1 - costheta0);
    }

    /* Now choose point uniformly on circle surface within angle theta0 */
    theta = acos (1 - rand0max(1 - costheta0)); /* radius on circle */
    phi = rand0max(2 * PI); /* rotation on circle at given radius */
    /* Now, to obtain the desired vector rotate (xi,yi,zi) angle theta around a
       perpendicular axis u=i x n and then angle phi around i. */
    if(xi == 0 && zi == 0)
    {
      nx = 1;
      ny = 0;
      nz = 0;
    }
    else
    {
      nx = -zi;
      nz = xi;
      ny = 0;
    }
  }

  /* [xyz]u = [xyz]i x n[xyz] (usually vertical) */
  vec_prod(xu,  yu,  zu, xi, yi, zi,        nx, ny, nz);
  /* [xyz]t = [xyz]i rotated theta around [xyz]u */
  rotate  (xt,  yt,  zt, xi, yi, zi, theta, xu, yu, zu);
  /* [xyz]o = [xyz]t rotated phi around n[xyz] */
  rotate (*xo, *yo, *zo, xt, yt, zt, phi, xi, yi, zi);
}


/* randvec_target_rect_angular: Choose random direction towards target at
 * (xi,yi,zi) with given ANGULAR dimension height x width. height=phi_x,
 * width=phi_y (radians)
 * If height or width is zero, choose random direction in full 4PI, no target. */
void
randvec_target_rect_angular(double *xo, double *yo, double *zo, double *solid_angle,
               double xi, double yi, double zi, double width, double height, Rotation A)
{
  double theta, phi, nx, ny, nz, xt, yt, zt, xu, yu, zu;
  Coords tmp;
  Rotation Ainverse;

  rot_transpose(A, Ainverse);

  if(height == 0.0 || width == 0.0)
  {
    randvec_target_circle(xo, yo, zo, solid_angle,
               xi, yi, zi, 0);
    return;
  }
  else
  {
    if(solid_angle)
    {
      /* Compute solid angle of target as seen from origin. */
      *solid_angle = 2*fabs(width*sin(height/2));
    }

    /* Go to global coordinate system */

    tmp = coords_set(xi, yi, zi);
    tmp = rot_apply(Ainverse, tmp);
    coords_get(tmp, &xi, &yi, &zi);

    /* Now choose point uniformly on quadrant within angle theta0/phi0 */
    theta = width*randpm1()/2.0;
    phi   = height*randpm1()/2.0;
    /* Now, to obtain the desired vector rotate (xi,yi,zi) angle phi around
       n, and then theta around u. */
    if(xi == 0 && zi == 0)
    {
      nx = 1;
      ny = 0;
      nz = 0;
    }
    else
    {
      nx = -zi;
      nz = xi;
      ny = 0;
    }
  }

  /* [xyz]u = [xyz]i x n[xyz] (usually vertical) */
  vec_prod(xu,  yu,  zu, xi, yi, zi,        nx, ny, nz);
  /* [xyz]t = [xyz]i rotated theta around [xyz]u */
  rotate  (xt,  yt,  zt, xi, yi, zi, phi, nx, ny, nz);
  /* [xyz]o = [xyz]t rotated phi around n[xyz] */
  rotate (*xo, *yo, *zo, xt, yt, zt, theta, xu,  yu,  zu);

  /* Go back to local coordinate system */
  tmp = coords_set(*xo, *yo, *zo);
  tmp = rot_apply(A, tmp);
  coords_get(tmp, &*xo, &*yo, &*zo);

}

/* randvec_target_rect_real: Choose random direction towards target at (xi,yi,zi)
 * with given dimension height x width (in meters !).
 *
 * Local emission coordinate is taken into account and corrected for 'order' times.
 * (See remarks posted to mcstas-users by George Apostolopoulus <gapost@ipta.demokritos.gr>)
 *
 * If height or width is zero, choose random direction in full 4PI, no target. 
 * 
 * Traditionally, this routine had the name randvec_target_rect - this is now a
 * a define (see mcstas-r.h) pointing here. If you use the old rouine, you are NOT
 * taking the local emmission coordinate into account. 
*/

void
randvec_target_rect_real(double *xo, double *yo, double *zo, double *solid_angle,
               double xi, double yi, double zi, double width, double height, Rotation A, double lx, double ly, double lz, int order)
{
  double dx, dy, dist, dist_p, nx, ny, nz, mx, my, mz, n_norm, m_norm;
  double cos_theta;
  Coords tmp;
  Rotation Ainverse;

  rot_transpose(A, Ainverse);

  if(height == 0.0 || width == 0.0)
  {
    randvec_target_circle(xo, yo, zo, solid_angle,
               xi, yi, zi, 0);
    return;
  }
  else
  {

    /* Now choose point uniformly on rectangle within width x height */
    dx = width*randpm1()/2.0;
    dy = height*randpm1()/2.0;

    /* Determine distance to target plane*/
    dist = sqrt(xi*xi + yi*yi + zi*zi);
    /* Go to global coordinate system */

    tmp = coords_set(xi, yi, zi);
    tmp = rot_apply(Ainverse, tmp);
    coords_get(tmp, &xi, &yi, &zi);

    /* Determine vector normal to neutron axis (z) and gravity [0 1 0] */
    vec_prod(nx, ny, nz, xi, yi, zi, 0, 1, 0);

    /* This now defines the x-axis, normalize: */
    n_norm=sqrt(nx*nx + ny*ny + nz*nz);
    nx = nx/n_norm;
    ny = ny/n_norm;
    nz = nz/n_norm;

    /* Now, determine our y-axis (vertical in many cases...) */
    vec_prod(mx, my, mz, xi, yi, zi, nx, ny, nz);
    m_norm=sqrt(mx*mx + my*my + mz*mz);
    mx = mx/m_norm;
    my = my/m_norm;
    mz = mz/m_norm;

    /* Our output, random vector can now be defined by linear combination: */

    *xo = xi + dx * nx + dy * mx;
    *yo = yi + dx * ny + dy * my;
    *zo = zi + dx * nz + dy * mz;

    /* Go back to local coordinate system */
    tmp = coords_set(*xo, *yo, *zo);
    tmp = rot_apply(A, tmp);
    coords_get(tmp, &*xo, &*yo, &*zo);

    if (solid_angle) {
      /* Calculate vector from local point to remote random point */
      lx = *xo - lx;
      ly = *yo - ly;
      lz = *zo - lz;
      dist_p = sqrt(lx*lx + ly*ly + lz*lz);
      
      /* Adjust the 'solid angle' */
      /* 1/r^2 to the chosen point times cos(\theta) between the normal */
      /* vector of the target rectangle and direction vector of the chosen point. */
      cos_theta = (xi * lx + yi * ly + zi * lz) / (dist * dist_p);
      *solid_angle = width * height / (dist_p * dist_p); 
      int counter;
      for (counter = 0; counter < order; counter++) {
	*solid_angle = *solid_angle * cos_theta;
      }
    }
  }
}


/* extend_list: Make sure a list is big enough to hold element COUNT.
*
* The list is an array, and the argument 'list' is a pointer to a pointer to
* the array start. The argument 'size' is a pointer to the number of elements
* in the array. The argument 'elemsize' is the sizeof() an element. The
* argument 'count' is the minimum number of elements needed in the list.
*
* If the old array is to small (or if *list is NULL or *size is 0), a
* sufficuently big new array is allocated, and *list and *size are updated.
*/
void extend_list(int count, void **list, int *size, size_t elemsize)
{
  if(count >= *size)
  {
    void *oldlist = *list;
    if(*size > 0)
      *size *= 2;
    else
      *size = 32;
    *list = malloc(*size*elemsize);
    if(!*list)
    {
      exit(fprintf(stderr, "\nError: Out of memory %li (extend_list).\n", (long)*size*elemsize));
    }
    if(oldlist)
    {
      memcpy(*list, oldlist, count*elemsize);
      free(oldlist);
    }
  }
}

/* mcsetn_arg: get ncount from a string argument */
static void
mcsetn_arg(char *arg)
{
  mcset_ncount(strtod(arg, NULL));
}

/* mcsetseed: set the random generator seed from a string argument */
static void
mcsetseed(char *arg)
{
  mcseed = atol(arg);
  if(mcseed) {
#ifdef USE_MPI
    if (mpi_node_count > 1) srandom(mcseed+mpi_node_rank);
    else
#endif
    srandom(mcseed);
  } else {
    fprintf(stderr, "Error: seed must not be zero (mcsetseed)\n");
    exit(1);
  }
}

/* mchelp: displays instrument executable help with possible options */
static void
mchelp(char *pgmname)
{
  int i;

  fprintf(stderr, "Usage: %s [options] [parm=value ...]\n", pgmname);
  fprintf(stderr,
"Options are:\n"
"  -s SEED   --seed=SEED      Set random seed (must be != 0)\n"
"  -n COUNT  --ncount=COUNT   Set number of neutrons to simulate.\n"
"  -d DIR    --dir=DIR        Put all data files in directory DIR.\n"
"  -f FILE   --file=FILE      Put all data in a single file.\n"
"  -t        --trace          Enable trace of neutron through instrument.\n"
"  -g        --gravitation    Enable gravitation for all trajectories.\n"
"  -a        --data-only      Do not put any headers in the data files.\n"
"  --no-output-files          Do not write any data files.\n"
"  -h        --help           Show this help message.\n"
"  -i        --info           Detailed instrument information.\n"
"  --format=FORMAT            Output data files using format FORMAT\n"
"                             (use option +a to include text header in files\n"
#ifdef USE_MPI
"This instrument has been compiled with MPI support. Use 'mpirun'.\n"
#endif
"\n"
);
  if(mcnumipar > 0)
  {
    fprintf(stderr, "Instrument parameters are:\n");
    for(i = 0; i < mcnumipar; i++)
      if (mcinputtable[i].val && strlen(mcinputtable[i].val))
        fprintf(stderr, "  %-16s(%s) [default='%s']\n", mcinputtable[i].name,
        (*mcinputtypes[mcinputtable[i].type].parminfo)(mcinputtable[i].name),
        mcinputtable[i].val);
      else
        fprintf(stderr, "  %-16s(%s)\n", mcinputtable[i].name,
        (*mcinputtypes[mcinputtable[i].type].parminfo)(mcinputtable[i].name));
  }
  fprintf(stderr, "Available output formats are (default is %s):\n  ", mcformat.Name);
  for (i=0; i < mcNUMFORMATS; fprintf(stderr,"\"%s\" " , mcformats[i++].Name) );
  fprintf(stderr, "\n  Format modifiers: FORMAT may be followed by 'binary float' or \n");
  fprintf(stderr, "  'binary double' to save data blocks as binary. This removes text headers.\n");
  fprintf(stderr, "  The MCSTAS_FORMAT environment variable may set the default FORMAT to use.\n");
#ifndef NOSIGNALS
  fprintf(stderr, "Known signals are: "
#ifdef SIGUSR1
  "USR1 (status) "
#endif
#ifdef SIGUSR2
  "USR2 (save) "
#endif
#ifdef SIGBREAK
  "BREAK (save) "
#endif
#ifdef SIGTERM
  "TERM (save and exit)"
#endif
  "\n");
#endif /* !NOSIGNALS */
}

/* mcshowhelp: show help and exit with 0 */
static void
mcshowhelp(char *pgmname)
{
  mchelp(pgmname);
#ifdef USE_MPI
#undef exit
#endif
  exit(0);
#ifdef USE_MPI
#define exit(code) MPI_Abort(MPI_COMM_WORLD, code)
#endif
}

/* mcusage: display usage when error in input arguments and exit with 1 */
static void
mcusage(char *pgmname)
{
  fprintf(stderr, "Error: incorrect command line arguments\n");
  mchelp(pgmname);
  exit(1);
}

/* mcenabletrace: enable trace/mcdisplay or error if requires recompile */
static void
mcenabletrace(void)
{
 if(mctraceenabled)
  mcdotrace = 1;
 else
 {
   fprintf(stderr,
           "Error: trace not enabled (mcenabletrace)\n"
           "Please re-run the McStas compiler "
                   "with the --trace option, or rerun the\n"
           "C compiler with the MC_TRACE_ENABLED macro defined.\n");
   exit(1);
 }
}

/* mcuse_dir: set data/sim storage directory and create it,
 * or exit with error if exists */
static void
mcuse_dir(char *dir)
{
#ifdef MC_PORTABLE
  fprintf(stderr, "Error: "
          "Directory output cannot be used with portable simulation (mcuse_dir)\n");
  exit(1);
#else  /* !MC_PORTABLE */
#ifdef USE_MPI  
    if(mpi_node_rank == mpi_node_root)
    {
#endif
     if(mkdir(dir, 0777)) {
#ifndef DANSE
         fprintf(stderr, "Error: unable to create directory '%s' (mcuse_dir)\n", dir);
         fprintf(stderr, "(Maybe the directory already exists?)\n");       
         exit(1);
#endif
     }
#ifdef USE_MPI
   }
#endif
   mcdirname = dir;
#endif /* !MC_PORTABLE */
}

/* mcinfo: display instrument simulation info to stdout and exit */
static void
mcinfo(void)
{
  if(strstr(mcformat.Name,"NeXus"))
    exit(fprintf(stderr,"Error: Can not display instrument informtion in NeXus binary format\n"));
  mcsiminfo_init(stdout);
  mcsiminfo_close();
#ifdef USE_MPI
#undef exit
#endif
  exit(0);
#ifdef USE_MPI
#define exit(code) MPI_Abort(MPI_COMM_WORLD, code)
#endif
}

/* mcparseoptions: parse command line arguments (options, parameters) */
void
mcparseoptions(int argc, char *argv[])
{
  int i, j;
  char *p;
  int paramset = 0, *paramsetarray;

  /* Add one to mcnumipar to avoid allocating zero size memory block. */
  paramsetarray = (int *)malloc((mcnumipar + 1)*sizeof(*paramsetarray));
  if(paramsetarray == NULL)
  {
    fprintf(stderr, "Error: insufficient memory (mcparseoptions)\n");
    exit(1);
  }
  for(j = 0; j < mcnumipar; j++)
    {
      paramsetarray[j] = 0;
      if (mcinputtable[j].val != NULL && strlen(mcinputtable[j].val))
      {
        int  status;
        char buf[CHAR_BUF_LENGTH];
        strncpy(buf, mcinputtable[j].val, CHAR_BUF_LENGTH);
        status = (*mcinputtypes[mcinputtable[j].type].getparm)
                   (buf, mcinputtable[j].par);
        if(!status) fprintf(stderr, "Invalid '%s' default value %s in instrument definition (mcparseoptions)\n", mcinputtable[j].name, buf);
        else paramsetarray[j] = 1;
      } else {
        (*mcinputtypes[mcinputtable[j].type].getparm)
          (NULL, mcinputtable[j].par);
        paramsetarray[j] = 0;
      }
    }
  for(i = 1; i < argc; i++)
  {
    if(!strcmp("-s", argv[i]) && (i + 1) < argc)
      mcsetseed(argv[++i]);
    else if(!strncmp("-s", argv[i], 2))
      mcsetseed(&argv[i][2]);
    else if(!strcmp("--seed", argv[i]) && (i + 1) < argc)
      mcsetseed(argv[++i]);
    else if(!strncmp("--seed=", argv[i], 7))
      mcsetseed(&argv[i][7]);
    else if(!strcmp("-n", argv[i]) && (i + 1) < argc)
      mcsetn_arg(argv[++i]);
    else if(!strncmp("-n", argv[i], 2))
      mcsetn_arg(&argv[i][2]);
    else if(!strcmp("--ncount", argv[i]) && (i + 1) < argc)
      mcsetn_arg(argv[++i]);
    else if(!strncmp("--ncount=", argv[i], 9))
      mcsetn_arg(&argv[i][9]);
    else if(!strcmp("-d", argv[i]) && (i + 1) < argc)
      mcuse_dir(argv[++i]);
    else if(!strncmp("-d", argv[i], 2))
      mcuse_dir(&argv[i][2]);
    else if(!strcmp("--dir", argv[i]) && (i + 1) < argc)
      mcuse_dir(argv[++i]);
    else if(!strncmp("--dir=", argv[i], 6))
      mcuse_dir(&argv[i][6]);
    else if(!strcmp("-f", argv[i]) && (i + 1) < argc)
      mcuse_file(argv[++i]);
    else if(!strncmp("-f", argv[i], 2))
      mcuse_file(&argv[i][2]);
    else if(!strcmp("--file", argv[i]) && (i + 1) < argc)
      mcuse_file(argv[++i]);
    else if(!strncmp("--file=", argv[i], 7))
      mcuse_file(&argv[i][7]);
    else if(!strcmp("-h", argv[i]))
      mcshowhelp(argv[0]);
    else if(!strcmp("--help", argv[i]))
      mcshowhelp(argv[0]);
    else if(!strcmp("-i", argv[i])) {
      mcformat=mcuse_format(MCSTAS_FORMAT);
      mcinfo();
    }
    else if(!strcmp("--info", argv[i]))
      mcinfo();
    else if(!strcmp("-t", argv[i]))
      mcenabletrace();
    else if(!strcmp("--trace", argv[i]))
      mcenabletrace();
    else if(!strcmp("-a", argv[i]))
      mcascii_only = 1;
    else if(!strcmp("+a", argv[i]))
      mcascii_only = 0;
    else if(!strcmp("--data-only", argv[i]))
      mcascii_only = 1;
    else if(!strcmp("--gravitation", argv[i]))
      mcgravitation = 1;
    else if(!strcmp("-g", argv[i]))
      mcgravitation = 1;
    else if(!strncmp("--format=", argv[i], 9)) {
      mcascii_only = 0;
      mcformat=mcuse_format(&argv[i][9]);
    }
    else if(!strcmp("--format", argv[i]) && (i + 1) < argc) {
      mcascii_only = 0;
      mcformat=mcuse_format(argv[++i]);
    }
    else if(!strncmp("--format_data=", argv[i], 14) || !strncmp("--format-data=", argv[i], 14)) {
      mcascii_only = 0;
      mcformat_data=mcuse_format(&argv[i][14]);
    }
    else if((!strcmp("--format_data", argv[i]) || !strcmp("--format-data", argv[i])) && (i + 1) < argc) {
      mcascii_only = 0;
      mcformat_data=mcuse_format(argv[++i]);
    }
    else if(!strcmp("--no-output-files", argv[i]))
      mcdisable_output_files = 1;   
    else if(argv[i][0] != '-' && (p = strchr(argv[i], '=')) != NULL)
    {
      *p++ = '\0';

      for(j = 0; j < mcnumipar; j++)
        if(!strcmp(mcinputtable[j].name, argv[i]))
        {
          int status;
          status = (*mcinputtypes[mcinputtable[j].type].getparm)(p,
                        mcinputtable[j].par);
          if(!status || !strlen(p))
          {
            (*mcinputtypes[mcinputtable[j].type].error)
              (mcinputtable[j].name, p);
            exit(1);
          }
          paramsetarray[j] = 1;
          paramset = 1;
          break;
        }
      if(j == mcnumipar)
      {                                /* Unrecognized parameter name */
        fprintf(stderr, "Error: unrecognized parameter %s (mcparseoptions)\n", argv[i]);
        exit(1);
      }
    }
    else if(argv[i][0] == '-') {
      fprintf(stderr, "Error: unrecognized option argument %s (mcparseoptions). Ignored.\n", argv[i++]);
    }
    else
      mcusage(argv[0]);
  }
  if (!mcascii_only) {
    if (strstr(mcformat.Name,"binary")) fprintf(stderr, "Warning: %s files will contain text headers.\n         Use -a option to clean up.\n", mcformat.Name);
    strcat(mcformat.Name, " with text headers");
  }
  if(!paramset)
    mcreadparams();                /* Prompt for parameters if not specified. */
  else
  {
    for(j = 0; j < mcnumipar; j++)
      if(!paramsetarray[j])
      {
        fprintf(stderr, "Error: Instrument parameter %s left unset (mcparseoptions)\n",
                mcinputtable[j].name);
        exit(1);
      }
  }
  free(paramsetarray);
#ifdef USE_MPI
  if (mcdotrace) mpi_node_count=1; /* disable threading when in trace mode */
#endif
} /* mcparseoptions */

#ifndef NOSIGNALS
mcstatic char  mcsig_message[256];  /* ADD: E. Farhi, Sep 20th 2001 */


/* sighandler: signal handler that makes simulation stop, and save results */
void sighandler(int sig)
{
  /* MOD: E. Farhi, Sep 20th 2001: give more info */
  time_t t1, t0;
#define SIG_SAVE 0
#define SIG_TERM 1
#define SIG_STAT 2
#define SIG_ABRT 3

  printf("\n# McStas: [pid %i] Signal %i detected", getpid(), sig);
#if defined(SIGUSR1) && defined(SIGUSR2) && defined(SIGKILL)
  if (!strcmp(mcsig_message, "sighandler") && (sig != SIGUSR1) && (sig != SIGUSR2))
  {
    printf("\n# Fatal : unrecoverable loop ! Suicide (naughty boy).\n");
    kill(0, SIGKILL); /* kill myself if error occurs within sighandler: loops */
  }
#endif
  switch (sig) {
#ifdef SIGINT
    case SIGINT : printf(" SIGINT (interrupt from terminal, Ctrl-C)"); sig = SIG_TERM; break;
#endif
#ifdef SIGILL
    case SIGILL  : printf(" SIGILL (Illegal instruction)"); sig = SIG_ABRT; break;
#endif
#ifdef SIGFPE
    case SIGFPE  : printf(" SIGFPE (Math Error)"); sig = SIG_ABRT; break;
#endif
#ifdef SIGSEGV
    case SIGSEGV : printf(" SIGSEGV (Mem Error)"); sig = SIG_ABRT; break;
#endif
#ifdef SIGTERM
    case SIGTERM : printf(" SIGTERM (Termination)"); sig = SIG_TERM; break;
#endif
#ifdef SIGABRT
    case SIGABRT : printf(" SIGABRT (Abort)"); sig = SIG_ABRT; break;
#endif
#ifdef SIGQUIT
    case SIGQUIT : printf(" SIGQUIT (Quit from terminal)"); sig = SIG_TERM; break;
#endif
#ifdef SIGTRAP
    case SIGTRAP : printf(" SIGTRAP (Trace trap)"); sig = SIG_ABRT; break;
#endif
#ifdef SIGPIPE
    case SIGPIPE : printf(" SIGPIPE (Broken pipe)"); sig = SIG_ABRT; break;
#endif
#ifdef SIGUSR1
    case SIGUSR1 : printf(" SIGUSR1 (Display info)"); sig = SIG_STAT; break;
#endif
#ifdef SIGUSR2
    case SIGUSR2 : printf(" SIGUSR2 (Save simulation)"); sig = SIG_SAVE; break;
#endif
#ifdef SIGHUP
    case SIGHUP  : printf(" SIGHUP (Hangup/update)"); sig = SIG_SAVE; break;
#endif
#ifdef SIGBUS
    case SIGBUS  : printf(" SIGBUS (Bus error)"); sig = SIG_ABRT; break;
#endif
#ifdef SIGURG
    case SIGURG  : printf(" SIGURG (Urgent socket condition)"); sig = SIG_ABRT; break;
#endif
#ifdef SIGBREAK
    case SIGBREAK: printf(" SIGBREAK (Break signal, Ctrl-Break)"); sig = SIG_SAVE; break;
#endif
    default : printf(" (look at signal list for signification)"); sig = SIG_ABRT; break;
  }
  printf("\n");
  printf("# Simulation: %s (%s) \n", mcinstrument_name, mcinstrument_source);
  printf("# Breakpoint: %s ", mcsig_message);
  if (strstr(mcsig_message, "Save") && (sig == SIG_SAVE))
    sig = SIG_STAT;
  SIG_MESSAGE("sighandler");
  if (mcget_ncount() == 0)
    printf("(0 %%)\n" );
  else
  {
    printf("%.2f %% (%10.1f/%10.1f)\n", 100*mcget_run_num()/mcget_ncount(), mcget_run_num(), mcget_ncount());
  }
  t0 = (time_t)mcstartdate;
  t1 = time(NULL);
  printf("# Date:      %s", ctime(&t1));
  printf("# Started:   %s", ctime(&t0));

  if (sig == SIG_STAT)
  {
    printf("# McStas: Resuming simulation (continue)\n");
    fflush(stdout);
    return;
  }
  else
  if (sig == SIG_SAVE)
  {
    printf("# McStas: Saving data and resume simulation (continue)\n");
    mcsave(NULL);
    fflush(stdout);
    return;
  }
  else
  if (sig == SIG_TERM)
  {
    printf("# McStas: Finishing simulation (save results and exit)\n");
    mcfinally();
    exit(0);
  }
  else
  {
    fflush(stdout);
    perror("# Last I/O Error");
    printf("# McStas: Simulation stop (abort)\n");
    exit(-1);
  }
#undef SIG_SAVE
#undef SIG_TERM
#undef SIG_STAT
#undef SIG_ABRT

}
#endif /* !NOSIGNALS */

/* main raytrace loop */
void *mcstas_raytrace(void *p_node_ncount)
{
  double node_ncount = *((double*)p_node_ncount);
  
  while(mcrun_num < node_ncount || mcrun_num < mcget_ncount())
  {
    mcsetstate(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1);
    /* old init: mcsetstate(0, 0, 0, 0, 0, 1, 0, sx=0, sy=1, sz=0, 1); */
    mcraytrace();
    mcrun_num++;
  }
  return (NULL);
}

/* mcstas_main: McStas main() function. */
int mcstas_main(int argc, char *argv[])
{
/*  double run_num = 0; */
  time_t t;
#ifdef USE_MPI
  char mpi_node_name[MPI_MAX_PROCESSOR_NAME];
  int  mpi_node_name_len;
#endif /* USE_MPI */
#if defined (USE_MPI)
  double mpi_mcncount;
#endif /* USE_MPI */

#ifdef MAC
  argc = ccommand(&argv);
#endif

#ifdef USE_MPI
  MPI_Init(&argc,&argv);
  MPI_Comm_size(MPI_COMM_WORLD, &mpi_node_count); /* get number of nodes */
  MPI_Comm_rank(MPI_COMM_WORLD, &mpi_node_rank);
  MPI_Get_processor_name(mpi_node_name, &mpi_node_name_len);
#endif /* USE_MPI */

/* *** print number of nodes *********************************************** */
  t = (time_t)mcstartdate;
#ifdef USE_MPI
  if (mpi_node_count > 1) {
    MPI_MASTER(
    printf("Simulation %s (%s) running on %i nodes (master is %s, MPI version %i.%i).\n", 
      mcinstrument_name, mcinstrument_source, mpi_node_count, mpi_node_name, MPI_VERSION, MPI_SUBVERSION);
    );
    /* adapt random seed for each node */
    mcseed=(long)(time(&t) + mpi_node_rank); 
    srandom(mcseed); 
    t += mpi_node_rank;
  }
#else /* !USE_MPI */
  mcseed=(long)time(&t);
  srandom(mcseed); 
#endif /* !USE_MPI */
  mcstartdate = t;  /* set start date before parsing options and creating sim file */

/* *** parse options ******************************************************* */
  SIG_MESSAGE("main (Start)");
  mcformat=mcuse_format((char *)(getenv("MCSTAS_FORMAT") ? getenv("MCSTAS_FORMAT") : MCSTAS_FORMAT));
  /* default is to output as McStas format */
  mcformat_data.Name=NULL;
  /* read simulation parameters and options */
  mcparseoptions(argc, argv); /* sets output dir and format */
  if (strstr(mcformat.Name, "NeXus")) {
    if (mcformat_data.Name) mcclear_format(mcformat_data);
    mcformat_data.Name=NULL;
  }
  if (!mcformat_data.Name && strstr(mcformat.Name, "HTML"))
    mcformat_data = mcuse_format("VRML");

/* *** install sig handler, but only once !! after parameters parsing ******* */
#ifndef NOSIGNALS
#ifdef SIGQUIT
  signal( SIGQUIT ,sighandler);   /* quit (ASCII FS) */
#endif
#ifdef SIGABRT
  signal( SIGABRT ,sighandler);   /* used by abort, replace SIGIOT in the future */
#endif
#ifdef SIGTERM
  signal( SIGTERM ,sighandler);   /* software termination signal from kill */
#endif
#ifdef SIGUSR1
  signal( SIGUSR1 ,sighandler);   /* display simulation status */
#endif
#ifdef SIGUSR2
  signal( SIGUSR2 ,sighandler);
#endif
#ifdef SIGHUP
  signal( SIGHUP ,sighandler);
#endif
#ifdef SIGILL
  signal( SIGILL ,sighandler);    /* illegal instruction (not reset when caught) */
#endif
#ifdef SIGFPE
  signal( SIGFPE ,sighandler);    /* floating point exception */
#endif
#ifdef SIGBUS
  signal( SIGBUS ,sighandler);    /* bus error */
#endif
#ifdef SIGSEGV
  signal( SIGSEGV ,sighandler);   /* segmentation violation */
#endif
#endif /* !NOSIGNALS */
  if (!strstr(mcformat.Name,"NeXus")) {
    mcsiminfo_init(NULL); mcsiminfo_close();  /* makes sure we can do that */
  }
  SIG_MESSAGE("main (Init)");
  mcinit();
#ifndef NOSIGNALS
#ifdef SIGINT
  signal( SIGINT ,sighandler);    /* interrupt (rubout) only after INIT */
#endif
#endif /* !NOSIGNALS */

/* ================ main neutron generation/propagation loop ================ */
#if defined (USE_MPI)
  mpi_mcncount = mpi_node_count > 1 ?
    floor(mcncount / mpi_node_count) :
    mcncount; /* number of neutrons per node */
  mcncount = mpi_mcncount;  /* sliced Ncount on each MPI node */
#endif

/* main neutron event loop */
mcstas_raytrace(&mcncount);

#ifdef USE_MPI
 /* merge data from MPI nodes */
  if (mpi_node_count > 1) {
  MPI_Barrier(MPI_COMM_WORLD);
  mc_MPI_Reduce(&mcrun_num, &mcrun_num, 1, MPI_DOUBLE, MPI_SUM, mpi_node_root, MPI_COMM_WORLD);
  }
#endif

/* save/finally executed by master node/thread */
  mcfinally();
  mcclear_format(mcformat);
  if (mcformat_data.Name) mcclear_format(mcformat_data);

#ifdef USE_MPI
  MPI_Finalize();
#endif /* USE_MPI */

  return 0;
} /* mcstas_main */

#endif /* !MCSTAS_H */
