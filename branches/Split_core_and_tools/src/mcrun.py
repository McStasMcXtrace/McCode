#!/usr/bin/env python
import sys
import os
import string

def get_binary(opts):
    if not(os.path.isfile(opts.cfile)) or opts.compile == True:
        run_mcstas(opts)
    if not(os.path.isfile(opts.binfile)) or opts.compile == True:
        run_cc(opts)
    # Check timestamps, run again if instrfile or cfile newer than binfile
    

def run_mcstas(opts):
    command = "%s -t %s" % (opts.mcstas, opts.instrfile)
    if opts.verbose:
        print "Running '%s'" % command
    os.system(command)
    
def run_cc(opts):
    command = "%s -lm %s -o %s" % (opts.cc, opts.cfile, opts.binfile)
    if opts.verbose:
        print "Running '%s'" % command
    os.system(command)
    
def singlesim(opts, args):
    get_binary(opts)
    cmd = opts.binfile
    for j in range(0,len(opts.parms)):
        cmd = cmd + " " + opts.parms[j]
    if opts.verbose:
        print "Performing single simulation: " + cmd 
    os.system(cmd)
    
def scan(opts, args):
    if opts.verbose:
        print "Scanning"

def optim(opts,args):
    if opts.verbose:
        print "Optimising"


if __name__ == "__main__":
    import optparse
    usage =  "Usage: %prog [-cpnN] Instr [-sndftgahi] params={val|min,max|min,guess,max}\n\n"
    usage += "  This program both runs mcstas with Instr and the C compiler to build an\n"
    usage += "  independent simulation program. The following environment variables may be\n"
    usage += "  specified for building the instrument:\n"
    usage += "  MCSTAS        Location of the McStas and component library\n"
    #usage += "  MCSTAS_CC     Name of the C compiler               (gcc)\n"
    #usage += "  MCSTAS_CFLAGS Options for compilation              (-g -O2)\n"
    usage += "  MCSTAS_FORMAT Default FORMAT to use for data files (PGPLOT)\n"

    parser = optparse.OptionParser(usage=usage)

    # Set defaults
    parser.set_defaults(
            cc = 'gcc',
            cflags = '-g -O2',
            verbose = False,
            compile = True,
            mcstas = "mcstas"
            )

    # Find default mcstas to use.
    if os.path.exists("./mcstas") and os.path.isfile("./mcstas"):
        parser.set_defaults(mcstas = "./mcstas")
    elif os.path.exists("./src/mcstas") and os.path.isfile("./src/mcstas"):
        parser.set_defaults(mcstas = "./src/mcstas")

    # Add options
    parser.add_option("--verbose", "-v",
            help = "Show what we're doing.",
            action = "store_true")
    #parser.add_option("-c", "--force-compile",
    #        action="store_true", dest="compile", help="Force rebuilding of instrument.")
    parser.add_option("-p", "--param",
            dest="param", help="Read parameters from FILE.", metavar="FILE")
    parser.add_option("-n", "--ncount",
            help="Set number of neutrons to simulate.", metavar="NCOUNT", dest="NCOUNT")
    parser.add_option("-N", "--numpoints",
            dest="NP", metavar="NP", help="Set number of scan points.")
    parser.add_option("--grid",
            action="store_true",dest="NB_GRID",help="Spawn simulations to multiple machine/cores grid.")
    parser.add_option("--mpi",
            dest="NB_MPI",action="store_true",help="Spread simulation over NB_CPU machines using MPI.")
    parser.add_option("--machines",
            dest="MACHINES", help="Read machine names from file MACHINES (MPI/grid).", metavar="MACHINES")
    parser.add_option("--optim",
            dest="COMP",help="Add COMP to the list of monitors to maximize.\nIf COMP not given will maximise all monitors")
    parser.add_option("--optim-prec",
            dest="PREC", help="Relative requested accuracy of criteria (1e-3)")    
    parser.add_option("--optim-file",
            dest="OPTFILE", help="Defines filename for storing optim results.\n(Defaults to \"mcoptim_XXXX.dat\")")
    parser.add_option("--test",
            help="Execute McStas selftest and generate report", action="store_true", dest="test")
    parser.add_option("-s","--seed",
            dest="SEED", help="Set random seed (must be != 0)")
    parser.add_option("-d", "--dir",
            dest="DIR", help="Put all data files in directory DIR.")
    parser.add_option("-f","--file",
            dest="FILE", help="Put all data in a single file.")
    parser.add_option("-t","--trace",
            action="store_true", help="Enable trace of neutron through instrument.")
    parser.add_option("-g","--gravitation",
            action="store_true", help="Enable gravitation for all trajectories.")
    parser.add_option("-a","--data-only",
            action="store_true", help="Do not put any headers in the data files.")
    parser.add_option("--no-output-files",
            action="store_true", help="Do not write any data files.")
    parser.add_option("-i","--info",
            action="store_true", help="Detailed instrument information.")
    parser.add_option("--format",
            dest="FORMAT", help="Output data files using format FORMAT. (format list obtained from <instr>.out -h)")


    optcc = optparse.OptionGroup(parser, "Controlling the C compilation.")
    #optcc.add_option("--nocflags",
    #        dest = "use_cflags",
    #        help = "Do not use CFLAGS (slightly faster compilation)",
    #        action = "store_false", default=True)
    #optcc.add_option("--keep-c", "-k",
    #        help = "Keep the intermediate C file.",
    #        action = "store_true", default=False)
    optcc.add_option("--cc",
            help = "C compiler to use.")
    optcc.add_option("--cflags",
            help = "Flags passed to the C compiler.")
    parser.add_option_group(optcc)



    (options, args) = parser.parse_args()

    filenames = list()
    options.parms = list()
    # Determine Instrument filename from args list
    for arg in args:
        equ = string.find(arg, "=")
        if string.find(arg, "=") >= 0:
             options.parms.append(arg);
        else:
             filenames.append(arg)
    
    if len(filenames) == 0:
        parser.error("No instrument file given.")

    if len(filenames) > 1:
        parser.error("Too many instruments given. (%s)" % \
                ", ".join(["'%s'" % fn for fn in filenames]))
    
    options.instrfile=filenames[0]

    if not os.path.isfile(options.instrfile):
        parser.error("Sorry, instrumentfile '%s' does not exist." %
                options.instrfile)

    # Add . if no path is in the filename on posix
    if os.name == 'posix' and os.path.dirname(options.instrfile) == '':
        options.instrfile = os.path.join('.',options.instrfile)

    # Strip off suffix if any
    fileparts = options.instrfile.split(".")
    options.cfile = ''
    options.binfile = ''
    if len(fileparts)>1:
        for j in range(0,len(fileparts)-1):
            options.cfile = options.cfile + fileparts[j] + "."
            options.binfile = options.binfile + fileparts[j] + "."
        options.cfile = options.cfile + "c"
        options.binfile = options.binfile + "out"
    else:
        options.cfile = options.instrfile + '.c'
        options.binfile = options.instrfile + '.out'
        
    # First, check if -c was given
    if options.compile == True:
        get_binary(options)

    # Determine if this is optim / scan / single sim
    if options.COMP != None or options.PREC != None or options.OPTFILE != None:
        optim(options, args)
    elif options.NP != None:
        scan(options, args)
    else:
        singlesim(options,args)

