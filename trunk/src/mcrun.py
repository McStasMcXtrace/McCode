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
    print "Running mcstas -t " + opts.instrfile
    os.system("mcstas -t " + opts.instrfile)
    
def run_cc(opts):
    print "Running gcc -lm -O2 " + opts.cfile + " -o " +  opts.binfile
    os.system("gcc -lm -O2 " + opts.cfile + " -o " +  opts.binfile)
    
def singlesim(opts, args):
    get_binary(opts)
    cmd = opts.binfile
    for j in range(0,len(opts.parms)):
        cmd = cmd + " " + opts.parms[j]
    print "Performing single simulation: " + cmd 
    os.system(cmd)
    
def scan(opts, args):
    print "Scanning"

def optim(opts,args):
    print "Optimising"


if __name__ == "__main__":
    from optparse import OptionParser
    usage =         "Usage: %prog [-cpnN] Instr [-sndftgahi] params={val|min,max|min,guess,max}\n\n"
    usage = usage + "  This program both runs mcstas with Instr and the C compiler to build an\n"
    usage = usage + "  independent simulation program. The following environment variables may be\n"
    usage = usage + "  specified for building the instrument:\n"
    usage = usage + "  MCSTAS        Location of the McStas and component library\n"
    usage = usage + "  MCSTAS_CC     Name of the C compiler               (gcc)\n"
    usage = usage + "  MCSTAS_CFLAGS Options for compilation              (-g -O2)\n"
    usage = usage + "  MCSTAS_FORMAT Default FORMAT to use for data files (PGPLOT)\n"
    parser = OptionParser(usage=usage)
    parser.add_option("-c", "--force-compile", action="store_true", dest="compile", help="Force rebuilding of instrument.")
    parser.add_option("-p", "--param", dest="param", help="Read parameters from FILE.", metavar="FILE")
    parser.add_option("-n","--ncount", help="Set number of neutrons to simulate.", metavar="NCOUNT", dest="NCOUNT")
    parser.add_option("-N","--numpoints", dest="NP", metavar="NP", help="Set number of scan points.")
    parser.add_option("--grid", action="store_true",dest="NB_GRID",help="Spawn simulations to multiple machine/cores grid.")
    parser.add_option("--mpi",dest="NB_MPI",action="store_true",help="Spread simulation over NB_CPU machines using MPI.")
    parser.add_option("--machines", dest="MACHINES", help="Read machine names from file MACHINES (MPI/grid).", metavar="MACHINES")
    parser.add_option("--optim", dest="COMP",help="Add COMP to the list of monitors to maximize.\nIf COMP not given will maximise all monitors")
    parser.add_option("--optim-prec", dest="PREC", help="Relative requested accuracy of criteria (1e-3)")    
    parser.add_option("--optim-file", dest="OPTFILE", help="Defines filename for storing optim results.\n(Defaults to \"mcoptim_XXXX.dat\")")
    parser.add_option("--test", help="Execute McStas selftest and generate report", action="store_true", dest="test")
    parser.add_option("-s","--seed",dest="SEED", help="Set random seed (must be != 0)")
    parser.add_option("-d", "--dir", dest="DIR", help="Put all data files in directory DIR.")
    parser.add_option("-f","--file", dest="FILE", help="Put all data in a single file.")
    parser.add_option("-t","--trace", action="store_true", help="Enable trace of neutron through instrument.")
    parser.add_option("-g","--gravitation", action="store_true", help="Enable gravitation for all trajectories.")
    parser.add_option("-a","--data-only", action="store_true", help="Do not put any headers in the data files.")
    parser.add_option("--no-output-files", action="store_true", help="Do not write any data files.")
    parser.add_option("-i","--info", action="store_true", help="Detailed instrument information.")
    parser.add_option("--format", dest="FORMAT", help="Output data files using format FORMAT. (format list obtained from <instr>.out -h)")

    (options, args) = parser.parse_args()

    filenames = list()
    options.parms = list()
    # Determine Instrument filename from args list
    for arg in args:
        print arg
        equ = string.find(arg, "=")
        if string.find(arg, "=") >= 0:
             options.parms.append(arg);
        else:
             filenames.append(arg)
    
    if len(filenames) > 1:
        print "Only a single instrument definition may be given - you gave:\n"
        for j in range(0,len(filenames)):
            print str(j+1) + "): " + filenames[j]
            
        exit()
    
    options.instrfile=filenames[0]

    if (not(os.path.isfile(options.instrfile))):
         print "Sorry, instrumentfile " + options.instrfile + " does not exist. Exiting"
         exit()

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

#    print options
#    print args
    
    
    
    
# Further stuff - read config (before parsing input args?)
# mcrun_single for performing a single sim / step / opt..
# Checking if run is scan or optim - functions


# The following have been left out for now...
#              --multi=NB_CPU     see the documentation for more info.
#    --slave=HOST               Execute simulation on distant HOST (SSH grid)

    

# End of __main__
