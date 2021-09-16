#!/usr/bin/env python3
from os import mkdir
from os.path import isfile, isdir, abspath, dirname, basename, join
from shutil import copyfile
from optparse import OptionParser, OptionGroup, OptionValueError
from decimal import Decimal, InvalidOperation
from datetime import datetime

from mccode import McStas, Process
from optimisation import Scanner, LinearInterval, MultiInterval

#import config
import sys
sys.path.append(join(dirname(__file__), '..'))
from mccodelib import mccode_config

from log import getLogger, setupLogger, setLogLevel, McRunException
from log import DEBUG

LOG = getLogger('main')

# File path friendly date format (avoid ':' and white space)
DATE_FORMAT_PATH = "%Y%m%d_%H%M%S"

# Helper functions
def build_checker(accept, msg='Invalid value'):
    ''' Build checker from accept() function '''
    def checker(option, _opt_str, value, parser):
        ''' value must be acceptable '''
        if not accept(value):
            raise OptionValueError('option %s: %s (was: "%s")' % \
                                   (option, msg, value))
        # Update parser with accepted value
        setattr(parser.values, option.dest, value)
    return checker


def add_mcrun_options(parser):
    ''' Add option group for McRun options to parser '''

    # McRun options
    opt = OptionGroup(parser, '%s options' % (mccode_config.configuration["MCRUN"]) )
    add = opt.add_option

    add('-c', '--force-compile',
        action='store_true',
        help='force rebuilding of instrument')

    add('-p', '--param',
        metavar='FILE',
        help='read parameters from file FILE')

    add('-N', '--numpoints',
        type=int, metavar='NP',
        help='set number of scan points')

    add('-L', '--list',
        action='store_true',
        help='use a fixed list of points for linear scanning')

    add('-M', '--multi',
        action='store_true',
        help='run a multi-dimensional scan')

    add('--autoplot',
        action='store_true',
        help='open plotter on generated dataset')
    
    add('--autoplotter',
        action='store',
        type=str,
        help='specify the plotter used with --autoplot')

    add('--embed',
        action='store_true', default=True,
        help='store copy of instrument file in output directory')
    
    # Multiprocessing
    add('--mpi',
        metavar='NB_CPU',
        help='spread simulation over NB_CPU machines using MPI')
    
    add('--machines',
        metavar='machines',
        help='defines path of MPI machinefile to use in parallel mode')

    # Optimisation
    add('--optimise-file',
        metavar='FILE',
        help='store scan results in FILE '
             '(defaults to: "mccode.dat")')

    add('--no-cflags',
        action='store_true', default=False,
        help='disable optimising compiler flags for faster compilation')

    add('--verbose',
        action='store_true', default=False,
        help='enable verbose output')

    add('--write-user-config',
        action='store_true', default=False,
        help='generate a user config file')

    add('--override-config',
        metavar='PATH', default=False,
        help='Load config file from specific dir')

    parser.add_option_group(opt)


def add_mcstas_options(parser):
    ''' Add option group for McStas options to parser '''

    opt = OptionGroup(parser, 'Instrument options')
    add = opt.add_option

    # Misc options
    check_seed = build_checker(lambda seed: seed != 0,
                               'SEED cannot be 0')

    add('-s', '--seed',
        metavar='SEED', type=int, action='callback', callback=check_seed,
        help='set random seed (must be: SEED != 0)')

    add('-n', '--ncount',
        metavar='COUNT', type=float, default=1000000,
        help='set number of %s to simulate' % (mccode_config.configuration["PARTICLE"]) )

    add('-t', '--trace',
        action='store_true', default=False,
        help='enable trace of %s through instrument' % (mccode_config.configuration["PARTICLE"]) )
    if (mccode_config.configuration["MCCODE"]=='mcstas'):
        add('-g', '--gravitation', '--gravity',
            action='store_true', default=False,
            help='enable gravitation for all trajectories')

    # Data options
    dir_exists = lambda path: isdir(abspath(path))

    def check_file(exist=True):
        ''' Validate the path to a file '''
        if exist:
            is_valid = isfile
        else:
            def is_valid(path):
                ''' Ensure path to file exists and filename is provided '''
                if path == "." or path == "./" or path == ".\\":
                    return True
                if not dir_exists(dirname(path)):
                    return False
                return not isdir(abspath(path))
        return build_checker(is_valid, 'invalid path')

    add('-d', '--dir',
        metavar='DIR', type=str,
        action='callback', callback=check_file(exist=False),
        help='put all data files in directory DIR')

    add('--format',
        metavar='FORMAT', default='McCode',
        help='output data files using format FORMAT '
             '(format list obtained from <instr>.%s -h)' % mccode_config.platform["EXESUFFIX"])

    add('--no-output-files',
        action='store_true', default=False,
        help='Do not write any data files')

    # Information
    add('-i', '--info',
        action='store_true', default=False,
        help='Detailed instrument information')

    parser.add_option_group(opt)


def expand_options(options):
    ''' Add extra options based on previous choices '''
    # McCode version and library
    options.mccode_bin = mccode_config.configuration['MCCODE']
    options.mccode_lib = mccode_config.configuration['MCCODE_LIB_DIR']

    # MPI
    if options.mpi is not None:
        options.use_mpi = True
        options.cc      = mccode_config.compilation['MPICC']
        options.mpirun  = mccode_config.compilation['MPIRUN']
    else:
        options.use_mpi = False
        options.cc = mccode_config.compilation['CC']

    # Output dir
    if options.dir is None:
        instr = options.instr
        instr = instr.endswith('.instr') and instr[:-6] or instr
        # use unique directory when unspecified
        options.dir = "%s_%s" % \
                      (basename(instr),
                       datetime.strftime(datetime.now(), DATE_FORMAT_PATH))
        # alert user
        LOG.info('No output directory specified (--dir)')
    # Output file
    if options.optimise_file is None:
        # use mccode.dat when unspecified
        options.optimise_file = '%s/mccode.dat' % options.dir


def is_decimal(string):
    ''' Check if string is parsable as decimal/float '''
    try:
        Decimal(string)
        return True
    except InvalidOperation:
        return False


def get_parameters(options):
    ''' Get fixed and scan/optimise parameters '''
    fixed_params = {}
    intervals = {}

    for param in options.params:
        if '=' in param:
            key, value = param.split('=', 1)
            interval = value.split(',')
            # When just one point is present, fix as constant
            if len(interval) == 1:
                fixed_params[key] = value
            else:
                LOG.debug('interval: %s', interval)
                intervals[key] = interval
        else:
            LOG.warning('Ignoring invalid parameter: "%s"', param)
    return (fixed_params, intervals)


def find_instr_file(instr):
    # Remove [-mpi].out to avoid parsing a binary file
    instr = clean_quotes(instr)
    if instr.endswith("-mpi." + mccode_config.platform['EXESUFFIX']):
        instr = instr[:-(5 + len(mccode_config.platform['EXESUFFIX']))]
    if instr.endswith("." + mccode_config.platform['EXESUFFIX']):
        instr = instr[:-(1 + len(mccode_config.platform['EXESUFFIX']))]

    # Append ".instr" if needed
    if not isfile(instr) and isfile(instr + ".instr"):
        instr += ".instr"

    return instr


def clean_quotes(string):
    ''' Remove all leading and ending quotes (" and \') '''
    return string.strip('"' + "'")


def main():
    ''' Main routine '''
    setupLogger()

    # Add options
    usage = ('usage: %prog [-cpnN] Instr [-sndftgahi] '
             'params={val|min,max|min,guess,max}...')
    parser = OptionParser(usage, version=mccode_config.configuration['MCCODE_VERSION'])

    add_mcrun_options(parser)
    add_mcstas_options(parser)

    # Parse options
    (options, args) = parser.parse_args()

    # Write user config file and exit
    if options.write_user_config:
        mccode_config.save_user_config()
        quit()

    # Override system and user level config files if prompted
    if options.override_config:
        mccode_config.load_config(options.override_config)
        mccode_config.check_env_vars()
        
    
    # Extract instrument and parameters
    if len(args) == 0:
        print(parser.get_usage())
        parser.exit()

    # Set path of instrument-file after locating it
    options.instr = find_instr_file(args[0])

    if options.param:
        # load params from file 
        text = open(options.param).read()
        import re
        params = re.findall('[\w0-9]+=[^=\s]+', text)
        options.params = map(clean_quotes, params)
    else:
        # Clean out quotes (perl mcgui requires this step)
        options.params = map(clean_quotes, args[1:])

    # On windows, ensure that backslashes in the filename are escaped
    if sys.platform == "win32":
        options.instr = options.instr.replace("\\","\\\\")
        
    # Fill out extra information
    expand_options(options)

    if options.verbose:
        setLogLevel(DEBUG)

    # Inform user of what is happening
    # TODO: More info?
    LOG.info('Using directory: "%s"' % options.dir)
    if options.dir == "." or options.dir == "./" or options == ".\\":
        LOG.warning('Existing files in "%s" will be overwritten!' % options.dir)
        LOG.warning(' - and datafiles catenated...')
        options.dir = '';

    # Run McStas
    mcstas = McStas(options.instr)
    mcstas.prepare(options)

    (fixed_params, intervals) = get_parameters(options)

    # Indicate end of setup / start of computations
    LOG.info('===')

    if options.info:
        mcstas.run(override_mpi=False)
        exit()

    # Set fixed parameters
    for key, value in fixed_params.items():
        mcstas.set_parameter(key, value)

    # Check for linear scanning
    interval_points = None

    # Can't both do list and interval scanning
    if options.list and options.numpoints:
        raise OptionValueError('--numpoints cannot be used with --list')

    if options.list:
        if len(intervals) == 0:
            raise OptionValueError(
                '--list was chosen but no lists was presented.')
        pointlist=list(intervals.values())
        points = len(pointlist[0])
        if not(all(map(lambda i: len(i) == points, intervals.values()))):
            raise OptionValueError(
                'All variables much have an equal amount of points.')
        interval_points = LinearInterval.from_list(
            points, intervals)

    scan = options.multi or options.numpoints
    if ((options.numpoints is not None and options.numpoints < 2)
        or (scan and options.numpoints is None)):
        raise OptionValueError(
            ('Cannot scan variable(s) %s using only one data point. '
             'Please use -N to specify the number of points.') % \
            ', '.join(intervals.keys()))
        # Check that input is valid decimals
        if not all(map(lambda i:
                       len(i) == 2 and
                       all(map(is_decimal, i)), intervals.values())):
            raise OptionValueError('Could not parse intervals -- result: %s'
                                   % str(intervals))

    if options.multi is not None:
        interval_points = MultiInterval.from_range(
            options.numpoints, intervals)

    elif options.numpoints is not None:
        interval_points = LinearInterval.from_range(
            options.numpoints, intervals)

    # Parameters for linear scanning present
    if interval_points:
        scanner = Scanner(mcstas, intervals)
        scanner.set_points(interval_points)
        if (not options.dir == ''):
            mkdir(options.dir)
        scanner.run()
    else:
        # Only run a simulation if we have a nonzero ncount
        if not options.ncount == 0.0:
            mcstas.run()

    if isdir(options.dir):
        LOG.info('Placing instr file copy %s in dataset %s',options.instr,options.dir)
        copyfile(options.instr, join(options.dir,basename(options.instr)))

    if options.autoplot is not None:
        autoplotter = mccode_config.configuration['MCPLOT']
        # apply selected autoplotter, if used
        if options.autoplotter is not None:
            autoplotter = options.autoplotter
        if isdir(options.dir):
            LOG.info('Running plotter %s on dataset %s',mccode_config.configuration['MCPLOT'],options.dir)
            Process(autoplotter).run([options.dir])

if __name__ == '__main__':
    try:
        mccode_config.load_config("user")
        mccode_config.check_env_vars()
        
        main()
    except KeyboardInterrupt:
        LOG.fatal('User interrupt.')
    except OptionValueError as e:
        LOG.fatal(str(e))
    except McRunException as e:
        LOG.fatal(str(e))
