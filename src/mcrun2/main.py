#!/usr/bin/env python2.6

import logging

from os.path import isfile, isdir, abspath, dirname
from optparse import OptionParser, OptionGroup, OptionValueError
from decimal import Decimal, InvalidOperation

from mcstas import McStas
from optimisation import Scanner, LinearInterval

LOG = logging.getLogger('mcstas')


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
    opt = OptionGroup(parser, 'mcrun options')
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

    # Multiprocessing
    add('--mpi',
        type=int, metavar='NB_CPU',
        help='spread simulation over NB_CPU machines using MPI')

    add('--machines',
        metavar='FILE',
        help='read machine names from FILE (MPI/grid)')

    add('--slave',
        metavar='HOST',
        help='execute simulation on distant HOST (SSH grid)')

    # Optimisation
    add('--optimise',
        metavar='COMP',
        help='Add COMP to the list of monitors to maximise '
             '(optimisation criteria, requires Math::Amoeba)')

    add('--optimise-all',
        action='store_true', default=False,
        help='Maximise all monitors')

    add('--optimise-prec',
        metavar='PREC', type=float, default=1e-3,
        help='relative requested accuracy of criteria (default: 1e-3)')

    add('--optimise-file',
        metavar='FILE', default='mcstas.dat',
        help='store optimisation results in FILE '
             '(defaults to: "mcstas.dat")')

    # Misc options
    add('--test',
        action='store_true', default=False,
        help='execute McStas self-test and generate report')

    add('--no-cflags',
        action='store_true', default=False,
        help='disable optimising compiler flags for faster compilation')

    add('--verbose',
        action='store_true', default=False,
        help='enable verbose output')

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
        metavar='COUNT', type=int, default=1000000,
        help='set number of neutrons to simulate')

    add('-t', '--trace',
        action='store_true', default=False,
        help='enable trace of neutron through instrument')

    add('-g', '--gravitation',
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
                if not dir_exists(dirname(path)):
                    return False
                return not isdir(abspath(path))
        return build_checker(is_valid, 'invalid path')

    add('-d', '--dir',
        metavar='DIR', type=str,
        action='callback', callback=check_file(exist=False),
        help='put all data files in directory DIR')

    add('-f', '--file',
        metavar='FILE', type=str,
        action='callback', callback=check_file(exist=False),
        help='put all data in a single file')

    add('-a', '--data-only',
        action='store_true', default=False,
        help='Do not put any headers in the data files')

    add('--format',
        metavar='FORMAT', default='McStas',
        help='output data files using format FORMAT '
             '(format list obtained from <instr>.out -h)')

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
    if options.mpi > 0:
        options.use_mpi = True
        options.cc = 'mpicc'
        options.mpirun = 'mpirun'
    else:
        options.use_mpi = False
        options.cc = 'gcc'


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
                fixed_params[key] = Decimal(value)
            else:
                LOG.debug('interval: %s', interval)
                intervals[key] = interval
        else:
            LOG.warning('Ignoring invalid parameter: "%s"', param)
    return (fixed_params, intervals)


def main():
    ''' Main routine '''

    # Setup logging
    formatter = logging.Formatter('%(created).2f, %(levelname)8s: %(message)s')

    handler = logging.StreamHandler()
    handler.setLevel(logging.INFO)
    handler.setFormatter(formatter)

    LOG.setLevel(logging.DEBUG)
    LOG.addHandler(handler)

    # Add options
    usage = ('usage: %prog [-cpnN] Instr [-sndftgahi] '
             'params={val|min,max|min,guess,max}...')
    parser = OptionParser(usage, version='mcrun 0.1')

    add_mcrun_options(parser)
    add_mcstas_options(parser)

    # Parse options
    (options, args) = parser.parse_args()
    parser.destroy()
    expand_options(options)

    if options.verbose:
        handler.setLevel(logging.DEBUG)

    # Extract instrument and parameters
    if len(args) == 0:
        raise OptionValueError('No instrument file specified.')
    options.instr = args[0]
    options.params = args[1:]

    # Run McStas
    mcstas = McStas(options.instr)
    mcstas.prepare(options)
    instrs = mcstas.get_info().get_instrument()
    LOG.debug('info: %s', instrs)

    (fixed_params, intervals) = get_parameters(options)

    # Set fixed parameters
    for key, value in fixed_params.items():
        mcstas.set_parameter(key, value)

    # Check for linear scanning
    linear_interval = None

    # Can't both do list and interval scanning
    if options.list and options.numpoints:
        raise OptionValueError('--numpoints cannot be used with --list')

    elif options.list:
        if len(intervals) == 0:
            raise OptionValueError(
                '--list was chosen but no lists was presented.')
        points = len(intervals.values()[0])
        if not(all(map(lambda i: len(i) == points, intervals.values()))):
            raise OptionValueError(
                'All variables much have an equal amount of points.')
        linear_interval = LinearInterval.from_list(
            points, intervals)

    elif options.numpoints is not None:
        if options.numpoints < 2:
            raise OptionValueError(
                ('Cannot scan variable(s) %s using only one data point. '
                 'Please use -N to specify the number of points.') % \
                ', '.join(intervals.keys()))
        # Check that input is valid decimals
        if not all(map(lambda i:
                       len(i) == 2 and
                       all(map(is_decimal, i)), intervals.values())):
            raise OptionValueError('Could not parse intervals.')

        linear_interval = LinearInterval.from_range(
            options.numpoints, intervals)

    # Parameters for linear scanning present
    if linear_interval:
        scanner = Scanner(mcstas, intervals)
        scanner.set_points(linear_interval)
        scanner.run()
    else:
        mcstas.run()


if __name__ == '__main__':
    main()
