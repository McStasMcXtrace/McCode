#!/usr/bin/env python2.6

from os import path
from optparse import OptionParser, OptionGroup, OptionValueError


# Helper functions
def build_checker(accept, update, msg='Invalid value'):
    ''' Build checker from accept() and update() functions '''
    def checker(option, _opt_str, value, parser):
        ''' value must be acceptable '''
        if not accept(value):
            raise OptionValueError('option %s: %s (was: "%s")' % (option, msg, value))
        # Update parser with accepted value
        update(parser.values, value)
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
        metavar='FILE',
        help='store optimisation results in FILE '
             '(defaults to: "mcoptim_####.dat")')

    # Misc options
    add('--test',
        action='store_true', default=False,
        help='execute McStas self-test and generate report')

    add('--no-cflags',
        action='store_true', default=False,
        help='disable optimising compiler flags for faster compilation')

    parser.add_option_group(opt)


def add_mcstas_options(parser):
    ''' Add option group for McStas options to parser '''

    opt = OptionGroup(parser, 'Instrument options')
    add = opt.add_option


    # Misc options
    check_seed = build_checker(lambda seed: seed != 0,
                               lambda vs, seed: setattr(vs, 'seed', seed),
                               'SEED cannot be 0')

    add('-s', '--seed',
        metavar='SEED', type=int, action='callback', callback=check_seed,
        help='set random seed (must be: SEED != 0)')

    add('-n', '--ncount',
        metavar='COUNT', type=int,
        help='set number of neutrons to simulate')

    add('-t', '--trace',
        action='store_true', default=False,
        help='enable trace of neutron through instrument')

    add('-g', '--gravitation',
        action='store_true', default=False,
        help='enable gravitation for all trajectories')

    # Data options
    def check_path(opt_name, is_valid):
        ''' Build function for checking existence of a path
            provided for option opt_name '''
        return build_checker(is_valid,
                             lambda vs, path: setattr(vs, opt_name, path),
                             'invalid path')

    check_dir = lambda opt: check_path(opt, path.isdir)
    check_file = lambda opt: check_path(opt, path.isfile)

    add('-d', '--dir',
        metavar='DIR', type=str,
        action='callback', callback=check_dir('dir'),
        help='put all data files in directory DIR')

    add('-f', '--file',
        metavar='FILE', type=str,
        action='callback', callback=check_file('file'),
        help='put all data in a single file')

    add('-a', '--data-only',
        action='store_true', default=False,
        help='Do not put any headers in the data files')

    add('--format',
        metavar='FORMAT',
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


def main():
    ''' Main routine '''

    usage = ('usage: %prog [-cpnN] Instr [-sndftgahi] '
             'params={val|min,max|min,guess,max}...')
    parser = OptionParser(usage, version='mcrun 0.1')

    # Add options
    add_mcrun_options(parser)
    add_mcstas_options(parser)

    # Parse options
    (options, args) = parser.parse_args()
    parser.destroy()

    print options
    print args


if __name__ == '__main__':
    main()
