#!/usr/bin/env python3
#
#  mcplot-gnuplot program. 
#
#  Use --noqt to omit loading of the qt gui interface (for non-qt, non-Perl systems).
#
import os
import argparse
import logging
from mcgnuplotter import McGnuplotter

def main(args):
    # ensure keyboardinterrupt ctr-c
    import signal
    signal.signal(signal.SIGINT, signal.SIG_DFL)
    
    logging.basicConfig(level=logging.INFO)
    
    if args.simulation:
        simulation = args.simulation[0]
    else:
        simulation = 'mccode.sim'
    logging.debug('simulation file/dir: %s', simulation)
    
    if os.path.isdir(simulation):
        simulation = os.path.join(simulation, 'mccode.sim')
        simulation = os.path.abspath(simulation)
    
    if not os.path.isfile(simulation):
        print('Sim file not found')
        exit()
        
    if os.path.splitext(simulation)[1] == '.sim':
        print('Using sim file: %s' % simulation) 
        plotter = McGnuplotter(simulation, noqt=args.noqt, log_scale=args.logscale)
    else:
        dat_file = simulation
        print('Plot single monitor')
        plotter = McGnuplotter(dat_file, noqt=args.noqt, log_scale=args.logscale)
    
    # set terminal
    if args.term:
        keys = plotter.getDataKeys()
        print('Setting gnuplot term %s' % args.term)
        for key in keys:
            plotter.setTerm(key,  args.term)
    
    # save only this then exit
    if args.save:
        keys = plotter.getDataKeys()
        if args.term:
            print('Saving %s using %s (or default)' % (simulation, args.term))
            plotter.save(keys[0], term=args.term, ext=args.term)
        else:
            plotter.save(keys[0])
        exit()
    # plot only this
    elif args.noqt:
        keys = plotter.getDataKeys()
        plotter.plot(keys[0])
    # load qt gui
    else:
        print('Loading qt gui...')
        exec('import mcgnuview')
        exec('mcgnuview.startGui(plotter, log_scale=args.logscale)')

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('simulation', nargs='*', help='simulation file (.sim), monitor file (.dat), directory, or none (current dir)')
    parser.add_argument('-n', '--noqt', action="store_true", default=False, help='plot simulation in gnuplot and exit')
    parser.add_argument('-l', '--logscale',  action="store_true", default=False, help='enables log scale at startup')
    parser.add_argument('-s', '--save',  action='store_true', default=False, help='save to file, but don\'t plot (defaults to png)')
    parser.add_argument('-t', '--term',  action='store', dest='term', help='set the gnuplot terminal used to display or save')
    args = parser.parse_args()

    main(args)
