#!/usr/bin/env python
#
#  mcplot-gnuplot program. 
#
#  Use --noqt to omit loading of the qt gui interface (for non-qt, non-Perl systems).
#
import os
import argparse
import logging
from mcgnuplotter import McGnuplotter

def main_noqt(args):
    logging.basicConfig(level=logging.INFO)
    
    if args.noqt:
        # 0 - handle sim file
        sim_file = "mccode.sim"
        if args.simulation:
            simulation = args.simulation[0]
            logging.debug('simulation file/dir: %s', simulation)
            
            if os.path.isdir(simulation):
                simulation = os.path.join(simulation, 'mccode.sim')
                
            if os.path.splitext(simulation)[1] == '.sim':
                sim_file = simulation
            else:
                # 1 - plot single
                print('Plot single monitor')
                dat_file = simulation
                plotter = McGnuplotter(dat_file)
                plotter.plot()
                exit()
    
        # check sim file
        if not os.path.isfile(sim_file):
            print('Sim file not found')
            exit()
    
        # 2 - multiplot
        print('Using sim file: %s' % os.path.abspath(sim_file)) 
        plotter = McGnuplotter(sim_file)
        plotter.plot()
    else:
        print('loading qt gui...')
        exec('import mcgnuview')
        exec('mcgnuview.startGui()')
        

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('simulation', nargs='*', help='simulation (.sim) or monitor (.dat) file, or directory')
    parser.add_argument('--noqt', action="store_true", default=False)
    args = parser.parse_args()

    main_noqt(args)
