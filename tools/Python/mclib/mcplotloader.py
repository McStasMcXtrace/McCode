'''
functionality for loading mccode data into suitable data types, 
and assembling it in a plot-friendly way.
'''
import glob
from os.path import isfile, isdir, join, dirname, basename, walk, splitext

from flowchart import *

'''
McCode sim output data types, pythonified.
'''
class Data1D(object):
    '''  '''
    pass

class Data2D(object):
    '''  '''
    pass

def load1DMonitor():
    '''  '''
    return Data1D()

def load2DMonitor():
    '''  '''
    return Data2D()

''' 
Flowchart decision functions.

NOTE: These functions probe disk contents, but they do not open any files, so 
all decisions are based on file exists, folder exists and filename.
Any errors are therefore encountered only at the actual load data step.

They assume that args['simfilee'] is a string that was input by the user (possibly empty).

They are implemented in a context-dependent way! Each one
can have assumptions in its implementation depending on its position in the flow chart.
'''
def has_filename(args):
    f = args['simfile']
    return isfile(f)

def is_mccodesim_or_mccodedat(args):
    f = basename(args['simfile'])
    return (f == 'mccode.sim' or f == 'mccode.dat') and isfile(f)

def is_monitorfile(args):
    f = args['simfile']
    ext = splitext(f)[1]
    return ext == '.dat' and isfile(f)

def is_sweepfolder(args):
    folder = args['simfile']
    if not isdir(folder):
        return False
    dotsim = join(folder, 'mccode.sim')
    dotdat = join(folder, 'mccode.dat')
    return isfile(dotsim) and isfile(dotdat)

def is_broken_sweepfolder(args):
    ''' not implemented (returns trivial answer) '''
    return False

def is_sweep_data_present(args):
    ''' not implemented '''
    raise Exception('is_sweep_data_present has not been not implemented.')

def is_mccodesim_w_monitors(args):
    f = args['simfile']
    # cover cases where f is a folder without forward slash, in which case dirname returns an empty string
    if not isfile(f):
        if not f == '':
            f = f + '/'
    # checks mccode.sim existence
    if not isfile(join(f, 'mccode.sim')):
        return False
    # assume f is mccode.sim
    d = dirname(f)
    datfiles = glob.glob(join(d, '*.dat'))
    return len(datfiles) > 0

def has_datfile(args):
    # assume folder
    d = args['simfile']
    datfiles = glob.glob(join(d, '*.dat'))
    return len(datfiles) > 0

def has_multiple_datfiles(args):
    # assume folder
    d = args['simfile']
    datfiles = glob.glob(join(d, '*.dat'))
    return len(datfiles) > 1

def test_decfuncs(simfile):
    args = {}
    args['simfile'] = simfile
    
    print 'has_filename:              %s' % str(has_filename(args))
    print 'is_mccodesim_or_mccodedat: %s' % str(is_mccodesim_or_mccodedat(args))
    print 'is_monitorfile:            %s' % str(is_monitorfile(args))
    print 'is_sweepfolder:            %s' % str(is_sweepfolder(args))
    #print 'is_broken_sweepfolder:     %s' % str(is_broken_sweepfolder(args))
    #print 'is_sweep_data_present:     %s' % str(is_sweep_data_present(args))
    print 'is_mccodesim_w_monitors:   %s' % str(is_mccodesim_w_monitors(args))
    print 'has_datfile:               %s' % str(has_datfile(args))
    print 'has_multiple_datfiles:     %s' % str(has_multiple_datfiles(args))

# terminal functions - loads data files, assembles and returns a plot data graph
def load_monitor(args):
    pass

def load_simulation(args):
    pass

def load_sweep(args):
    pass

def load_sweep_b(args):
    pass

def load_sweep_c(args):
    pass

def load_monitor_folder(args):
    pass

class McPlotDataLoader():
    ''' assembly and execution of mccode data loader flowchart '''
    def __init__(self, simfile):
        '''  '''
        self.simfile = simfile
    
    def load(self):
        ''' loads mccode data and assembles the plotable data graph '''
        
        # exit terminals
        exit_error = FCNTerminal(key="error")
        exit_case1 = FCNTerminal(key="case1")
        exit_case2 = FCNTerminal(key="case2")
        exit_case3 = FCNTerminal(key="case3")
        exit_case3b = FCNTerminal(key="case3b")
        exit_case3c = FCNTerminal(key="case3c")
        exit_case4 = FCNTerminal(key="case4")
        # decision nodes assembled in backwards order
        dec_multiplefiles = FCNDecisionBool(eval_fct=has_multiple_datfiles, node_T=exit_case4, node_F=exit_case1)
        dec_hasdatfile = FCNDecisionBool(eval_fct=has_datfile, node_T=dec_multiplefiles, node_F=exit_error)
        dec_ismccodesimwmonitors = FCNDecisionBool(eval_fct=is_mccodesim_w_monitors, node_T=exit_case2, node_F=dec_hasdatfile)
        dec_datafolderspresent = FCNDecisionBool(eval_fct=is_sweep_data_present, node_T=exit_case3b, node_F=exit_case3c)
        dec_isbrokensweep = FCNDecisionBool(eval_fct=is_broken_sweepfolder, node_T=dec_datafolderspresent, node_F=dec_ismccodesimwmonitors)
        dec_issweepfolder = FCNDecisionBool(eval_fct=is_sweepfolder, node_T=exit_case3, node_F=dec_isbrokensweep)
        dec_ismonitor = FCNDecisionBool(eval_fct=is_monitorfile, node_T=exit_case1, node_F=exit_error)
        dec_ismccodesimordat = FCNDecisionBool(eval_fct=is_mccodesim_or_mccodedat, node_T=dec_issweepfolder, node_F=dec_ismonitor)
        dec_hasfilename = FCNDecisionBool(eval_fct=has_filename, node_T=dec_ismccodesimordat, node_F=dec_issweepfolder)
        # enter node
        enter_simfile = FCNTerminal(node_next=dec_hasfilename)
        
        # get the "args" which is just the simfile, a string, which may correspond to a file or a folder
        args = {}
        args['simfile'] = self.simfile
        
        # traverse the flow chart
        control = FlowChartControl(terminal_enter=enter_simfile)
        exit_node = control.process(args=args)
        
        print
        print 'flowchart exit terminal: %s' % exit_node.key

        # TODO: assemble data graph
        self.data_graph = None



