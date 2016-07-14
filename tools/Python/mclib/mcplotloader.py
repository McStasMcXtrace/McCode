'''
functionality for loading mccode data into suitable data types, 
and assembling it in a plot-friendly way.
'''
from os.path import isfile, isdir
from flowchart import *

# mcplot
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

# flowchart decision functions - probe disk contents, does not open files
def has_filename(args):
    #os.path.isfile(args['simfile'])
    return False

def is_mccodesim_or_mccodedat(args):
    return False

def is_monitorfile(args):
    return False

def is_sweepfolder(args):
    #folder = args['simfile']
    #os.pah.isdir(folder)
    return False

def is_broken_sweepfolder(args):
    return False

def is_sweep_data_present(args):
    return False

def is_mccodesim_w_monitors(args):
    return False

def has_datfile(args):
    return False

def has_multiple_datfiles(args):
    return False

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
    ''' implements the mccode data load cases as a flow chart '''
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
        exit_node = control.process(args=(self.simfile))
        
        print exit_node.key

        # TODO: assemble data graph
        self.data_graph = None



