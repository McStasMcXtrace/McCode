from numpy import array
from chaco.api import ArrayPlotData

def parse_monitor(it):
    '''
    Parse the data from a line iterator representing a mcstas monitor file
    > parse_monitor(open("monitor1.dat"))
    '''

    header = {}
    data = []

    for line in it:
        if line.startswith('#'):
            # extend header
            k, v = line[1:].strip().split(':', 1)
            header[k] = v.lstrip()
        else:
            data.append(line.split())

    return header, data



def monitor_to_plotdata(it):
    ''' Parse a mcstas monitor file file and convert it to a chaco plotdata object '''
    header, data = parse_monitor(it)

    # build plotdata object
    lines = {}
    for i, name in enumerate(header['variables'].split()):
        lines[name] = array(map(lambda line: line[i], data), dtype='float64')
    plotdata = ArrayPlotData(**lines)

    # compute value_low and value_high using I_err
    i, i_err = plotdata['I'], plotdata['I_err']
    plotdata['value_low'] = i - i_err
    plotdata['value_high'] = i + i_err

    return plotdata
