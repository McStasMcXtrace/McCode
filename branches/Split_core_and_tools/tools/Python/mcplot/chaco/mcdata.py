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

    lines = {}
    for i, name in enumerate(header['variables'].split()):
        lines[name] = map(lambda line: float(line[i]), data)

    return ArrayPlotData(**lines)
