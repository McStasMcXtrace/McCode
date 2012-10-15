from numpy import array, log, maximum, max
from chaco.array_plot_data import ArrayPlotData

from os.path import dirname, join, isdir

from plotting import PlotDesc


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

    dtype = header['type']
    if dtype.startswith('array_1d'):
        # 1d
        # populate with point data
        plotdata = ArrayPlotData()
        for i, name in enumerate(header['variables'].split()):
            plotdata[name] = array(map(lambda line: line[i], data), dtype='float64')

        # compute error bar value_low and value_high using I_err
        i, i_err = plotdata['I'], plotdata['I_err']
        plotdata['value_error'] = i_err
        plotdata['value_low'] = i - i_err
        plotdata['value_high'] = i + i_err

        return (header, plotdata)
    if dtype.startswith('array_2d'):
        # 2d
        w, h = map(int, dtype[len('array_2d'):].strip('() ').split(','))

        data = array(data[:h], dtype='float64')
        plotdata = ArrayPlotData(imagedata=data, imagedata_log=log(maximum(1, data)))

        return (header, plotdata)
    else:
        print 'Skipping unknown type:', header['type']
        return (header, None)


def mcstas_to_plotdescs(path):
    ''' Parse mcstas.sim and give plot descs for all instrument files in it '''
    # try /mcstas.sim if path is a directory
    if isdir(path):
        path = join(path, 'mcstas.sim')

    # extract instrument files from mcstas.sim
    files = []
    fname = None
    for line in file(path):
        sline = line.strip()
        if sline.startswith('begin component'):
            fname = None
        elif sline.startswith('filename:'):
            fname = sline.split()[-1]
        elif sline.startswith('end component') and fname is not None:
            files.append(fname)

    # parse instrument files
    base = dirname(path)
    for fname in files:
        print fname
        header, plotdata = monitor_to_plotdata(file(join(base, fname)))
        if plotdata is not None:
            yield PlotDesc(header['xvar'], 'I', plotdata, title=fname, type='line')
