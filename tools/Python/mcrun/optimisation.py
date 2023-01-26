from os.path import basename
from log import McRunException, getLogger
from datetime import datetime
from decimal import Decimal
from mccode import McStasResult
from os.path import join 

LOG = getLogger('optimisation')


def build_header(options, params, intervals, detectors):
    template = """
# Instrument-source: '%(instr)s'
# Date: %(date)s
# Ncount: %(ncount)i
# Numpoints: %(numpoints)i
# Param: %(params)s
# type: %(type)s
# title: %(title)s
# xlabel: '%(xvars)s'
# ylabel: 'Intensity'
# xvars: %(xvars)s
# yvars: %(yvars)s
# xlimits: %(xmin)s %(xmax)s
# filename: %(filename)s
# variables: %(variables)s
    """.strip()

    # Date format: Fri Aug 26 12:21:39 2011
    date = datetime.strftime(datetime.now(), '%a %b %d %H %M %Y')

    xvars = ', '.join(params)
    lst = intervals[list(params)[0]]
    xmin = min(lst)
    xmax = max(lst)
    # Get Numpoints from length of -L list
    N = len(lst)
    # ... or using options.numponts if in fact a normal scan
    if options.numpoints:
        N = options.numpoints

    # TODO: figure out correct scan type
    title = 'Scan of %s' % xvars
    scantype = 'multiarray_1d(%d)' % N

    variables = list(params)
    for detector in detectors:
        variables += [detector + '_I', detector + '_ERR']

    values = {
        'instr': options.instr,
        'date': date,

        'ncount': options.ncount,
        'numpoints': N,

        'params': ', '.join('%s = %s' % (xvar, intervals[xvar][0])
                            for xvar in params),
        'type': scantype,
        'title': title,

        'xvars': xvars,
        'yvars': ' '.join('(%s_I,%s_ERR)' % (d, d) for d in detectors),

        'xmin': xmin,
        'xmax': xmax,

        'filename': basename(options.optimise_file),
        'variables': ' '.join(variables),
    }
    
    result = (template % values) + '\n'
    return result

def build_mccodesim_header(options, params, intervals, detectors, version):
    template = """
begin instrument:
  Creator: %(version)s
  Source: %(instr)s
  Parameters:  %(xvars)s
  Trace_enabled: %(istrace)s
  Default_main: yes
  Embedded_runtime: yes
end instrument

begin simulation
Date: %(date)s
Ncount: %(ncount)i
Numpoints: %(scanpoints)i
Param: %(params)s
end simulation

begin data
type: multiarray_1d(%(scanpoints)i)
title: Scan of %(xvars)s
xvars: %(xvars)s
yvars: %(yvars)s
xlabel: '%(xvars)s'
ylabel: 'Intensity'
xlimits: %(xmin)s %(xmax)s
filename: %(filename)s
variables: %(variables)s
end data
    """.strip()
    
    # Date format: Fri Aug 26 12:21:39 2011
    date = datetime.strftime(datetime.now(), '%a %b %d %H %M %Y')

    xvars = ', '.join(params)
    lst = intervals[list(params)[0]]
    xmin = min(lst)
    xmax = max(lst)

    # TODO: figure out correct scan type
    title = 'Scan of %s' % xvars
    scantype = 'multiarray_1d(%d)' % options.numpoints

    variables = list(params)
    for detector in detectors:
        variables += [detector + '_I', detector + '_ERR']
    
    istrace = 'no'
    if options.trace:
        istrace = 'yes'
    
    values = {
        'instr': options.instr,
        'date': date,

        'ncount': options.ncount,
        'scanpoints': options.numpoints,

        'params': ', '.join('%s = %s' % (xvar, intervals[xvar][i]) for xvar in params for i in range(len(intervals[xvar]))),
        'type': scantype,
        'title': title,

        'xvars': xvars,
        'yvars': ' '.join('(%s_I,%s_ERR)' % (d, d) for d in detectors),

        'xmin': xmin,
        'xmax': xmax,

        'filename': basename(options.optimise_file) or 'mccode.dat',
        'variables': ' '.join(variables),
        
        'version': version,
        'istrace': istrace
    }
    
    result = (template % values) + '\n'
    return result

def point_at(N, key, minmax, step):
    """ Helper to compute the point for key at step """
    low, high = map(Decimal, minmax)
    return step * (high - low) / Decimal(N - 1) + low


class LinearInterval:
    """ Intervals for linear scanning """

    @staticmethod
    def from_range(N, intervals):
        for step in range(N):
            yield dict((key, point_at(N, key, intervals[key], step))
                       for key in intervals)

    @staticmethod
    def from_list(N, intervals):
        for step in range(N):
            yield dict((key, intervals[key][step]) for key in intervals)


class MultiInterval:
    """ Points for multi-dimensional scanning """

    @staticmethod
    def from_range(N, intervals):
        # base case: no intervals yields empty dict
        if len(intervals) == 0:
            yield {}
            return
        # recursively generate the multi dict
        intervals = intervals.copy()
        key, minmax = intervals.popitem()
        for step in range(N):
            point = point_at(N, key, minmax, step)
            for dic in MultiInterval.from_range(N, intervals):
                dic[key] = point
                yield dic


class InvalidInterval(McRunException):
    pass


class Scanner:
    def __init__(self, mcstas, intervals):
        self.mcstas = mcstas
        self.intervals = intervals
        self.points = None
        self.outfile = mcstas.options.optimise_file
        self.simfile = join(mcstas.options.dir, 'mccode.sim')

    def set_points(self, points):
        self.points = points

    def set_outfile(self, path):
        self.outfile = path

    def run(self):
        LOG.info('Running Scanner, result file is "%s"' % self.outfile)

        if len(self.intervals) == 0:
            raise InvalidInterval('No interval range specified')
        
        # get file handles
        fid = open(self.outfile, 'w')        
        mccodesim = open(self.simfile, 'w')
        
        wrote_header = False
        
        # each run will be in "dir/1", "dir/2", ...
        mcstas_dir = self.mcstas.options.dir
        if mcstas_dir == '':
            mcstas_dir ='.'
        
        for i, point in enumerate(self.points):
            par_values = []
            for key in self.intervals:
                self.mcstas.set_parameter(key, point[key])
                LOG.debug("%s: %s", key, point[key])
                par_values.append(point[key])
            
            is_decimal = lambda x: type(x) == Decimal
            to_string = (lambda x: x if not is_decimal(x) else ( '%f' % x if abs(x)>1e-4 else '%e' % x))

            LOG.info(', '.join('%s: %s' % (a, to_string(b))
                               for (a, b) in point.items()))
            
            # Change sub-directory as an extra option (dir/1 -> dir/2)
            current_dir = '%s/%i' % (mcstas_dir, i)
            out = self.mcstas.run(pipe=True, extra_opts={'dir': current_dir})

            dets = McStasResult(out).get_detectors()

            if not wrote_header:
                fid.write(build_header(self.mcstas.options,
                   self.intervals.keys(), self.intervals,
                   [det.name for det in dets]))
                mccodesim.write(build_mccodesim_header(self.mcstas.options,
                   self.intervals.keys(), self.intervals,
                   [det.name for det in dets], version=self.mcstas.version))
                wrote_header = True

            dets_vals = ['%s %s' % (d.intensity, d.error) for d in dets]
            line = '%s %s\n' % (' '.join(map(str, par_values)), ' '.join(dets_vals))
            fid.write(line)
            fid.flush()
        
        fid.close()
        mccodesim.close()
