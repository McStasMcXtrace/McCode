from rpy2.robjects import r as R

# load R source file
source = R('source')
source('rplot/eplot.r')

eplot1d = R('eplot1d')
eplot2d = R('eplot2d')


import sys, re
from tempfile import mkstemp


def plotSim(path, logy=False, fmat='eps'):
    raw = file(path).read()
    lines = filter(lambda x: not x.startswith('#'), raw.split('\n'))

    def grab(line):
        match = re.search('# %s: (.*)' % line, raw)
        if match:
            return match.group(1)

    plot_type = grab(r'type')

    title = grab(r'title')
    comp  = grab(r'component')

    vals = grab(r'values').split()
    vals = 'I=%s, Err=%s, N=%s' % tuple(vals)

    stats = grab(r'statistics')

    xlab = grab(r'xlabel')
    ylab = grab(r'ylabel')

    _, tmpf = mkstemp()
    file(tmpf, 'w').write('\n'.join(lines) + '\n')

    # 2d image plot
    if plot_type.startswith('array_2d('):
        # extract extra information for 2d plot
        zlab = grab(r'zlabel')
        xylim = R('c(%s)' % ','.join(grab(r'xylimits').split()))

        # cut away extra rows
        rows, cols = map(int, plot_type[len('array_2d('):-1].split(','))
        lines = lines[:rows]

        # set 2d plotter
        eplot2d('%s (%s)' % (title, comp), '[%s] [%s]' % (vals, stats),
                xlab, ylab, zlab, tmpf,
                '%s.%s' % (path, fmat), logy, fmat, xylim, rows, cols)

    # 1d regular plot
    elif plot_type.startswith('array_1d'):
        file('lines.dat','w').write('\n'.join(lines)+'\n')
        # set 1d plotter
        eplot1d(title='%s (%s)' % (title, comp), subt='[%s] [%s]' % (vals, stats),
                xlab=xlab, ylab=ylab,
                input=tmpf, output='%s.%s' % (path, fmat),
                logy=logy, format=fmat)

if __name__ == '__main__':
    files = sys.argv[1:]
    fmat = 'eps'
    if files[0] == '--png':
        fmat = 'png'
        files = files[1:]

    map(lambda f: plotSim(f, False, fmat), files)
