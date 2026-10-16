#!/usr/bin/env python3
"""Display simulation results as text in the terminal (single monitor files or whole directories)."""
import argparse
import math
import os
import shutil
import sys
import numpy as np

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from mccodelib.mcplotloader import McCodeDataLoader, Data1D, Data2D

# character sets, following the text backends of MCPL (pymcpltool --backend unicode/ascii)
UNI = dict(fill=' ▁▂▃▄▅▆▇█', shades=' ·░▒▓█', v='│', h='─', corner='└', ytick='┤', xtick='┬')
ASC = dict(fill=' #', shades=' .:-=+*#%@', v='|', h='-', corner='+', ytick='+', xtick='+')
LW = 10  # width of the y axis labels

def fmt(v):
    return '%.4g' % v

def pool(v, n, axis=0):
    ''' shrink v to at most n entries along axis by max-pooling, so narrow peaks survive '''
    m = v.shape[axis]
    if m <= n:
        return v
    return np.maximum.reduceat(v, np.linspace(0, m, n, endpoint=False).astype(int), axis=axis)

def scale(v, log):
    ''' map values to [0,1] (NaN: draw nothing) and give the (top, bottom) labels of the scale:
        linear from min(0, vmin) to vmax, log over the positive values only '''
    if log:
        pos = v[v > 0]
        if not pos.size:
            return np.full(v.shape, np.nan), ('1', '1')
        lo, hi = np.log10(pos.min()), np.log10(pos.max())
        if hi <= lo:
            lo, hi = lo - 1, hi + 1
        with np.errstate(divide='ignore', invalid='ignore'):
            t = np.where(v > 0, np.log10(np.where(v > 0, v, 1)), np.nan)
        return (t - lo) / (hi - lo), (fmt(10**hi), fmt(10**lo))
    lo, hi = min(0., v.min()), v.max()
    if hi <= lo:
        hi = lo + 1
    return (v - lo) / (hi - lo), (fmt(hi), fmt(lo))

def jet(t):
    return '%d;%d;%d' % tuple(int(255 * np.clip(1.5 - abs(4 * t - k), 0, 1)) for k in (3, 2, 1))

def halfblock(a, b):
    ''' one character showing pixel a above pixel b (NaN: terminal background) '''
    if np.isnan(a):
        return '\033[0m ' if np.isnan(b) else '\033[0;38;2;%sm▄' % jet(b)
    return '\033[0;38;2;%sm▀' % jet(a) if np.isnan(b) else '\033[38;2;%s;48;2;%sm▀' % (jet(a), jet(b))

def frame(rows, ncols, ylabels, x0, x1, xlabel, c):
    ''' add y axis (labels at top and bottom) and x axis (labels at the ends and the middle) '''
    out = []
    for i, r in enumerate(rows):
        lab = ylabels[0] if i == 0 else ylabels[1] if i == len(rows) - 1 else ''
        out.append('%*s %s%s' % (LW, lab, c['ytick'] if lab else c['v'], r))
    out.append(' ' * (LW + 1) + c['corner'] + c['xtick'] + c['h'] * (ncols - 2) + c['xtick'])
    axis = [' '] * (ncols + 2)
    def put(text, pos):
        pos = max(0, min(len(axis) - len(text), pos))
        axis[pos:pos + len(text)] = text
    a, m, b = fmt(x0), fmt((x0 + x1) / 2), fmt(x1)
    put(a, 0)
    put(b, ncols + 1 - len(b))
    if ncols > len(a) + len(m) + len(b) + 4:
        put(m, (ncols + 1) // 2 - len(m) // 2)
    out.append(' ' * (LW + 1) + ''.join(axis).rstrip())
    out.append(' ' * (LW + 2) + xlabel.center(ncols).rstrip())
    return out

def header(data):
    lines = ['%s [%s] %s' % (data.component, data.filename, data.title)]
    if len(data.values) == 3:
        lines.append('I = %s  Err = %s  N = %s' % tuple(data.values))
    if data.statistics:
        lines.append(data.statistics.strip())
    return lines

def plot1d(data, ncols, height, log, c, color):
    y = pool(np.asarray(data.yvals, dtype=float), ncols)
    t, ylabels = scale(y, log)
    nlev = len(c['fill']) - 1
    units = [0 if np.isnan(v) else round(v * height * nlev) for v in t]
    rows = [''.join(c['fill'][min(nlev, max(0, u - r * nlev))] for u in units) for r in range(height - 1, -1, -1)]
    return ['(%s)' % data.ylabel] + frame(rows, len(t), ylabels, *data.xlimits, data.xlabel, c)

def plot2d(data, ncols, height, log, c, color):
    z = pool(pool(np.asarray(data.zvals, dtype=float), ncols, axis=1), height * (2 if color else 1), axis=0)
    z = np.flipud(z)  # row 0 of zvals is ymin, the terminal draws top-down
    t, (zhi, zlo) = scale(z, log)
    x0, x1, y0, y1 = data.xlimits
    if color:
        if len(t) % 2:
            t = np.vstack([t, np.full(t.shape[1], np.nan)])
        rows = [''.join(halfblock(a, b) for a, b in zip(top, bot)) + '\033[0m' for top, bot in zip(t[0::2], t[1::2])]
        zscale = '%s %s\033[0m %s' % (zlo, ''.join('\033[48;2;%sm ' % jet(i / 29) for i in range(30)), zhi)
    else:
        nlev = len(c['shades']) - 1
        rows = [''.join(c['shades'][max(1, math.ceil(v * nlev)) if raw > 0 else 0] for v, raw in zip(tr, zr))
                for tr, zr in zip(t, z)]  # non-zero cells always visible
        zscale = "'%s' from %s to %s" % (c['shades'], zlo, zhi)
    return (['(y: %s)' % data.ylabel] + frame(rows, t.shape[1], (fmt(y1), fmt(y0)), x0, x1, data.xlabel, c)
            + [zscale + (' (log scale)' if log else '')])

def main(args):
    simfile = args.simulation[0] if args.simulation else '.'
    if os.path.isdir(simfile):
        simfile = os.path.join(simfile, 'mccode.sim')
    if not os.path.isfile(simfile):
        sys.exit(simfile + ' is neither a .sim file or directory, exiting')

    loader = McCodeDataLoader(simfile=simfile)
    try:
        loader.load()
    except Exception as e:
        sys.exit('mcplot loader: ' + str(e))

    # Unicode unless asked for ASCII or the output encoding can not show it (e.g. legacy Windows code pages)
    c = ASC if args.ascii else UNI
    try:
        ''.join(c.values()).encode(sys.stdout.encoding or 'ascii')
    except (UnicodeEncodeError, LookupError):
        c = ASC
    color = c is UNI and sys.stdout.isatty() and not args.no_color and 'NO_COLOR' not in os.environ
    if color and os.name == 'nt':
        os.system('')  # enables ANSI escape sequences in the legacy Windows console
    width = max(40, args.width or min(100, shutil.get_terminal_size((80, 24)).columns))
    ncols = width - LW - 3
    height = max(5, args.height or 15)

    for data in loader.plot_graph.getdata_lst():
        if type(data) not in (Data1D, Data2D):
            continue
        if args.monitor and not any(m in data.filename or m in data.component for m in args.monitor):
            continue
        plot = plot1d if type(data) is Data1D else plot2d
        print('\n'.join(header(data) + plot(data, ncols, height, args.log, c, color)) + '\n')

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('simulation', nargs='*', help='file or directory to plot (default: current directory)')
    parser.add_argument('-l', '--log', action='store_true', help='enable logscale on plot')
    parser.add_argument('-m', '--monitor', nargs='+', help='only plot monitors whose filename or component name contains one of these strings')
    parser.add_argument('-W', '--width', type=int, help='plot width in characters (default: terminal width, up to 100)')
    parser.add_argument('-H', '--height', type=int, help='plot height in lines (default: 15)')
    parser.add_argument('--ascii', action='store_true', help='plain ASCII characters only (no Unicode, no colour)')
    parser.add_argument('--no-color', action='store_true', help='draw 2D maps with shading characters instead of colours (default when not writing to a terminal, or NO_COLOR is set)')
    main(parser.parse_args())
