#!/usr/bin/env python3
'''
mcplopp: plot McStas/McXtrace output with plopp (scipp).

UI:      mcplopp.py <dir|mccode.sim|monitor.dat>       (opens JupyterLab)
Static:  mcplopp.py <dir|mccode.sim|monitor.dat> -o out.png [--log]
'''
import argparse
import json
import math
import os
import re
import subprocess
import sys

import numpy as np
try:
    import scipp as sc
    import plopp as pp
except ImportError as e:
    sys.exit(f'mcplot-plopp: {e}. scipp and plopp are required for this tool to function '
             '(plus ipympl and jupyterlab for the interactive UI), e.g.:\n'
             '  conda install -c conda-forge plopp ipympl jupyterlab\n'
             '  python3 -m pip install plopp ipympl jupyterlab')

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))
from mccodelib.mcplotloader import McCodeDataLoader, Data1D, Data2D
from mccodelib.plotgraph import PNMultiple

UNITS = {'AA': 'angstrom', 'Angs': 'angstrom', 'meV': 'meV', 'deg': 'deg', 'us': 'us', 'cm': 'cm', 'm': 'm', 's': 's'}


def _edges(label, lo, hi, n):
    ''' bin-edge coord from an axis label like "Wavelength [AA]"; unknown units stay in the label '''
    m = re.search(r'\s*\[(.*?)\]', label)
    unit = UNITS.get(m.group(1)) if m else None
    dim = label.replace(m.group(0), '').strip() if unit else label.strip()
    return dim, sc.linspace(dim, lo, hi, n + 1, unit=unit or '')


def to_dataarray(d):
    ''' Data1D/Data2D -> DataArray with bin edges; 0D/event lists -> None '''
    if isinstance(d, Data1D):
        x, ex = _edges(d.xlabel, *d.xlimits, len(d.yvals))
        da = sc.DataArray(sc.array(dims=[x], values=d.yvals, variances=np.square(d.y_err_vals)), coords={x: ex})
    elif isinstance(d, Data2D):
        z = np.array(d.zvals)  # rows are y: (ny, nx), as in the matplotlib frontend
        x, ex = _edges(d.xlabel, *d.xlimits[:2], z.shape[1])
        y, ey = _edges(d.ylabel, *d.xlimits[2:], z.shape[0])
        if y == x:
            y += ' (y)'
            ey = ey.rename_dims({x: y})
        # ponytail: no variances for 2D, mcplotloader skips the "# Errors" block
        da = sc.DataArray(sc.array(dims=[y, x], values=z), coords={x: ex, y: ey})
    else:
        return None
    da.name = 'I'  # colorbar/y label; the title goes in the load() key
    return da


def load(path):
    ''' {name: DataArray}. Scans stack each monitor over the scan parameter (1D->2D, 2D->3D). '''
    loader = McCodeDataLoader(simfile=path)
    loader.load()
    root = loader.plot_graph
    secs = root.get_secondaries()
    if not (secs and isinstance(secs[0], PNMultiple)):  # single run or single monitor
        ds = root.getdata_lst()
        return {f'{d.title.strip()} [{i}]': to_dataarray(d) for i, d in enumerate(ds) if isinstance(d, (Data1D, Data2D))}

    scan = root.getdata_idx(0)  # mccode.dat curve: xvals are the scan points
    out = {}
    for i, sec in enumerate(secs):
        steps = [to_dataarray(d) for d in sec.getdata_lst()]
        if steps[0] is None:
            continue
        da = sc.concat(steps, dim=scan.xvar)
        # ponytail: failed scan steps leave no subfolder; fall back to step index if counts differ
        vals = scan.xvals if len(scan.xvals) == len(steps) else np.arange(len(steps))
        da.coords[scan.xvar] = sc.array(dims=[scan.xvar], values=vals)
        out[f'{sec.getdata_idx(0).title.strip()} [{i}]'] = da
    return out


def scan_slicer(da, log=False):
    ''' Jupyter (ipympl) only: step through a stacked scan monitor '''
    return pp.slicer(da, keep=list(da.dims[1:]),  # load() puts the scan dim first
                     enable_player=True, mode='single', autoscale=log, norm='log' if log else 'linear')


def ui(path):
    ''' Jupyter (%matplotlib widget): pick a monitor; scans get the step player, log via the plot toolbar '''
    import ipywidgets as w
    from IPython.display import display
    das = load(path)
    pick = w.Dropdown(options=list(das), description='monitor', layout={'width': '40em'})
    out = w.Output()

    def show(_=None):  # ponytail: one figure at a time, so 100+ monitor instruments stay responsive
        da = das[pick.value]
        out.clear_output(wait=True)
        with out:
            display(scan_slicer(da) if not da.coords.is_edges(da.dims[0]) else pp.plot(da, title=pick.value))
    pick.observe(show, 'value')
    show()
    return w.VBox([pick, out])


def launch(call):
    ''' write a one-cell notebook running `call` next to this tool's import, open it in JupyterLab '''
    src = f"%matplotlib widget\nimport sys; sys.path.insert(0, {os.path.dirname(os.path.abspath(__file__))!r})\n{call}"
    nb = {'nbformat': 4, 'nbformat_minor': 4, 'metadata': {'kernelspec': {'name': 'python3', 'display_name': 'Python 3', 'language': 'python'}},
          'cells': [{'cell_type': 'code', 'metadata': {}, 'execution_count': None, 'outputs': [], 'source': src}]}
    name = os.path.splitext(os.path.basename(__file__))[0] + '.ipynb'
    with open(name, 'w') as f:
        json.dump(nb, f)
    print(f'wrote {name}; run its cell in JupyterLab')
    subprocess.run([sys.executable, '-m', 'jupyter', 'lab', name])


def figures(das, log=False):
    ''' static figures; 3D scan monitors become one tile per step '''
    norm = 'log' if log else 'linear'
    figs = []
    for name, da in das.items():
        if da.ndim == 3:
            scan_dim = da.dims[0]
            figs += [pp.plot(da[scan_dim, j], norm=norm, title=f'{name} {scan_dim}={da.coords[scan_dim][j].value:g}')
                     for j in range(da.sizes[scan_dim])]
        else:
            figs.append(pp.plot(da, norm=norm, title=name))
    return figs


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument('simfile', nargs='?', default='.', help='output dir, mccode.sim, or a monitor file')
    p.add_argument('-o', '--output', help='write a static png/pdf/svg instead of opening the UI')
    p.add_argument('--log', action='store_true', help='log intensity scale')
    args = p.parse_args()
    if not args.output:
        return launch(f'import mcplopp; mcplopp.ui({os.path.abspath(args.simfile)!r})')

    figs = figures(load(args.simfile), args.log)
    ncols = math.ceil(math.sqrt(len(figs)))
    nrows = math.ceil(len(figs) / ncols)
    tiled = pp.tiled(nrows, ncols, figsize=(4 * ncols, 3 * nrows))
    for k, f in enumerate(figs):
        tiled[k // ncols, k % ncols] = f
    tiled.save(args.output)
    print(f'saved {args.output}')


if __name__ == '__main__':
    main()
