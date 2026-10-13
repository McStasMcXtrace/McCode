#!/usr/bin/env python3
'''
mcdisplopp: draw a McStas/McXtrace instrument and traced rays with plopp (scipp).

UI:      mcdisplopp.py <instr> [-n 100] [param=value ...]   (opens JupyterLab)
Static:  mcdisplopp.py <instr> [-n 100] [param=value ...] -o out.png
'''
import argparse
import json
import os
import subprocess
import sys
import warnings

import numpy as np
import scipp as sc
import plopp as pp

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))
from mccodelib.mcdisplayutils import McDisplayReader
from mccodelib.instrgeom import Vector3d, DrawLine, DrawMultiline, DrawCircle

warnings.filterwarnings('ignore', 'The input contains a coordinate with unsorted', RuntimeWarning)  # polylines, not histograms
BREAK = [(np.nan,) * 3]  # NaN between polylines lifts the pen, so one line per component


def _xyz(v):
    return v.x, v.y, v.z


def _polylines(pls):
    return np.array([p for pl in pls for p in list(pl) + BREAK]).reshape(-1, 3)


def trace(instr, n=100, params=()):
    ''' run mcrun --trace (unset params take defaults); returns ({component: (N,3) xyz}, (M,3) ray xyz), NaN-separated '''
    reader = McDisplayReader(instr=instr, n=n, options=list(params), no_mpi=True, default=True)
    instrument = reader.read_instrument()
    bundle = reader.read_particles()

    geom = {}
    for c in instrument.components:
        pls = []
        for d in c.drawcalls:
            # ponytail: wireframe primitives only (as mcdisplay-pyqtgraph); --trace=2 solids are skipped
            pts = ([d.point_1, d.point_2] if isinstance(d, DrawLine) else d.points if isinstance(d, DrawMultiline)
                   else d.get_points_on_circle() if isinstance(d, DrawCircle) else None)
            if pts:
                pls.append([_xyz(c.transform.apply(p)) for p in pts])
        if pls:
            geom[c.name] = _polylines(pls)

    tf = {c.name: c.transform for c in instrument.components}
    ray_pls = [[_xyz(tf[g.compname].apply(Vector3d(*e.args[:3]))) for g in story.groups if g.compname in tf
                for e in g.events] for story in (bundle.rays if bundle else [])]
    return geom, _polylines(ray_pls)


def _projection(xyz, h, v):
    ''' 1D DataArray: values = vertical axis, coord = horizontal axis (McStas: z beam, y up) '''
    ax = 'xyz'
    return sc.DataArray(sc.array(dims=[h], values=xyz[:, ax.index(v)], unit='m'),
                        coords={h: sc.array(dims=[h], values=xyz[:, ax.index(h)], unit='m')})


def view(geom, rays):
    ''' top (z-x) above side (z-y) projection; one colour per component, rays in grey '''
    z = np.concatenate([rays[:, 2]] + [xyz[:, 2] for xyz in geom.values()])
    pad = 0.02 * (np.nanmax(z) - np.nanmin(z))  # plopp's autoscale overshoots on the NaN separators
    figs = []
    for v in 'xy':
        lines = {'rays': _projection(rays, 'z', v)}  # first, so components draw on top
        lines.update({name: _projection(xyz, 'z', v) for name, xyz in geom.items()})
        figs.append(pp.plot(lines, linestyle='-', marker='', legend=False,
                            color={'rays': 'lightgrey'}, ylabel=f'{v} [m]', title=f'z-{v}',
                            xmin=np.nanmin(z) - pad, xmax=np.nanmax(z) + pad))
    return figs[0] / figs[1]


def ui(instr, n=100, params=''):
    ''' Jupyter (%matplotlib widget): edit params / ray count, re-trace on click '''
    import ipywidgets as w
    from IPython.display import display
    pars = w.Text(value=params, description='params', placeholder='lambda=2.36 ...', layout={'width': '40em'})
    rays = w.IntText(value=n, description='rays')
    go = w.Button(description='Trace', button_style='primary')
    out = w.Output()

    def run(_=None):
        go.disabled = True
        out.clear_output(wait=True)
        try:
            with out:
                display(view(*trace(instr, rays.value, pars.value.split())))
        finally:
            go.disabled = False
    go.on_click(run)
    run()
    return w.VBox([w.HBox([pars, rays, go]), out])


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


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument('instr', help='.instr or compiled instrument')
    p.add_argument('params', nargs='*', help='instrument parameters, name=value')
    p.add_argument('-n', type=int, default=100, help='number of rays to trace')
    p.add_argument('-o', '--output', help='write a static png/pdf/svg instead of opening the UI')
    args = p.parse_args()
    if not args.output:
        return launch(f'import mcdisplopp; mcdisplopp.ui({os.path.abspath(args.instr)!r}, {args.n}, {" ".join(args.params)!r})')

    view(*trace(args.instr, args.n, args.params)).save(args.output)
    print(f'saved {args.output}')


if __name__ == '__main__':
    main()
