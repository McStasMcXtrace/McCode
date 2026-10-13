#!/usr/bin/env python3
'''
mcdisplopp: draw a McStas/McXtrace instrument and traced rays with plopp (scipp).

UI:      mcdisplopp.py <instr> [-n 100] [param=value ...]   (opens JupyterLab, 3D)
Static:  mcdisplopp.py <instr> [-n 100] [param=value ...] -o out.png   (2D projections)
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
                   else d.get_points_on_circle(24) if isinstance(d, DrawCircle) else None)  # 60 by default; 24 is round enough
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


_G = (1 + 5 ** 0.5) / 2  # icosahedron: the sphere at each polyline vertex
ICO_V = np.array([(-1, _G, 0), (1, _G, 0), (-1, -_G, 0), (1, -_G, 0), (0, -1, _G), (0, 1, _G),
                  (0, -1, -_G), (0, 1, -_G), (_G, 0, -1), (_G, 0, 1), (-_G, 0, -1), (-_G, 0, 1)]) / np.hypot(1, _G)
ICO_F = np.array([(0, 11, 5), (0, 5, 1), (0, 1, 7), (0, 7, 10), (0, 10, 11), (1, 5, 9), (5, 11, 4),
                  (11, 10, 2), (10, 7, 6), (7, 1, 8), (3, 9, 4), (3, 4, 2), (3, 2, 6), (3, 6, 8),
                  (3, 8, 9), (4, 9, 5), (2, 4, 11), (6, 2, 10), (8, 6, 7), (9, 8, 1)])


def _tubes(xyz, r, k=6):
    ''' NaN-separated polylines -> (vertices, faces): a k-sided open cylinder per segment, a sphere per corner '''
    a, b = xyz[:-1], xyz[1:]
    keep = np.isfinite(a).all(1) & np.isfinite(b).all(1) & (np.linalg.norm(b - a, axis=1) > 0)
    a, b = a[keep], b[keep]
    d = (b - a) / np.linalg.norm(b - a, axis=1, keepdims=True)
    u = np.cross(d, np.where(abs(d[:, :1]) < 0.9, [1., 0, 0], [0., 1, 0]))  # helper axis not parallel to d
    u /= np.linalg.norm(u, axis=1, keepdims=True)
    v = np.cross(d, u)
    th = 2 * np.pi * np.arange(k) / k
    ring = r * (np.cos(th)[None, :, None] * u[:, None] + np.sin(th)[None, :, None] * v[:, None])  # (n, k, 3)
    cyl = np.concatenate([a[:, None] + ring, b[:, None] + ring], axis=1).reshape(-1, 3)
    j, j1 = np.arange(k), (np.arange(k) + 1) % k
    side = np.concatenate([np.stack([j, j1, k + j], 1), np.stack([j1, k + j1, k + j], 1)])
    cyl_f = (side[None] + 2 * k * np.arange(len(a))[:, None, None]).reshape(-1, 3)

    d1, d2 = xyz[1:-1] - xyz[:-2], xyz[2:] - xyz[1:-1]  # joints only where the polyline turns > ~20 deg;
    with np.errstate(invalid='ignore', divide='ignore'):  # NaN breaks, ends and smooth arcs get none
        cos = (d1 * d2).sum(1) / (np.linalg.norm(d1, axis=1) * np.linalg.norm(d2, axis=1))
    pts = xyz[1:-1][cos < 0.94]
    sph = (pts[:, None] + r * ICO_V[None]).reshape(-1, 3)
    sph_f = (ICO_F[None] + len(cyl) + 12 * np.arange(len(pts))[:, None, None]).reshape(-1, 3)
    return np.concatenate([cyl, sph]), np.concatenate([cyl_f, sph_f])


def view3d(geom, rays, thickness=2.5e-4):
    ''' 3D (Jupyter/pythreejs): components as tubes coloured per component, rays as thinner grey tubes.
        Tube radius = thickness x the instrument's bounding-box diagonal; rays get half of it '''
    allpts = np.concatenate([rays] + list(geom.values()))
    r = thickness * np.linalg.norm(np.nanmax(allpts, 0) - np.nanmin(allpts, 0))
    parts = [(_tubes(xyz, r), i % 20) for i, xyz in enumerate(geom.values())] + [(_tubes(rays, r / 2), np.nan)]
    verts, faces, cols, off = [], [], [], 0
    for (vv, ff), c in parts:
        verts.append(vv), faces.append(ff + off), cols.append(np.full(len(vv), c, dtype=float))
        off += len(vv)
    return pp.mesh3d(vertices=sc.vectors(dims=['vertices'], values=np.concatenate(verts), unit='m'),
                     faces=sc.array(dims=['faces', 'vertex'], values=np.concatenate(faces)),
                     vertexcolors=sc.array(dims=['vertices'], values=np.concatenate(cols)),
                     cmap='tab20', cmin=0, cmax=19, nan_color='lightgrey', figsize=(900, 600))


def ui(instr, n=100, params=''):
    ''' Jupyter: 3D view; edit params / ray count, re-trace on click '''
    import ipywidgets as w
    from IPython.display import display
    pars = w.Text(value=params, description='params', placeholder='lambda=2.36 ...', layout={'width': '40em'})
    rays = w.IntText(value=n, description='rays')
    go = w.Button(description='Trace', button_style='primary')
    thick = w.FloatLogSlider(value=2.5e-4, base=10, min=-5, max=-2, description='thickness', readout_format='.1e')
    out = w.Output()
    last = {}

    def draw(_=None):  # redraws the last trace, no mcrun
        out.clear_output(wait=True)
        with out:
            display(view3d(*last['trace'], thick.value))

    def run(_=None):
        go.disabled = True
        try:
            with out:
                last['trace'] = trace(instr, rays.value, pars.value.split())
            draw()
        finally:
            go.disabled = False
    go.on_click(run)
    thick.observe(draw, 'value')
    run()
    return w.VBox([w.HBox([pars, rays, go, thick]), out])


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
