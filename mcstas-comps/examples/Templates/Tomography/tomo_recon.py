#!/usr/bin/env python3
"""Reconstruct a volume from a Tomography.instr omega scan, e.g.

    mcrun Tomography.instr omega=0,350 -N36 -n1e7 -d TomoScan
    python tomo_recon.py TomoScan

Filtered back-projection (ramp filter with Matlab iradon's windows, default
'hann', optional --cutoff), or with --sirt N: N iterations of SIRT with
non-negativity, which suppresses FBP streaks for few/noisy projections.
--median K applies a KxK median filter to each projection first. Reads text
or NeXus (mcrun --format=NeXus, mccode.h5) scans. Shows 3 central slices and
a 3D isosurface (marching cubes, threshold slider). --save writes vol.nxs (NeXus/HDF5 NXdata,
readable by e.g. ParaView, Fiji, silx, nexusformat) and surface.png on close.
Python port of tools/matlab/tomo_recon.m (PW, 20080620).
"""
import argparse, glob, os
import numpy as np
import scipy.sparse as sp
from scipy.ndimage import median_filter
import matplotlib.pyplot as plt
from matplotlib.widgets import Slider
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
from pyqtgraph.functions import isosurface  # numpy marching cubes, pyqtgraph is a McCode dep


def header(path, key):
    for line in open(path):
        if line.startswith('# ' + key):
            return line[len(key) + 2:].strip()


def projector(n, theta):
    """Sparse (nproj*n, n*n) linear-interpolation ray-sum matrix; A.T back-projects."""
    x = np.arange(n) - (n - 1) / 2
    X, Y = np.meshgrid(x, -x)
    pix = np.arange(n * n)
    r, c, v = [], [], []
    for k, t in enumerate(np.deg2rad(theta)):
        u = (X * np.cos(t) + Y * np.sin(t)).ravel() + (n - 1) / 2
        ok = (u >= 0) & (u <= n - 1)                    # like np.interp(left=0, right=0)
        i = np.minimum(np.floor(u[ok]).astype(int), n - 2)
        w = u[ok] - i
        r += [k * n + i, k * n + i + 1]; c += [pix[ok]] * 2; v += [1 - w, w]
    return sp.csr_matrix((np.concatenate(v), (np.concatenate(r), np.concatenate(c))),
                         shape=(len(theta) * n, n * n))


WINDOWS = dict(ramlak=lambda g: 1, shepplogan=np.sinc, cosine=lambda g: np.cos(np.pi * g),
               hamming=lambda g: 0.54 + 0.46 * np.cos(2 * np.pi * g),
               hann=lambda g: (1 + np.cos(2 * np.pi * g)) / 2)


def fbp(b, A, n, window='hann', cutoff=1.0):
    """b: (nproj*n, nslice) sinograms -> (n*n, nslice)."""
    pad = 1 << int(np.ceil(np.log2(2 * n)))
    f = np.fft.fftfreq(pad)
    g = np.abs(f) / cutoff
    h = 2 * np.abs(f) * WINDOWS[window](g) * (g <= 0.5)
    q = b.reshape(-1, n, b.shape[1])
    q = np.fft.ifft(np.fft.fft(q, pad, axis=1) * h[:, None], axis=1).real[:, :n]
    return A.T @ q.reshape(b.shape) * np.pi / (2 * q.shape[0])


def sirt(b, A, iters):
    R = 1 / np.maximum(A.sum(1).A, 1e-12)
    C = 1 / np.maximum(A.sum(0).A.T, 1e-12)
    v = np.zeros((A.shape[1], b.shape[1]))
    for _ in range(iters):
        v = np.maximum(v + C * (A.T @ (R * (b - A @ v))), 0)
    return v


def save_nexus(path, vol, axes):
    import h5py
    with h5py.File(path, 'w') as f:
        f.attrs['default'] = 'entry'
        e = f.create_group('entry')
        e.attrs.update(NX_class='NXentry', default='volume')
        d = e.create_group('volume')
        d.attrs.update(NX_class='NXdata', signal='mu', axes=list(axes))
        mu = d.create_dataset('mu', data=vol, compression='gzip')
        mu.attrs['long_name'] = 'reconstructed attenuation (arb.)'
        for name, ax in axes.items():
            d.create_dataset(name, data=ax).attrs['units'] = 'm'


def surface(vol, axes, path=None):
    """3D isosurface window with threshold/resolution sliders."""
    d = [a[1] - a[0] for a in axes.values()]
    o = [a[0] for a in axes.values()]
    fig = plt.figure(figsize=(8, 8))
    ax = fig.add_axes([0, 0.12, 1, 0.88], projection='3d')
    sl = Slider(fig.add_axes([0.2, 0.06, 0.6, 0.03]), 'threshold',
                vol.min(), vol.max(), valinit=0.3 * vol.max())
    st = Slider(fig.add_axes([0.2, 0.02, 0.6, 0.03]), 'stride', 1, 4,
                valinit=1, valstep=1)

    def draw(_=None):
        s = int(st.val)
        v, f = isosurface(np.ascontiguousarray(vol[::s, ::s, ::s]), sl.val)
        ax.clear()
        if len(f):
            v = v * s * np.array(d) + o
            ax.add_collection3d(Poly3DCollection(v[f], facecolors='tab:blue',
                                                 edgecolor='none', shade=True))
        x, y, z = (sorted((a[0], a[-1])) for a in axes.values())
        ax.set(xlim=x, ylim=y, zlim=z, box_aspect=[b - a for a, b in (x, y, z)],
               **{k + 'label': n + ' [m]' for k, n in zip('xyz', axes)})
        ax.set_title('Isosurface at %.3g, %d triangles' % (sl.val, len(f)))
        fig.canvas.draw_idle()

    sl.on_changed(draw)
    st.on_changed(draw)
    draw()
    if path:
        fig.canvas.mpl_connect('close_event', lambda e: fig.savefig(path))
    return fig, sl, st                         # keep widgets alive


def main(datadir, save=False, window='hann', cutoff=1.0, iters=0, median=1):
    dat = os.path.join(datadir, 'mccode.dat')
    nproj = int(header(dat, 'Numpoints:'))
    lo, hi = map(float, header(dat, 'xlimits:').split())
    theta = np.linspace(lo, hi, nproj)

    h5 = os.path.join(datadir, 'mccode.h5')
    if os.path.exists(h5):                     # mcrun --format=NeXus: one entry<j+1> per scan step
        import h5py
        mons = []
        with h5py.File(h5, 'r') as fh:
            for j in range(nproj):
                e = fh['entry%d' % (j + 1)]
                m = e['data'][next(k for k in e['data'] if k.endswith('_x_y'))]
                mons.append(m['data'][()].T)   # stored (x, y) -> rows = y slices
            geometry = e['simulation/Param'].attrs['geometry'].decode()
            xylimits = m.attrs['xylimits'].decode()
    else:
        mons = []
        for j in range(nproj):
            f = glob.glob(os.path.join(datadir, str(j), '*.x_y'))[0]
            I = np.loadtxt(f)
            mons.append(I[:len(I) // 3])       # I block only, rows = y slices
        geometry = header(f, 'Param: geometry=')
        xylimits = header(f, 'xylimits:')
    xmin, xmax, ymin, ymax = map(float, xylimits.split())
    mons = np.array(mons)                      # (nproj, nslice, nbins)
    pos = mons[mons > 0]
    mons = np.maximum(mons, pos.min() if pos.size else 1)  # avoid log(0)

    # I0 per detector row; log difference, as max/min overflows for subnormal min
    P = np.log(mons.max(axis=(0, 2), keepdims=True)) - np.log(mons)
    if median > 1:
        P = median_filter(P, size=(1, median, median))
    nx, ny = mons.shape[2], mons.shape[1]
    A = projector(nx, theta)
    b = P.transpose(0, 2, 1).reshape(nproj * nx, ny)
    vol = (sirt(b, A, iters) if iters else fbp(b, A, nx, window, cutoff)).reshape(nx, nx, ny)  # (x, z, y)
    r = (np.arange(nx) - (nx - 1) / 2) * (xmax - xmin) / nx
    # iradon rows run along -Y, columns along +X, slices along detector y
    axes = {'x': -r, 'z': r, 'y': ymin + (np.arange(ny) + 0.5) * (ymax - ymin) / ny}
    if save:
        save_nexus(os.path.join(datadir, 'vol.nxs'), vol, axes)

    title = 'slice from %s, %s' % (geometry, 'SIRT %d' % iters if iters else window)
    c = [s // 2 for s in vol.shape]
    fig, ax = plt.subplots(1, 3, figsize=(15, 5))
    for a, img, name in zip(ax, (vol[c[0]].T, vol[:, c[1]].T, vol[:, :, c[2]]), 'xyz'):
        a.imshow(img, origin='lower')
        a.set_title('Central %s %s' % (name, title))
    keep = surface(vol, axes, save and os.path.join(datadir, 'surface.png'))
    plt.show()
    return vol


if __name__ == '__main__':
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('datadir', help='mcrun scan directory (text or NeXus format)')
    ap.add_argument('--save', action='store_true', help='write vol.nxs and surface.png')
    ap.add_argument('--filter', default='hann', choices=WINDOWS, help='FBP window (default hann)')
    ap.add_argument('--cutoff', type=float, default=1.0, help='FBP cutoff, fraction of Nyquist (default 1)')
    ap.add_argument('--sirt', type=int, default=0, metavar='N', help='use N SIRT iterations instead of FBP')
    ap.add_argument('--median', type=int, default=1, metavar='K', help='KxK median filter on projections')
    a = ap.parse_args()
    main(a.datadir, a.save, a.filter, a.cutoff, a.sirt, a.median)
