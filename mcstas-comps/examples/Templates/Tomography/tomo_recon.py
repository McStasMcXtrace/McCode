#!/usr/bin/env python3
"""Reconstruct a volume from a Tomography.instr omega scan, e.g.

    mcrun Tomography.instr omega=0,350 -N36 -n1e7 -d TomoScan
    python tomo_recon.py TomoScan

Filtered back-projection (ramp*Hann filter, like Matlab iradon's 'hann'),
one sinogram per detector row. Shows 3 central slices; --save writes vol.npy.
Python port of tools/matlab/tomo_recon.m (PW, 20080620).
"""
import glob, os, sys
import numpy as np
import matplotlib.pyplot as plt


def header(path, key):
    for line in open(path):
        if line.startswith('# ' + key):
            return line[len(key) + 2:].strip()


def iradon(sino, theta):
    """sino: (nbins, nproj), theta in degrees -> (nbins, nbins) slice."""
    n = sino.shape[0]
    pad = 1 << int(np.ceil(np.log2(2 * n)))
    f = np.fft.fftfreq(pad)
    h = 2 * np.abs(f) * (1 + np.cos(2 * np.pi * f)) / 2
    filt = np.fft.ifft(np.fft.fft(sino, pad, axis=0) * h[:, None], axis=0).real[:n]
    x = np.arange(n) - (n - 1) / 2
    X, Y = np.meshgrid(x, -x)
    img = np.zeros((n, n))
    for p, t in zip(filt.T, np.deg2rad(theta)):
        img += np.interp(X * np.cos(t) + Y * np.sin(t), x, p, left=0, right=0)
    return img * np.pi / (2 * len(theta))


def main(datadir, save=False):
    dat = os.path.join(datadir, 'mccode.dat')
    nproj = int(header(dat, 'Numpoints:'))
    lo, hi = map(float, header(dat, 'xlimits:').split())
    theta = np.linspace(lo, hi, nproj)

    mons = []
    for j in range(nproj):
        f = glob.glob(os.path.join(datadir, str(j), '*.x_y'))[0]
        I = np.loadtxt(f)
        mons.append(I[:len(I) // 3])           # I block only, rows = y slices
    geometry = header(f, 'Param: geometry=')
    mons = np.array(mons)                      # (nproj, nslice, nbins)
    pos = mons[mons > 0]
    mons = np.maximum(mons, pos.min() if pos.size else 1)  # avoid log(0)

    vol = np.stack([iradon(np.log(m.max() / m).T, theta)
                    for m in mons.transpose(1, 0, 2)], axis=2)  # (x, z, y)
    if save:
        np.save(os.path.join(datadir, 'vol.npy'), vol)

    title = 'slice from %s, Hann filtered' % geometry
    c = [s // 2 for s in vol.shape]
    fig, ax = plt.subplots(1, 3, figsize=(15, 5))
    for a, img, name in zip(ax, (vol[c[0]].T, vol[:, c[1]].T, vol[:, :, c[2]]), 'xyz'):
        a.imshow(img, origin='lower')
        a.set_title('Central %s %s' % (name, title))
    plt.show()
    return vol


if __name__ == '__main__':
    if len(sys.argv) < 2 or not os.path.isdir(sys.argv[1]):
        sys.exit('usage: tomo_recon.py <mcrun scan dir> [--save]')
    main(sys.argv[1], '--save' in sys.argv)
