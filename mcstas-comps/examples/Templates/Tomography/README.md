# The `Tomography` Instrument

*McStas: Instrument to study tomographic imaging by means of the feature of OFF shape samples.*

## Identification

- **Site:** Templates
- **Author:** Peter Willendrup, based on work by Reynald ARNERIN
- **Origin:** Risoe
- **Date:** June 20th, 2008

## Description

```text
Instrument to study tomographic imaging by means of the feature of OFF shape samples.
The sample (geometry, an OFF file, default socket.off) is rotated by omega around the
vertical axis, and the transmitted beam is recorded on a 2D detector (monitor).

A tomography is a scan of omega over a full rotation. Settings suitable for a
full 3D reconstruction (72 projections of 4e6 rays each):
mcrun Tomography.instr omega=0:5:355 -n4e6 sigma_abs=1 -d TomoScan

Use the provided tomo_recon.py (numpy, scipy, matplotlib, pyqtgraph, h5py) in this folder to reconstruct a 3D
volume of the object from the scan directory: python tomo_recon.py TomoScan [--save]
(3 central slices + 3D isosurface; --save writes vol.nxs NeXus volume and surface.png;
--sirt 100 --median 3 gives fewer streaks than the default Hann FBP, see -h)
```

## Examples

- **Test: omega=0 -n4e6 Detector: monitor_I=1.0321e-09 monitor_ERR=7.92929e-13**
- **TestScan: mcrun Tomography.instr --scan_split=auto omega=0,355 -N72 -n4e6 Detector: monitor_I={72 values} monitor_ERR={72 values}**

## Input parameters

Parameters in **boldface** are required; the others are optional.

| Name | Unit | Description | Default |
|------|------|-------------|---------|
| geometry | string | Name of the OFF file describing the sample shape | "socket.off" |
| omega | deg | Sample rotation around y | 0 |
| sigma_abs | barn | Sample absorption xs | 1 |
| frac_scatt | 1 | Fraction of neutrons to scatter in the sample | 0 |
| div_v | deg | Source vertical divergence (angular height) | 1e-4 |
| div_h | deg | Source horisontal divergence (angular width) | 1e-4 |
| source_w | m | Source width | 0.4 |
| source_h | m | Source height | 0.2 |
| det_w | m | Detector width | 0.25 |
| det_h | m | Detector height | 0.15 |
| opts | string | Monitor_nD options string | "x bins=128 y bins=64" |

## Links

- [Source code](Tomography.instr) for `Tomography.instr`.
- http://shape.cs.princeton.edu/benchmark/documentation/off_format.html

---
