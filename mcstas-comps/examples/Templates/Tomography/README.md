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

A tomography is a scan of omega over a full rotation, e.g.
mcrun Tomography.instr omega=0,355 -N72 -n1e7 -d TomoScan
(to achieve proper statistics for tomographic reconstruction, MUCH higher ncounts
are needed)

Use the provided tomo_recon.py (numpy + matplotlib) in this folder to reconstruct a 3D
volume of the object from the scan directory: python tomo_recon.py TomoScan [--save]
```

## Examples

- **Test: omega=0 Detector: monitor_I=9.37708e-10**
- **TestScan: mcrun Tomography.instr omega=0,355 -N72 -n1e6 Detector: monitor_I={72 values}**

## Input parameters

Parameters in **boldface** are required; the others are optional.

| Name | Unit | Description | Default |
|------|------|-------------|---------|
| geometry | string | Name of the OFF file describing the sample shape | "socket.off" |
| omega | deg | Sample rotation around y | 0 |
| sigma_abs | barn | Sample absorption xs | 100 |
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
