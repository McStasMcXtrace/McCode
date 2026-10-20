# The `Test_mcdisplay_shapes` Instrument

*McXtrace: Test instrument showing all mcdisplay drawing primitives*

## Identification

- **Site:** Tests_other
- **Author:** Peter Willendrup
- **Origin:** DTU
- **Date:** 20261020

## Description

```text
Places display-only components that together use every mcdisplay
primitive, to check the mcdisplay tools at --trace=1 and --trace=2:
- cone (Arm, Cone)
- box, hollow box and box tilted along (nx, ny, nz) (Shape)
- cylinder, hollow cylinder and cylinder along z (Shape)
- sphere (Shape)
- polyhedron from an OFF file (Shape)
- disc, annulus and circle (Disc, Annulus, Circle)

The shapes are display-only and sit beside the beam. No McXtrace component
draws polygons, but those are drawn as polyhedra at --trace=2, as covered
by the OFF file.

Example: mxdisplay-webgl-classic Test_mcdisplay_shapes.instr --trace=2 -n 20
```

## Examples

- **Test: -n1e5 Detector: Mon_I=7.96e-06**

## Input parameters

Parameters in **boldface** are required; the others are optional.

| Name | Unit | Description | Default |
|------|------|-------------|---------|
| offfile | str | OFF file for the polyhedron Shape | "king.off" |

## Links

- [Source code](Test_mcdisplay_shapes.instr) for `Test_mcdisplay_shapes.instr`.

---
