#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
Converts MCDISPLAY geometry information, read from an instrument binary,
to common CAD / 3D formats WITHOUT CadQuery / OpenCASCADE.

Supported output formats: glTF (.gltf), GLB, OBJ (+MTL), STL, PLY, VRML97 (.wrl),
X3D (also as legacy 'xml'), VTK legacy (.vtk), STEP (AP214 faceted BREP, pure
Python), and VTKJS (.vtkjs zip of the vtk.js JSON scene format).
No CadQuery/OpenCASCADE is required for any format.

Each instrument component is assigned a color from a cycling palette, so
adjacent components are visually distinguishable in the output.
Color-capable formats: glTF, GLB, OBJ (via companion .mtl), PLY (vertex colors),
VRML97, X3D. STL and legacy VTK carry no color information.

Only dependency: numpy (already required by mccodelib.instrgeom).

All primitives are tessellated in pure Python/numpy:
  - lines / dashed lines / multilines  -> polylines (wireframe)
  - circles / new circles              -> polylines
  - rectangles                         -> quad (mesh + outline)
  - boxes                              -> 12-triangle cube + edges
  - spheres, cones, cylinders, discs, annuli -> triangulated meshes
  - polyhedron                         -> triangulated faces (fan)
Every draw call is placed in global coordinates via the component m4 matrix.
'''
__author__ = "Gregory Tucker (CadQuery version); dependency-free version"
__date__ = "2026-10-17"

import sys
import math
import struct
from pathlib import Path
import subprocess

sys.path.append(str(Path(__file__).resolve().parent.parent.parent))
from mccodelib.instrgeom import (Vector3d, DrawMagnify, DrawLine, DrawDashedLine,
                                  DrawMultiline, DrawRectangle, DrawBox, DrawCircle,
                                  DrawNewCircle, DrawSphere, DrawCone, DrawCylinder,
                                  DrawDisc, DrawAnnulus, DrawPolygon, DrawPolyhedron)
from mccodelib import mccode_config

import numpy as np


# ---------------------------------------------------------------------------
# Component color palette (cycled per component)
# ---------------------------------------------------------------------------

# RGB tuples in [0, 1]; chosen to be reasonably distinguishable in sequence.
COLOR_PALETTE = [
    (0.90, 0.10, 0.10),  # red
    (0.10, 0.55, 0.10),  # green
    (0.15, 0.35, 0.95),  # blue
    (1.00, 0.70, 0.00),  # orange
    (0.60, 0.20, 0.80),  # purple
    (0.00, 0.80, 0.80),  # cyan
    (1.00, 0.45, 0.70),  # pink
    (0.45, 0.35, 0.20),  # brown
    (0.50, 0.50, 0.50),  # gray
    (0.20, 0.90, 0.40),  # lime
]


def color_for(index):
    ''' cycling palette color for component number `index` '''
    return COLOR_PALETTE[index % len(COLOR_PALETTE)]


def _rgb01(c):
    return ' '.join(f'{max(0.0, min(1.0, x)):.4g}' for x in c)


def _rgb255(c):
    return ' '.join(str(int(round(max(0.0, min(1.0, x)) * 255))) for x in c)


# ---------------------------------------------------------------------------
# Small geometry containers
# ---------------------------------------------------------------------------

class Mesh:
    ''' triangle soup: verts (N,3) float64, tris (M,3) int '''
    def __init__(self, verts=None, tris=None):
        self.verts = np.asarray(verts, dtype=np.float64).reshape((-1, 3))
        self.tris = np.asarray(tris, dtype=np.int64).reshape((-1, 3))
        if self.tris.size and self.tris.max() >= len(self.verts):
            raise ValueError("Mesh: triangle index out of range")

    def empty(self):
        return len(self.tris) == 0

    def transformed(self, m4):
        return Mesh(transform_points(m4, self.verts), self.tris)

    def merge(self, other):
        if other is None or other.empty():
            return self
        if self.empty():
            return other
        tris = np.vstack((self.tris, other.tris + len(self.verts)))
        return Mesh(np.vstack((self.verts, other.verts)), tris)


class Part:
    ''' one named component: an optional surface mesh plus wireframe polylines '''
    def __init__(self, name, color=(0.8, 0.8, 0.8)):
        self.name = name
        self.color = color  # (r, g, b) in [0, 1]
        self.mesh = None
        self.polylines = []  # list of (N,3) arrays

    def add_mesh(self, mesh):
        self.mesh = mesh if self.mesh is None else self.mesh.merge(mesh)

    def add_polyline(self, pts):
        pts = np.asarray(pts, dtype=np.float64).reshape((-1, 3))
        if len(pts) >= 2:
            self.polylines.append(pts)

    def transformed(self, m4):
        p = Part(self.name, self.color)
        if self.mesh is not None:
            p.mesh = self.mesh.transformed(m4)
        p.polylines = [transform_points(m4, pl) for pl in self.polylines]
        return p

    def is_empty(self):
        return (self.mesh is None or self.mesh.empty()) and not self.polylines


def transform_points(m4, pts):
    ''' apply a row-major 4x4 (3x4 usable) matrix to an (N,3) point array '''
    pts = np.asarray(pts, dtype=np.float64).reshape((-1, 3))
    r = np.array(m4, dtype=np.float64).reshape(4, 4)[:3, :3]
    t = np.array(m4, dtype=np.float64).reshape(4, 4)[:3, 3]
    return pts @ r.T + t


# ---------------------------------------------------------------------------
# Tessellation of the mccodelib draw calls (local coordinates)
# ---------------------------------------------------------------------------

def _basis_from_axis(axis):
    ''' orthonormal (w, u, v) with w parallel to axis '''
    w = np.asarray(axis, dtype=np.float64).reshape(3)
    n = np.linalg.norm(w)
    if n < 1e-12:
        w = np.array([0.0, 0.0, 1.0])
    else:
        w = w / n
    a = np.array([1.0, 0.0, 0.0]) if abs(w[0]) < 0.9 else np.array([0.0, 1.0, 0.0])
    u = np.cross(w, a)
    u /= np.linalg.norm(u)
    v = np.cross(w, u)
    return w, u, v


def _circle_pts(center, radius, axis, segments=64, closed=True):
    ''' points on a circle of given center, radius and axis (default z) '''
    c = np.asarray(center, dtype=np.float64).reshape(3)
    if axis is not None:
        _, u, v = _basis_from_axis(axis)
    else:
        u, v = np.array([1.0, 0.0, 0.0]), np.array([0.0, 1.0, 0.0])
    theta = np.linspace(0.0, 2.0 * np.pi, segments, endpoint=not closed)
    pts = c + radius * (np.outer(np.cos(theta), u) + np.outer(np.sin(theta), v))
    if closed:  # close the loop
        pts = np.vstack((pts, pts[0]))
    return pts


def _fan(center, ring_pts):
    ''' triangulate a (closed, non-repeated) ring into a fan from center '''
    c = np.asarray(center, dtype=np.float64).reshape((1, 3))
    ring = np.asarray(ring_pts, dtype=np.float64).reshape((-1, 3))
    n = len(ring)
    verts = np.vstack((c, ring))
    tris = np.array([[0, 1 + i, 1 + (i + 1) % n] for i in range(n)], dtype=np.int64)
    return Mesh(verts, tris)


def _strip(ring_a, ring_b, flip=False):
    ''' triangulate the side wall between two same-length rings '''
    a = np.asarray(ring_a, dtype=np.float64).reshape((-1, 3))
    b = np.asarray(ring_b, dtype=np.float64).reshape((-1, 3))
    n = len(a)
    verts = np.vstack((a, b))
    off = n
    tris = []
    for i in range(n):
        j = (i + 1) % n
        if flip:
            tris.append([i, j, off + j])
            tris.append([i, off + j, off + i])
        else:
            tris.append([i, off + j, j])
            tris.append([i, off + i, off + j])
    return Mesh(verts, np.array(tris, dtype=np.int64))


def _v3(p):
    return (p.x, p.y, p.z)


def tessellate(d, segments=64):
    ''' convert one draw call into (mesh_or_None, [polylines]) in local coords '''
    if isinstance(d, DrawMagnify):
        return None, []
    if isinstance(d, DrawPolygon):
        return None, []  # awaiting C code upstream; nothing to draw
    if isinstance(d, (DrawLine,)):
        p1, p2 = _v3(d.point_1), _v3(d.point_2)
        return None, [np.array([p1, p2])]
    if isinstance(d, DrawDashedLine):
        p1, p2 = np.asarray(_v3(d.point_1)), np.asarray(_v3(d.point_2))
        n = max(2, int(np.linalg.norm(p2 - p1) / 0.05) + 1)  # 5 cm dashes
        t = np.linspace(0, 1, n)
        seg = [p1 + (p2 - p1) * ti for ti in t]
        lines = [np.array([seg[i], seg[i + 1]]) for i in range(0, len(seg) - 1, 2)]
        return None, lines
    if isinstance(d, DrawMultiline):
        return None, [np.array([_v3(p) for p in d.points])]
    if isinstance(d, DrawRectangle):
        plane = d.plane.lower()
        c = _v3(d.center)
        w, h = d.width / 2.0, d.height / 2.0
        if plane == 'xy':
            quad = [(c[0]-w, c[1]-h, c[2]), (c[0]+w, c[1]-h, c[2]),
                    (c[0]+w, c[1]+h, c[2]), (c[0]-w, c[1]+h, c[2])]
        elif plane == 'yz':
            quad = [(c[0], c[1]-w, c[2]-h), (c[0], c[1]-w, c[2]+h),
                    (c[0], c[1]+w, c[2]+h), (c[0], c[1]+w, c[2]-h)]
        else:  # xz
            quad = [(c[0]-w, c[1], c[2]-h), (c[0]+w, c[1], c[2]-h),
                    (c[0]+w, c[1], c[2]+h), (c[0]-w, c[1], c[2]+h)]
        quad = np.array(quad)
        closed = np.vstack((quad, quad[0]))
        mesh = Mesh(quad, [[0, 1, 2], [0, 2, 3]])
        return mesh, [closed]
    if isinstance(d, DrawBox):
        c = _v3(d.center)
        x, y, z = d.xwidth / 2.0, d.yheight / 2.0, d.zlength / 2.0
        if getattr(d, 'thickness', 0) and d.thickness > 0:
            t = d.thickness / 2.0
            # hollow box: keep the outline of the two faces (simple wireframe)
            lines = []
            for s in (+1, -1):
                r = [(c[0]-x, c[1]-y, c[2]+s*z), (c[0]+x, c[1]-y, c[2]+s*z),
                     (c[0]+x, c[1]+y, c[2]+s*z), (c[0]-x, c[1]+y, c[2]+s*z)]
                lines.append(np.vstack((r, r[0])))
            return None, lines
        V = np.array([(sx*x+c[0], sy*y+c[1], sz*z+c[2])
                      for sz in (-1, 1) for sy in (-1, 1) for sx in (-1, 1)])
        # faces: bottom(z-), top(z+), sides
        F = [[0, 1, 3, 2], [4, 6, 7, 5], [0, 4, 5, 1], [2, 3, 7, 6], [0, 2, 6, 4], [1, 5, 7, 3]]
        tris, lines = [], []
        for f in F:
            tris += [[f[0], f[1], f[2]], [f[0], f[2], f[3]]]
            lines.append(np.vstack((V[f], V[f[0]])))
        return Mesh(V, tris), lines
    if isinstance(d, DrawCircle):
        axis = {'xy': (0, 0, 1), 'xz': (0, 1, 0), 'yz': (1, 0, 0),
                'yx': (0, 0, 1), 'zx': (0, 1, 0), 'zy': (1, 0, 0)}.get(d.plane.lower(), (0, 0, 1))
        return None, [_circle_pts(_v3(d.center), d.radius, axis, segments)]
    if isinstance(d, DrawNewCircle):
        return None, [_circle_pts(_v3(d.center), d.radius, _v3(d.axis_vector), segments)]
    if isinstance(d, DrawSphere):
        c = np.asarray(_v3(d.center))
        r = d.radius
        nu, nv = segments, max(8, segments // 2)
        u = np.linspace(0, 2 * np.pi, nu, endpoint=False)
        v = np.linspace(0, np.pi, nv)
        uu, vv = np.meshgrid(u, v)
        pts = np.column_stack((c[0] + r * np.cos(uu) * np.sin(vv),
                               c[1] + r * np.sin(uu) * np.sin(vv),
                               c[2] + r * np.cos(vv))).reshape(-1, 3)
        tris = []
        for i in range(nv - 1):
            for j in range(nu):
                a = i * nu + j
                b = i * nu + (j + 1) % nu
                cc = (i + 1) * nu + j
                dd = (i + 1) * nu + (j + 1) % nu
                tris += [[a, b, cc], [b, dd, cc]]
        return Mesh(pts, tris), []
    if isinstance(d, DrawCone):
        c = np.asarray(_v3(d.center))
        w, u, v = _basis_from_axis(_v3(d.axis_vector))
        th = np.linspace(0, 2*np.pi, segments, endpoint=False)
        ring = c + d.radius * (np.outer(np.cos(th), u) + np.outer(np.sin(th), v))
        apex = (c + w * d.height).reshape((1, 3))
        mesh = _strip(ring, np.repeat(apex, len(ring), axis=0), flip=True)
        closed = np.vstack((ring, ring[0]))
        return mesh, [np.vstack((closed, apex[0], closed[0]))]
    if isinstance(d, DrawCylinder):
        c = np.asarray(_v3(d.center))
        w, u, v = _basis_from_axis(_v3(d.axis_vector))
        r, h = d.radius, d.height
        th = np.linspace(0, 2 * np.pi, segments, endpoint=False)
        ring = c + r * (np.outer(np.cos(th), u) + np.outer(np.sin(th), v))
        bot = ring - w * (h / 2.0)
        top = ring + w * (h / 2.0)
        mesh = _strip(bot, top)
        mesh = mesh.merge(_fan(c - w * (h / 2.0), bot))
        mesh = mesh.merge(_fan(c + w * (h / 2.0), top))
        t = getattr(d, 'thickness', 0.0) or 0.0
        lines = []
        if 0 < t < r:  # hollow tube: add inner wall lines
            inring = c + (r - t) * (np.outer(np.cos(th), u) + np.outer(np.sin(th), v))
            for rr in (bot, top, inring - w * (h / 2.0), inring + w * (h / 2.0)):
                lines.append(np.vstack((rr, rr[0])))
        else:
            lines = [np.vstack((bot, bot[0])), np.vstack((top, top[0]))]
        return mesh, lines
    if isinstance(d, DrawDisc):
        th = np.linspace(0, 2 * np.pi, segments, endpoint=False)
        c = np.asarray(_v3(d.center))
        # build in the plane defined by axis_vector
        _, u, v = _basis_from_axis(_v3(d.axis_vector))
        ring = c + d.radius * (np.outer(np.cos(th), u) + np.outer(np.sin(th), v))
        mesh = _fan(c, ring)
        return mesh, [np.vstack((ring, ring[0]))]
    if isinstance(d, DrawAnnulus):
        c = np.asarray(_v3(d.center))
        _, u, v = _basis_from_axis(_v3(d.axis_vector))
        th = np.linspace(0, 2 * np.pi, segments, endpoint=False)
        outer = c + d.outer_radius * (np.outer(np.cos(th), u) + np.outer(np.sin(th), v))
        inner = c + d.inner_radius * (np.outer(np.cos(th), u) + np.outer(np.sin(th), v))
        mesh = _strip(outer, inner, flip=True)
        return mesh, [np.vstack((outer, outer[0])), np.vstack((inner, inner[0]))]
    if isinstance(d, DrawPolyhedron):
        return _polyhedron(d.faces_vertices)
    print(f"Drawing of {type(d)} not supported; please extend {__file__}!")
    return None, []


def _polyhedron(faces_vertices):
    ''' best-effort polyhedron: accepts [ [ (x,y,z)... ] per face ] or
    [ [(x,y,z)...], ... ] style structures produced by instrparser '''
    try:
        faces = faces_vertices
        # accept (vertices, faces-as-index-lists) tuple
        if isinstance(faces, tuple) and len(faces) == 2:
            verts, idxs = faces
            verts = np.asarray(verts, dtype=np.float64).reshape((-1, 3))
            faces = [verts[np.asarray(ix).astype(int)] for ix in idxs]
        meshes, lines = [], []
        for f in faces:
            f = np.asarray(f, dtype=np.float64).reshape((-1, 3))
            if len(f) < 3:
                if len(f) == 2:
                    lines.append(f)
                continue
            tris = np.array([[0, i, i + 1] for i in range(1, len(f) - 1)], dtype=np.int64)
            meshes.append(Mesh(f, tris))
            lines.append(np.vstack((f, f[0])))
        mesh = None
        for m in meshes:
            mesh = m if mesh is None else mesh.merge(m)
        return mesh, lines
    except Exception as e:
        print(f"Polyhedron interpretation failed ({e}); skipped")
        return None, []


# ---------------------------------------------------------------------------
# instrument -> parts
# ---------------------------------------------------------------------------

def component_to_part(comp, color):
    part = Part(comp.name, color)
    for d in comp.drawcalls:
        mesh, lines = tessellate(d)
        if mesh is not None:
            part.add_mesh(mesh)
        for l in lines:
            part.add_polyline(l)
    return part.transformed(comp.m4)


def instrument_to_parts(inst):
    ''' convert components to parts, assigning a cycling color per component '''
    parts = []
    ci = 0  # color index over all components (so colors differ even if some are empty)
    for comp in inst.components:
        part = component_to_part(comp, color_for(ci))
        ci += 1
        if not part.is_empty():
            parts.append(part)
    return parts


# ---------------------------------------------------------------------------
# Exporters (all pure Python)
# ---------------------------------------------------------------------------

def _fmt(x):
    return f'{x:.6g}'


def export_obj(parts, path):
    ''' OBJ with a companion .mtl carrying one material (color) per component '''
    mtl_path = str(path)[:-len('.obj')] + '.mtl' if str(path).endswith('.obj') \
        else str(path) + '.mtl'

    def mtl_name(i, p):
        # material names must not contain whitespace or '#'
        return 'comp_' + ''.join(ch if ch.isalnum() else '_' for ch in p.name) + f'_{i}'

    with open(mtl_path, 'w') as mf:
        mf.write('# McCode mcdisplay export (MTL)\n')
        for i, p in enumerate(parts):
            r, g, b = p.color
            mf.write(f'newmtl {mtl_name(i, p)}\n')
            mf.write(f'Kd {r:.4g} {g:.4g} {b:.4g}\nKa 0 0 0\nKs 0.2 0.2 0.2\nNs 10\n\n')
    with open(path, 'w') as f:
        f.write('# McCode mcdisplay export (OBJ)\n')
        f.write(f'mtllib {Path(mtl_path).name}\n')
        # OBJ vertex indices are global across the whole file: one running offset
        voff = 1
        for i, p in enumerate(parts):
            f.write(f'o {p.name}\ng {p.name}\n')
            for pl in p.polylines:
                f.write(''.join(f'v {_fmt(x)} {_fmt(y)} {_fmt(z)}\n' for x, y, z in pl))
                f.write('l ' + ' '.join(str(voff + i2) for i2 in range(len(pl))) + '\n')
                voff += len(pl)
            if p.mesh is not None and not p.mesh.empty():
                f.write(f'usemtl {mtl_name(i, p)}\n')
                f.write(''.join(f'v {_fmt(x)} {_fmt(y)} {_fmt(z)}\n' for x, y, z in p.mesh.verts))
                for a, b, c in p.mesh.tris:
                    f.write(f'f {voff+a} {voff+b} {voff+c}\n')
                voff += len(p.mesh.verts)


def export_stl(parts, path):
    tris = []
    for p in parts:
        if p.mesh is not None and not p.mesh.empty():
            tris.append(p.mesh.verts[p.mesh.tris])
    if tris:
        tris = np.vstack(tris)
    else:
        tris = np.zeros((0, 3, 3))
    with open(path, 'wb') as f:
        f.write(b'mcdisplay STL export'.ljust(80, b'\0'))
        f.write(struct.pack('<I', len(tris)))
        for t in tris:
            n = np.cross(t[1] - t[0], t[2] - t[0])
            ln = np.linalg.norm(n)
            n = n / ln if ln > 0 else n
            f.write(struct.pack('<3f', *n))
            for v in t:
                f.write(struct.pack('<3f', *v))
            f.write(struct.pack('<H', 0))
    if not len(tris):
        print("WARNING: STL has no triangles (only lines were drawn); wireframe is not representable in STL")
    nmeshed = sum(1 for p in parts if p.mesh is not None and not p.mesh.empty())
    if nmeshed < len(parts):
        print(f"NOTE: {len(parts) - nmeshed} of {len(parts)} components are wireframe-only "
              f"and cannot be represented in STL (surfaces only: {nmeshed})")


def export_ply(parts, path):
    ''' ASCII PLY with per-vertex colors (each vertex carries its component color) '''
    verts, colors, edges, faces = [], [], [], []
    for p in parts:
        r, g, b = (int(round(c * 255)) for c in p.color)
        for pl in p.polylines:
            if len(pl) == 2:
                edges.append([len(verts), len(verts) + 1])
                verts.extend(map(tuple, pl))
                colors.extend([(r, g, b)] * 2)
            else:
                for i in range(len(pl) - 1):
                    edges.append([len(verts) + i, len(verts) + i + 1])
                verts.extend(map(tuple, pl))
                colors.extend([(r, g, b)] * len(pl))
        if p.mesh is not None and not p.mesh.empty():
            base = len(verts)
            verts.extend(map(tuple, p.mesh.verts))
            colors.extend([(r, g, b)] * len(p.mesh.verts))
            faces.extend([tuple(base + int(i) for i in t) for t in p.mesh.tris])
    with open(path, 'w') as f:
        f.write('ply\nformat ascii 1.0\ncomment McCode mcdisplay export\n')
        f.write('element vertex ' + str(len(verts)) + '\nproperty float x\nproperty float y\nproperty float z\n'
                'property uchar red\nproperty uchar green\nproperty uchar blue\n')
        f.write(f'element edge {len(edges)}\nproperty int vertex1\nproperty int vertex2\n')
        f.write('element face ' + str(len(faces)) + '\nproperty list uchar int vertex_indices\n'
                'property uchar red\nproperty uchar green\nproperty uchar blue\nend_header\n')
        for v, c in zip(verts, colors):
            f.write(f'{_fmt(v[0])} {_fmt(v[1])} {_fmt(v[2])} {c[0]} {c[1]} {c[2]}\n')
        for a, b in edges:
            f.write(f'{a} {b}\n')
        for fi, fc in enumerate(faces):
            c = colors[fc[0]]  # face color = color of its first vertex
            f.write('3 ' + ' '.join(map(str, fc)) + f' {c[0]} {c[1]} {c[2]}\n')


def export_vrml(parts, path):
    with open(path, 'w') as f:
        f.write('#VRML V2.0 utf8\n# McCode mcdisplay export\n')
        for p in parts:
            f.write(f'DEF {p.name.replace(" ", "_")} Transform {{ children [\n')
            if p.polylines:
                f.write('Shape { appearance Appearance { material Material { diffuseColor '
                        + _rgb01(p.color) + ' emissiveColor ' + _rgb01(p.color) + ' } } '
                        'geometry IndexedLineSet { '
                        'coord Coordinate { point [')
                pts, idx, off = [], [], 0
                for pl in p.polylines:
                    for v in pl:
                        pts.append(f'{_fmt(v[0])} {_fmt(v[1])} {_fmt(v[2])}')
                    idx.append(','.join(str(off + i) for i in range(len(pl))) + ',-1')
                    off += len(pl)
                f.write(','.join(pts) + '] } coordIndex [' + ','.join(idx) + '] } },\n')
            if p.mesh is not None and not p.mesh.empty():
                m = p.mesh
                f.write('Shape { appearance Appearance { material Material { diffuseColor '
                        + _rgb01(p.color) + ' } } geometry IndexedFaceSet { '
                        'coord Coordinate { point [')
                f.write(','.join(f'{_fmt(x)} {_fmt(y)} {_fmt(z)}' for x, y, z in m.verts))
                f.write('] } coordIndex [')
                f.write(','.join(f'{a},{b},{c},-1' for a, b, c in m.tris))
                f.write('] solid FALSE } }')
            f.write('] }\n')


def export_x3d(parts, path):
    with open(path, 'w') as f:
        f.write('<?xml version="1.0" encoding="UTF-8"?>\n')
        f.write('<X3D profile="Interchange" version="3.3" '
                'xmlns:xsd="http://www.w3.org/2001/XMLSchema-instance">\n')
        f.write('<Scene>\n')
        for p in parts:
            nm = p.name.replace('"', '')
            mat = (f'<Material diffuseColor="{_rgb01(p.color)}" '
                   f'emissiveColor="{_rgb01(p.color)}"/>')
            if p.polylines:
                pts, idx, off = [], [], 0
                for pl in p.polylines:
                    for v in pl:
                        pts.append('%.6g %.6g %.6g' % tuple(v))
                    idx.extend([off + i for i in range(len(pl))] + [-1])
                    off += len(pl)
                f.write(f'<Transform><Shape><Appearance>{mat}</Appearance>')
                f.write(f'<IndexedLineSet coordIndex="{" ".join(map(str, idx))}">'
                        f'<Coordinate point="{" ".join(pts)}"/></IndexedLineSet>')
                f.write(f'</Shape></Transform><!-- {nm} lines -->\n')
            if p.mesh is not None and not p.mesh.empty():
                m = p.mesh
                pts = ' '.join('%.6g %.6g %.6g' % tuple(v) for v in m.verts)
                idx = ' '.join(map(str, sum(([int(a), int(b), int(c), -1] for a, b, c in m.tris), [])))
                f.write(f'<Transform><Shape><Appearance>{mat}</Appearance>')
                f.write(f'<IndexedFaceSet solid="false" coordIndex="{idx}">'
                        f'<Coordinate point="{pts}"/></IndexedFaceSet>')
                f.write(f'</Shape></Transform><!-- {nm} -->\n')
        f.write('</Scene>\n</X3D>\n')


def export_vtk(parts, path):
    pts, lines, faces = [], [], []
    for p in parts:
        for pl in p.polylines:
            base = len(pts)
            pts.extend(map(tuple, pl))
            lines.append(len(pl))
            lines.extend(range(base, base + len(pl)))
        if p.mesh is not None and not p.mesh.empty():
            base = len(pts)
            pts.extend(map(tuple, p.mesh.verts))
            for t in p.mesh.tris:
                faces.append(3)
                faces.extend(base + int(i) for i in t)
    with open(path, 'w') as f:
        f.write('# vtk DataFile Version 3.0\nMcCode mcdisplay export\nASCII\nDATASET POLYDATA\n')
        f.write(f'POINTS {len(pts)} double\n')
        for v in pts:
            f.write(f'{_fmt(v[0])} {_fmt(v[1])} {_fmt(v[2])}\n')
        nlpoly = sum(len(pp.polylines) for pp in parts)
        f.write(f'LINES {nlpoly} {len(lines)}\n')
        f.write(' '.join(map(str, lines)) + '\n')
        nface = sum(1 for x in faces if x == 3)
        f.write(f'POLYS {nface} {len(faces)}\n')
        f.write(' '.join(map(str, faces)) + '\n')
        # per-point component color scalars (COLORSCALAR names are auto-loaded by many VTK viewers)
        f.write(f'POINT_DATA {len(pts)}\n')
        f.write('COLORSCALARS component_color 3\n')
        for p in parts:
            npts = sum(len(pl) for pl in p.polylines)
            if p.mesh is not None and not p.mesh.empty():
                npts += len(p.mesh.verts)
            for _ in range(npts):
                f.write(' '.join(f'{c:.4g}' for c in p.color) + '\n')


def _tube_mesh(p0, p1, radius, segments=6):
    ''' open cylinder (no caps) between two points, as a triangle mesh;
    viewers see the inside via doubleSided materials '''
    p0 = np.asarray(p0, dtype=np.float64).reshape(3)
    p1 = np.asarray(p1, dtype=np.float64).reshape(3)
    w, u, v = _basis_from_axis(p1 - p0)
    th = np.linspace(0, 2 * np.pi, segments, endpoint=False)
    ring = radius * (np.outer(np.cos(th), u) + np.outer(np.sin(th), v))
    return _strip(p0 + ring, p1 + ring)


def _gltf_buffers(parts):
    ''' build accessor+buffers for all parts; returns glTF dict + binary blob '''
    bin_chunks = []
    accessors = []
    buffviews = []
    materials = []

    def add(arr, dtype, target):
        a = np.ascontiguousarray(arr)
        off = sum(len(b) for b in bin_chunks)
        # pad to 4 bytes
        pad = (4 - (off % 4)) % 4
        if pad:
            bin_chunks.append(b'\0' * pad)
            off += pad
        data = a.astype(dtype).tobytes()
        bin_chunks.append(data)
        buffviews.append({'buffer': 0, 'byteOffset': off, 'byteLength': len(data),
                          'target': target})
        comp = {np.dtype('float32'): 5126, np.dtype('uint32'): 5125}[np.dtype(dtype)]
        if a.ndim == 2 and a.shape[1] == 3:
            atype, amin, amax = 'VEC3', a.min(axis=0), a.max(axis=0)
        else:
            atype, amin, amax = 'SCALAR', np.array([a.min()]), np.array([a.max()])
        acc = {'bufferView': len(buffviews) - 1, 'componentType': comp,
               'count': int(len(a)), 'type': atype,
               'min': [float(x) for x in amin], 'max': [float(x) for x in amax]}
        accessors.append(acc)
        return len(accessors) - 1

    def add_material(color):
        materials.append({'pbrMetallicRoughness': {
            'baseColorFactor': [float(c) for c in color] + [1.0],
            'metallicFactor': 0.1, 'roughnessFactor': 0.7},
            'doubleSided': True})  # wireframe tubes have no caps
        return len(materials) - 1

    nodes, meshes = [], []
    # Wireframe lines are exported as thin tubes (triangle meshes) instead of
    # glTF line primitives (mode 1): Blender's glTF-IO importer crashes on line
    # primitives with a numpy concatenation error, and web viewers (three.js,
    # f3d, castle-model-viewer) support them inconsistently. Tubes load as
    # plain triangles everywhere. Radius scales with the scene size.
    all_pts = []
    for p in parts:
        if p.mesh is not None and not p.mesh.empty():
            all_pts.append(p.mesh.verts)
        all_pts.extend(p.polylines)
    diag = float(np.linalg.norm(np.vstack(all_pts).max(axis=0) - np.vstack(all_pts).min(axis=0))) \
        if all_pts else 1.0
    tube_r = max(diag * 0.00002, 1e-6)
    for i, p in enumerate(parts):
        mat_idx = add_material(p.color)
        mesh = p.mesh
        if p.polylines:
            tubes = None
            for pl in p.polylines:
                # merge consecutive collinear segments into one tube
                runs = [[pl[0], pl[1]]]
                for j in range(1, len(pl) - 1):
                    a, b = pl[j], pl[j + 1]
                    prev = runs[-1]
                    d_prev = prev[1] - prev[0]
                    d_cur = b - a
                    n_p, n_c = np.linalg.norm(d_prev), np.linalg.norm(d_cur)
                    if n_p > 0 and n_c > 0 and abs(np.dot(d_prev / n_p, d_cur / n_c) - 1.0) < 1e-9:
                        prev[1] = b  # collinear continuation: extend the previous run
                    else:
                        runs.append([a, b])
                for a, b in runs:
                    t = _tube_mesh(a, b, tube_r)
                    tubes = t if tubes is None else tubes.merge(t)
            if tubes is not None:
                mesh = tubes if mesh is None else mesh.merge(tubes)
        if mesh is not None and not mesh.empty():
            pv = add(mesh.verts, 'float32', 34962)
            pi = add(mesh.tris.reshape(-1), 'uint32', 34963)
            meshes.append({'primitives': [{'attributes': {'POSITION': pv}, 'indices': pi,
                                           'mode': 4, 'material': mat_idx}]})
            nodes.append({'mesh': len(meshes) - 1, 'name': p.name})
    blob = b''.join(bin_chunks)
    gltf = {'asset': {'version': '2.0', 'generator': 'mcdisplay (dependency-free)'},
            'scene': 0,
            'scenes': [{'nodes': list(range(len(nodes)))}],
            'nodes': nodes, 'meshes': meshes, 'materials': materials,
            'buffers': [{'byteLength': len(blob)}],
            'bufferViews': buffviews, 'accessors': accessors}
    return gltf, blob


def export_gltf(parts, path):
    gltf, blob = _gltf_buffers(parts)
    gltf['buffers'][0]['uri'] = Path(path).name + '.bin'
    with open(path, 'w') as f:
        import json
        json.dump(gltf, f, separators=(',', ':'))
    with open(str(path) + '.bin', 'wb') as f:
        f.write(blob)


def export_glb(parts, path):
    import json
    gltf, blob = _gltf_buffers(parts)
    gltf['buffers'][0]['bufferView'] = len(gltf['bufferViews'])
    gltf['bufferViews'].append({'buffer': 0, 'byteOffset': 0, 'byteLength': len(blob)})
    js = json.dumps(gltf, separators=(',', ':')).encode()
    while len(js) % 4:
        js += b' '
    while len(blob) % 4:
        blob += b'\0'
    with open(path, 'wb') as f:
        f.write(b'glTF' + struct.pack('<II', 2, 12 + 8 + len(js) + 8 + len(blob)))
        f.write(struct.pack('<I', len(js)) + b'JSON' + js)
        f.write(struct.pack('<I', len(blob)) + b'BIN\0' + blob)


def export_step(parts, path):
    ''' Pure-Python STEP (AP214-ish) writer -- no CadQuery/OpenCASCADE needed.

    Each component becomes one PRODUCT:
      - components with a mesh -> faceted BREP: OPEN_SHELL of ADVANCED_FACEs,
        each triangle a PLANE bounded by a POLY_LOOP of 3 VERTEX_POINTs,
        wrapped in a SHELL_BASED_SURFACE_MODEL
        (MANIFOLD_SURFACE_SHAPE_REPRESENTATION)
      - wireframe-only components -> GEOMETRICALLY_BOUNDED_WIREFRAME_SHAPE_
        REPRESENTATION of a GEOMETRIC_CURVE_SET of POLYLINEs
    Component colors are attached as STYLED_ITEMs (COLOUR_RGB) where the
    reader supports it (e.g. FreeCAD). Coordinates are in metres. '''
    entities = []
    nid = 0

    def E(text):
        nonlocal nid
        nid += 1
        entities.append(f'#{nid}={text};')
        return nid

    def pt(p):
        return E(f"CARTESIAN_POINT('',({_fmt(p[0])},{_fmt(p[1])},{_fmt(p[2])}))")

    # --- global contexts and units ---
    appctx = E("APPLICATION_CONTEXT('core data for automotive mechanical design processes')")
    E(f"APPLICATION_PROTOCOL_DEFINITION('international standard','automotive_design',2010,{appctx})")
    lu = E('SI_UNIT($,.METRE.)')
    pau = E('SI_UNIT($,.RADIAN.)')
    sau = E('SI_UNIT($,.STERADIAN.)')
    unc = E(f"UNCERTAINTY_MEASURE_WITH_UNIT(LENGTH_MEASURE(1.E-07),{lu},"
            f"'distance_accuracy_value','')")
    # one complex instance for the representation context
    nid += 1
    ctx = nid
    entities.append(f"#{ctx}=(GEOMETRIC_REPRESENTATION_CONTEXT(3)"
                    f"GLOBAL_UNCERTAINTY_ASSIGNED_CONTEXT(({unc}))"
                    f"GLOBAL_UNIT_ASSIGNED_CONTEXT(({lu},{pau},{sau}))"
                    f"REPRESENTATION_CONTEXT('',''));" )

    # --- one product per component ---
    for p in parts:
        nm = p.name.replace("'", "''").replace(';', '').replace('#', '')
        pctx = E(f"PRODUCT_CONTEXT('',{appctx},'mechanical')")
        prod = E(f"PRODUCT('{nm}','{nm}','',({pctx}))")
        form = E(f"PRODUCT_DEFINITION_FORMATION_WITH_SPECIFIED_SOURCE('','',{prod},.NOT_KNOWN.)")
        pdctx = E(f"PRODUCT_DEFINITION_CONTEXT('part definition',{appctx},'design')")
        pdef = E(f"PRODUCT_DEFINITION('design','',{form},{pdctx})")
        pds = E(f"PRODUCT_DEFINITION_SHAPE('','',{pdef})")

        styled = []  # styled item ids for this component
        rep = None
        if p.mesh is not None and not p.mesh.empty():
            faces = []
            for tri in p.mesh.verts[p.mesh.tris]:
                a, b, c = tri
                n = np.cross(b - a, c - a)
                ln = np.linalg.norm(n)
                if ln < 1e-12:
                    continue  # skip degenerate triangles
                n = n / ln
                pa, pb, pc = pt(a), pt(b), pt(c)
                va = E(f'VERTEX_POINT(\'\',{pa})')
                vb = E(f'VERTEX_POINT(\'\',{pb})')
                vc = E(f'VERTEX_POINT(\'\',{pc})')
                loop = E(f'POLY_LOOP(\'\',({va},{vb},{vc}),.T.)')
                fb = E(f'FACE_OUTER_BOUND(\'\',{loop},.T.)')
                dn = E(f"DIRECTION('',({_fmt(n[0])},{_fmt(n[1])},{_fmt(n[2])}))")
                ref = np.array([1.0, 0.0, 0.0]) if abs(n[0]) < 0.9 else np.array([0.0, 1.0, 0.0])
                dr = np.cross(n, ref)
                dr /= np.linalg.norm(dr)
                dd = E(f"DIRECTION('',({_fmt(dr[0])},{_fmt(dr[1])},{_fmt(dr[2])}))")
                ax = E(f'AXIS2_PLACEMENT_3D(\'\',{pa},{dn},{dd})')
                pl = E(f'PLANE(\'\',{ax})')
                faces.append(E(f'ADVANCED_FACE(\'\',({fb}),{pl},.T.)'))
            if faces:
                shell = E(f"OPEN_SHELL('',({','.join(map(str, faces))}))")
                sbsm = E(f'SHELL_BASED_SURFACE_MODEL(\'\',({shell}))')
                rep = E(f'MANIFOLD_SURFACE_SHAPE_REPRESENTATION(\'\',({sbsm}),{ctx})')
                # attach the component color to every face
                col = E(f"COLOUR_RGB('',{p.color[0]:.4g},{p.color[1]:.4g},{p.color[2]:.4g})")
                fasc = E(f'FILL_AREA_STYLE_COLOUR(\'\',{col})')
                fas = E(f'FILL_AREA_STYLE(\'\',({fasc}))')
                ssf = E(f'SURFACE_STYLE_FILL_AREA({fas})')
                sss = E(f"SURFACE_SIDE_STYLE('',({ssf}))")
                ssu = E(f'SURFACE_STYLE_USAGE(.BOTH.,{sss})')
                psa = E(f'PRESENTATION_STYLE_ASSIGNMENT(({ssu}))')
                for f_id in faces:
                    styled.append(E(f"STYLED_ITEM('',({psa}),{f_id})"))
        if rep is None and p.polylines:
            # wireframe-only component
            pols = []
            for pl in p.polylines:
                ids = ','.join(str(pt(v)) for v in pl)
                pols.append(E(f"POLYLINE('',({ids}))"))
            gcs = E(f"GEOMETRIC_CURVE_SET('',({','.join(map(str, pols))}))")
            rep = E(f'GEOMETRICALLY_BOUNDED_WIREFRAME_SHAPE_REPRESENTATION(\'\',({gcs}),{ctx})')
        if rep is None:
            continue  # nothing representable in STEP
        E(f'SHAPE_DEFINITION_REPRESENTATION({pds},{rep})')
        if styled:
            E(f"MECHANICAL_DESIGN_GEOMETRIC_PRESENTATION_REPRESENTATION('',"
              f"({','.join(map(str, styled))}),{ctx})")

    with open(path, 'w') as f:
        f.write('ISO-10303-21;\n')
        f.write('HEADER;\n')
        f.write("FILE_DESCRIPTION(('MCDISPLAY geometry export'),'2;1');\n")
        f.write(f"FILE_NAME('{Path(path).name}','{__date__}',('mcdisplay'),"
                f"('McCode'),'mcdisplay dependency-free','');\n")
        f.write("FILE_SCHEMA(('AUTOMOTIVE_DESIGN { 1 0 10303 214 1 1 1 1 }'));\n")
        f.write('ENDSEC;\nDATA;\n')
        f.write('\n'.join(entities) + '\n')
        f.write('ENDSEC;\nEND-ISO-10303-21;\n')


def export_vtkjs(parts, path):
    ''' Pure-Python VTKJS export -- no CadQuery/VTK needed.

    Writes a .vtkjs (zip) in the VTK JSON scene format used by the vtk.js
    HttpSceneLoader / ParaView Glance viewers:
      index.json             scene description (one entry per component)
      <n>/index.json         vtkPolyData dataset for component n, with binary
                             arrays base64-encoded (float32 points, uint32
                             connectivity), plus a per-component RGB point
                             data array for the cycling colors. '''
    import base64
    import json
    import zipfile

    def b64(arr, dtype):
        return base64.b64encode(np.ascontiguousarray(arr, dtype=dtype).tobytes()).decode()

    def ranges(pts):
        out = []
        for c, nm in ((0, 'X'), (1, 'Y'), (2, 'Z')):
            col = pts[:, c]
            out.append({'min': float(col.min()), 'max': float(col.max()),
                        'component': c, 'name': nm})
        return out

    scene = []
    datasets = []
    for i, p in enumerate(parts):
        pts, lines, polys = [], [], []
        for pl in p.polylines:
            base = len(pts)
            pts.extend(map(tuple, pl))
            lines.append(len(pl))
            lines.extend(range(base, base + len(pl)))
        if p.mesh is not None and not p.mesh.empty():
            m = p.mesh
            base = len(pts)
            pts.extend(map(tuple, m.verts))
            for t in m.tris:
                polys += [3] + [base + int(x) for x in t]
        if not pts:
            continue
        pts = np.asarray(pts, dtype=np.float64).reshape((-1, 3))
        ds = {
            'metadata': {'name': p.name},
            'vtkClass': 'vtkPolyData',
            'points': {'vtkClass': 'vtkPoints', 'name': '_points',
                       'numberOfComponents': 3, 'size': int(len(pts) * 3),
                       'dataType': 'Float32Array', 'buffer': b64(pts, '<f4'),
                       'ranges': ranges(pts)},
            'pointData': {
                'vtkClass': 'vtkDataSetAttributes',
                'attributes': [{
                    'vtkClass': 'vtkDataArray', 'name': 'color',
                    'numberOfComponents': 3, 'size': int(len(pts) * 3),
                    'dataType': 'Float32Array',
                    'buffer': b64(np.tile(np.asarray(p.color, dtype=np.float64), (len(pts), 1)), '<f4'),
                }]},
        }
        if polys:
            ds['polys'] = {'vtkClass': 'vtkCellArray', 'name': '_polys',
                           'numberOfComponents': 1, 'size': len(polys),
                           'dataType': 'Uint32Array', 'buffer': b64(polys, '<u4')}
        if lines:
            ds['lines'] = {'vtkClass': 'vtkCellArray', 'name': '_lines',
                           'numberOfComponents': 1, 'size': len(lines),
                           'dataType': 'Uint32Array', 'buffer': b64(lines, '<u4')}
        datasets.append((f'{i}', p.name, ds))
        scene.append({
            'name': p.name, 'type': 'httpdataset',
            'httpdataset': {'url': f'{i}/'},
            'rendering': {
                'actor': {'origin': [0, 0, 0], 'scale': [1, 1, 1], 'position': [0, 0, 0]},
                'mapper': {'colorByArrayName': 'color', 'scalarMode': 'usePointData'},
            },
        })
    index = {'metadata': {'fetchGzip': False}, 'scene': scene}
    with zipfile.ZipFile(path, 'w', zipfile.ZIP_DEFLATED) as z:
        z.writestr('index.json', json.dumps(index))
        for folder, _, ds in datasets:
            z.writestr(f'{folder}/index.json', json.dumps(ds))


EXPORTERS = {
    'obj': (export_obj, '.obj'),
    'stl': (export_stl, '.stl'),
    'ply': (export_ply, '.ply'),
    'vrml': (export_vrml, '.wrl'),
    'x3d': (export_x3d, '.x3d'),
    'xml': (export_x3d, '.x3d'),
    'vtk': (export_vtk, '.vtk'),
    'gltf': (export_gltf, '.gltf'),
    'glb': (export_glb, '.glb'),
    'step': (export_step, '.step'),
    'vtkjs': (export_vtkjs, '.vtkjs'),
}


# ---------------------------------------------------------------------------
# main (same flow as the original script)
# ---------------------------------------------------------------------------

def main(instr=None, dirname=None, **kwds):
    from logging import basicConfig, INFO
    import time
    basicConfig(level=INFO)

    fmt = kwds.get('format', 'stl')

    # output directory
    if dirname is None:
        from datetime import datetime as dt
        p = Path(instr).resolve()
        dirname = str(p.parent.joinpath(f"{p.stem}_{dt.strftime(dt.now(), '%Y%m%d_%H%M%S')}"))

    from mccodelib.mcdisplayutils import McDisplayReader
    reader = McDisplayReader(dir=dirname, instr=instr, **kwds)
    instrument = reader.read_instrument()

    # The reader fails if this is done before reader.read_instrument() above ?!
    root = Path(dirname)
    root.mkdir(parents=True, exist_ok=True)

    # Convert the instrument into tessellated parts, then save
    parts = instrument_to_parts(instrument)
    output = str(root.joinpath(f"{Path(instr).stem}{EXPORTERS[fmt][1]}"))
    EXPORTERS[fmt][0](parts, output)
    print(f"Wrote {output} ({len(parts)} components)")

    # Use mccode_config / the OS to open the resulting file...
    if Path(output).is_file():
        subprocess.Popen('%s %s' % (mccode_config.configuration['BROWSER'], output), shell=True)


def output_format(astring):
    f = astring.lower()
    if f in ('step', 'stl', 'xml', 'vrml', 'gltf', 'vtkjs', 'obj', 'ply', 'glb', 'x3d', 'vtk'):
        return f
    print(f"Unknown file format {astring}. Choose between STEP, STL, VRML, GLTF, GLB, "
          f"OBJ, PLY, X3D, VTK, VTKJS, and XML (X3D)")
    return 'stl'


if __name__ == '__main__':
    from mccodelib.mcdisplayutils import make_common_parser
    # Only pre-sets instr, --default, options
    parser, prefix = make_common_parser(__file__, __doc__)

    parser.add_argument('--dirname', help='output directory name override')
    parser.add_argument('-n', '--ncount', dest='n', type=float, default=0, help='Number of particles to simulate')
    parser.add_argument('-f', '--format', dest='format', type=output_format, default='stl', help='Output CAD file format (STEP, STL, VRML, GLTF, GLB, OBJ, PLY, X3D, VTK, and XML/X3D)')

    args, unknown = parser.parse_known_args()
    # if --inspect --first or --last are given after instr, the remaining args become "unknown",
    # but we assume that they are instr_options
    args = {k: args.__getattribute__(k) for k in dir(args) if k[0] != '_'}
    if len(unknown):
        args['options'] = unknown

    main(**args)
