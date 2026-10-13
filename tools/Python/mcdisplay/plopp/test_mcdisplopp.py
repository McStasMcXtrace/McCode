import numpy as np
from mcdisplopp import _polylines, _projection

xyz = _polylines([[(0, 0, 0), (1, 2, 3)], [(4, 5, 6)]])
assert xyz.shape == (5, 3) and np.isnan(xyz[2]).all() and np.isnan(xyz[4]).all()
assert _polylines([]).shape == (0, 3)  # -n 0: no rays
da = _projection(xyz, 'z', 'x')  # z horizontal, x vertical
assert da.dims == ('z',) and da.coords['z'].values[1] == 3 and da.values[1] == 1
print('ok')

from collections import Counter
from mcdisplopp import _tubes, ICO_F, ICO_V
edges = Counter(tuple(sorted(e)) for f in ICO_F for e in ((f[0], f[1]), (f[1], f[2]), (f[2], f[0])))
assert len(edges) == 30 and set(edges.values()) == {2}  # closed icosahedron
assert np.allclose(np.linalg.norm(ICO_V, axis=1), 1)

v, f = _tubes(xyz, r=0.1, k=6)  # 1 valid segment (others hit NaN), no corners
assert v.shape == (2 * 6, 3) and f.shape == (2 * 6, 3) and f.max() == len(v) - 1
vL, fL = _tubes(np.array([(0, 0, 0), (1, 0, 0), (1, 1, 0), (1, 2, 0.)]), r=0.1, k=6)  # one 90 deg corner, one straight joint
assert vL.shape == (3 * 12 + 1 * 12, 3) and fL.shape == (3 * 12 + 1 * 20, 3) and np.allclose(vL[36:].mean(0), (1, 0, 0))
a, b = np.array([0, 0, 0]), np.array([1, 2, 3])
d = (b - a) / np.linalg.norm(b - a)
off = v[:12] - a
assert np.allclose(np.linalg.norm(off - np.outer(off @ d, d), axis=1), 0.1)  # ring radius about the axis
print('ok tubes')
