import numpy as np
from mcdisplopp import _polylines, _projection

xyz = _polylines([[(0, 0, 0), (1, 2, 3)], [(4, 5, 6)]])
assert xyz.shape == (5, 3) and np.isnan(xyz[2]).all() and np.isnan(xyz[4]).all()
assert _polylines([]).shape == (0, 3)  # -n 0: no rays
da = _projection(xyz, 'z', 'x')  # z horizontal, x vertical
assert da.dims == ('z',) and da.coords['z'].values[1] == 3 and da.values[1] == 1
print('ok')
