from mcplopp import _edges, to_dataarray
from mccodelib.mcplotloader import Data2D

dim, e = _edges('Wavelength [AA]', 0, 10, 20)
assert dim == 'Wavelength' and str(e.unit) == 'Å' and e.sizes == {dim: 21}
dim, e = _edges('TOF [\\gms]', 0, 1, 4)  # unknown unit: kept in label, dimensionless
assert dim == 'TOF [\\gms]' and str(e.unit) == 'dimensionless'

d = Data2D()  # array_2d(3, 2): 2 rows (y) x 3 columns (x)
d.xlabel, d.ylabel, d.xlimits, d.zvals = 'x [cm]', 'y [cm]', (0, 3, 0, 2), [[1, 2, 3], [4, 5, 6]]
da = to_dataarray(d)
assert da.dims == ('y', 'x') and da.coords['x'].sizes == {'x': 4} and da['x', 2]['y', 1].value == 6
print('ok')
