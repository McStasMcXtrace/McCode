'''
Classes for representing a mcstas instruments and neutron rays.
'''
class InstrumentConcrete(object):
    ''' represents a mcstas instrument with params choice '''
    name = ''
    abspath = ''
    params = []
    params_defaults = []
    
    params_values = []
    components = []
    rays = []
    
    def __init__(self, name, params, params_defaults):
        self.name = name
        self.params = params
        self.params_defaults = params_defaults

class Component(object):
    ''' represents a mcstas component in some context '''
    name = ''
    drawcommands = []
    pos = None
    rot = None
    m4_str = ''
    def __init__(self, name, pos, rot):
        self.name = name
        self.pos = pos
        self.rot = rot
        self.drawcommands = []
        if pos != None and rot != None:
            self.m4_str = '%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, 0, 0, 0, 1' % (str(rot.a11), str(rot.a12), str(rot.a13), str(pos.x), str(rot.a21), str(rot.a22), str(rot.a23), str(pos.y), str(rot.a31), str(rot.a32), str(rot.a33), str(pos.z))
    
    @classmethod
    def from_m4_str(cls, name, m4_str):
        obj = cls(name, None, None)
        obj.m4_str = m4_str
        return obj
        
class NeutronStory(object):
    events = []
    def __init__(self):
        self.events = []

class NeutronState(object):
    args_str = ''
    position = None
    velocity = None
    time = None
    spin = None
    intensity = None
    def __init__(self, args):
        ''' x, y, z, vx, vy, vz, t, sx, sy, sz, intensity '''
        self.args_str = str(args[0])
        if len(args) > 0:
            self.args_str = str(args[0])
            for i in range(len(args)-1):
                self.args_str = self.args_str + ', ' + str(args[i+1])
        
        self.position = Vector3d(float(args[0]), float(args[1]), float(args[2]))
        self.velocity = Vector3d(float(args[3]), float(args[4]), float(args[5]))
        self.time = float(args[6])
        self.spin = Vector3d(float(args[7]), float(args[8]), float(args[9]))
        self.intensity = float(args[10])

class Vector3d(object):
    def __init__(self, x, y, z):
        self.x = float(x)
        self.y = float(y)
        self.z = float(z)
    
    def add(self, v):
        ''' just add another vector to this one and return the result '''
        return Vector3d(x=v.x+self.x, y=v.y+self.y, z=v.z+self.z)
    
    def to_lst(self):
        return [self.x, self.y, self.z]
    
    def to_args_str(self):
        return '%s, %s, %s' % (str(self.x), str(self.y), str(self.z))

class Matrix3(object):
    ''' a 3x3 matrix representation '''
    def __init__(self, a11, a12, a13, a21, a22, a23, a31, a32, a33):
        self.a11 = float(a11)
        self.a12 = float(a12)
        self.a13 = float(a13)
        self.a21 = float(a21)
        self.a22 = float(a22)
        self.a23 = float(a23)
        self.a31 = float(a31)
        self.a32 = float(a32)
        self.a33 = float(a33)

    def mult(self, v):
        ''' multiply a matrix by a vector from the right '''
        x = self.a11*v.x + self.a12*v.y + self.a13*v.z 
        y = self.a21*v.x + self.a22*v.y + self.a23*v.z
        z = self.a31*v.x + self.a32*v.y + self.a33*v.z
        return Vector3d(x, y, z)

class Transform(object):
    ''' a rudimentary matrix4 transform '''
    def __init__(self, rot, pos):
        self.a11 = rot.a11
        self.a12 = rot.a12
        self.a13 = rot.a13
        self.a21 = rot.a21
        self.a22 = rot.a22
        self.a23 = rot.a23
        self.a31 = rot.a31
        self.a32 = rot.a32
        self.a33 = rot.a33
        
        self.a14 = pos.x
        self.a24 = pos.y
        self.a34 = pos.z
        
        self.a41 = 0
        self.a42 = 0
        self.a43 = 0
        self.a44 = 1
    
    def apply(self, v3):
        x = self.a11*v3.x + self.a12*v3.y + self.a13*v3.z + self.a14
        y = self.a21*v3.x + self.a22*v3.y + self.a23*v3.z + self.a24
        z = self.a31*v3.x + self.a32*v3.y + self.a33*v3.z + self.a34
        return Vector3d(x, y, z)

class Matrix3Identity(Matrix3):
    def __init__(self):
        Matrix3.__init__(self, 1, 0, 0, 0, 1, 0, 0, 0, 1)

