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
    ''' represents a mcstas component, in context-free form '''
    name = ''
    drawcommands = []
    pos = None
    rot = None
    def __init__(self, name, pos, rot):
        self.name = name
        self.pos = pos
        self.rot = rot

class NeutronStory(object):
    events = []
    def __init__(self):
        self.events= []

class NeutronState(object):
    position = None
    velocity = None
    time = None
    spin = None
    intensity = None
    def __init__(self, args):
        ''' x, y, z, vx, vy, vz, t, sx, sy, sz, intensity '''
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
    
    def tolst(self):
        return [self.x, self.y, self.z]

class Matrix3(object):
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

class Matrix3Identity(Matrix3):
    def __init__(self):
        Matrix3.__init__(self, 1, 0, 0, 0, 1, 0, 0, 0, 1)

