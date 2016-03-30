'''
Classes for representing a mcstas instruments and neutron rays.
'''
class InstrumentConcrete:
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

class Component:
    ''' represents a mcstas component, in context-free form '''
    name = ''
    draw_commands = []
    pos = None
    rot = None
    def __init__(self, name, pos, rot):
        self.name = name
        self.pos = pos
        self.rot = rot

class NeutronRayStory:
    ''' represents a neutron ray storyline - each entry of args should be an 11-list '''
    events = []
    def __init__(self, *args):
        try: 
            for a in args:
                event = NeutronRayState(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10])
                self.events.append(event)
        except:
            raise Exception('NeutronRayStory: Each entry of args should be an 11-list of mcstas neutron ray coordinates.')

class NeutronRayState:
    ''' represents a semi-classical neutron with position, velocity, time, spin and intensity '''
    position = None
    velocity = None
    time = None
    spin = None
    intensity = None
    def __init__(self, x, y, z, vx, vy, vz, t, sx, sy, sz, intensity):
        self.position = Vector3d(x, y, z)
        self.velocity = Vector3d(vx, vy, vz)
        self.time = t
        self.spin = Vector3d(sx, sy, sz)
        self.intensity = intensity

class Vector3d:
    ''' mcstas- and pythified 3d vector '''
    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z

