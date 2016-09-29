'''
Classes for representing a mcstas instruments and particle trace rays,
and classes used for organizing component drawing calls.
'''
import math

class InstrumentSpecific(object):
    ''' represents a mcstas instrument with params choice '''
    def __init__(self, name, params, params_defaults):
        self.name = name
        self.abspath = ''
        self.params = params
        self.params_defaults = params_defaults
        self.params_values = []
        self.components = []
        self.rays = []
        self.cmd = None
    
    def setCmd(self, cmd):
        self.cmd = cmd
    
    def get_boundingbox(self, first=None, last=None):
        components = self.components
        
        cnames = map(lambda c: c.name, self.components)
        if first in cnames and last in cnames:
            i_first = cnames.index(first)
            i_last = cnames.index(last)
            components = filter(lambda c: self.components.index(c) > i_first and self.components.index(c) < i_last, self.components)
        elif first in cnames:
            i_first = cnames.index(first)
            components = filter(lambda c: self.components.index(c) > i_first, self.components)
        elif last in cnames:
            i_last = cnames.index(last)
            components = filter(lambda c: self.components.index(c) < i_last, self.components)
        
        # run the bounding box calculation
        oldbox = None
        for c in components:
            box = c.get_tranformed_bb()
            if oldbox: 
                box = box.add(oldbox)
            oldbox = box
            
        return box
    
    def jsonize(self):
        ''' returns this object in jsonized form '''
        instr = {}

        # properties
        instr['name'] = self.name
        instr['abspath'] = self.abspath
        instr['params'] = self.params
        instr['params_defaults'] = self.params_defaults
        instr['params_values'] = self.params_values
        instr['cmd'] = self.cmd
        
        # bounding box
        instr['boundingbox'] = self.get_boundingbox().jsonize()
        
        # components
        lst = []
        for c in self.components:
            lst.append(c.jsonize())
        instr['components'] = lst
        
        # rays
        lst = []
        for r in self.rays:
            lst.append(r.jsonize())
        instr['rays'] = lst
        
        return instr

class Component(object):
    ''' represents a mcstas component in some context '''
    def __init__(self, name, pos, rot):
        self.name = name
        self.pos = pos
        self.rot = rot
        self.transform = Transform(rot, pos)
        self.m4 = [rot.a11, rot.a12, rot.a13, pos.x, rot.a21, rot.a22, rot.a23, pos.y, rot.a31, rot.a32, rot.a33, pos.z, 0, 0, 0, 1]
        self.drawcalls = []
    
    def get_bounding_box(self):
        ''' calculate and return bounding box in naiive/local coordinates '''
        box = BoundingBox()
        for d in self.drawcalls:
            box = d.get_boundingbox().add(box)
        
        return box
    
    def get_tranformed_bb(self):
        ''' calculate and return bounding box in transformed coordinates '''
        box = BoundingBox()
        for d in self.drawcalls:
            box = d.get_boundingbox(self.transform).add(box)
        
        return box
    
    def jsonize(self):
        ''' returns a jsonized version of this object '''
        component = {}
        
        # properties
        component['name'] = self.name
        component['m4'] = self.m4
        
        # drawcalls
        lst = []
        for d in self.drawcalls:
            lst.append(d.jsonize())
        component['drawcalls'] = lst
        
        return component
    
    def __str__(self):
        return self.name

class BoundingBox(object):
    ''' bounding box '''
    def __init__(self, x1=None, x2=None, y1=None, y2=None, z1=None, z2=None):
        ''' properly initialize the bounding box by infinity/ minus infinity '''
        inf = float("inf")
        ninf = - inf
        self.x1 = x1 or inf 
        self.x2 = x2 or ninf
        self.y1 = y1 or inf
        self.y2 = y2 or ninf
        self.z1 = z1 or inf
        self.z2 = z2 or ninf
        
        self.m4 = [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1]
    
    def add(self, box):
        ''' add and return a combined bounding box '''
        x1 = min(self.x1, box.x1)
        x2 = max(self.x2, box.x2)
        y1 = min(self.y1, box.y1)
        y2 = max(self.y2, box.y2)
        z1 = min(self.z1, box.z1)
        z2 = max(self.z2, box.z2)
        
        return BoundingBox(x1, x2, y1, y2, z1, z2)
    
    def _get_drawcalls(self):
        ''' private method used to describe the bounding box in terms of mcdisplay drawcalls '''
        x1 = self.x1
        x2 = self.x2
        y1 = self.y1
        y2 = self.y2
        z1 = self.z1
        z2 = self.z2
        # the rectangle front and back sides
        d1 = DrawMultiline(args=[x1, y1, z1, x1, y2, z1, x2, y2, z1, x2, y1, z1, x1, y1, z1])
        d2 = DrawMultiline(args=[x1, y1, z2, x1, y2, z2, x2, y2, z2, x2, y1, z2, x1, y1, z2])
        # the four lines connecting
        d3 = DrawLine(args=[x1, y1, z1, x1, y1, z2])
        d4 = DrawLine(args=[x2, y1, z1, x2, y1, z2])
        d5 = DrawLine(args=[x1, y2, z1, x1, y2, z2])
        d6 = DrawLine(args=[x2, y2, z1, x2, y2, z2])
        
        return [d1, d2, d3, d4, d5, d6]
    
    def _get_drawcalls_gridticks(self, unit=1):
        ''' creates the tick marks by drawing a partial grid on the six box faces '''
        
        def gridticks(p1, p2, ticksize=0.1):
            ''' returns DrawLine objects corresponding to a list of "tickmarks" between p1 and p2 '''
            p1 = Vector3d(p1[0], p1[1], p1[2])
            p2 = Vector3d(p2[0], p2[1], p2[2])
            
            v = p2.subtract(p1).normalize()
            v = v.scalarmult(ticksize)
            d1 = DrawLine(args=[p1, p1.add(v)])
            d2 = DrawLine(args=[p2, p2.subtract(v)])
            
            return [d1, d2]
        
        def gridlines(p1, p2):
            ''' returns DrawLine objects corresponding to all gridlines '''
            p1 = Vector3d(p1[0], p1[1], p1[2])
            p2 = Vector3d(p2[0], p2[1], p2[2])
            
            d1 = DrawLine(args=[p1, p2])
            
            return [d1]
        
        def gridpoints(a1, a2, b1, b2):
            ''' returns a list of pairs of (a, b) points, the endpoints of a grid line on axes a and b '''
            unit = 1
            lst = []
            
            # vertical lines 
            a = a1
            while a < a2 - unit:
                a = a + unit
                lst.append( ((a, b1), (a, b2)) )
            
            # horizontal lines 
            b = b1
            while b < b2 - unit:
                b = b + unit
                lst.append( ((a1, b), (a2, b)) )
            
            return lst
        
        # get min/max values shorthand
        x1 = self.x1
        x2 = self.x2
        y1 = self.y1
        y2 = self.y2
        z1 = self.z1
        z2 = self.z2
        
        # orientation planes in local 2d coordinates
        gridlines2d_xy = gridpoints(x1, x2, y1, y2)
        gridlines2d_xz = gridpoints(x1, x2, z1, z2)
        gridlines2d_yz = gridpoints(y1, y2, z1, z2)
        
        # transform gridlines to 3d coordinates
        gridlines3d_xy_min = map(lambda p: ( (p[0][0], p[0][1],      z1), (p[1][0], p[1][1],      z1) ), gridlines2d_xy)
        gridlines3d_xy_max = map(lambda p: ( (p[0][0], p[0][1],      z2), (p[1][0], p[1][1],      z2) ), gridlines2d_xy)
        gridlines3d_xz_min = map(lambda p: ( (p[0][0],      y1, p[0][1]), (p[1][0],      y1, p[1][1]) ), gridlines2d_xz)
        gridlines3d_xz_max = map(lambda p: ( (p[0][0],      y2, p[0][1]), (p[1][0],      y2, p[1][1]) ), gridlines2d_xz)
        gridlines3d_yz_min = map(lambda p: ( (x1,      p[0][0], p[0][1]), (     x1, p[1][0], p[1][1]) ), gridlines2d_yz)
        gridlines3d_yz_max = map(lambda p: ( (x2,      p[0][0], p[0][1]), (     x2, p[1][0], p[1][1]) ), gridlines2d_yz)
        
        # do the gridticks for all six planes:
        calls = []
        for gridlinepoints in [gridlines3d_xy_min, gridlines3d_xy_max, gridlines3d_xz_min, gridlines3d_xz_max, gridlines3d_yz_min, gridlines3d_yz_max]:
            for pair in gridlinepoints:
                calls = calls + gridticks(pair[0], pair[1])
                #calls = calls + gridlines(pair[0], pair[1])
        
        return calls
    
    def jsonize(self):
        ''' returns a jsonized version of this object '''
        box = {}
        
        box['xmin'] = self.x1
        box['xmax'] = self.x2
        box['ymin'] = self.y1
        box['ymax'] = self.y2
        box['zmin'] = self.z1
        box['zmax'] = self.z2
        
        # drawcalls
        lst = []
        drawcalls = self._get_drawcalls() + self._get_drawcalls_gridticks()
        for d in drawcalls:
            lst.append(d.jsonize())
        box['drawcalls'] = lst
        
        return box
    
    def __str__(self):
        return '%s, %s, %s, %s, %s, %s' % (self.x1, self.x2, self.y1, self.y2, self.z1, self.z2)

class RayBundle(object):
    ''' represents a bundle of particles '''
    def __init__(self, rays):
        self.rays = rays
    
    def jsonize(self):
        ''' returns a jsonized version of this object '''
        bundle = {}
        
        # rays
        lst =  []
        for r in self.rays:
            lst.append(r.jsonize())
        bundle['rays'] = lst
        
        # number of rays
        bundle['numrays'] = len(lst)
        
        # min and max velocity
        vmin = None
        vmax = None
        for r in self.rays:
            speed = r.get_speed()
            vmin = min(vmin, speed) or speed
            vmax = max(vmax, speed) or speed
        bundle['vmin'] = vmin
        bundle['vmax'] = vmax
        
        return bundle

class ParticleStory(object):
    ''' represents a whole particle ray from start to finish '''
    def __init__(self):
        self.groups = []
        self.speed = None
    
    def add_group(self, group):
        self.groups.append(group)
    
    def get_speed(self):
        ''' on-demand speed of this particle ray, which is incorrectly assumed to be constant '''
        if not self.speed:
            args = self.groups[len(self.groups)-1].events[0].args
            self.speed = math.sqrt(args[3]*args[3] + args[4]*args[4] + args[5]*args[5])
        return self.speed
    
    def jsonize(self):
        ''' returns a jsonized version of this object '''
        story = {}
        
        # component-coordinate event groups
        lst = []
        for g in self.groups:
            lst.append(g.jsonize())
        story['groups'] = lst
        
        # speed
        story['speed'] = self.get_speed()
        
        return story

class ParticleCompGroup(object):
    ''' represents particle events / states within the context of a specific component '''
    def __init__(self, compname):
        self.compname = compname
        self.events = []
    
    def add_event(self, event):
        self.events.append(event)
        
    def jsonize(self):
        ''' returns a jsonized version of this object '''
        group = {}
        
        # properties
        group['compname'] = self.compname
        
        # lists
        lst = []
        for e in self.events:
            lst.append(e.jsonize())
        group['events'] = lst
        
        return group

class ParticleState(object):
    ''' represents a single particle (ray) event, a semiclassical state, used for ray interpolation inferrence '''
    def __init__(self, args, verbose=False):
        ''' x, y, z, vx, vy, vz, t, sx, sy, sz, intensity '''
        self.args = floatify(args)
        self.args_str = str(args[0])
        if len(args) > 0:
            self.args_str = str(args[0])
            for i in range(len(args)-1):
                self.args_str = self.args_str + ', ' + str(args[i+1])
        
        if verbose:
            self.position = Vector3d(float(args[0]), float(args[1]), float(args[2]))
            self.velocity = Vector3d(float(args[3]), float(args[4]), float(args[5]))
            self.time = float(args[6])
            self.spin = Vector3d(float(args[7]), float(args[8]), float(args[9]))
            self.intensity = float(args[10])
    
    @classmethod
    def fromcoords(cls, coords):
        "Initialize from a list of floats"
        #data = open(filename).readlines()
        #return cls(data)
    
    def jsonize(self):
        ''' returns a jsonized version of this object '''
        state = {}
        
        # properties
        state['args'] = self.args
        
        return state

# links mcstas draw api to the corresponding python class names '''
drawcommands = {
    'magnify'     : 'DrawMagnify',
    'line'        : 'DrawLine',
    'dashed_line' : 'DrawDashedLine',
    'multiline'   : 'DrawMultiline',
    'rectangle'   : 'DrawRectangle',
    'box'         : 'DrawBox',
    'circle'      : 'DrawCircle',
    }
# reduced set containing wholly implemented and non-trivial commands
reduced_drawcommands = {
    'multiline'   : 'DrawMultiline',
    'circle'      : 'DrawCircle',
    }

def drawclass_factory(commandname, args, reduced=False):
    ''' a pythonic object factory by command name '''
    try:
        # return None if we are dealing with a reduced set (mainly a way to get rid of Magnify)
        if reduced and commandname not in reduced_drawcommands:
            return None
        klass = globals()[drawcommands[commandname]]
        return klass(args)
    except:
        raise Exception('DrawCommandFactory: error with commandname: %s, args: %s' % (commandname, args))

class DrawCommand(object):
    ''' superclass of all draw commands '''
    def __init__(self, args):
        self.args = floatify(args)
        self.args_str = ''
        self.key = ''
        self.boundingbox = None
        if len(args) > 0:
            self.args_str = str(args[0])
            for i in range(len(args)-1):
                self.args_str = self.args_str + ', ' + str(args[i+1])
    
    def get_boundingbox(self, transform=None):
        self.boundingbox = self._calc_boundingbox(self._get_points(), transform)
        return self.boundingbox
    
    def _get_points(self):
        return
    
    @classmethod
    def _calc_boundingbox(self, points, transform):
        ''' override to implement alternative OR implement get_points '''
        if not points:
            return
        
        box = BoundingBox()
        x_set = []
        y_set = []
        z_set = []
        
        for p in points:
            if transform:
                p = transform.apply(p)
            x_set.append(p.x)
            y_set.append(p.y)
            z_set.append(p.z)
        
        box.x1 = min(x_set)
        box.x2 = max(x_set)
        box.y1 = min(y_set)
        box.y2 = max(y_set)
        box.z1 = min(z_set)
        box.z2 = max(z_set)
                
        return box
    
    def jsonize(self):
        ''' returns a jsonzied version of this object '''
        call = {}
        
        # properties
        call['key'] = self.key
        #call['args_str'] = self.args_str
        call['args'] = self.args
        
        # forget the bounding box for now
        
        return call

class DrawMagnify(DrawCommand):
    ''' not implemented, a placeholder '''
    def __init__(self, args):
        super(DrawMagnify, self).__init__(args)
        self.key = 'magnify'

class DrawLine(DrawCommand):
    ''' '''
    point_1 = None
    point_2 = None
    # x_1, y_1, z_1, x_2, y_2, z_2
    def __init__(self, args):
        super(DrawLine, self).__init__(args)
        self.key = 'line'
        
        if type(args[0]) is Vector3d and type(args[1]) is Vector3d:
            self.point_1 = args[0]
            self.point_2 = args[1]
            self.args = [args[0].x, args[0].y, args[0].z, args[1].x, args[1].y, args[1].z]
        else:
            self.point_1 = Vector3d(float(args[0]), float(args[1]), float(args[2]))
            self.point_2 = Vector3d(float(args[3]), float(args[4]), float(args[5]))
            self.args = args

class DrawDashedLine(DrawCommand):
    ''' '''
    point_1 = None
    point_2 = None
    # x_1, y_1, z_1, x_2, y_2, z_2
    def __init__(self, args):
        super(DrawDashedLine, self).__init__(args)
        self.key = 'dashed_line'
        
        self.point_1 = Vector3d(float(args[0]), float(args[1]), float(args[2]))
        self.point_2 = Vector3d(float(args[3]), float(args[4]), float(args[5]))

class DrawMultiline(DrawCommand):
    ''' '''
    points = []
    def __init__(self, args):
        super(DrawMultiline, self).__init__(args)
        self.key = 'multiline'
        self.points = []
        
        l = len(args)
        try:
            if not ((l % 3) == 0):
                raise Exception()
            for i in range(l / 3):
                x = float(args[i*3])
                y = float(args[i*3+1])
                z = float(args[i*3+2])
                self.points.append(Vector3d(x, y, z))
        except:
            raise Exception('DrawMultiline: args must contain a whole number of tripples.')
    
    def _get_points(self):
        return self.points
    
class DrawRectangle(DrawCommand):
    ''' '''
    plane = ''
    center = None
    width = None
    height = None
    # plane, x, y, z, width, height
    def __init__(self, args):
        super(DrawRectangle, self).__init__(args)
        self.key = 'rectangle'
        
        self.plane = str(args[0])
        self.center = Vector3d(float(args[1]), float(args[2]), float(args[3]))
        self.width = float(args[4])
        self.height = float(args[5])

class DrawBox(DrawCommand):
    ''' '''
    center = None
    xwidth = None
    yheight = None
    zlength = None
    # x, y, z, xwidth, yheight, zlength
    def __init__(self, args):
        super(DrawBox, self).__init__(args)
        self.key = 'box'
        
        self.center = Vector3d(float(args[0]), float(args[1]), float(args[2]))
        self.xwidth = float(args[3])
        self.yheight = float(args[4])
        self.zlength = float(args[5])

class DrawCircle(DrawCommand):
    ''' '''
    plane = ''
    center = None
    radius = None
    # plane, x, y, z, radius
    def __init__(self, args):
        super(DrawCircle, self).__init__(args)
        self.key = 'circle'
        
        self.plane = str(args[0])
        self.center = Vector3d(float(args[1]), float(args[2]), float(args[3]))
        self.radius = float(args[4])
        
        # override default behavior to ensure quotes around the first arg, plane
        idx = self.args_str.find(',')
        self.args_str = '\"' + self.args_str[:idx] + '\"' + self.args_str[idx:]
    
    def _get_points(self):
        ''' returns the corners of a flat sqare around the circle, transformed into the proper plane '''
        rad = self.radius
        cen = self.center
        
        ne = Vector3d(rad, rad, 0)
        nw = Vector3d(-rad, rad, 0)
        sw = Vector3d(-rad, -rad, 0)
        se = Vector3d(rad, -rad, 0)
        
        square = [ne, nw, sw, se]
        
        if self.plane == 'xy':
            return map(lambda p: cen.add(p), square)
        elif self.plane == 'xz':
            return map(lambda p: cen.add(Vector3d(p.x, 0, p.y)), square)
        elif self.plane == 'yz':
            return map(lambda p: cen.add(Vector3d(0, p.x, p.y)), square)
        else:
            raise Exception('DrawCircle: invalid plane argument')

class Vector3d(object):
    def __init__(self, x, y, z):
        self.x = float(x)
        self.y = float(y)
        self.z = float(z)
    
    def add(self, v):
        ''' just add another vector to this one and return the result (does not change this instance) '''
        return Vector3d(x = self.x + v.x, y = self.y + v.y, z = self.z + v.z)
    
    def subtract(self, v):
        ''' subtract a vector from this one and return the result (does not change this instance) '''
        return Vector3d(x = self.x - v.x, y = self.y - v.y, z = self.z - v.z)
    
    def scalarmult(self, s):
        ''' multiply this by a scalar and return the result (does not change this instance) '''
        return Vector3d(x = self.x*s, y = self.y*s, z = self.z*s)
    
    def norm(self):
        ''' returns the norm of this object '''
        return math.sqrt(self.x*self.x + self.y*self.y + self.z*self.z)
    
    def normalize(self):
        ''' returns the norm of this object '''
        factor = 1 / self.norm()
        return self.scalarmult(factor)
    
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

def floatify(org_lst):
    ''' returns a transformed list with entries converted to floats, if possible '''
    new_lst = []
    for a in org_lst:
        try:
            new_lst.append(float(a))
        except:
            new_lst.append(a)
    return new_lst
