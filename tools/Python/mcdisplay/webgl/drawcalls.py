'''
Classes used for organizing component drawing calls.
'''
from django.template import Context, Template
from django.conf import settings
from instrrep import Vector3d, floatify

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

class DrawCommandVisitor(object):
    ''' implements visitor interface for drawcalls '''
    def visit(self, data_obj):
        for key in drawcommands:
            # get type object from draw command dict
            klass = globals()[drawcommands[key]]
            # check for match with data_obj
            if isinstance(data_obj, klass):
                if key == 'magnify':
                    self.visit_DrawMagnify(data_obj)
                elif key == 'line':
                    self.visit_DrawLine(data_obj)
                elif key == 'dashed_line':
                    self.visit_DrawDashedLine(data_obj)
                elif key == 'multiline':
                    self.visit_DrawMultiline(data_obj)
                elif key == 'rectangle':
                    self.visit_DrawRectangle(data_obj)
                elif key == 'box':
                    self.visit_DrawBox(data_obj)
                elif key == 'circle':
                    self.visit_DrawCircle(data_obj)
    
    def throwabstracterror(self):
        raise Exception('DrawCommandVisitor: Abstract fct called, subclass and implement.')
    
    # subclass and implement draw call action functions below
    def visit_DrawMagnify(self, data_obj):
        self.throwabstracterror()
    
    def visit_DrawLine(self, data_obj):
        self.throwabstracterror()
    
    def visit_DrawDashedLine(self, data_obj):
        self.throwabstracterror()
    
    def visit_DrawMultiline(self, data_obj):
        self.throwabstracterror()
    
    def visit_DrawRectangle(self, data_obj):
        self.throwabstracterror()
    
    def visit_DrawBox(self, data_obj):
        self.throwabstracterror()
    
    def visit_DrawCircle(self, data_obj):
        self.throwabstracterror()
    
class DjangoVisitor(DrawCommandVisitor):
    ''' implements translation from data object to django template readable '''
    dct = {}
    def __init__(self, dct):
        self.dct = dct
    
    def visit_DrawMagnify(self, data_obj):
        pass
    
    def visit_DrawLine(self, data_obj):
        pass
    
    def visit_DrawDashedLine(self, data_obj):
        pass
    
    def visit_DrawMultiline(self, data_obj):
        ''' implement '''
        return ''
    
    def visit_DrawRectangle(self, data_obj):
        pass
    
    def visit_DrawBox(self, data_obj):
        pass
    
    def visit_DrawCircle(self, data_obj):
        ''' implement '''
        return ''
    
class Visited(object):
    ''' visitor interface '''
    def accept(self, visitor):
        visitor.visit(self)

class BoundingBox(object):
    ''' bounding box '''
    def __init__(self, x1=None, x2=None, y1=None, y2=None, z1=None, z2=None):
        self.x1 = x1
        self.x2 = x2
        self.y1 = y1
        self.y2 = y2
        self.z1 = z1
        self.z2 = z2
        
    def add(self, box):
        x1 = min(self.x1, box.x1)
        x2 = max(self.x2, box.x2)
        y1 = min(self.y1, box.y1)
        y2 = max(self.y2, box.y2)
        z1 = min(self.z1, box.z1)
        z2 = max(self.z2, box.z2)
        return BoundingBox(x1, x2, y1, y2, z1, z2)

def calcLargesBoundingVolume(drawcalls):
    ''' adds all of the boudning boxes of elements in drawcalls '''
    box = BoundingBox(0, 0, 0, 0, 0, 0)
    for drawcal in drawcalls:
        box = box.add(drawcal.get_boundingbox())
    return box

def calcLargestBoundingVolumeWT(drawcalls_transforms):
    ''' 
    adds all of the boudning boxes of elements in drawcalls
    
        drawcalls_transforms : a list of 2 tuples containing a drawcall and a tranform
    '''
    box = drawcalls_transforms[0][0].get_boundingbox(transform=drawcalls_transforms[0][1])
    for d_t in drawcalls_transforms:
        newbox = d_t[0].get_boundingbox(transform=d_t[1])
        box = box.add(newbox)
        
    return box

class DrawCommand(Visited):
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
    
    def _calc_boundingbox(self, points, transform=None):
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
        
        self.point_1 = Vector3d(float(args[0]), float(args[1]), float(args[2]))
        self.point_2 = Vector3d(float(args[3]), float(args[4]), float(args[5]))

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
        rad = self.radius
        cen = self.center
        ne = cen.add(Vector3d(rad, rad, 0))
        nw = cen.add(Vector3d(-rad, rad, 0))
        sw = cen.add(Vector3d(-rad, -rad, 0))
        se = cen.add(Vector3d(rad, -rad, 0))
        return [ne, nw, se, sw]

class TemplateWebGLWrite(object):
    ''' writes the django template from the instrument representation '''
    instrument = None
    text = ''
    templatefile = ''
    campos = None
    
    def __init__(self, instrument, templatefile, campos):
        self.instrument = instrument
        self.templatefile = templatefile
        self.campos = campos
        settings.configure()
    
    def build(self):
        templ = open(self.templatefile).read()
        t = Template(templ)
        c = Context({'instrument': self.instrument, 
                     'campos_x': self.campos.x, 'campos_y': self.campos.y, 'campos_z': self.campos.z,})
        self.text = t.render(c)

    def save(self, filename):
        ''' save template to disk '''
        try:
            f = open(filename, 'w')
            f.write(self.text)
        finally:
            f.close()

