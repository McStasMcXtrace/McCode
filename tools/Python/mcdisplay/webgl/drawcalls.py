'''
Classes used for organizing component drawing calls.
'''
from instrrep import Vector3d

drawcommands = {
    'magnify'     : 'DrawMagnify',
    'line'        : 'DrawLine',
    'dashed_line' : 'DrawDashedLine',
    'multiline'   : 'DrawMultiline',
    'rectangle'   : 'DrawRectangle',
    'box'         : 'DrawBox',
    'circle'      : 'DrawCircle',
    }

def drawclass_factory(commandname, args):
    ''' a pythonic object factory by command name '''
    try:
        klass = globals()[drawcommands[commandname]]
        return klass(args)
    except:
        raise Exception('DrawCommandFactory: error with commandname: %s, args: %s' % (commandname, args))

class DrawCommand(object):
    ''' superclass of all draw commands '''

class DrawNeutronRayStory(DrawCommand):
    ''' just input a NeutronRayStory here '''
    events = []
    def __init__(self, story):
        for s in story.events:
            # TODO: should s be cloned?
            self.story.append(s)

class DrawMagnify(DrawCommand):
    ''' '''
    def __init__(self, args):
        pass

class DrawLine(DrawCommand):
    ''' '''
    point_1 = None
    point_2 = None
    # x_1, y_1, z_1, x_2, y_2, z_2
    def __init__(self, *args):
        self.point_1 = Vector3d(float(args[0]), float(args[1]), float(args[2]))
        self.point_2 = Vector3d(float(args[3]), float(args[4]), float(args[5]))

class DrawDashedLine(DrawCommand):
    ''' '''
    point_1 = None
    point_2 = None
    # x_1, y_1, z_1, x_2, y_2, z_2
    def __init__(self, args):
        self.point_1 = Vector3d(float(args[0]), float(args[1]), float(args[2]))
        self.point_2 = Vector3d(float(args[3]), float(args[4]), float(args[5]))

class DrawMultiline(DrawCommand):
    ''' '''
    points = []
    def __init__(self, args):
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
            raise Exception('DrawMultiline: *args must contain a whole number of tripples.')

class DrawRectangle(DrawCommand):
    ''' '''
    plane = ''
    center = None
    width = None
    height = None
    # plane, x, y, z, width, height
    def __init__(self, args):
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
        self.plane = str(args[0])
        self.center = Vector3d(float(args[1]), float(args[2]), float(args[3]))
        self.radius = float(args[4])
