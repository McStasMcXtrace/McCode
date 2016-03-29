'''
Classes used for organizing component drawing calls.
'''
from instrrep import Vector3d


class DrawCommand():
    ''' superclass of all draw commands '''
    pass

class DrawNeutronRayStory(DrawCommand):
    ''' just input a NeutronRayStory here '''
    events = []
    def __init__(self, story):
        for s in story.events:
            # TODO: should s be cloned?
            self.story.append(s)

class DrawMagnify(DrawCommand):
    ''' magnify is not used for full 3d traces '''
    pass

class DrawLine(DrawCommand):
    ''' '''
    point_1 = None
    point_2 = None
    def __init__(self, x_1, y_1, z_1, x_2, y_2, z_2):
        self.point_1 = Vector3d(x_1, y_1, z_1)
        self.point_2 = Vector3d(x_2, y_2, z_2)

class DrawDashedLine(DrawCommand):
    ''' '''
    point_1 = None
    point_2 = None
    def __init__(self, x_1, y_1, z_1, x_2, y_2, z_2):
        self.point_1 = Vector3d(x_1, y_1, z_1)
        self.point_2 = Vector3d(x_2, y_2, z_2)

class DrawMultiline(DrawCommand):
    ''' construct with a list of length divisible by 3, containing 1d points '''
    points = []
    def __init__(self, *args):
        l = len(args)
        try:
            if not ((l % 3) == 0):
                raise Exception()
            for i in range(l / 3):
                x = args[i*3]
                y = args[i*3+1]
                z = args[i*3+2]
                self.points.append(Vector3d(x, y, z))
        except:
            raise Exception('DrawMultiline: *args must contain a whole number of tripples.')

class DrawRectangle(DrawCommand):
    ''' '''
    plane = ''
    center = None
    width = None
    height = None
    def __init__(self, plane, x, y, z, width, height):
        self.plane = plane
        self.center = Vector3d(x, y, x)
        self.width = width
        self.height = height

class DrawBox(DrawCommand):
    ''' '''
    center = None
    xwidth = None
    yheight = None
    zlength = None
    def __init__(self, x, y, z, xwidth, yheight, zlength):
        self.center = Vector3d(x, y, x)
        self.xwidth = xwidth
        self.yheight = yheight
        self.zlength = zlength

class DrawCircle(DrawCommand):
    ''' '''
    plane = ''
    center = None
    radius = None
    def __init__(self, plane, x, y, z, radius):
        self.plane = plane
        self.center = Vector3d(x, y, z)
        self.radius = radius
