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

class DrawCommand(Visited):
    ''' superclass of all draw commands '''
    args_str = ''
    key = ''
    def __init__(self, args):
        if len(args) > 0:
            self.args_str = str(args[0])
            for i in range(len(args)-1):
                self.args_str = self.args_str + ', ' + str(args[i+1])

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
