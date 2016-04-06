'''
Implementation of classes involved in the PLY-based translation of the mcdisplay "--trace output" 
mini language.

Read the PLY documentation here: http://www.dabeaz.com/ply/ply.html#ply_nn23.
'''
from ply import lex, yacc
from instrrep import InstrumentConcrete, Component, Vector3d, NeutronStory, NeutronState, Matrix3, Matrix3Identity
from drawcalls import drawclass_factory

class Node(object):
    ''' 
    Node objects are used to construct the trace parse tree. 
    '''
    def __init__(self, type, children=None, leaf=None):
        self.type = type
        if children:
            self.children = children
        else:
            self.children = [ ]
        self.leaf = leaf
    def __str__(self):
        return 'type: %s, leaf: %s, numchildren: %s' % (self.type, str(self.leaf), str(len(self.children)))

class NodeTreePrint(object):
    '''
    Functional-ish programming implementation of node tree operation: iteratively print nodes 
    in human readable form.
    
    Node tree assumptions: children is a list of Node's, leaf is for data
    Does not print any rays by default, enable by using printrays=True during construction.
    '''
    def __init__(self, rootnode, printrays=False):
        self.level = 0
        self.recurse(rootnode, printrays, self.printfunc, self.inclevel, self.declevel, self.getlevel)
    
    def getlevel(self):
        return self.level
    
    def inclevel(self):
        self.level += 1
    
    def declevel(self):
        self.level -= 1
    
    @staticmethod
    def printfunc(node, level):
        fs = '{:>' + str(level*4) + '}' # e.g. '{:<16}'
        type = node.type
        leaf = ''
        if node.leaf:
            leaf = node.leaf
        print fs.format('') + '%s: %s' % (type, leaf)
    
    @staticmethod
    def recurse(node, printrays, printfunc, inclevel, declevel, getlevel):
        for c in node.children:
            # exit if printrays is not set
            if c.type=='rays' and not printrays:
                return
            
            level = getlevel()
            printfunc(c, level)
            if len(c.children) > 0:
                inclevel()
                NodeTreePrint.recurse(c, printrays, printfunc, inclevel, declevel, getlevel)
        declevel()

class InstrProduction:
    '''
    Completes the instrument and ray reconstruction from the "syntax tree" produced by TraceParser,
    outputting a wholly interpreted instrument and ray model from the mcdisplay trace output.
    
    It is very specific, because the instrument tree structure is very specific and non-recursive.
    '''
    instrument_tree = None
    def __init__(self, parsetreeroot):
        self.root = parsetreeroot
        if (type(self.root) is not Node) or (self.root.type != 'traceparsetree'):
            raise Exception('InstrProduction: parsetreeroot must be a Node of type "traceparsetree"')
    
    def build(self):
        ''' builds the instrument representation class based on the parse tree constructor arg '''
        
        # create instrument object
        self.instrument_tree = InstrumentConcrete(name='', params=[], params_defaults=[])
        
        # iterate through parse tree
        for dc in self.root.children:
            # handle instrument branch
            if dc.type == 'instrument':
                for ic in dc.children:    
                    if ic.type == 'instr_name':
                        self.instrument_tree.name = ic.leaf
                    if ic.type == 'abspath':
                        self.instrument_tree.abspath = ic.leaf
            
            # handle component branch
            if dc.type == 'comps':
                for csc in dc.children:
                    
                    if csc.type == 'comp':
                        name = ''
                        pos = None
                        rot = None
                        
                        # get name, pos and rot
                        for cc in csc.children:
                            if cc.type == 'comp_name':
                                name = cc.leaf
                            if cc.type == '12dec':
                                pos = Vector3d(x=float(cc.leaf[0]), y=float(cc.leaf[1]), z=float(cc.leaf[2]))
                                rot = Matrix3(
                                        a11=float(cc.leaf[3]),
                                        a12=float(cc.leaf[4]),
                                        a13=float(cc.leaf[5]),
                                        a21=float(cc.leaf[6]),
                                        a22=float(cc.leaf[7]),
                                        a23=float(cc.leaf[8]),
                                        a31=float(cc.leaf[9]),
                                        a32=float(cc.leaf[10]),
                                        a33=float(cc.leaf[11])
                                        )
                        comp = Component(name=name, pos=pos, rot=rot)
                        
                        # get draw commands (please print a parse tree with NodeTreePrint to understand this)
                        for cc in csc.children:
                            if cc.type == 'draw_commands':
                                for dc in cc.children:
                                    # get args
                                    try:
                                        if len(dc.children) != 1:
                                            raise Exception()
                                        
                                        argsnode = dc.children[0]
                                        args = argsnode.leaf
                                    except:
                                        raise Exception('InstrProduction: node "comp" -> "draw" -> "args" must be only child')
                                    
                                    commandname = dc.leaf
                                    draw = drawclass_factory(commandname, args)
                                    comp.drawcommands.append(draw)
                        
                        self.instrument_tree.components.append(comp)
            
            # handle rays
            if dc.type == 'rays':
                for rayc in dc.children:
                    
                    # a netron ray's path through the instrument
                    story = NeutronStory()
                    
                    # scope 
                    state = None
                    pos = None
                    rot = None
                    
                    for rayevent in rayc.children:
                        if rayevent.type == 'ENTER':
                            pos = Vector3d(0, 0, 0)
                            rot = Matrix3Identity()
                            state = NeutronState(rayevent.leaf)
                            
                        if rayevent.type == 'COMP':
                            comp_name = rayevent.children[0].leaf
                            pos, rot = self.getcomp_posrot(comp_name)
                            
                            vlst = rayevent.leaf[0:3]
                            v = Vector3d(vlst[0], vlst[1], vlst[2])
                            
                            vg = self.transform_local(v, pos, rot)
                            state = NeutronState(vg.tolst() + rayevent.leaf[3:])
                            
                        if rayevent.type == 'SCATTER':
                            vlst = rayevent.leaf[0:3]
                            v = Vector3d(vlst[0], vlst[1], vlst[2])
                            
                            vg = self.transform_local(v, pos, rot)
                            state = NeutronState(vg.tolst() + rayevent.leaf[3:])
                        
                        if rayevent.type == 'ABSORB':
                            pass
                        
                        if rayevent.type == 'LEAVE':
                            vlst = rayevent.leaf[0:3]
                            v = Vector3d(vlst[0], vlst[1], vlst[2])
                            
                            vg = self.transform_local(v, pos, rot)
                            state = NeutronState(vg.tolst() + rayevent.leaf[3:])
                        
                        story.events.append(state)
                        
                    self.instrument_tree.rays.append(story)
                    
            # handle comments
            # TODO: implement
            if dc.type == 'comments':
                for cmc in dc.children:
                    
                    if cmc.type == '':
                        pass
    
    compdict = None
    def getcomp_posrot(self, comp_name):
        ''' returns a 2-tuple containing component pos and rot vectors, given component name '''
        if not self.compdict:
            self.compdict = {}
            for comp in self.instrument_tree.components:
                self.compdict[comp.name] = (comp.pos, comp.rot)
                
        pos, rot = self.compdict[comp_name]
        return pos, rot
    
    def transform_local(self, v, pos, rot):
        ''' transforms a neutron from local to global coordinates '''
        rotated = rot.mult(v)
        return rotated.add(pos)


class TraceParser:
    '''
    Parser for --trace output enabling mcdisplay instrument drawing minilanguage
    
    In addition to the default INITIAL state in the lexer, the 'initialize', 'save' and 'finally' states are
    intended to parse the corresponding stdout as pure lines of comments.
    This way we can avoid defining a "catchall" token in INITIAL.
    
    The grammar rules only partially handles instrument tree construction, and even the "action code"
    does not enterprit every detail - a lot of stuff regarding coordinates in hanlded in post-production.
    '''
    parsetree = None
    def __init__(self, data=None):
        if data:
            self.build_lexer()
            self.build_parser()
            self.parse(data)
    
    # these tokens match ID, but are of these types (handled in t_ID)
    reserved = {
        'INSTRUMENT'  : 'INSTRUMENT',
        'Instrument'  : 'INSTRKW',
        'COMPONENT'   : 'COMPONENT',
        'Component'   : 'COMPKW',
        'POS'         : 'POS',
        'AT'          : 'AT',
        'MCDISPLAY'   : 'MCDISPLAY',
        'start'       : 'STARTKWLC',
        'end'         : 'ENDKWLC',
        'component'   : 'COMPKWLC',
        'END'         : 'END',
        
        'ENTER'       : 'ENTER',
        'COMP'        : 'COMP',
        'STATE'       : 'STATE',
        'SCATTER'     : 'SCATTER',
        'ABSORB'      : 'ABSORB',
        'LEAVE'       : 'LEAVE',
        
        'magnify'     : 'DRAWCALL',
        'line'        : 'DRAWCALL',
        'dashed_line' : 'DRAWCALL',
        'multiline'   : 'DRAWCALL',
        'rectangle'   : 'DRAWCALL',
        'box'         : 'DRAWCALL',
        'circle'      : 'DRAWCALL',
    }
    
    # tokens 
    tokens = [
              'LB',
              'RB',
              'COLON',
              'QUOTE',
              'SQUOTE',
              'COMMA',
              'NL',
              
              'ABSPATH',
              'DEC',
              'ID',
              
              # tokens only used in states "save" and "finally" (comment blocks)
              'COMMENT'
              ] + list(set(reserved.values()))

    ###############################
    # special states 
    ##############################
    
    # NOTE: these states must be implemented before 'ID' token, which would catch the states' 
    # "start condition" as an ID.
    
    states = (
        ('initialize', 'exclusive'),
        ('save', 'exclusive'),
        ('finally','exclusive'),
    )
    
    # initialize state
    def t_begin_initialize(self, t):
        r'\[\w+\]\s+Initialize'
        self.lexer.begin('initialize')
        t.type = 'COMMENT'
        return t
    def t_initialize_end(self, t):
        r'MCDISPLAY'
        self.lexer.begin('INITIAL')
        t.type = 'MCDISPLAY'
        return t
    t_initialize_COMMENT = r'.+'
    
    # save state
    def t_begin_save(self, t):
        r'Save.*'
        self.lexer.begin('save')
        t.type = 'COMMENT'
        return t
    def t_save_finallybegin(self, t):
        r'Finally.*'
        self.lexer.begin('finally')
        t.type = 'COMMENT'
        return t
    t_save_COMMENT = r'.+'
    
    # finally state (NOTE: there is no begin token, because it is initiated during save state)
    t_finally_COMMENT = r'.+'

    ##################################
    # INITIAL state
    ##################################
    
    t_LB = r'\('
    t_RB = r'\)'
    t_COLON = r':'
    t_QUOTE = r'"'
    t_SQUOTE = r'\''
    t_COMMA = r','
    
    def t_ANY_NL(self, t):
        r'\n'
        self.lexer.lineno += 1
        return t
    
    def t_ABSPATH(self, t):
        r'/[/\w\.]+'
        return t
    
    def t_DEC(self, t):
        r'-?[\d.]+(e[+-][0-9]+)?'
        return t
    
    def t_ID(self, t):
        r'[a-zA-Z_]\w*'
        t.type = self.reserved.get(t.value, 'ID')
        return t
    
    # ignore whitespaces and tabs means that we do not tokenize them, but they are still applied in the regex checks defined above for our tokens
    t_ignore = r' \t'
    
    def t_error(self, t):
        print('error: %s' % t.value)
    
    ##################################
    # parsing rules and action code
    ##################################
    
    def p_document(self, p):
        'document : instr_open comp_defs comments draw_lines instr_close comments ray_statements comments'
        print 'mcdisplay document parsed'
        # quirky: reverse ordering of components
        self.comps.children = self.comps.children[::-1]
        # assemble parse tree
        self.parsetree = Node(type='traceparsetree', children=[self.instr, self.comps, self.rays, Node(type='comments', leaf=self.comments)])
    
    instr = None
    def p_instr_open(self, p):
        'instr_open : INSTRUMENT COLON NL INSTRKW SQUOTE instr_name SQUOTE LB ABSPATH RB NL'
        self.instr = Node(type='instrument', children=[p[6], Node(type='abspath', leaf=p[9])])
        
    def p_instr_name(self, p):
        'instr_name : ID'
        p[0] = Node(type='instr_name', leaf=p[1])
    
    comps = Node(type='comps')
    def p_comp_defs(self, p):
        '''comp_defs : comp_def comp_defs
                     | comp_def '''
        self.comps.children.append(p[1])
        p[0] = self.comps

    def p_comp_def(self, p):
        'comp_def : COMPONENT COLON QUOTE comp_name QUOTE NL POS COLON 12dec NL COMPKW comp_name AT LB 3dec RB NL'
        p[0] = Node(type='comp', children=[p[4], p[9], p[15]])
    
    def p_comp_name(self, p):
        'comp_name : ID'
        p[0] = Node(type="comp_name", leaf=p[1])

    def p_12dec(self, p):
        '12dec : DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC'
        p[0] = Node(type='12dec', leaf=[p[1], p[3], p[5], p[7], p[9], p[11], p[13], p[15], p[17], p[19], p[21], p[21]])
        
    def p_3dec(self, p):
        '3dec :  DEC COMMA DEC COMMA DEC'
        p[0] = Node(type='3dec', leaf=[p[1], p[3], p[5]])
    
    def p_comments(self, p):
        '''comments : comment comments
                    | empty '''
    
    comments = []
    def p_comment(self, p):
        '''comment : COMMENT NL
                   | NL'''
        self.comments.append(p[1]) 
    
    def p_draw_lines(self, p):
        '''draw_lines : draw_line draw_lines
                      | draw_line '''
    
    def p_draw_line(self, p):
        '''draw_line : draw_header
                     | draw_command
                     | draw_open
                     | draw_close'''
    
    def p_draw_header(self, p):
        '''draw_header : MCDISPLAY COLON COMPKWLC comp_name NL'''
        self.commands = Node(type='draw_commands')
        for comp in self.comps.children:
            for c in comp.children:
                if c.leaf==p[4].leaf:
                    comp.children.append(self.commands)
                    break
    
    commands = None
    def p_draw_command(self, p):
        '''draw_command : MCDISPLAY COLON DRAWCALL LB args RB NL
                        | MCDISPLAY COLON DRAWCALL LB SQUOTE arg SQUOTE RB NL
                        | MCDISPLAY COLON DRAWCALL LB SQUOTE arg SQUOTE COMMA args RB NL
                        | MCDISPLAY COLON DRAWCALL LB SQUOTE SQUOTE RB NL
                        | MCDISPLAY COLON DRAWCALL LB RB NL'''
        # special case: remove first argument of args
        if p[3] == 'multiline':
            self.args.leaf = self.args.leaf[1:]
        
        self.commands.children.append(Node(type='draw', children=[self.args], leaf=p[3]))
        # reset args after having parsed them all, which is now
        self.args = Node(type='args', leaf=[])
    
    args = Node(type='args', leaf=[])
    def p_args(self, p):
        '''args : arg COMMA args
                | arg'''
    
    def p_arg(self, p):
        '''arg : DEC
               | ID'''
        self.args.leaf.append(p[1])
    
    def p_empty(self, p):
        'empty :'
    
    def p_draw_open(self, p):
        '''draw_open : MCDISPLAY COLON STARTKWLC NL'''
    
    def p_draw_close(self, p):
        'draw_close : MCDISPLAY COLON ENDKWLC NL'
    
    def p_instr_close(self, p):
        'instr_close : INSTRUMENT END COLON NL'
    
    rays = Node(type='rays')
    def p_ray_statements(self, p):
        '''ray_statements : ray_statement ray_statements
                          | ray_statement '''
        # this function only receives a value in p[1] - the p[0] og p_ray_statement - when a ray is "finished" at that lower level
        if p[1] != None:
            self.rays.children.append(p[1])
        p[0] = self.rays
    
    ray = None
    def p_ray_statement(self, p):
        '''ray_statement : ray_enterstate
                         | ray_compstate
                         | ray_compstatestate
                         | ray_scatterstate
                         | ray_scatter
                         | ray_absorb
                         | ray_leavestate'''
        if p[1].type=='ENTER' and self.ray==None:
            self.ray = Node(type='ray', children=[p[1]])
        elif p[1].type=='LEAVE':
            self.ray.children.append(p[1])
            p[0] = self.ray
            self.ray = None
        elif self.ray==None:
            raise Exception('p_ray_statement: ray must begin start with a ray_enterstate')
        else:
            self.ray.children.append(p[1])
        
    def p_ray_compstate(self, p):
        'ray_compstate : COMP COLON QUOTE comp_name QUOTE NL STATE COLON 11dec NL'
        p[0] = Node(type='COMP', children=[p[4]], leaf=p[9].leaf)
    
    def p_ray_compstatestate(self, p):
        'ray_compstatestate : COMP COLON QUOTE comp_name QUOTE NL STATE COLON 11dec NL STATE COLON 11dec NL'
        p[0] = Node(type='COMP', children=[p[4]], leaf=p[9].leaf)
    
    def p_ray_scatterstate(self, p):
        'ray_scatterstate : SCATTER COLON 11dec NL STATE COLON 11dec NL'
        p[0] = Node(type='SCATTER', leaf=p[3].leaf)
    
    def p_ray_scatter(self, p):
        'ray_scatter : SCATTER COLON 11dec NL'
        p[0] = Node(type='SCATTER', leaf=p[3].leaf)
    
    def p_ray_absorb(self, p):
        'ray_absorb : ABSORB COLON NL'
        p[0] = Node(type='ABSORB')
    
    def p_ray_enterstate(self, p):
        'ray_enterstate : ENTER COLON NL STATE COLON 11dec NL'
        p[0] = Node(type='ENTER', leaf=p[6].leaf)
    
    def p_ray_leavestate(self, p):
        'ray_leavestate : LEAVE COLON NL STATE COLON 11dec NL'
        p[0] = Node(type='LEAVE', leaf=p[6].leaf)
    
    def p_11dec(self, p):
        '11dec : DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC'
        p[0] = Node(type='11dec', leaf=[p[1], p[3] ,p[5] ,p[7] ,p[9] ,p[11] ,p[13] ,p[15] ,p[17] ,p[19] ,p[21]])
    
    # error rule for syntax errors
    def p_error(self, p):
        print("Syntax error in input!")
        print(p)

    ##################################
    # build and test 
    ##################################
    
    def build_lexer(self, **kwargs):
        ''' builds the lexer '''
        self.lexer = lex.lex(module=self, **kwargs)

    def test_lexer(self, data):
        ''' test built lexer on data '''
        self.lexer.input(data)
        for token in self.lexer:
            print(token)
    
    def build_parser(self, **kwargs):
        ''' builds the lexer '''
        self.parser = yacc.yacc(module=self, debug=True)

    def parse(self, data):
        ''' attempts to parse data '''
        self.parser.parse(data, lexer=self.lexer)

