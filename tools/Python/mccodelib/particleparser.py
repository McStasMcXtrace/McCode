'''
Implementation of classes involved in the PLY-based translation of the mcdisplay "--trace output" 
mini language.

Read the PLY documentation here: http://www.dabeaz.com/ply/ply.html#ply_nn23.
'''
from ply import lex, yacc
from .nodetree import Node
from .instrgeom import RayBundle, ParticleStory, ParticleCompGroup, ParticleState

class ParticleTraceParser:
    '''
    Python lex/yacc parser for the particle ray section of mcdisplay --trace output
    '''
    parsetree = None
    def __init__(self, data=None):
        self.build_lexer()
        self.build_parser()
        if data:
            self.parse(data)
    
    # these tokens are handled in t_ID
    reserved = {
        'ENTER'       : 'ENTER',
        'COMP'        : 'COMP',
        'STATE'       : 'STATE',
        'SCATTER'     : 'SCATTER',
        'ABSORB'      : 'ABSORB',
        'LEAVE'       : 'LEAVE',
    }
    
    # tokens 
    tokens = [
              'COLON',
              'QUOTE',
              'COMMA',
              'NL',
              
              'DEC',
              'ID',
              ] + list(set(reserved.values()))
    
    t_COLON = r':'
    t_QUOTE = r'"'
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
        'document : ray_statements'
        print('particle rays parsed')
        # assemble parse tree
        self.parsetree = Node(type='raystatements', children=[self.rays])
    
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
        p[0] = Node(type='COMPENTER', children=[p[4]], leaf=p[9].leaf)
    
    def p_comp_name(self, p):
        'comp_name : ID'
        p[0] = Node(type="comp_name", leaf=p[1])
    
    def p_ray_compstatestate(self, p):
        'ray_compstatestate : COMP COLON QUOTE comp_name QUOTE NL STATE COLON 11dec NL STATE COLON 11dec NL'
        p[0] = Node(type='COMPENTEREXIT', children=[p[4]], leaf=p[13].leaf)
    
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


class ParticleBundleRayFactory:
    '''
    Ray reconstruction from the syntax tree produced by ParticleTraceParser,
    outputting a wholly interpreted instrument and ray model from the mcdisplay trace output.
    '''
    rays = None
    def __init__(self, parsetreeroot):
        self.root = parsetreeroot
        if (type(self.root) is not Node) or (self.root.type != 'raystatements'):
            raise Exception('TraceObjectConstructor: parsetreeroot must be a Node of type "raystatements"')
    
    def build_rays(self):
        ''' builds the particle ray representation '''
        rays = []
        bundle = RayBundle(rays)
        
        # iterate through parse tree
        for dc in self.root.children:
            # handle rays
            if dc.type == 'rays':
                for ray in dc.children:
                    
                    # prepare state variables
                    story = None
                    comp_group = None
                    comp_name = None
                    
                    # NOTE: the node tree is a bit weird in that there are no COMPEXIT's, these are just SCATTER 
                    for event in ray.children:
                        if event.type == 'ENTER':
                            story = ParticleStory()
                            rays.append(story)
                        
                        if event.type == 'COMPENTER' or event.type == 'COMPENTEREXIT':
                            # get component name
                            comp_name = event.children[0].leaf
                            
                            # record entry
                            if event.type == 'COMPENTER' or self.iszerovector_str(event.leaf[0:3]):
                                comp_group = ParticleCompGroup(comp_name)
                            else:
                                comp_group = ParticleCompGroup(comp_name)
                                comp_group.add_event(ParticleState(event.leaf))
                            story.add_group(comp_group)
                        
                        if event.type == 'SCATTER':
                            comp_group.add_event(ParticleState(event.leaf))
                        
                        if event.type == 'ABSORB':
                            # TODO: when --trace has been updated with ABSORB coordinates
                            #comp_group.append(ParticleState(event.leaf))
                            pass
                        
                        if event.type == 'LEAVE':
                            # append LEAVE state, although equal to SCATTER states, may be the result of COMPENTER, ABSORB
                            # TODO: read ABSORB todo and accomodate, removing this then completely redundant LEAVE state
                            comp_group.add_event(ParticleState(event.leaf))
                            comp_group = None
                            story = None
        
        return bundle
    
    def iszerovector_str(self, v):
        ''' returns true if v is the zero vector, otherwise false '''
        if float(v[0]) == 0 and float(v[1]) == 0 and float(v[2]) == 0:
            return True
        else:
            return False

