'''

'''
import re
from instrgeom import RayBundle, ParticleStory, ParticleCompGroup, ParticleState
from mclib.flowchart import FCNTerminal, FCNDecisionBool, FCNDecisionMulti, FCNProcess, FlowChartControl

# terminal nodes implementation
def t_begin(args):
    print "starting particle parsing"

def t_end(args):
    print "ended particle parsing"

def t_error(args):
    raise Exception("error")

# decision nodes implementation
def d_isenter(args):
    m = re.match('ENTER:', args['linegetter'].current())
    return m is not None

def d_done(args):
    return args['linegetter'].isempty()

def d_isstate(args):
    m = re.match('STATE:', args['linegetter'].current())
    return m is not None

def d_isleave(args):
    m = re.match('LEAVE:', args['linegetter'].current())
    return m is not None

def d_iskeywd(args):
    line = args['linegetter'].current()
    m0 = re.match('COMP:', line)
    if m0:
        return 0 
    m1 = re.match('SCATTER:', line)
    if m1:
        return 1
    m2 = re.match('ABSORB:', line)
    if m2:
        return 2
    m3 = re.match('LEAVE:', line)
    if m3:
        return 3
    m4 = re.match('STATE:', line)
    if m4:
        return 4

# process nodes implementation  --- NOTE: all process nodes increment line idx by one
def p_newparticle(args):
    args['weaver'].new_story()
    
    args['linegetter'].inc()

def p_ignoreline(args):
    args['linegetter'].inc()

def p_addcomp(args):
    linegetter = args['linegetter']
    
    weaver = args['weaver']
    weaver.close_comp()
    weaver.new_comp(_get_compname(linegetter.current()))
    
    linegetter.inc()

def p_addpoint(args):
    linegetter = args['linegetter']
    args['weaver'].new_point(_get_strcoords(linegetter.current()))
    
    linegetter.inc()

def p_closeparticle
    weaver = args['weaver']
    weaver.close_comp()
    weaver.close_story()
    
    args['linegetter'].inc()

# helper functions implementation
def _get_strcoords(line):
    m = re.match('\w+: ([\d\.\+\-e]), ([\d\.\+\-e]), ([\d\.\+\-e]), ([\d\.\+\-e]), ([\d\.\+\-e]), ([\d\.\+\-e]), ([\d\.\+\-e]), ([\d\.\+\-e]), ([\d\.\+\-e]), ([\d\.\+\-e]), ([\d\.\+\-e])\n', line)
    #return [float(m.group(1)), float(m.group(2)), float(m.group(3)), float(m.group(4)), float(m.group(5)), float(m.group(6)), float(m.group(7)), float(m.group(8)), float(m.group(9)), float(m.group(10)), float(m.group(11))]
    return [m.group(1), m.group(2), m.group(3), m.group(4), m.group(5), m.group(6), m.group(7), m.group(8), m.group(9), m.group(10), m.group(11)]

def _get_compname(line):
    m = re.match('COMP: \"(\w+)\"', line)
    return m.group(1)

# action code helper classes
class ParticleBundleWeaver(object):
    ''' creates the particle ray data structure by aid of a bunch of functions that can be called in succession '''
    def __init__(self):
        self._rays = []
        self._bundle = RayBundle(self._rays)
        self._compgroup = None
        self._story = None
    
    def new_story(self):
        if self._story is not None:
            raise Exception("Close the current story before adding a new one.")
        self._story = ParticleStory()
        self._rays.append(self._story)
    
    def new_comp(self, compname):
        if self._story is None:
            raise Exception("You must add a particle story before adding a compgroup.")
        if self._compgroup is not None:
            raise Exception("Close the current compgroup before adding a new one.")
        self._compgroup = ParticleCompGroup(comp_name)
        self._story.add_group(self._compgroup)

    def new_point(self, point_str):
        if self._compgroup is None:
            raise Exception("You must add a compgroup before adding points.")
        point = ParticleState(point_str)
        self._compgroup.add_event(point)
    
    def close_comp(self):
        self._compgroup = None
    
    def close_story(self):
        sellf._story = None
    
    def get_particles(self):
        if self._story == None and self._compgroup == None: 
            return self._bundle
        else:
            raise Exception("Close compgroup and story before fetching the particle bundle.")

class LineGetter(object):
    def __init__(self, text):
        self.lines = text.splitlines()
        self.idx = 0
    
    def current(self):
        return self.lines[self.idx]
    
    def inc(self):
        self.idx += 1

# flowchart assembly and execution
class FlowChartParticleTraceParser(object):
    def __init__(self, text):
        
        # terminal nodes
        t1 = FCNTerminal(key="begin", fct=t_begin)
        t2 = FCNTerminal(key="end", fct=t_end)
        t3 = FCNTerminal(key="error", fct=t_error)
        
        # decision nodes
        d0 = FCNDecisionBool(fct=d_done)
        d1 = FCNDecisionBool(fct=d_isenter)
        d2 = FCNDecisionBool(fct=d_isstate)
        d3 = FCNDecisionMulti(fct=d_iskeywd)
        d4 = FCNDecisionBool(fct=d_isstate)
        d5 = FCNDecisionBool(fct=d_isstate)
        d6 = FCNDecisionBool(fct=d_isstate)
        d7 = FCNDecisionBool(fct=d_isstate)
        d8 = FCNDecisionBool(fct=d_isleave)
        
        # process nodes
        p1 = FCNProcess(fct=p_newparticle)
        p2 = FCNProcess(fct=p_ignoreline)
        p3 = FCNProcess(fct=p_addcomp)
        p4 = FCNProcess(fct=p_addpoint)
        p5 = FCNProcess(fct=p_ignoreline)
        p6 = FCNProcess(fct=p_addpoint)
        p7 = FCNProcess(fct=p_ignoreline)
        p8 = FCNProcess(fct=p_addpoint)
        p9 = FCNProcess(fct=p_ignoreline)
        p10 = FCNProcess(fct=p_addpoint)
        p11 = FCNProcess(fct=p_closeparticle)
        p12 = FCNProcess(fct=p_ignoreline)
        
        # assemble the flowchart from top
        t1.set_nodenext(node_next=d0)
        d0.set_nodes(node_T=d1, node_F=t2)
        d1.set_nodes(node_T=p1, node_F=t3)
        p1.set_nodenext(node_next=d2)
        d2.set_nodes(node_T=p2, node_F=t3)
        p2.set_nodenext(node_next=d3)
        d3.set_node_lst(node_lst=[p3, p5, p7, p9, p12])
        
        p3.set_nodenext(node_next=d4)
        d4.set_nodes(node_T=p4, node_F=t3)
        p4.set_nodenext(node_next=d3)
        
        p5.set_nodenext(node_next=d5)
        d5.set_nodes(node_T=p6, node_F=t3)
        p6.set_nodenext(node_next=d3)
        
        p7.set_nodenext(node_next=d6)
        d6.set_nodes(node_T=p8, node_F=t3)
        p8.set_nodenext(node_next=d8)
        d6.set_nodes(node_T=p9, node_F=t3)
        
        p9.set_nodenext(node_next=d4)
        d7.set_nodes(node_T=p10, node_F=t3)
        p10.set_nodenext(node_next=p11)
        p11.set_nodenext(node_next=d0)
        
        p12.set_nodenext(node_next=d3)
        
        # set args according to the above implementation and execute the flowchart
        args = {}
        args['linegetter'] = LineGetter(text)
        args['weaver'] = ParticleBundleWeaver()

        flowchart = FlowChartControl(t1)
        flowchart.process(args)
        
        return = args['weaver'].get_particles

