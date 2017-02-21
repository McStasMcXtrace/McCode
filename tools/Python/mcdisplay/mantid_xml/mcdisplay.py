#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
mcdisplay webgl script.
'''
import sys
import os
import logging
import argparse
import re
from mccodelib.instrgeom import DrawMultiline

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from mccodelib.mcdisplayutils import McDisplayReader
from mccodelib.instrparser import InstrTraceParser, InstrObjectConstructor
from mccodelib.fcparticleparser import FlowChartParticleTraceParser

class MantidPixelWriter:
    def __init__(self, components):
        self.components = components
    
    source_type = '''
<component type="sourceMantid-type" name="sourceMantid">
    <location x="X_LOC" y="Y_LOC" z="Z_LOC"  />
</component>'''
    
    source_line_type = '''
<type name="line-sourceMantid-LINE_IDX" >
    <cylinder id="dummy" >
        <centre-of-bottom-base x="0" y="0" z="0" />
        <axis x="X_POINT" y="Y_POINT" z="Z_POINT" />
        <radius val="0.005" />
        <height val="0.333333925927047" />
    </cylinder >
</type>'''
    
    source_header = '''
<type name="sourceMantid-type" is="Source" >'''
    
    source_line = '''
    <component type="line-sourceMantid-0" >
        <location x="0" y="0" z="0" />
    </component >'''
    
    source_footer = '''</type>'''
    
    def _get_mantid_source(self):
        for c in self.components:
            if c.name == 'sourceMantid':
                for d in c.drawcalls:
                    # only supported output is multiline
                    if type(d) == DrawMultiline:
                        # TODO: implement source drawing
                        pass
                s = self.source_type.replace('X_LOC', str(c.pos.x))
                s = s.replace('Y_LOC', str(c.pos.y))
                s = s.replace('Z_LOC', str(c.pos.z))
                break
        
        return '\n'.join([s, self.source_header, self.source_footer])
    
    sample_type = '''
<component type="sampleMantid-type" name="sampleMantid">
    <location x=" 0" y=" 0" z=" 6.2"  />
</component>'''
    
    sample_line_type = '''
<type name="line-sampleMantid-0" >
    <cylinder id="dummy" >
        <centre-of-bottom-base x="-0.005" y="-0.005" z="-0.0025" />
        <axis x="0.01" y="0" z="0" />
        <radius val="0.005" />
        <height val="0.01" />
    </cylinder >
</type>'''
    
    sample_line = '''
    <component type="line-sourceMantid-0" >
        <location x="0" y="0" z="0" />
    </component >'''
    
    sample_header = '''
<type name="sourceMantid-type" is="Source" >'''
    
    sample_footer = '''
</type>'''
    
    def _get_mantid_sample(self):
        # TODO: implement
        return ''
    
    monitor_type = '''
<component type="MonNDtype-IDX_MONITOR" name="MONITOR_NAME" idlist="MonNDtype-IDX_MONITOR-list">
    <location x="X_LOC" y="Y_LOC" z="Z_LOC"  />
</component>

<idlist idname="MonNDtype-IDX_MONITOR-list">
    <id start="IDX_PIX_START" end="IDX_PIX_END"/>
</idlist>'''
    
    monitor_header = '''
<type name="MonNDtype-IDX_MONITOR">
    <properties />'''
    
    monitor_footer = '''
</type>'''
    
    s1 = '''
<type name="MonNDtype-IDX_MONITOR-pix-IDX_PIXEL" is="detector">
    <hexahedron id="hexapix-IDX_PIXEL">
        <left-back-bottom-point  x="-COORD_X" y="y_1" z="0"  />
        <left-front-bottom-point x="-COORD_X" y="y_2" z="0"  />
        <right-front-bottom-point x="COORD_X" y="y_3" z="0"  />
        <right-back-bottom-point  x="COORD_X" y="y_4" z="0"  />
        <left-back-top-point  x="-COORD_X" y="y_1" z="0.001"  />
        <left-front-top-point  x="-COORD_X" y="y_2" z="0.001"  />
        <right-front-top-point  x="COORD_X" y="y_3" z="0.001"  />
        <right-back-top-point   x="COORD_X" y="y_4" z="0.001"  />
    </hexahedron>
    <bounding-box>
        <x-min val="-COORD_X"/>
        <x-max val="-COORD_X"/>
        <y-min val="y_4"/>
        <y-max val="y_2"/>
        <z-min val="0"/>
        <z-max val="0.001"/>
    </bounding-box>
    <algebra val="hexapix-IDX_PIXEL" />
</type>'''
    
    s2 = '''
<component type="MonNDtype-IDX_MONITOR-pix-IDX_PIXEL">
    <location x="x_cp" y="y_cp" z="0" />
</component>'''
    
    #['IDX_PIXEL', '0', 'NUM_PIXELS', '4', 'x_cp'    , 'y_cp'    , '0', '-COORD_X'   , 'y_1'       , '0', '-COORD_X'   , 'y_2'      , '0', 'COORD_X'   , 'y_3'      , '0', 'COORD_X'   , 'y_4'       , '0']
    #['1009'     , '0', '1009'      , '4', '0.301485', '0.111419', '0', '-0.00148515', '-0.0139429', '0', '-0.00148515', '0.0128452', '0', '0.00148515', '0.0140756', '0', '0.00148515', '-0.0129778', '0']
    
    def _get_mantid_monitors(self):
        ''' creates the mantid xml by means of a couple of template strings '''
        def join_s1_block(s1_s):
            return '\n\n'.join(s1_s)
        def wrap_join_s2_block(s2_s, monitor_name, idx_monitor):
            s2 = ''.join(s2_s)
            h = self.monitor_header.replace('IDX_MONITOR', idx_monitor)
            return ''.join([h, s2, self.monitor_footer])
        
        monitors = []
        for c in self.components:
            if re.search('nD_Mantid', c.name):
                monitors.append(c)
        
        mon_txt_blocks = []
        for m in monitors:
            pixels = []
            for d in m.drawcalls:
                if type(d) == list:
                    pixels.append(d)
            
            
            idx_monitor = re.search('nD_Mantid_([0-9]+)', m.name).group(1)
            mt = self.monitor_type.replace('IDX_MONITOR', idx_monitor)
            mt = mt.replace('IDX_PIX_START', pixels[0][1])
            mt = mt.replace('IDX_PIX_END', pixels[0][2])
            mt = mt.replace('MONITOR_NAME', m.name)
            
            
            #count = 0
            s1_s = []
            s2_s = []
            #for line in self.pixel_statements:
            for line in pixels:
                s1 = self.s1.replace('COORD_X', line[13])
                s1 = s1.replace('IDX_PIXEL', line[0])
                s1 = s1.replace('IDX_MONITOR', idx_monitor)
                s1 = s1.replace('y_1', line[9])
                s1 = s1.replace('y_2', line[12])
                s1 = s1.replace('y_3', line[15])
                s1 = s1.replace('y_4', line[18])
                s1_s.append(s1)
                
                s2 = self.s2.replace('IDX_PIXEL', line[13])
                s2 = s2.replace('IDX_MONITOR', idx_monitor)
                s2 = s2.replace('x_cp', line[4])
                s2 = s2.replace('y_cp', line[5])
                s2_s.append(s2)
                
                # DEBUG STUFF
                #count +=1
                #if count > 2:
                #    break
            
            
            mon_txt_blocks.append('\n\n'.join([mt, join_s1_block(s1_s), wrap_join_s2_block(s2_s,monitor_name=m.name, idx_monitor=idx_monitor)]))
        
        return '\n\n'.join(mon_txt_blocks)
    
    
    header = '''
<?xml version="1.0" encoding="UTF-8"?>
<!-- IDF generated using McStas McDisplay and the Mantid backend -->
<!-- For help on the notation used to specify an Instrument Definition File see http://www.mantidproject.org/IDF -->
<instrument name="single_nD.out" valid-from   ="1900-01-31 23:59:59"
valid-to     ="2100-01-31 23:59:59" last-modified="Thu Feb 16 16:37:46 2017">
<defaults>
    <length unit="meter"/>
    <angle unit="degree"/>
    <reference-frame>
        <!-- The z-axis is set parallel to and in the direction of the beam. The y-axis points up and the coordinate system is right handed. -->
        <along-beam axis="z"/>
        <pointing-up axis="y"/>
        <handedness val="right"/>
    </reference-frame>
    <default-view axis-view="z-"/>
</defaults>

<!-- LIST OF PHYSICAL COMPONENTS (which the instrument consists of) -->

<type name="Othercomp"></type>'''
    
    footer = '''</instrument>'''
        
    def do_work(self):
        source = self._get_mantid_source()
        sample = self._get_mantid_sample()
        monitors = self._get_mantid_monitors()
        
        print('\n\n'.join([self.header, source, sample, monitors, self.footer]))
        

def debug_load_instr(filename):
    instrdef = open(filename).read()
    
    instrparser = InstrTraceParser(instrdef)
    instrbuilder = InstrObjectConstructor(instrparser.parsetree)
    return instrbuilder.build_instr()

def debug_load_rays(filename):
    particles = open(filename).read()
    
    parser = FlowChartParticleTraceParser()
    rays = parser.execute(particles)
    return rays

def debug_file_save(data, filename):
    ''' saves data for debug purposes '''
    f = open(filename, 'w')
    f.write(data)
    f.close()

def main(args):
    ''' script execution '''
    logging.basicConfig(level=logging.INFO)
    
    reader = McDisplayReader(args, n=1, debug=True)
    instrument = reader.read_instrument()
    
    writer = MantidPixelWriter(instrument.components)
    writer.do_work()
    

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('instr', help='display this instrument file (.instr or .out)')
    parser.add_argument('--default', action='store_true', help='automatically use instrument defaults for simulation run')
    parser.add_argument('--dirname', help='output directory name override')
    parser.add_argument('--inspect', help='display only particle rays reaching this component')
    parser.add_argument('instr_options', nargs='*', help='simulation options and instrument params')
    
    args, unknown = parser.parse_known_args()
    # if --inspect --first or --last are given after instr, the remaining args become "unknown",
    # but we assume that they are instr_options
    if len(unknown)>0:
        args.instr_options = unknown
    
    try:
        main(args)
    except KeyboardInterrupt:
        print('')
    except Exception as e:
        print(e)
        raise e

