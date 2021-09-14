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
import math

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from mccodelib.instrgeom import DrawMultiline, Vector3d
from mccodelib.mcdisplayutils import McDisplayReader
from mccodelib.instrparser import InstrTraceParser, InstrObjectConstructor, MantidPixelLine, \
    MantidRectangularDetectorLine, MantidBananaDetectorLine
from mccodelib.fcparticleparser import FlowChartParticleTraceParser


class MantidPixelWriter:
    def __init__(self, components):
        self.components = components
        self.monitors = []
        for c in self.components:
            if re.search('nD_Mantid', c.name):
                self.monitors.append(c)

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

    def _sample_Z(self):
        for c in self.components:
            if c.name == 'sampleMantid':
                return (c.pos.z)


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
                s = s.replace('Z_LOC', str(c.pos.z-self._sample_Z()))
                break
        return '\n'.join([s, self.source_header, self.source_footer])

    sample = '''
<component type="sampleMantid-type" name="sampleMantid">
    <location x="X_COORD" y="Y_COORD" z="Z_COORD"  />
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
    <component type="line-sampleMantid-0" >
        <location x="0" y="0" z="0" />
    </component >'''

    sample_type = '''
<type name="sampleMantid-type" is="SamplePos" >'''

    sample_footer = '''
</type>'''

    def _get_mantid_sample(self):
        for c in self.components:
            if c.name == 'sampleMantid':
                for d in c.drawcalls:
                    # only supported output is multiline
                    if type(d) == DrawMultiline:
                        # TODO: implement sample drawing
                        pass

                h = self.sample.replace('X_COORD', str(c.pos.x))
                h = h.replace('Y_COORD', str(c.pos.y))
                h = h.replace('Z_COORD', str(c.pos.z-self._sample_Z()))
                break
        return '\n\n'.join([h, self.sample_type, self.sample_footer])

    pixels_monitor_type = '''
<component type="MonNDtype-IDX_MONITOR" name="MONITOR_NAME" idlist="MonNDtype-IDX_MONITOR-list">
    <location x="X_LOC" y="Y_LOC" z="Z_LOC"  />
</component>

<idlist idname="MonNDtype-IDX_MONITOR-list">
    <id start="IDX_PIX_START" end="IDX_PIX_END"/>
</idlist>'''

    pixels_monitor_header = '''
<type name="MonNDtype-IDX_MONITOR">
    <properties />'''

    pixels_monitor_footer = '''
</type>'''

    pixels_s1 = '''
<type name="MonNDtype-IDX_MONITOR-pix-IDX_PIXEL" is="detector">
    <hexahedron id="hexapix-1009">
        <left-back-bottom-point  x="x_1" y="y_1" z="z_1"/>
        <left-front-bottom-point  x="x_2" y="y_2" z="z_2"/>
        <right-front-bottom-point  x="x_3" y="y_3" z="z_3"/>
        <right-back-bottom-point  x="x_4" y="y_4" z="z_4"/>
        <left-back-top-point  x="x_1" y="y_1" z="z_PLUS_1"/>
        <left-front-top-point  x="x_2" y="y_2" z="z_PLUS_2"/>
        <right-front-top-point  x="x_3" y="y_3" z="z_PLUS_3"/>
        <right-back-top-point  x="x_4" y="y_4" z="z_PLUS_4"/>
    </hexahedron>
    <bounding-box>
        <x-min val="x_bb_min"/>
        <x-max val="x_bb_max"/>
        <y-min val="y_bb_min"/>
        <y-max val="y_bb_max"/>
        <z-min val="y_bb_min"/>
        <z-max val="z_bb_max"/>
    </bounding-box>
</type>'''

    pixels_s2 = '''
<component type="MonNDtype-IDX_MONITOR-pix-IDX_PIXEL">
    <location x="x_cp" y="y_cp" z="0" />
</component>'''

    # ['IDX_PIXEL', '0', 'NUM_PIXELS', '4', 'x_cp'    , 'y_cp'    , '0', '-COORD_X'   , 'y_1'       , '0', '-COORD_X'   , 'y_2'      , '0', 'COORD_X'   , 'y_3'      , '0', 'COORD_X'   , 'y_4'       , '0']
    # ['1009'     , '0', '1009'      , '4', '0.301485', '0.111419', '0', '-0.00148515', '-0.0139429', '0', '-0.00148515', '0.0128452', '0', '0.00148515', '0.0140756', '0', '0.00148515', '-0.0129778', '0']

    def _get_mantid_pixels_monitors(self):
        ''' creates the mantid xml by means of a couple of template strings '''

        def join_s1_block(s1_s):
            return '\n\n'.join(s1_s)

        def wrap_join_s2_block(s2_s, monitor_name, idx_monitor):
            s2 = ''.join(s2_s)
            h = self.pixels_monitor_header.replace('IDX_MONITOR', idx_monitor)
            return ''.join([h, s2, self.pixels_monitor_footer])

        mon_txt_blocks = []
        for m in self.monitors:
            pixels = []
            for d in m.drawcalls:
                if type(d) == MantidPixelLine:
                    pixels.append(d.line)

            if len(pixels) == 0:
                return ''

            idx_monitor = re.search('nD_Mantid_([0-9]+)', m.name).group(1)
            mt = self.pixels_monitor_type.replace('IDX_MONITOR', idx_monitor)
            mt = mt.replace('IDX_PIX_START', pixels[0][1])
            mt = mt.replace('IDX_PIX_END', pixels[0][2])
            mt = mt.replace('MONITOR_NAME', m.name)

            s1_s = []
            s2_s = []
            for line in pixels:
                idx_pix = line[0]
                s1 = self.pixels_s1.replace('IDX_PIXEL', idx_pix)
                s1 = s1.replace('IDX_MONITOR', idx_monitor)

                pix = MantidPixel(line, m.transform)

                x = [pix.p1.x, pix.p2.x, pix.p3.x, pix.p4.x]
                y = [pix.p1.y, pix.p2.y, pix.p3.y, pix.p4.y]
                z = [pix.p1.z, pix.p2.z, pix.p3.z, pix.p4.z]
                z_PLUS = [pix.p1.z + 0.001, pix.p2.z + 0.001, pix.p3.z + 0.001, pix.p4.z + 0.001]

                s1 = s1.replace('x_1', str(x[0]))
                s1 = s1.replace('x_2', str(x[1]))
                s1 = s1.replace('x_3', str(x[2]))
                s1 = s1.replace('x_4', str(x[3]))

                s1 = s1.replace('y_1', str(y[0]))
                s1 = s1.replace('y_2', str(y[1]))
                s1 = s1.replace('y_3', str(y[2]))
                s1 = s1.replace('y_4', str(y[3]))

                s1 = s1.replace('z_1', str(z[0]))
                s1 = s1.replace('z_2', str(z[1]))
                s1 = s1.replace('z_3', str(z[2]))
                s1 = s1.replace('z_4', str(z[3]))

                s1 = s1.replace('z_PLUS_1', str(z_PLUS[0]))
                s1 = s1.replace('z_PLUS_2', str(z_PLUS[1]))
                s1 = s1.replace('z_PLUS_3', str(z_PLUS[2]))
                s1 = s1.replace('z_PLUS_4', str(z_PLUS[3]))

                s1 = s1.replace('x_bb_min', str(min(x)))
                s1 = s1.replace('x_bb_max', str(max(x)))
                s1 = s1.replace('y_bb_min', str(min(y)))
                s1 = s1.replace('y_bb_max', str(max(y)))
                s1 = s1.replace('z_bb_min', str(min(z)))
                s1 = s1.replace('z_bb_max', str(max(z_PLUS)))
                s1_s.append(s1)

                s2 = self.pixels_s2.replace('IDX_PIXEL', idx_pix)
                s2 = s2.replace('IDX_MONITOR', idx_monitor)
                s2 = s2.replace('x_cp', str(pix.p_cp.x))
                s2 = s2.replace('y_cp', str(pix.p_cp.y))
                s2_s.append(s2)

            mon_txt_blocks.append('\n\n'.join(
                [mt, join_s1_block(s1_s), wrap_join_s2_block(s2_s, monitor_name=m.name, idx_monitor=idx_monitor)]))

        return '\n\n'.join(mon_txt_blocks)

    rect_monitor = '''
<component type="MonNDtype" name="MONITOR_NAME" idstart="PIXEL_MIN" idfillbyfirst="x" idstepbyrow="X_NUM">
    <location x="X_LOC" y="Y_LOC" z="Z_LOC" rot="ROT_ANGLE" axis-x="ROT_X" axis-y="ROT_Y" axis-z="ROT_Z"/>
</component>'''
    rect_monitor_type = '''
<type name="MonNDtype" is="RectangularDetector" type="rectangular_det_type"
    xpixels="X_NUM" xstart="X_MIN" xstep="X_STEP"
    ypixels="Y_NUM" ystart="Y_MIN" ystep="Y_STEP">
</type>'''
    rect_monitor_pixel = '''
<type is="detector" name="rectangular_det_type">
    <cuboid id="pixel-shape-0">
        <left-front-bottom-point x="-X_STP_HALF" y="-Y_STP_HALF" z="0.0" />
        <left-front-top-point x="-X_STP_HALF" y="Y_STP_HALF" z="0.0" />
        <left-back-bottom-point x="-X_STP_HALF" y="-Y_STP_HALF" z="-0.00005" />
        <right-front-bottom-point x="X_STP_HALF" y="-Y_STP_HALF" z="0.0" />
    </cuboid>
    <algebra val="pixel-shape-0" />
</type>'''

    def _get_mantid_rectangular_monitor(self):
        text = []
        rec_nx_to_type = {}
        # rec_ny = {}
        rec_xmin = []
        rec_xmax = []
        # num_types =1
        monitor_type_id = 0
        det_num = 0
        for m in self.monitors:

            rec = None
            for d in m.drawcalls:
                if type(d) == MantidRectangularDetectorLine:
                    rec = MantidRectangularDetector(d.line)

            if not rec:
                return ''

            rot_vector, alpha = m.transform.get_rotvector_alpha(deg=True)

            print ('alpha', alpha)
            if alpha ==0.0:
                rot_vector.y = 1
                print('rot_vector_y', rot_vector.y)

            x_step = (float(rec.xmax) - float(rec.xmin)) / float(rec.nx)
            y_step = (float(rec.ymax) - float(rec.ymin)) / float(rec.ny)
            x_step_half = x_step / 2
            y_step_half = y_step / 2

            s = self.rect_monitor
            s_type = self.rect_monitor_type
            p_type = self.rect_monitor_pixel

            s = s.replace('MONITOR_NAME', m.name)
            s = s.replace('X_LOC', str(m.pos.x))
            s = s.replace('Y_LOC', str(m.pos.y))
            s = s.replace('Z_LOC', str(m.pos.z-self._sample_Z()))
            s = s.replace('ROT_ANGLE', str(alpha))
            s = s.replace('ROT_X', str(rot_vector.x))
            s = s.replace('ROT_Y', str(rot_vector.y))
            s = s.replace('ROT_Z', str(rot_vector.z))
            s_type = s_type.replace('X_MIN', rec.xmin)
            s_type = s_type.replace('X_STEP', str(x_step))
            s_type = s_type.replace('Y_MIN', rec.ymin)
            s_type = s_type.replace('Y_STEP', str(y_step))
            s_type = s_type.replace('X_NUM', rec.nx)
            s_type = s_type.replace('Y_NUM', rec.ny)
            s = s.replace('PIXEL_MIN', rec.pixelmin)
            s = s.replace('X_NUM', rec.nx)
            p_type = p_type.replace('X_STP_HALF', str(x_step_half))
            p_type = p_type.replace('Y_STP_HALF', str(y_step_half))

            if rec.nx not in rec_nx_to_type:
                monitor_type_id += 1  #

                s_type = s_type.replace('MonNDtype', 'MonNDtype{}'.format(monitor_type_id))
                s_type = s_type.replace('rectangular_det_type', 'rectangular_det_type{}'.format(monitor_type_id))
                text.append(s_type)
                rec_nx_to_type[rec.nx] = monitor_type_id

            if rec.xmin not in rec_xmin:
                det_num = det_num + 1
                p_type = p_type.replace('rectangular_det_type', 'rectangular_det_type{}'.format(det_num))
                text.append(p_type)

            s = s.replace('MonNDtype', 'MonNDtype{}'.format(rec_nx_to_type[rec.nx]))

            text.append(s)

            rec_xmin.append(rec.xmin)
            rec_xmax.append(rec.xmax)

        return '\n\n'.join(text)

    banana_monitor = '''
    <component type="MonNDtype-0" name="MONITOR_NAME" idlist="MonNDtype-0-list">
        <locations x="X_LOC" y="Y_MIN" y-end="Y_MAX" n-elements="Y_NUM" z="Z_LOC" rot="ROT_ANGLE" axis-x="ROT_X" axis-y="ROT_Y" axis-z="ROT_Z"/> 
    </component>

    <type name="MonNDtype-0">
    <component type="pixel-0">
        <locations r="RADIUS" t="T_MIN" t-end="T_MAX" n-elements="T_NUM" rot="T_MIN" rot-end="T_MAX" axis-x="0.0" axis-y="1.0" axis-z="0.0"/>
    </component>
    </type>

    <type is="detector" name="pixel-0">
        <cuboid id="pixel-shape-0">
            <left-front-bottom-point x="X_STP_HALF" y="-Y_STP_HALF" z="0.0" />
            <left-front-top-point x="X_STP_HALF" y="Y_STP_HALF" z="0.00005" />
            <left-back-bottom-point x="X_STP_HALF" y="-Y_STP_HALF" z="0.0" />
            <right-front-bottom-point x="-X_STP_HALF" y="-Y_STP_HALF" z="0.0" />
        </cuboid>
        <algebra val="pixel-shape-0"/>
    </type>

    <idlist idname="MonNDtype-0-list">
        <id start="PIXEL_MIN" end="PIXEL_MAX"/></idlist>

    <type name="in5_t-type">
    </type>'''

    def _get_mantid_banana_monitor(self):
        text = []

        for m in self.monitors:

            ban = None
            for d in m.drawcalls:
                if type(d) == MantidBananaDetectorLine:
                    ban = MantidBananaDetector(d.line)

            if not ban:
                return ''

            rot_vector, alpha = m.transform.get_rotvector_alpha(deg=True)

            t_step = (float(ban.tmax) - float(ban.tmin)) / float(ban.nt)
            y_step = (float(ban.ymax) - float(ban.ymin)) / float(ban.ny)
            x_step_half = 2 * math.pi / 360 * float(ban.radius) * (float(ban.tmax) - float(ban.tmin)) / float(
                ban.nt) / 2
            y_step_half = y_step / 2

            s = self.banana_monitor
            s = s.replace('MONITOR_NAME', m.name)
            s = s.replace('X_LOC', str(m.pos.x))
            s = s.replace('Y_LOC', str(m.pos.y))
            s = s.replace('Z_LOC', str(m.pos.z-self._sample_Z()))
            s = s.replace('ROT_ANGLE', str(alpha))
            s = s.replace('ROT_X', str(rot_vector.x))
            s = s.replace('ROT_Y', str(rot_vector.y))
            s = s.replace('ROT_Z', str(rot_vector.z))
            s = s.replace('RADIUS', ban.radius)
            s = s.replace('T_MIN', ban.tmin)
            s = s.replace('T_MAX', ban.tmax)
            s = s.replace('T_STEP', str(t_step))
            s = s.replace('Y_MIN', ban.ymin)
            s = s.replace('Y_MAX', ban.ymax)
            s = s.replace('Y_STEP', str(y_step))
            s = s.replace('T_NUM', ban.nt)
            s = s.replace('Y_NUM', ban.ny)
            s = s.replace('PIXEL_MIN', ban.pixelmin)
            s = s.replace('PIXEL_MAX', str(int(float(ban.pixelmin) + float(ban.nt) * float(ban.ny)) - 1))
            s = s.replace('X_STP_HALF', str(x_step_half))
            s = s.replace('Y_STP_HALF', str(y_step_half))

            text.append(s)

        return '\n\n'.join(text)

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
        pixmonitors = self._get_mantid_pixels_monitors()
        rectmonitor = self._get_mantid_rectangular_monitor()
        bananamonitor = self._get_mantid_banana_monitor()

        return '\n\n'.join([self.header, source, sample, pixmonitors, rectmonitor, bananamonitor, self.footer]).strip()


class MantidPixel:
    def __init__(self, pixel_line_lst, transform):
        l = pixel_line_lst
        self.p_cp = transform.apply(Vector3d(float(l[4]), float(l[5]), float(l[6])))
        self.p1 = transform.apply(Vector3d(float(l[7]), float(l[8]), float(l[9])))
        self.p2 = transform.apply(Vector3d(float(l[10]), float(l[11]), float(l[12])))
        self.p3 = transform.apply(Vector3d(float(l[13]), float(l[14]), float(l[15])))
        self.p4 = transform.apply(Vector3d(float(l[16]), float(l[17]), float(l[18])))


class MantidRectangularDetector:
    def __init__(self, det_line_lst):
        # $xmin, $xmax, $ymin, $ymax, $nx, $ny, $pixelmin
        l = det_line_lst
        self.xmin = l[0]
        self.xmax = l[1]
        self.ymin = l[2]
        self.ymax = l[3]
        self.nx = l[4]
        self.ny = l[5]
        self.pixelmin = l[6]


class MantidBananaDetector:
    def __init__(self, det_line_lst):
        # $radius, $tmin, $tmax, $ymin, $ymax, $nt, $ny, $pixelmin
        l = det_line_lst
        self.radius = l[0]
        self.tmin = l[1]
        self.tmax = l[2]
        self.ymin = l[3]
        self.ymax = l[4]
        self.nt = l[5]
        self.ny = l[6]
        self.pixelmin = l[7]


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


def file_save(data, filename):
    ''' saves data for debug purposes '''
    f = open(filename, 'w')
    f.write(data)
    f.close()


def main(args):
    ''' script execution '''
    logging.basicConfig(level=logging.INFO)

    # inspect is required b McDisplayReader
    args.inspect = None
    reader = McDisplayReader(args, n=1, debug=True)
    instrument = reader.read_instrument()

    writer = MantidPixelWriter(instrument.components)
    print("assembling mantid xml...")
    text = writer.do_work()
    filename = args.instr + '.xml'
    file_save(text, filename)
    print("saved file %s" % filename)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('instr', help='display this instrument file (.instr or .out)')
    parser.add_argument('--default', action='store_true',
                        help='automatically use instrument defaults for simulation run')
    parser.add_argument('instr_options', nargs='*', help='simulation options and instrument params')

    args, unknown = parser.parse_known_args()
    # if --inspect --first or --last are given after instr, the remaining args become "unknown",
    # but we assume that they are instr_options
    if len(unknown) > 0:
        args.instr_options = unknown

    try:
        main(args)
    except KeyboardInterrupt:
        print('')
    except Exception as e:
        print(e)
        raise e

