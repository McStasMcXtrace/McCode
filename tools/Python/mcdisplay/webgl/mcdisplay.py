#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
mcdisplay webgl script.
'''
import sys
import os
import logging
import argparse
import json
import subprocess
from datetime import datetime

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from mccodelib import pipetools
from mccodelib import mccode_config
from mccodelib.mcdisplayutils import McDisplayReader
from mccodelib.instrgeom import Vector3d

class SimpleWriter(object):
    ''' a minimal, django-omiting "glue file" writer tightly coupled to some comments in the file template.html '''
    def __init__(self, templatefile, campos, box, html_filename):
        self.template = templatefile
        self.campos = campos
        self.box = box
        self.html_filename = html_filename
    
    def write(self):
        # load and modify
        template = open(self.template).read()
        lines = template.splitlines()
        for i in range(len(lines)):
            if 'INSERT_CAMPOS_HERE' in lines[i]:
                lines[i] = '        campos_x = %s, campos_y = %s, campos_z = %s; // line written by SimpleWriter' % (str(self.campos.x), str(self.campos.y), str(self.campos.z))
                box = self.box
                lines[i+1] = '        box_x1 = %s, box_x2 = %s, box_y1 = %s, box_y2 = %s, box_z1 = %s, box_z2 = %s; // line written by SimpleWriter' % (str(box.x1), str(box.x2), str(box.y1), str(box.y2), str(box.z1), str(box.z2))
        self.text = '\n'.join(lines)
        
        # write to disk
        try:
            f = open(self.html_filename, 'w')
            f.write(self.text)
        finally:
            f.close()

class DjangoWriter(object):
    ''' writes a django template from the instrument representation '''
    instrument = None
    text = ''
    templatefile = ''
    campos = None
    
    def __init__(self, instrument, templatefile, campos):
        self.instrument = instrument
        self.templatefile = templatefile
        self.campos = campos
        
        # django stuff
        from django.template import Context
        from django.template import Template
        self.Context = Context
        self.Template = Template
        from django.conf import settings
        settings.configure()
    
    def build(self):
        templ = open(self.templatefile).read()
        t = self.Template(templ)
        c = self.Context({'instrument': self.instrument, 
            'campos_x': self.campos.x, 'campos_y': self.campos.y, 'campos_z': self.campos.z,})
        self.text = t.render(c)
    
    def save(self, filename):
        ''' save template to disk '''
        try:
            f = open(filename, 'w')
            f.write(self.text)
        finally:
            f.close()

def _write_html(instrument, html_filepath, first=None, last=None):
    ''' writes instrument definition to html/js '''
    box = instrument.get_boundingbox(first, last)
    box_total = instrument.get_boundingbox()
    
    # create camera view coordinates given the bounding box
    dx = box.x2 - box.x1
    dy = box.y2 - box.y1
    dz = box.z2 - box.z1
    x = -(box.x1 + max(dx, dz)/2)
    y = max(dx, dz)/2
    z = box.z1 + dz/2
    campos = Vector3d(x, y, z)
    
    # render html
    templatefile = os.path.join(os.path.dirname(__file__), "template.html")
    writer = SimpleWriter(templatefile, campos, box_total, html_filepath)
    writer.write()

def write_browse(instrument, raybundle, dir, nobrowse=False):
    ''' writes instrument definitions to html/ js '''
    # write mcdisplay.js
    mcd_filepath = os.path.join(os.path.dirname(__file__), 'mcdisplay.js')
    file_save(open(mcd_filepath).read(), os.path.join(dir, '_mcdisplay.js'))
    
    # write html
    html_filepath = os.path.join(dir, 'index.html')
    _write_html(instrument, html_filepath, first=args.first, last=args.last)
    
    # write instrument
    json_instr = 'MCDATA_instrdata = %s;' % json.dumps(instrument.jsonize(), indent=0)
    file_save(json_instr, os.path.join(dir, '_instr.js'))
    
    # write particles
    json_neutr = 'MCDATA_particledata = %s;' % json.dumps(raybundle.jsonize(), indent=0)
    file_save(json_neutr, os.path.join(dir, '_particles.js'))
    
    # exit if nobrowse flag has been set
    if nobrowse:
        return
    
    # open a web-browser in a cross-platform way
    try:
        subprocess.Popen('%s %s' % (mccode_config.configuration['BROWSER'], html_filepath), shell=True)
    except Exception as e:
        raise Exception('Os-specific open browser: %s' % e.__str__())
    
def get_datadirname(instrname):
    ''' returns an mcrun-like name-date-time string '''
    return "%s_%s" % (instrname, datetime.strftime(datetime.now(), "%Y%m%d_%H%M%S"))

def file_save(data, filename):
    ''' saves data for debug purposes '''
    f = open(filename, 'w')
    f.write(data)
    f.close()

def main(args):
    logging.basicConfig(level=logging.INFO)
    debug = False
    
    # output directory
    dirname = get_datadirname(os.path.splitext(os.path.basename(args.instr))[0])
    if args.dirname:
        dirname = args.dirname
    
    # set up a pipe, read and parse the particle trace
    reader = McDisplayReader(args, n=100, dir=dirname, debug=debug)
    instrument = reader.read_instrument()
    instrument.setCmd(reader.cmd)
    raybundle = reader.read_particles()
    
    # write output files
    write_browse(instrument, raybundle, dirname, args.nobrowse)
    
    if debug:
        # this should enable template.html to load directly
        jsonized = json.dumps(instrument.jsonize(), indent=0)
        file_save(jsonized, 'jsonized.json')

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('instr', help='display this instrument file (.instr or .out)')
    parser.add_argument('--default', action='store_true', help='automatically use instrument defaults for simulation run')
    parser.add_argument('--nobrowse', action='store_true', help='do not open a webbrowser viewer')
    parser.add_argument('--dirname', help='name of the output directory requested to mcrun')
    parser.add_argument('--inspect', help='display only particle rays reaching this component passed to mcrun')
    parser.add_argument('--first', help='zoom range first component')
    parser.add_argument('--last', help='zoom range last component')
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

