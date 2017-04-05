#!/usr/bin/env python
# -*- coding: utf-8 -*-
import logging
import argparse
import sys
import os

sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from mccodelib import utils, mccode_config

class InstrParInfo:
    ''' Component parameter info, used as McComponentParser.pars '''
    def __init__(self, info=None):
        if info:
            self.par_name = info.par_name
            self.type = info.type
            self.default_value = info.default_value
            self.doc_and_unit = info.doc_and_unit
        else:
            self.par_name = ''       # parameter par_name
            self.type = ''           # can be "string" or "int", but is mostly empty
            self.default_value = ''
            self.doc_and_unit = ''   # doc string and unit (no linebreaks)

class InstrParser:
    ''' parses an instr file, extracting all relevant information into python '''
    def __init__(self, instr_file):
        self.instr_file = instr_file
        self.info = None
    
    def parse(self):
        try:
            self._parse()
        except:
            self._parse_legacy()
        return self.info
        
    def _parse(self):
        raise Exception()
    
    def _parse_legacy(self):
        ''' parses the given instr file '''
        
        f = open(self.instr_file)
        logging.debug('parsing file "%s"' % self.instr_file)
        
        header = utils.read_header(f)
        info = utils.parse_instr_header(header)
        info.site = utils.get_instr_site_fromtxt(header)
        info.name, info.params = utils.parse_define_instr(utils.read_define_instr(f))
        
        self.info = info

class InstrDocWriter:
    html = '''
    <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">
<HTML><HEAD>
<TITLE>McStas: test Instrument at  Union_demos</TITLE>
<LINK REV="made" HREF="mailto:peter.willendrup@risoe.dk">
</HEAD>

<BODY>

<P ALIGN=CENTER>
 [ <A href="#id">Identification</A>
 | <A href="#desc">Description</A>
 | <A href="#ipar">Input parameters</A>
 | <A href="#opar">Output parameters</A>
 | <A href="#links">Links</A> ]
</P>

<H1>The <CODE>test</CODE> Instrument</H1>

Simple test instrument for sample component.


<H2><A NAME=id></A>Identification</H2>

<UL>
  <LI> <B>Site:    Union_demos</B>
  <LI> <B>Author:</B> Mads Bertelsen</B>
  <LI> <B>Origin:</B> University of Copenhagen</B>
  <LI> <B>Date:</B> September 2015</B>
  <LI> <B>Version:</B> 0.1 </B>
</UL>
<H2><A NAME=desc></A>Description</H2>

<PRE>
simple test instrument for sample component.

Example: filename="source_sct091_tu_02_1.dat" Detector: det_I=9.89304e+09

</PRE>

<H2><A NAME=ipar></A>Input parameters</H2>
Parameters in <B>boldface</B> are required;
the others are optional.
<TABLE BORDER=1>
<TR><TH>Name</TH>  <TH>Unit</TH>  <TH>Description</TH> <TH>Default</TH></TR>
<TR> <TD>stick_displacement</TD>
     <TD></TD> <TD></TD>
<TD ALIGN=RIGHT>0</TD> </TR>
</TABLE>


<H2><A NAME=links></A>Links</H2>

<UL>
  <LI> <A HREF="Union_demonstration_absorption_image.instr">Source code</A> for <CODE>test.instr</CODE>.
</UL>
<HR>
<P ALIGN=CENTER>
 [ <A href="#id">Identification</A>
 | <A href="#desc">Description</A>
 | <A href="#ipar">Input parameters</A>
 | <A href="#opar">Output parameters</A>
 | <A href="#links">Links</A> ]
</P>

<ADDRESS>
Generated automatically by McDoc, Peter Willendrup
&lt;<A HREF="mailto:peter.willendrup@risoe.dk">peter.willendrup@risoe.dk</A>&gt; /
Tue Feb  7 12:58:21 2017</ADDRESS>
</BODY></HTML>
    '''
    def __init__(self, instr_parser):
        self.instr_parser = instr_parser
    def _create_html(self):
        pass
    def write_file(self):
        pass

def write_file(filename, text):
    f = open(filename, 'w')
    f.write(text)
    f.close()

def test():
    text = open('/home/jaga/test/simulations/ISIS_SANS2d.instr').read()
    #print(text)
    
    #ip = InstrParser('/home/jaga/test/simulations/ILL_IN5.instr')
    #ip = InstrParser('/home/jaga/test/simulations/ILL_IN5_mod01.instr')
    ip = InstrParser('/usr/share/mcstas/2.3/examples/template_header_simple.instr')
    #ip = InstrParser('/usr/share/mcstas/2.3/examples/Test_Sample_nxs_diffraction.instr')
    
    ip.parse()
    
    quit()

def main(args):
    logging.basicConfig(level=logging.INFO)
    localdir = args.localdir or '.'
    
    # get lib dir
    libdir = mccode_config.configuration["MCCODE_LIB_DIR"]
    print("lib directory: " + libdir)
    lib_instr_files, lib_comp_files = utils.get_instr_comp_files(libdir)
    print("local directory: " + localdir)
    local_instr_files, local_comp_files = utils.get_instr_comp_files(localdir)
    
    # write debug files with a header property each 
    rows = []
    files = []
    for f in lib_instr_files:
        try:
            print("parsing... %s" % f)
            info = InstrParser(f).parse()
            files.append(f)
            rows.append(info)
        except:
            print("failed parsing instr file: %s" % f)
            quit()
    print("parsed instr files: %s" % str(len(lib_instr_files)))
        
    if True:
        text = '\n'.join(['%4d: %s' % (i, files[i]) for i in range(len(files))])
        write_file('files', text)
        
        for i in range(utils.InstrHeaderInfo.__len__()-2):
            text = '\n'.join(['%4d: %s' % (j, rows[j][i]) for j in range(len(rows))])
            write_file(utils.InstrHeaderInfo.colname(i), text)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('localdir', nargs='?', help='local dir to parse (in addition to lib dir)')
    args = parser.parse_args()
    
    #test()
    main(args)

