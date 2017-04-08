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
    ip = InstrParser('/home/jaga/source/McCode/mcstas-comps/examples/Test_Magnetic_Rotation.instr')
    print(ip.parse())
    #quit()

import re
def repair(args):
    local_instr_files, local_comp_files = utils.get_instr_comp_files(args.localdir)
    
    files = []
    rows = []
    for filename in local_instr_files:
        try:
            print("parsing... %s" % filename)
            info = InstrParser(filename).parse()
            files.append(filename)
            rows.append(info)
        except:
            print("failed parsing instr file: %s" % filename)
            quit()
    
    cnt = 0
    for filename in local_instr_files:
        f = open(filename, 'r')
        header = utils.read_header(f)
        #define = utils.read_define_instr(f)
        
        print('*****')
        print(filename)
        print()
        
        seen_P = False
        par_docs = []
        idxs = []
        lines = header.splitlines()
        for i in range(len(lines)):
            l = lines[i]
            
            # fast-forward to %P / %Parameters tag
            if not seen_P and re.match('\* \%Parameters', l):
                seen_P = True
            elif not seen_P:
                continue
            # exit if we reach %L / %Link tag 
            if re.match('\* \%L', l):
                break
            
            l = l.lstrip('*').strip()
            m = re.match('(\w+):[ \t]*\[([ \w\/\(\)\\\~\-.,\":\%\^]+)\][ \t]*(.*)', l)
            if m:
                healthy_par_doc = (m.group(1), m.group(2), m.group(3).strip())
                par_docs.append(healthy_par_doc)
                idxs.append(i)
                continue
            elif re.match('(\w+):', l):
                # empty docstrings
                m3 = re.match('(\w+):[ \t]*$', l)
                if m3:
                    empty_par_doc = (m3.group(1), '', '')
                    par_docs.append(empty_par_doc)
                    idxs.append(i)
                    continue
                # no-unit docstrings
                m4 = re.match('(\w+):[ \t]*([ \t\w,.\-\(\)\=\^\/:\"\'\%\<\>]+)$', l)
                if m4:
                    limp_par_doc = (m4.group(1), '', m4.group(2).strip())
                    par_docs.append(limp_par_doc)
                    idxs.append(i)
                    continue
                # "inversed" docstrings
                m2 = re.match('(\w+):[ \t]*(.*)[ \t]*\[([ \w\/\(\)\\\~\-.,\":\%]+)\]', l)
                if m2:
                    par_doc = (m2.group(1), m2.group(3).strip(), m2.group(2))
                    par_docs.append(par_doc)
                    idxs.append(i)
                    continue
        
        # continue working on transforming the lines
        if len(par_docs) == 0:
            continue
        l01 = max([len(p[0]) for p in par_docs]) + max([len(p[1]) for p in par_docs])
        
        for i in range(len(par_docs)):
            p = par_docs[i]
            idx = idxs[i]
            
            # reorganize the docstring line
            format_str = '* %s: %-' +str(l01-len(p[0])+2)+ 's %s'
            l = format_str % (p[0], '['+p[1]+']', p[2])
            print(l)
            
            # replace l in lines:
            lines[idx] = l
        
        for l in f: 
            lines.append(l.rstrip('\n'))
        
        for l in lines:
            print(l)
        
        f.close()
        f = open(filename, 'w')
        f.write('\n'.join(lines) + '\n')
        f.close()
        
        cnt += 1
        print(cnt) 
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
    for f in local_instr_files:
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
        text = '\n'.join(['%4d: \n%s' % (i, files[i]) for i in range(len(files))])
        write_file('files', text)
        
        for i in range(utils.InstrHeaderInfo.__len__()-2):
            text = '\n'.join(['%4d: %s' % (j, rows[j][i]) for j in range(len(rows))])
            write_file(utils.InstrHeaderInfo.colname(i), text)
        for i in range(8, 10):
            text = '\n'.join(['%4d: \n%s' % (j, '\n'.join(['%-20s, %-10s, %s' % (str(k[0]), str(k[1]), str(k[2])) for k in rows[j][i]])) for j in range(len(rows))])
            write_file(utils.InstrHeaderInfo.colname(i), text)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('localdir', nargs='?', help='local dir to parse (in addition to lib dir)')
    args = parser.parse_args()
    
    #test()
    repair(args)

    main(args)

