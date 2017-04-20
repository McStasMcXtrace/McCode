#!/usr/bin/env python
# -*- coding: utf-8 -*-
import logging
import argparse
import sys
import os
import re
from datetime import datetime

sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from mccodelib import utils, mccode_config

def repair(localdir):
    '''
    Dev function used to alter instr file headers.
    '''
    local_instr_files, local_comp_files = utils.get_instr_comp_files(localdir)
    
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
        # read the first two instr file sections
        header = utils.read_header(f)
        define = utils.read_define_instr(f)
        
        # doc lines
        print('*****')
        print(filename)
        print()
        
        seen_P = False
        par_docs = []
        idxs = []
        idxs_remove = []
        lines = header.splitlines()
        for i in range(len(lines)):
            l = lines[i]
            
            # remove unwanted lines
            m = re.match('\* Release:', l)
            if m:
                idxs_remove.append(i)
                continue
            m = re.match('\* Version:', l)
            if m:
                idxs_remove.append(i)
                continue
            m = re.match('\* INPUT PARAMETERS:', l)
            if m:
                idxs_remove.append(i)
                continue
            
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
        
        # edit par doc lines, remove superfluous
        if True:
            if len(par_docs) == 0:
                continue
            l01 = max([len(p[0]) + len(p[1]) for p in par_docs])
            
            name, real_pars = utils.parse_define_instr(define)
            real_parnames = [par[1] for par in real_pars]
            
            # rewrite par doc lines, remove "bonus" doc lines without a parameter to document
            for i in range(len(par_docs)):
                p = par_docs[i]
                idx = idxs[i]
                
                # reorganize the docstring line
                format_str = '* %s: %-' +str(l01-len(p[0])+3)+ 's %s'
                l = format_str % (p[0], '['+p[1]+']', p[2])
                print(l)
                
                # replace l in lines:
                lines[idx] = l
                
                # flag superfluous doc lines for removal
                if p[0] not in real_parnames:
                    # (!!!!)
                    # TODO: take care of the ordering of idxs_remove. Today, we know that all previuosly 
                    # removed lines are above, but this may change
                    # (!!!!)
                    idxs_remove.append(idx)
            
            # add a stub par doc line for each par that isn't represented (WARNING: do not use while removing lines!
            if True:
                print()
                extra_pardoc_lines = []
                par_doc_names = [q[0] for q in par_docs]
                for i in range(len(real_pars)):
                    par_name = real_pars[i][1]
                    if par_name not in par_doc_names:
                        l = '* %s:' % par_name
                        print(l)
                        extra_pardoc_lines.append(l)
                # insert those extra lines ...
                good_idx = idxs[-1]
                for i in range(len(extra_pardoc_lines)):
                    l = extra_pardoc_lines[i]
                    lines.insert(good_idx + i + 1, l)
        
        # append/read-append remaining lines
        for l in define.splitlines():
            lines.append(l)
        for l in f:
            lines.append(l.rstrip('\n'))
        
        # remove unwanted lines:
        for idx in reversed(idxs_remove):
            del lines[idx]
        
        for l in lines:
            print(l)
        
        #continue
        
        f.close()
        f = open(filename, 'w')
        f.write('\n'.join(lines) + '\n')
        f.close()
        
        cnt += 1
        print(cnt)
    quit()

class InstrParser:
    ''' parses an instr file, extracting all relevant information into python '''
    def __init__(self, instr_file):
        self.instr_file = instr_file
        self.info = None
        self.has_parsed = False
    
    def parse(self):
        try:
            self._parse()
            self.has_parsed = True
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
    ''' create html doc text by means of a instr parser '''
    def __init__(self, info):
        self.info = info
    
    def create(self):
        i = self.info
        t = self.tags
        h = self.html
        
        h = h.replace(t[0], i.name)
        h = h.replace(t[1], i.name)
        h = h.replace(t[2], i.short_descr)
        h = h.replace(t[3], i.site)
        h = h.replace(t[4], i.author)
        h = h.replace(t[5], i.origin)
        h = h.replace(t[6], i.date)
        h = h.replace(t[7], i.description)
        
        
        h = h.replace(t[8], self.par_header)
        doc_rows = ''
        for p in i.params:
            lst = [pd[2] for pd in i.params_docs if p[1] == pd[0]] # TODO: rewrite to speed up 
            doc = lst[0] if len(lst) > 0 else ''
            doc_rows = doc_rows + '\n' + self.par_str % (p[0], p[1], doc, p[2])
        h = h.replace(t[9], doc_rows)
        
        h = h.replace(t[10], i.filepath)
        h = h.replace(t[11], os.path.basename(i.filepath))
        
        # TODO: implement links writing
        lstr = ''
        for l in i.links:
            lstr = lstr + self.lnk_str % l + '\n'
        h = h.replace(t[12], lstr)
        
        h = h.replace(t[13], datetime.now().strftime("%Y%m%d"))
        
        self.text = h
        return self.text
    
    tags = ['%TITLE%', '%INSTRNAME%', '%SHORT_DESCRIPTION%',
            '%SITE%', '%AUTHOR%', '%ORIGIN%', '%DATE%', '%DESCRIPTION%',
            '%T_HEAD%', '%T_ROWS%',
            '%INSTRFILE%', '%INSTRFILE_BASE%', '%LINKS%','%GENDATE%']
    par_str = "<TR> <TD>%s</TD><TD>%s</TD><TD>%s</TD><TD ALIGN=RIGHT>%s</TD></TR>"
    par_header = par_str % ('Name', 'Unit', 'Description', 'Default')
    lnk_str = "<LI>%s"
    
    
    html = '''
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">
<HTML><HEAD>
<TITLE>McStas: %TITLE%</TITLE>
<LINK REV="made" HREF="mailto:pkwi@fysik.dtu.dk">
</HEAD>

<BODY>

<P ALIGN=CENTER>
 [ <A href="#id">Identification</A>
 | <A href="#desc">Description</A>
 | <A href="#ipar">Input parameters</A>
 | <A href="#opar">Output parameters</A>
 | <A href="#links">Links</A> ]
</P>

<H1>The <CODE>%INSTRNAME%</CODE> Instrument</H1>

%SHORT_DESCRIPTION%

<H2><A NAME=id></A>Identification</H2>

<UL>
  <LI> <B>Site: </B>%SITE%
  <LI> <B>Author: </B>%AUTHOR%
  <LI> <B>Origin: </B>%ORIGIN%
  <LI> <B>Date: </B>%DATE%
</UL>
<H2><A NAME=desc></A>Description</H2>

<PRE>
%DESCRIPTION%
</PRE>

<H2><A NAME=ipar></A>Input parameters</H2>
Parameters in <B>boldface</B> are required;
the others are optional.

<TABLE BORDER=1>
%T_HEAD%
%T_ROWS%
</TABLE>

<H2><A NAME=links></A>Links</H2>

<UL>
  <LI> <A HREF="%INSTRFILE%">Source code</A> for <CODE>%INSTRFILE_BASE%</CODE>.
  %LINKS%
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
&lt;<A HREF="mailto:peter.willendrup@risoe.dk">pkwi@fysik.dtu.dk</A>&gt; /
%GENDATE%</ADDRESS>
</BODY></HTML>
'''


def write_file(filename, text):
    f = open(filename, 'w')
    f.write(text)
    f.close()


def main(args):
    logging.basicConfig(level=logging.INFO)
    
    localdir = args.localdir or '.'
    print("local directory: " + localdir)
    
    # repair mode - do not run mcdoc, just the "repair" function
    if args.repair:
        repair()
        quit()
    
    # get lib dir
    libdir = mccode_config.configuration["MCCODE_LIB_DIR"]
    print("lib directory: " + libdir)
    lib_instr_files, lib_comp_files = utils.get_instr_comp_files(libdir)
    local_instr_files, local_comp_files = utils.get_instr_comp_files(localdir)
    
    # parse all instr files
    rows = []
    files = []
    for f in local_instr_files:
        try:
            print("parsing... %s" % f)
            info = InstrParser(f).parse()
            info.filepath = f
            files.append(f)
            rows.append(info)
        except:
            print("failed parsing instr file: %s" % f)
            quit()
    print("parsed instr files: %s" % str(len(lib_instr_files)))
    
    # debug mode - write files with a header property each, then quit
    if args.debug:
        text = '\n'.join(['%4d: \n%s' % (i, files[i]) for i in range(len(files))])
        write_file('files', text)
        
        for i in range(utils.InstrHeaderInfo.__len__()-3):
            text = '\n'.join(['%4d: %s' % (j, rows[j][i]) for j in range(len(rows))])
            write_file(utils.InstrHeaderInfo.colname(i), text)
        for i in range(8, 9):
            text = '\n'.join(['%4d: \n%s' % (j, '\n'.join(['%-20s, %-10s, %s' % (str(k[0]), str(k[1]), str(k[2])) for k in rows[j][i]])) for j in range(len(rows))])
            write_file(utils.InstrHeaderInfo.colname(i), text)
        
        text = '\n'.join(['%4d: \n%s' % (j, '\n'.join(rows[j][10])) for j in range(len(rows))])
        #  '\n'.join(info.links)
        write_file(utils.InstrHeaderInfo.colname(10), text)
        quit()
    
    # generate and save all html pages docs
    html_files = []
    
    for p in rows:
        doc = InstrDocWriter(p)
        text = doc.create()
        h = os.path.splitext(f)[0] + '.html'
        print("writing doc file... %s" % h)
        write_file(h, text)
        html_files.append(h)
    
    # TODO: write overview files, properly assembling links to instr- and html-files
    for h in html_files:
        pass


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('localdir', nargs='?', help='local dir to parse (in addition to lib dir)')
    parser.add_argument('--debug', action='store_true', help='enable debug mode')
    parser.add_argument('--repair', action='store_true', help='enable repair mode')
    
    args = parser.parse_args()
    
    main(args)

