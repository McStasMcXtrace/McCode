#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
Generates html docpages from mccode instrument and component files.

A docpage is generated for every instrument and component file, and an 
overview page is written and browsed. Default option Read installed docpage.

Specify a directory to add local results, and a search term for filtered or 
specific file results.
'''
import logging
import argparse
import sys
import os
import re
import subprocess
from datetime import datetime
from os.path import join, basename

sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from mccodelib import utils, mccode_config

def repair_instr(localdir):
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
            m = re.match('(\w+):[ \t]*\[([ \w\/\(\)\\\~\-.,\":\%\^\|\{\};\*]+)\][ \t]*(.*)', l)
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
                m4 = re.match('(\w+):[ \t]*([ \t\w,.\-\(\)\=\^\/:\"\'\%\<\>\|\{\};\*]+)$', l)
                if m4:
                    limp_par_doc = (m4.group(1), '', m4.group(2).strip())
                    par_docs.append(limp_par_doc)
                    idxs.append(i)
                    continue
                # "inversed" docstrings
                m2 = re.match('(\w+):[ \t]*(.*)[ \t]*\[([ \w\/\(\)\\\~\-.,\":\%\|\{\}]+)\]', l)
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
            if False:
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
        
        continue
        
        f.close()
        f = open(filename, 'w')
        f.write('\n'.join(lines) + '\n')
        f.close()
        
        cnt += 1
        print(cnt)
    quit()


def repair_comp(localdir):
    '''
    Dev function used to alter comp file headers.
    '''
    local_instr_files, local_comp_files = utils.get_instr_comp_files(localdir)
    
    files = []
    rows = []
    
    for filename in local_comp_files:
        try:
            print("parsing... %s" % filename)
            info = CompParser(filename).parse()
            files.append(filename)
            rows.append(info)
        except:
            print("failed parsing instr file: %s" % filename)
            quit()
    
    cnt = 0
    for filename in local_comp_files:
        f = open(filename, 'r')
        # read the first two instr file sections
        header = utils.read_header(f)
        define = utils.read_define_comp(f)
        
        # doc lines
        print('*****')
        print(filename)
        print()
        
        if filename == '/home/jaga/source/McCode/mcstas-comps/samples/Single_crystal.comp':
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
            
            # fast-forward to %P / %Parameters tag
            if not seen_P and re.match('\* \%P', l):
                seen_P = True
            elif not seen_P:
                continue
            
            # exit if we reach %L / %Link tag 
            if re.match('\* \%L', l):
                break
            
            l = l.lstrip('*').strip()
            m = re.match('(\w+):[ \t]*\[([ \w\/\(\)\\\~\-.,\":\%\^\|\{\};\*]+)\][ \t]*(.*)', l)
            if m:
                healthy_par_doc = (m.group(1), m.group(2), m.group(3).strip())
                par_docs.append(healthy_par_doc)
                idxs.append(i)
                continue
            elif re.match('\w+:', l):
                # "inversed" docstrings
                m2 = re.match('(\w+):[ \t]*(.*)[ \t]*\[([ \w\/\(\)\\\~\-.,\":\%\|\{\}]+)\]', l)
                if m2:
                    par_doc = (m2.group(1), m2.group(3).strip(), m2.group(2))
                    par_docs.append(par_doc)
                    idxs.append(i)
                    continue
                # docstrings with soft brackets
                m5 = re.match('(\w+):[ \t]*\(([\w\/\\\~\-.,\":\%\^\|\{\};\*]+)\)[ \t]*(.*)', l)
                if m5:
                    par_doc = (m5.group(1), m5.group(2).strip(), m5.group(3))
                    par_docs.append(par_doc)
                    idxs.append(i)
                    continue
                # "inversed" docstrings with soft brackets
                m2 = re.match('(\w+):[ \t]*(.*)[ \t]*\(([\w\/\(\)\\\~\-.,\":\%\|\{\}]+)\)[ \t]*$', l)
                if m2:
                    par_doc = (m2.group(1), m2.group(3).strip(), m2.group(2))
                    par_docs.append(par_doc)
                    idxs.append(i)
                    continue
                # empty docstrings
                m3 = re.match('(\w+):[ \t]*$', l)
                if m3:
                    empty_par_doc = (m3.group(1), '', '')
                    par_docs.append(empty_par_doc)
                    idxs.append(i)
                    continue
                # no-unit docstrings
                m4 = re.match('(\w+):[ \t]*([ \t\w,.\-\(\)\=\^\/:\"\'\%\<\>\|\{\};\*]+)$', l)
                if m4:
                    limp_par_doc = (m4.group(1), '', m4.group(2).strip())
                    par_docs.append(limp_par_doc)
                    idxs.append(i)
                    continue
        
        # edit par doc lines, remove superfluous
        if True:
            if len(par_docs) == 0:
                continue
            l01 = max([len(p[0]) + len(p[1]) for p in par_docs])
            
            name, defpar, setpar, outpar = utils.parse_define_comp(define)
            real_pars = defpar + setpar + outpar
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
                if False:
                    if p[0] not in real_parnames:
                        # (!!!!)
                        # TODO: take care of the ordering of idxs_remove. Today, we know that all previuosly 
                        # removed lines are above, but this may change
                        # (!!!!)
                        idxs_remove.append(idx)
            
            # add a stub par doc line for each par that isn't represented (WARNING: do not use while removing lines!
            if False:
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
        
        continue
        
        f.close()
        f = open(filename, 'w')
        f.write('\n'.join(lines) + '\n')
        f.close()
        
        cnt += 1
        print(cnt)
    quit()


def get_html_filepath(filepath):
    ''' transform from .anything to .html '''
    return os.path.splitext(filepath)[0] + '.html'

class OverviewDocWriter:
    ''' Creates the mcdoc overview html page. '''
    def __init__(self, comp_info_lst, instr_info_lst, comp_info_local_lst, instr_info_local_lst, mccode_libdir):
        self.comp_info_lst = comp_info_lst
        self.instr_info_lst = instr_info_lst
        self.comp_info_local_lst = comp_info_local_lst
        self.instr_info_local_lst = instr_info_local_lst
        self.mccode_libdir = mccode_libdir
        self.text = ''
    
    def create(self):
        ''' action code for create overview page! '''
        i_lst = self.instr_info_lst
        c_loc_lst = self.comp_info_local_lst
        i_loc_lst = self.instr_info_local_lst
        t = self.tab_line
        
        # create comp tables
        #'%TAB_LINES_SOURCES%', '%TAB_LINES_OPTICS%', '%TAB_LINES_SAMPLES%', '%TAB_LINES_MONITORS%', '%TAB_LINES_MISC%', '%TAB_LINES_OBSOLETE%',
        # Sources
        sources_lst = [c for c in self.comp_info_lst if c.category=='sources']
        sources_tab = ''
        for c in sources_lst:
            sources_tab = sources_tab + t % (get_html_filepath(c.filepath), c.name, c.origin, c.author, c.filepath, 'comp', c.short_descr) + '\n'
        # Optics
        optics_lst = [c for c in self.comp_info_lst if c.category=='optics']
        optics_tab = ''
        for c in optics_lst:
            optics_tab = optics_tab + t % (get_html_filepath(c.filepath), c.name, c.origin, c.author, c.filepath, 'comp', c.short_descr) + '\n'
        # Samples
        samples_lst = [c for c in self.comp_info_lst if c.category=='samples']
        samples_tab = ''
        for c in samples_lst:
            samples_tab = samples_tab + t % (get_html_filepath(c.filepath), c.name, c.origin, c.author, c.filepath, 'comp', c.short_descr) + '\n'
        # Detectors
        monitors_lst = [c for c in self.comp_info_lst if c.category=='monitors']
        monitors_tab = ''
        for c in monitors_lst:
            monitors_tab = monitors_tab + t % (get_html_filepath(c.filepath), c.name, c.origin, c.author, c.filepath, 'comp', c.short_descr) + '\n'
        # Misc
        misc_lst = [c for c in self.comp_info_lst if c.category=='misc']
        misc_tab = ''
        for c in misc_lst:
            misc_tab = misc_tab + t % (get_html_filepath(c.filepath), c.name, c.origin, c.author, c.filepath, 'comp', c.short_descr) + '\n'
        # Contributed
        contrib_lst = [c for c in self.comp_info_lst if c.category=='contrib']
        contrib_tab = ''
        for c in contrib_lst:
            contrib_tab = contrib_tab + t % (get_html_filepath(c.filepath), c.name, c.origin, c.author, c.filepath, 'comp', c.short_descr) + '\n'
        # Obsolete
        obsolete_lst = [c for c in self.comp_info_lst if c.category=='obsolete']
        obsolete_tab = ''
        for c in obsolete_lst:
            obsolete_tab = obsolete_tab + t % (get_html_filepath(c.filepath), c.name, c.origin, c.author, c.filepath, 'comp', c.short_descr) + '\n'
        
        # create instr examples table
        ex_tab = ''
        for i in i_lst:
            ex_tab = ex_tab + t % (get_html_filepath(i.filepath), i.name, i.origin, i.author, i.filepath, 'instr', i.short_descr) + '\n'

        # create local instrs / comps table
        local_comp_tab = ''
        for c in c_loc_lst:
            local_comp_tab  = local_comp_tab + t % (get_html_filepath(c.filepath), c.name, c.origin, c.author, c.filepath, 'comp', c.short_descr) + '\n'
        
        local_instr_tab = ''
        for i in i_loc_lst:
            local_instr_tab = local_instr_tab + t % (get_html_filepath(i.filepath), i.name, i.origin, i.author, i.filepath, 'instr', i.short_descr) + '\n'

        text = self.html
        
        text = text.replace('%MCCODE_LIBDIR%', self.mccode_libdir)
        text = text.replace('%TAB_HEAD%', self.tab_header)
        text = text.replace('%TAB_LINES_SOURCES%', sources_tab)
        text = text.replace('%TAB_LINES_OPTICS%', optics_tab)
        text = text.replace('%TAB_LINES_SAMPLES%', samples_tab)
        text = text.replace('%TAB_LINES_MONITORS%', monitors_tab)
        text = text.replace('%TAB_LINES_MISC%', misc_tab)
        text = text.replace('%TAB_LINES_CONTRIB%', contrib_tab)
        text = text.replace('%TAB_LINES_OBSOLETE%', obsolete_tab)
        text = text.replace('%TAB_LINES_EXAMPLES%', ex_tab)
        text = text.replace('%TAB_LINES_COMPS_LOCAL%', local_comp_tab)
        text = text.replace('%TAB_LINES_INSTR_LOCAL%', local_instr_tab)
        text = text.replace('%LINK_FILECOLON_DATA%', 'file://%s/data' % self.mccode_libdir)
        text = text.replace('%LINK_FILECOLON_SHARE%', 'file://%s/share' % self.mccode_libdir)
        text = text.replace('%GENDATE%', '{0:%Y-%m-%d %H:%M:%S}'.format(datetime.now()))

        #some McXtrace specific edits
        if (mccode_config.get_mccode_prefix() == 'mx'):
          text = text.replace('McStas','McXtrace')
          text = text.replace('mcstas','mcxtrace')
          test = text.replace('MCSTAS','MCXTRACE')

        self.text = text
        return self.text
    
    tab_header = '''
<TR>
<TD><B><I>Name</I></B></TD>
<TD WIDTH="10%"><B><I>Origin</I></B></TD>
<TD WIDTH="10%"><B><I>Author(s)</I></B></TD>
<TD><B><I>Source code</I></B></TD>
<TD><B><I>Description</I></B></TD>
</TR>
'''
    tab_line_items = ['%DOCFILE%', '%NAME%', '%ORIGIN%', '%AUTHOR%', '%SOURCEFILE%', '%COMP_OR_INSTR%', '%DOC%']
    tab_line = '''
<TR>
<TD><B><A HREF="%s">%s</A></B></TD>
<TD>%s</TD>
<TD>%s</TD>
<TD><A HREF="%s">%s</A></TD>
<TD>%s</TD>
</TR>
'''
    tags = ['%MCCODE_LIBDIR%',
            '%TAB_HEAD%',
            '%TAB_LINES_SOURCES%',
            '%TAB_LINES_OPTICS%',
            '%TAB_LINES_SAMPLES%',
            '%TAB_LINES_MONITORS%',
            '%TAB_LINES_CONTRIB%',
            '%TAB_LINES_MISC%',
            '%TAB_LINES_OBSOLETE%',
            '%TAB_LINES_EXAMPLES%',
            '%TAB_LINES_LOCAL%',
            '%LINK_FILECOLON_DATA%',
            '%LINK_FILECOLON_SHARE%',
            '%GENDATE%']
    html = '''
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">
<HTML>
<HEAD>
   <META NAME="GENERATOR" CONTENT="McDoc">
   <TITLE>McStas : Components/Instruments Library </TITLE>
</HEAD>
<BODY>

<P ALIGN=CENTER>
 [ <A href="#sources">sources</A>
 | <A href="#optics">optics</A>
 | <A href="#samples">samples</A>
 | <A href="#monitors">monitors</A>
 | <A href="#misc">misc</A>
 | <A href="#contrib">contrib</A>
 | <A href="#obsolete">obsolete</A>
 | <A href="#examples">examples</A>
 | <A href="#localcomps">local comps</A>
 | <A href="#localinstrs">local instruments</A>
 | <A href="%LINK_FILECOLON_DATA%">data</A>
 | <A href="%LINK_FILECOLON_SHARE%">share</A> ]
</P>
<P ALIGN=CENTER>
[ <a href="file://%MCCODE_LIBDIR%/doc/manuals/mcstas-manual.pdf">User Manual</a>
| <a href="file://%MCCODE_LIBDIR%/doc/manuals/mcstas-components.pdf">Component Manual</a> ]
| <a href="file://%MCCODE_LIBDIR%/">McCode lib dir</a> ]
</P>

<CENTER><H1>Components and Instruments from the Library for <i>McStas</i></H1></CENTER>

<P><A NAME="sources"></A>
<B><FONT COLOR="#FF0000">Sources</FONT></B>
<TABLE BORDER COLS=5 WIDTH="100%" NOSAVE>

%TAB_HEAD%

%TAB_LINES_SOURCES%

</TABLE>


<P><A NAME="optics"></A>
<B><FONT COLOR="#FF0000">Optics</FONT></B>
<TABLE BORDER COLS=5 WIDTH="100%" NOSAVE>

%TAB_HEAD%

%TAB_LINES_OPTICS%

</TABLE>


<P><A NAME="samples"></A>
<B><FONT COLOR="#FF0000">Samples</FONT></B>
<TABLE BORDER COLS=5 WIDTH="100%" NOSAVE>

%TAB_HEAD%

%TAB_LINES_SAMPLES%

</TABLE>


<P><A NAME="monitors"></A>
<B><FONT COLOR="#FF0000">Detectors and monitors</FONT></B>
<TABLE BORDER COLS=5 WIDTH="100%" NOSAVE>

%TAB_HEAD%

%TAB_LINES_MONITORS%

</TABLE>


<P><A NAME="misc"></A>
<B><FONT COLOR="#FF0000">Misc</FONT></B>
<TABLE BORDER COLS=5 WIDTH="100%" NOSAVE>

%TAB_HEAD%

%TAB_LINES_MISC%

</TABLE>


<P><A NAME="contrib"></A>
<B><FONT COLOR="#FF0000">Contributed components</FONT></B>
<TABLE BORDER COLS=5 WIDTH="100%" NOSAVE>

%TAB_HEAD%

%TAB_LINES_CONTRIB%

</TABLE>


<P><A NAME="obsolete"></A>
<B><FONT COLOR="#FF0000">Obsolete (avoid usage whenever possible)</FONT></B>
<TABLE BORDER COLS=5 WIDTH="100%" NOSAVE>

%TAB_HEAD%

%TAB_LINES_OBSOLETE%

</TABLE>


<P><A NAME="examples"></A>
<B><FONT COLOR="#FF0000">Instrument Examples</FONT></B>
<TABLE BORDER COLS=5 WIDTH="100%" NOSAVE>

%TAB_HEAD%

%TAB_LINES_EXAMPLES%

</TABLE>


<P><A NAME="localcomps"></A>
<B><FONT COLOR="#FF0000">Local components</FONT></B>
<TABLE BORDER COLS=5 WIDTH="100%" NOSAVE>

%TAB_HEAD%

%TAB_LINES_COMPS_LOCAL%

</TABLE>


<P><A NAME="localinstrs"></A>
<B><FONT COLOR="#FF0000">Local instruments</FONT></B>
<TABLE BORDER COLS=5 WIDTH="100%" NOSAVE>

%TAB_HEAD%

%TAB_LINES_INSTR_LOCAL%

</TABLE>


<HR WIDTH="100%">
<CENTER>
[ <A href="http://www.mcstas.org/"><I>McStas web site</I></A> ]
</CENTER>

<P><BR>
<ADDRESS>
Generated on %GENDATE%
</ADDRESS>
</BODY>
</HTML>
'''

class InstrParser:
    ''' parses an instr or comp file, extracting all relevant information into python '''
    def __init__(self, filename):
        self.filename = filename
        self.info = None
        self.has_parsed = False
    
    def stub(self):
        ''' fallback parsing '''
        self.info = utils.InstrCompHeaderInfo()
        self.has_parsed = True
        return self.info
    
    def parse(self):
        ''' parses the given file '''
        f = open(self.filename)
        logging.debug('parsing file "%s"' % self.filename)
        
        header = utils.read_header(f)
        info = utils.parse_header(header)
        info.site = utils.get_instr_site_fromtxt(header)
        dfine = utils.read_define_instr(f)
        info.name, info.params = utils.parse_define_instr(dfine)
        
        self.info = info
        return self.info


class InstrDocWriter:
    ''' create html doc text by means of a instr parser '''
    def __init__(self, info):
        self.info = info
        self.text = ''

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
            unit = [pd[1] for pd in i.params_docs if p[1] == pd[0]]
            unit = unit[0] if len(unit) > 0 else ''
            defval = p[2] if p[2] != None else ''
            doc = [pd[2] for pd in i.params_docs if p[1] == pd[0]] # TODO: rewrite to speed up 
            doc = doc[0] if len(doc) > 0 else ''
            if defval == '':
                doc_rows = doc_rows + '\n' + self.par_str_boldface % (p[1], unit, doc, defval)
            else:
                doc_rows = doc_rows + '\n' + self.par_str % (p[1], unit, doc, defval)
        h = h.replace(t[9], doc_rows)

        h = h.replace(t[10], i.filepath)
        h = h.replace(t[11], os.path.basename(i.filepath))
        
        # TODO: implement links writing
        lstr = ''
        for l in i.links:
            lstr = lstr + self.lnk_str % l + '\n'
        h = h.replace(t[12], lstr)
        
        h = h.replace(t[13], '{0:%Y-%m-%d %H:%M:%S}'.format(datetime.now()))

        self.text = h
        return self.text

    tags = ['%TITLE%',
            '%INSTRNAME%',
            '%SHORT_DESCRIPTION%',
            '%SITE%',
            '%AUTHOR%',
            '%ORIGIN%',
            '%DATE%',
            '%DESCRIPTION%',
            '%T_HEAD%',
            '%T_ROWS%',
            '%INSTRFILE%',
            '%INSTRFILE_BASE%',
            '%LINKS%',
            '%GENDATE%']
    par_str = "<TR> <TD>%s</TD><TD>%s</TD><TD>%s</TD><TD ALIGN=RIGHT>%s</TD></TR>"
    par_str_boldface = "<TR> <TD><strong>%s</strong></TD><TD>%s</TD><TD>%s</TD><TD ALIGN=RIGHT>%s</TD></TR>"
    par_header = par_str % ('<strong>Name</strong>', '<strong>Unit</strong>', '<strong>Description</strong>', '<strong>Default</strong>')
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
Generated on %GENDATE%
</ADDRESS>
</BODY></HTML>
'''


class CompParser(InstrParser):
    def parse(self):
        ''' override '''
        f = open(self.filename)
        logging.debug('parsing file "%s"' % self.filename)
        
        header = utils.read_header(f)
        info = utils.parse_header(header)
        info.site = utils.get_instr_site_fromtxt(header)
        
        dfine = utils.read_define_comp(f)
        name, setpar, defpar, outpar = utils.parse_define_comp(dfine)
        
        info.name = name
        info.category = utils.get_comp_category(self.filename)
        
        # basically just for debug use
        info.params = setpar + defpar + outpar
        
        # these are used by CompDocWriter
        info.setparams = setpar
        info.defparams = defpar
        info.outparams = outpar
        
        self.info = info
        return self.info

class CompDocWriter:
    ''' create html doc text by means of a instr parser '''
    def __init__(self, info):
        self.info = info
        self.text = ''
    
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
        doc_rows_in = ''

        for p in i.setparams + i.defparams:
            unit = [pd[1] for pd in i.params_docs if p[1] == pd[0]]
            unit = unit[0] if len(unit) > 0 else ''
            defval = p[2] if p[2] != None else ''
            doc = [pd[2] for pd in i.params_docs if p[1] == pd[0]]
            doc = doc[0] if len(doc) > 0 else ''
            if defval == '':
                doc_rows_in = doc_rows_in + '\n' + self.par_str_boldface % (p[1], unit, doc, defval)
            else:
                doc_rows_in = doc_rows_in + '\n' + self.par_str % (p[1], unit, doc, defval)
        h = h.replace(t[9], doc_rows_in)

        doc_rows_out = ''
        for p in i.outparams:
            unit = [pd[1] for pd in i.params_docs if p[1] == pd[0]]
            unit = unit[0] if len(unit) > 0 else ''
            defval = p[2] if p[2] != None else ''
            doc = [pd[2] for pd in i.params_docs if p[1] == pd[0]]
            doc = doc[0] if len(doc) > 0 else ''
            if defval == '':
                doc_rows_out = doc_rows_out + '\n' + self.par_str_boldface % (p[1], unit, doc, defval)
            else:
                doc_rows_out = doc_rows_out + '\n' + self.par_str % (p[1], unit, doc, defval)
        h = h.replace(t[10], doc_rows_out)

        h = h.replace(t[11], i.filepath)
        h = h.replace(t[12], os.path.basename(i.filepath))

        # TODO: implement links writing
        lstr = ''
        for l in i.links:
            lstr = lstr + self.lnk_str % l + '\n'
        h = h.replace(t[13], lstr)

        h = h.replace(t[14], '{0:%Y-%m-%d %H:%M:%S}'.format(datetime.now()))

        self.text = h
        return self.text

    tags = ['%TITLE%',
            '%COMPNAME%',
            '%SHORT_DESCRIPTION%',
            '%SITE%',
            '%AUTHOR%',
            '%ORIGIN%',
            '%DATE%',
            '%DESCRIPTION%',
            '%T_HEAD%',
            '%T_ROWS_IN%',
            '%T_ROWS_OUT%',
            '%COMPFILE%',
            '%COMPFILE_BASE%',
            '%LINKS%',
            '%GENDATE%']
    par_str = "<TR> <TD>%s</TD><TD>%s</TD><TD>%s</TD><TD ALIGN=RIGHT>%s</TD></TR>"
    par_str_boldface = "<TR> <TD><strong>%s</strong></TD><TD>%s</TD><TD>%s</TD><TD ALIGN=RIGHT>%s</TD></TR>"
    par_header = par_str % ('<strong>Name</strong>', '<strong>Unit</strong>', '<strong>Description</strong>', '<strong>Default</strong>')
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

<H1>The <CODE>%COMPNAME%</CODE> Component</H1>

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
%T_ROWS_IN%
</TABLE>

<H2><A NAME=ipar></A>Output parameters</H2>
Parameters in <B>boldface</B> are required;
the others are optional.

<TABLE BORDER=1>
%T_HEAD%
%T_ROWS_OUT%
</TABLE>

<H2><A NAME=links></A>Links</H2>

<UL>
  <LI> <A HREF="%COMPFILE%">Source code</A> for <CODE>%COMPFILE_BASE%</CODE>.
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
Generated on %GENDATE%
</ADDRESS>
</BODY></HTML>
'''


def write_file(filename, text, failsilent=False):
    try:
        f = open(filename, 'w')
        f.write(text)
        f.close()
    except Exception as e:
        if failsilent:
            pass
        else:
            raise e

def parse_and_filter(indir, namefilter=None, recursive=False, printlog=False):
    ''' read and parse headers and definitions of component and instrument files '''
    instr_files, comp_files = utils.get_instr_comp_files(indir, recursive)
    print("parsing root folder:", indir)
    comp_info_lst = []
    for f in comp_files:
        try:
            if printlog:
                print("parsing comp... %s" % f)
            info = CompParser(f).parse()
            info.filepath = os.path.abspath(f)
            comp_info_lst.append(info)
        except:
            print("failed parsing file: %s" % f)
            comp_info_lst.append(CompParser(f).stub())
    print("comp files: %s" % str(len(comp_files)))

    instr_info_lst = []
    for f in instr_files:
        try:
            if printlog:
                print("parsing instr... %s" % f)
            info = InstrParser(f).parse()
            info.filepath = os.path.abspath(f)
            instr_info_lst.append(info)
        except:
            print("failed parsing file: %s" % f)
            instr_info_lst.append(InstrParser(f).stub())
    print("instr files: %s" % str(len(instr_files)))

    if namefilter != None:
        comp_info_lst = [c for c in comp_info_lst if re.search(namefilter.lower(), c.name.lower())]
        instr_info_lst = [c for c in instr_info_lst if re.search(namefilter.lower(), c.name.lower())]
        comp_files = [f for f in comp_files if re.search(namefilter.lower(), os.path.splitext(os.path.basename(f))[0].lower())]
        instr_files = [f for f in instr_files if re.search(namefilter.lower(), os.path.splitext(os.path.basename(f))[0].lower())]

    return comp_info_lst, instr_info_lst, comp_files, instr_files

def write_doc_files_or_continue(comp_infos, instr_infos, comp_files, instr_files, printlog=False):
    ''' Writes component and instrument docs files '''
    for i in range(len(comp_infos)):
        p = comp_infos[i]
        f = comp_files[i]
        doc = CompDocWriter(p)
        text = doc.create()
        h = os.path.splitext(f)[0] + '.html'
        if printlog:
            print("writing doc file... %s" % h)
        write_file(h, text, failsilent=True)

    for i in range(len(instr_infos)):
        p = instr_infos[i]
        f = instr_files[i]
        doc = InstrDocWriter(p)
        text = doc.create()
        h = os.path.splitext(f)[0] + '.html'
        if printlog:
            print("writing doc file... %s" % h)
        write_file(h, text, failsilent=True)


def main(args):
    logging.basicConfig(level=logging.INFO)

    usedir = mccode_config.configuration["MCCODE_LIB_DIR"]

    if args.dir==None and args.install==False and args.searchterm==None and args.manual==False and args.comps==False and args.web==False:
        ''' browse system docs and exit '''
        subprocess.Popen('%s %s' % (mccode_config.configuration['BROWSER'], os.path.join(usedir,mccode_config.get_mccode_prefix()+'doc.html')), shell=True)
        quit()

    elif args.manual == True:
        ''' open manual and exit ''' 
        subprocess.Popen('%s %s' % (mccode_config.configuration['BROWSER'], os.path.join(usedir,'doc','manuals',mccode_config.configuration['MCCODE']+'-manual.pdf')), shell=True)
        quit()

    elif args.comps == True:
        ''' open component manual and exit '''
        subprocess.Popen('%s %s' % (mccode_config.configuration['BROWSER'], os.path.join(usedir,'doc','manuals',mccode_config.configuration['MCCODE']+'-components.pdf')), shell=True)
        quit()

    elif args.web == True:
        ''' open website and exit '''
        subprocess.Popen('%s %s' % (mccode_config.configuration['BROWSER'], 'http://www.'+mccode_config.configuration['MCCODE']+'.org'), shell=True)
        quit()

    elif args.install == True:
        ''' write system doc files '''
        if args.searchterm:
            print("will not write filtered system docs, exiting...")
            quit()
        print("writing mccode distribution docs...")
        if args.dir:
            usedir = args.dir
            print("using custom dir: %s" % usedir)

        comp_infos, instr_infos, comp_files, instr_files = parse_and_filter(usedir, recursive=True, printlog=args.verbose)
        write_doc_files_or_continue(comp_infos, instr_infos, comp_files, instr_files, args.verbose)

        masterdoc = OverviewDocWriter(comp_infos, instr_infos, [], [], mccode_config.configuration['MCCODE_LIB_DIR'])
        text = masterdoc.create()

        mcdoc_html_filepath = os.path.join(usedir,mccode_config.get_mccode_prefix()+'doc.html')
        try:
            write_file(mcdoc_html_filepath, text)
            print("master doc file: %s" % mcdoc_html_filepath)
        except Exception as e:
            print('ERROR writing master doc file: %s', e)

    elif args.dir != None or args.searchterm != None:
        ''' filtered and/or local results '''
        flter = '.*'
        usedir = mccode_config.configuration['MCCODE_LIB_DIR']
        rec = True
        if args.searchterm:
            flter = args.searchterm

        # single, specific file
        if args.searchterm is not None and re.search('\.', args.searchterm):
            usedir2 = '.'
            if args.dir is not None:
                usedir2 = args.dir
            f = os.path.join(usedir2, args.searchterm)
            
            # find matcing filenames
            instr_files, comp_files = utils.get_instr_comp_files(mccode_config.configuration['MCCODE_LIB_DIR'], True)
            comp_files = [f for f in comp_files if os.path.basename(f) == args.searchterm]
            instr_files = [f for f in instr_files if os.path.basename(f) == args.searchterm]

            # accumulate results
            results = []
            if os.path.isfile(f):
                results.append(f)
            results.extend(instr_files)
            results.extend(comp_files)
            if len(results) == 1:
                f = results[0]

                instr = re.search('[\w0-9]+\.instr', args.searchterm)
                comp = re.search('[\w0-9]+\.comp', args.searchterm)

                if instr:
                    f_html = os.path.splitext(f)[0] + ".html"
                    info = InstrParser(f).parse()
                    info.filepath = os.path.abspath(f)
                    write_doc_files_or_continue([], [info], [], [f])
                    subprocess.Popen('%s %s' % (mccode_config.configuration['BROWSER'], f_html), shell=True)
                elif comp:
                    f_html = os.path.splitext(f)[0] + ".html"
                    info = CompParser(f).parse()
                    info.filepath = os.path.abspath(f)
                    write_doc_files_or_continue([info], [], [f], [])
                    subprocess.Popen('%s %s' % (mccode_config.configuration['BROWSER'], f_html), shell=True)
                quit()
            # there were multiple matches - fall back to general search term mode
            else:
                flter = os.path.splitext(os.path.basename(args.searchterm))[0]

        # system
        comp_infos, instr_infos, comp_files, instr_files = parse_and_filter(usedir, flter, recursive=True)
        write_doc_files_or_continue(comp_infos, instr_infos, comp_files, instr_files)

        # local
        comp_infos_local = []
        instr_infos_local = []
        if args.dir != None:
            usedir = args.dir
            comp_infos_local, instr_infos_local, comp_files, instr_files = parse_and_filter(args.dir, flter, recursive=False)
            write_doc_files_or_continue(comp_infos_local, instr_infos_local, comp_files, instr_files)

        if len(comp_infos_local) + len(instr_infos_local) + len(comp_infos) + len(instr_infos) == 0:
            print("no matches found")
            quit()

        masterdoc = OverviewDocWriter(comp_infos, instr_infos, comp_infos_local, instr_infos_local, usedir)
        text = masterdoc.create()

        mcdoc_html_filepath = os.path.join('.', mccode_config.get_mccode_prefix()+'doc.html')
        if args.verbose:
            print('writing local overview doc file... %s' % mcdoc_html_filepath)
        write_file(mcdoc_html_filepath, text)

        subprocess.Popen('%s %s' % (mccode_config.configuration['BROWSER'], os.path.join('.',mccode_config.get_mccode_prefix()+'doc.html')), shell=True)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('searchterm', nargs='?', help='search filter or .instr/.comp file')
    parser.add_argument('--install', '-i', action='store_true', help='generate installation master doc page')
    parser.add_argument('--dir', '-d', help='add search results from this directory')
    parser.add_argument('--manual','-m', action='store_true', help='open the system manual')
    parser.add_argument('--comps','-c', action='store_true', help='open the component manual')
    parser.add_argument('--web','-w', action='store_true', help='open the '+mccode_config.configuration['MCCODE']+' website')
    parser.add_argument('--verbose','-b', action='store_true', help='prints a parsing log during execution')
    
    
    args = parser.parse_args()
    
    try:
        main(args)
    except Exception as e:
        print(str(e))

