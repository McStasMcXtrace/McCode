#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Attempts to to parse component- and instrument files in subfolders, generate docpages and 
browse the generated overview page, mccode.html. The first arg, libdir, determines the directory,
which defaults to current installation "mccode lib dir".

The attempt to browse a mcdoc.html can be omited by --nobrowse.
'''
import logging
import argparse
import sys
import os
import re
import subprocess
from datetime import datetime

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
            optics_tab = optics_tab + t % (get_html_filepath(i.filepath), c.name, c.origin, c.author, c.filepath, 'comp', c.short_descr) + '\n'
        # Samples
        samples_lst = [c for c in self.comp_info_lst if c.category=='samples']
        samples_tab = ''
        for c in samples_lst:
            samples_tab = samples_tab + t % (get_html_filepath(i.filepath), c.name, c.origin, c.author, c.filepath, 'comp', c.short_descr) + '\n'
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
        text = text.replace('%TAB_LINES_INSTR_LOCAL%', local_instr_tab)
        text = text.replace('%TAB_LINES_COMPS_LOCAL%', local_comp_tab)
        text = text.replace('%LINK_FILECOLON_DATA%', 'file://%s/data' % self.mccode_libdir)
        text = text.replace('%LINK_FILECOLON_SHARE%', 'file://%s/share' % self.mccode_libdir)
        text = text.replace('%GENDATE%', '{0:%Y-%m-%d %H:%M:%S}'.format(datetime.now()))
        
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

%TAB_LINES_INSTR_LOCAL%

</TABLE>


<P><A NAME="localinstrs"></A>
<B><FONT COLOR="#FF0000">Local instruments</FONT></B>
<TABLE BORDER COLS=5 WIDTH="100%" NOSAVE>

%TAB_HEAD%

%TAB_LINES_COMPS_LOCAL%

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
        self.info = utils.InstrCompHeaderInfo()
        self.has_parsed = True
        return self.info
    
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
        ''' parses the given file '''
        f = open(self.filename)
        logging.debug('parsing file "%s"' % self.filename)
        
        header = utils.read_header(f)
        info = utils.parse_header(header)
        info.site = utils.get_instr_site_fromtxt(header)
        dfine = utils.read_define_instr(f)
        info.name, info.params = utils.parse_define_instr(dfine)
        
        self.info = info


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
            lst = [pd[2] for pd in i.params_docs if p[1] == pd[0]]
            doc = lst[0] if len(lst) > 0 else ''
            if p[0] == None:
                p = ("", p[1], p[2])
            doc_rows = doc_rows + '\n' + self.par_str % (p[0], p[1], doc, p[2])
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
    par_header = par_str % ('Unit', 'Name', 'Description', 'Default')
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
    def _parse_legacy(self):
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
            lst = [pd[2] for pd in i.params_docs if p[1] == pd[0]] # TODO: rewrite to speed up 
            doc = lst[0] if len(lst) > 0 else ''
            doc_rows_in = doc_rows_in + '\n' + self.par_str % (p[0], p[1], doc, p[2])
        h = h.replace(t[9], doc_rows_in)
        
        doc_rows_out = ''
        for p in i.outparams:
            lst = [pd[2] for pd in i.params_docs if p[1] == pd[0]] # TODO: rewrite to speed up 
            doc = lst[0] if len(lst) > 0 else ''
            doc_rows_out = doc_rows_out + '\n' + self.par_str % (p[0], p[1], doc, p[2])
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

<H1>The <CODE>%COMPNAME%</CODE> Instrument</H1>

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


def write_file(filename, text):
    f = open(filename, 'w')
    f.write(text)
    f.close()


def main(args):
    '''
    Behavior: See file header.
    '''
    logging.basicConfig(level=logging.INFO)

    usedir = args.crawl or mccode_config.configuration["MCCODE_LIB_DIR"]
    print("using source directory: " + usedir)

    '''
    # repair mode - do not run mcdoc, just the "repair" function
    if args.repair:
        #repair_instr(localdir)
        repair_comp(usedir)
        quit()
    '''

    # local files
    instr_files, comp_files = utils.get_instr_comp_files(usedir)
    instr_files_local, comp_files_local = utils.get_instr_comp_files('.')

    # parse comp files
    comp_info_lst = []
    for f in comp_files:
        try:
            print("parsing... %s" % f)
            info = CompParser(f).parse()
            info.filepath = os.path.abspath(f)
            comp_info_lst.append(info)
        except:
            print("failed parsing file: %s" % f)
            comp_info_lst.append(CompParser(f).stub())
    print("parsed comp files: %s" % str(len(comp_files)))
    
    # parse instr files
    instr_info_lst = []
    for f in instr_files:
        try:
            print("parsing... %s" % f)
            info = InstrParser(f).parse()
            info.filepath = os.path.abspath(f)
            instr_info_lst.append(info)
        except:
            print("failed parsing file: %s" % f)
            instr_info_lst.append(InstrParser(f).stub())
    print("parsed instr files: %s" % str(len(instr_files)))

    # parse comp files
    comp_info_local_lst = []
    for f in comp_files_local:
        try:
            print("parsing... %s" % f)
            info = CompParser(f).parse()
            info.filepath = os.path.abspath(f)
            comp_info_local_lst.append(info)
        except:
            print("failed parsing file: %s" % f)
            comp_info_local_lst.append(CompParser(f).stub())
    print("parsed comp files: %s" % str(len(comp_files_local)))
    
    # parse instr files
    instr_info_local_lst = []
    for f in instr_files_local:
        try:
            print("parsing... %s" % f)
            info = InstrParser(f).parse()
            info.filepath = os.path.abspath(f)
            instr_info_local_lst.append(info)
        except:
            print("failed parsing file: %s" % f)
            instr_info_local_lst.append(InstrParser(f).stub())
    print("parsed instr files: %s" % str(len(instr_files_local)))
    
    # apply a name-filter (matches instr / comp name, not filename)
    if args.namefilter:
        comp_info_lst = [c for c in comp_info_lst if re.search(args.namefilter.lower(), c.name.lower())]
        instr_info_lst = [c for c in instr_info_lst if re.search(args.namefilter.lower(), c.name.lower())]
        comp_info_local_lst = [c for c in comp_info_local_lst if re.search(args.namefilter.lower(), c.name.lower())]
        instr_info_local_lst = [c for c in instr_info_local_lst if re.search(args.namefilter.lower(), c.name.lower())]
    
    '''
    # debug mode - write files with a header property each, then quit
    if args.debug:
        text = '\n'.join(['%4d: \n%s' % (i, files[i]) for i in range(len(files))])
        write_file('files', text)
        
        for i in range(utils.InstrCompHeaderInfo.__len__()-3):
            text = '\n'.join(['%4d: %s' % (j, instr_info_lst[j][i]) for j in range(len(instr_info_lst))])
            write_file(utils.InstrCompHeaderInfo.colname(i), text)
        for i in range(8, 10):
            text = '\n'.join(['%4d: \n%s' % (j, '\n'.join(['%-20s, %-10s, %s' % (str(k[0]), str(k[1]), str(k[2])) for k in instr_info_lst[j][i]])) for j in range(len(instr_info_lst))])
            write_file(utils.InstrCompHeaderInfo.colname(i), text)
        
        text = '\n'.join(['%4d: \n%s' % (j, '\n'.join(instr_info_lst[j][10])) for j in range(len(instr_info_lst))])
        #  '\n'.join(info.links)
        write_file(utils.InstrCompHeaderInfo.colname(10), text)
        quit()
    '''
    
    mcdoc_html = os.path.join(usedir,'mcdoc.html')
    # try-catch section for file write
    try:
        print("writing lib files...")
        # generate and save comp html doc pages
        for i in range(len(comp_info_lst)):
            p = comp_info_lst[i]
            f = comp_files[i]
            doc = CompDocWriter(p)
            text = doc.create()
            h = os.path.splitext(f)[0] + '.html'
            print("writing doc file... %s" % h)
            write_file(h, text)

        # generate and save instr html doc pages
        for i in range(len(instr_info_lst)):
            p = instr_info_lst[i]
            f = instr_files[i]
            doc = InstrDocWriter(p)
            text = doc.create()
            h = os.path.splitext(f)[0] + '.html'
            print("writing doc file... %s" % h)
            write_file(h, text)
        
        print("writing local files...")
        # generate and save local comp html doc pages
        for i in range(len(comp_info_local_lst)):
            p = comp_info_local_lst[i]
            f = comp_files_local[i]
            doc = CompDocWriter(p)
            text = doc.create()
            h = os.path.splitext(f)[0] + '.html'
            print("writing doc file... %s" % h)
            write_file(h, text)

        # generate and save local instr html doc pages
        for i in range(len(instr_info_local_lst)):
            p = instr_info_local_lst[i]
            f = instr_files_local[i]
            doc = InstrDocWriter(p)
            text = doc.create()
            h = os.path.splitext(f)[0] + '.html'
            print("writing doc file... %s" % h)
            write_file(h, text)
        
        # write overview files, properly assembling links to instr- and html-files
        masterdoc = OverviewDocWriter(comp_info_lst, instr_info_lst, comp_info_local_lst, instr_info_local_lst, mccode_config.configuration['MCCODE_LIB_DIR'])
        text = masterdoc.create()
        print('writing master doc file... %s' % os.path.abspath('mcdoc.html'))
        write_file(mcdoc_html, text)
    
    except Exception as e:
        print("Could not write to disk: %s" % e.__str__())
    
    # open a web-browser in a cross-platform way, unless --nobrowse has been enabled
    if not args.nobrowse:
        subprocess.Popen('%s %s' % (mccode_config.configuration['BROWSER'], mcdoc_html), shell=True)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('namefilter', nargs='?', help='Filter results by component/instrument definition name (not file-name or header-name). Ignores case.')
    parser.add_argument('--crawl', help='Crawl from rootdir to write docpages to disk, defaults to current installation mccode lib dir.')
    parser.add_argument('--nobrowse', action='store_true', help='Disable opening a webbrowser viewer.')
    #parser.add_argument('--debug', action='store_true', help='enable debug mode')
    #parser.add_argument('--repair', action='store_true', help='enable repair mode')
    
    args = parser.parse_args()
    
    main(args)

