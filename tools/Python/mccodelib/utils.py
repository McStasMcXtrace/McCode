'''
Analysis tools for mcstas component files and instrument files.
'''
import re
import os
import subprocess


''' Component parser
'''
class McComponentParser(object):
    file = '' # component file path
    name = '' # component name
    info = '' # info section all in one string  (multiple lines)
    description = '' # component doc description (multiple lines)
    pars = [] # list of McComponentParInfo
    
    display = None # optional MCDISPLAY section
    
    class McComponentParInfo(object):
        ''' component parameter info object - appended to the list member McComponentParser.pars '''
        def __init__(self, par_info=None):
            if par_info:
                self.par_name = par_info.par_name
                self.type = par_info.type
                self.default_value = par_info.default_value
                self.doc_and_unit = par_info.doc_and_unit
        par_name = '' # parameter par_name
        type = '' # mostly empty, can also be "string" or "int" 
        default_value = ''
        doc_and_unit = '' # doc string and unit (single line)
    
    def __init__(self, comp_file):
        if comp_file == '':
            raise Exception('McComponentParser: "comp_file" cannot be an empty string.')
        
        self.file = comp_file
        self.__hasParsed = False
    
    def parse(self):
        ''' main parse component for info function '''
        
        if self.__hasParsed:
            return
        
        # load component from file
        text = open(self.file).read()
        if text == '':
            raise Exception('parse: component file is empty.')
        
        # get component info, description and par docs from header section
        self.info, self.description, header_P_section = self.__parseComponentHeader(text)
        
        # get component par_name from main section
        self.name = self.__parseComponentName(text)
        
        # get component pars from main section and match with par docs from header section
        self.pars = self.__parseComponentPars(text)
        self.__matchDocStringsToPars(self.pars, header_P_section)

        self.__hasParsed = True
    
    def parseDisplaySection(self):
        ''' optional: call to parse MCDISPLAY section and save in member "mcdisplay" '''
        
        # load component from file
        text = open(self.file).read()
        if text == '':
            raise Exception('parse: component file is empty.')
        
        pat = 'MCDISPLAY\s*\%\{([-+.\w\s=<>,/*{}\"\'()\.&$=?;:*|]*)\%\}'
        sr = re.search(pat, text)
        if sr:
            self.mcdisplay = sr.group()
        else:
            self.mcdisplay = 'missing MCDISPLAY string'
    
    @staticmethod
    def __parseComponentHeader(text):
        # identify the start and end positions of the I, D and P sections
        pos_I = text.find('%I')
        pos_D = text.find('%D')
        pos_P = text.find('%P')
        pos_E = text.find('%E')
        
        # validate match for %I, %D, %P and %E positions
        if pos_I == -1:
            raise Exception('parseComponentHeader: Missing %I tag.')
        if pos_D == -1:
            raise Exception('parseComponentHeader: Missing %D tag.')
        if pos_P == -1:
            raise Exception('parseComponentHeader: Missing %P tag.')
        if pos_E == -1:
            raise Exception('parseComponentHeader: Missing %E tag.')
        
        # extract strings for I, D and P sections
        text_I = McComponentParser.__removeStarsFromLines(text[pos_I+2: pos_D])
        text_D = McComponentParser.__removeStarsFromLines(text[pos_D+2: pos_P])
        text_P = McComponentParser.__removeStarsFromLines(text[pos_P+2: pos_E])
        
        return text_I, text_D, text_P
        
    @staticmethod
    def __parseComponentName(text):
        re_out = re.search('DEFINE\s+COMPONENT\s+(\w*)', text)
        if re_out:
            return re_out.group(1)
    
    @staticmethod
    def __parseComponentPars(text):
        ''' parse definition parameters and setting parameters from the main component section
        '''
        # get definition parameters
        result_1 = []
        re_out = re.search(r'DEFINITION\s+PARAMETERS\s*\([-+.\w\s=,/*{}\"]+\)', text)
        if re_out:
            result_sub, substituted_text = McComponentParser.__substituteCurlyPars(re_out.group(0))
            result_1 = McComponentParser.__parseParLine(substituted_text)
            # restitute curly pars default values
            for r in result_1:
                for s in result_sub:
                    if r.par_name == s.par_name:
                        r.default_value = s.default_value
        
        # get setting parameters
        result_2 = []
        re_out = re.search(r'SETTING\s+PARAMETERS\s*\([-+.\w\s=,/*\"]+\)', text)
        if re_out:
            result_2 = McComponentParser.__parseParLine(re_out.group(0))
        
        return result_1 + result_2
    
    @staticmethod
    def __substituteCurlyPars(text):
        # Extract stuff like def_par={1.0, 2.0, 3.0} which contains commas inside a curly bracket.
        # Returns curly bracket (name, default value), and text with such values replaced by placeholder string
        
        curly = re.findall(r'(\w+)=(\{[-+.\w\s,/*\"]*\})', text)
        result = []
        substituted_text = str(text)
        
        # get par_info into the par info par_info structure: 
        for p in curly:
            par_info = McComponentParser.McComponentParInfo()
            par_info.par_name = p[0]
            par_info.default_value = p[1]
            result.append(par_info)
            
            # reduce
            substituted_text = substituted_text.replace(par_info.par_name + '=' + par_info.default_value, par_info.par_name + '=' + par_info.par_name + '_substituted')
        
        return result, substituted_text
    
    @staticmethod 
    def __parseParLine(text):
        # assuming no curly brackets - these must have been processed and removed from text
        # always return something 
        result = []
        allowed = r'[-+.\w\s=,/*{}\"]'
        
        # get the text in parenthesis
        out1 = re.search(r'\(([-+.\w\s=,/*{}\"]+)\)', text)
        if not out1:
            return result
            # TODO: error handling
        
        # get comma separated sections
        comma_sep = re.findall(r'([-+.\w\s=/*\"]+),*', out1.group(1))
        
        # get par_info into the par info par_info structure: 
        for p in comma_sep:
            par_info = McComponentParser.McComponentParInfo()
            
            out = re.search(r'(\w+)\s+([\w\-]+)\s*=\s*([-+.\w/*\"]+)', p)
            if out:
                # type par_name = def_val
                par_info.type = out.group(1)
                par_info.par_name = out.group(2)
                par_info.default_value = out.group(3)
            else:
                # par_name = def_val
                out = re.search(r'([\w\-]+)\s*=\s*([-+.\w/*\"]+)', p)
                if out:
                    par_info.par_name = out.group(1)
                    par_info.default_value = out.group(2)
                else:
                    # par_name
                    par_info.par_name = p.strip()
            
            result.append(par_info)
        
        return result
    
    @staticmethod
    def __matchDocStringsToPars(pars, header_P_section):
        # sets docstring values on objects in the "pars" list of McComponentParInfo 
        parnames = []
        for i in range(len(pars)):
            parnames.append(pars[i].par_name)
        
        lines = header_P_section.splitlines()
        lastpar = None
        for i in range(len(lines)):
            line = lines[i]
            
            # check for colon
            out = re.search(r'(.*):(.*)', line)
            if out:
                # assume that stars in lines have already been stripped
                candidate = out.group(1).strip()
                if candidate in parnames:
                    # candidate is a known par name
                    j = parnames.index(candidate)
                    pars[j].doc_and_unit += out.group(2).strip() + ' '
                    lastpar = pars[j]
                elif lastpar:
                    lastpar.doc_and_unit += out.group(2).strip()
            
            elif lastpar and McComponentParser.__stringIsEmptyLine(line):
                # empty line, close lastpar appending
                lastpar = None
            elif lastpar:
                # continuation line, append to lastpar
                lastpar.doc_and_unit += line.strip()
    
    @staticmethod
    def __stringIsEmptyLine(s):
        out = re.match(r'[\s\t\r\n]*', s)

        empty = False
        if out:
                empty = out.group() == s
        return empty
    
    @staticmethod
    def __removeStarsFromLines(text):
        result = ''        
        text_lns = list(iter(text.splitlines()))
        for l in text_lns:
            l = l.lstrip('*')
            l = l.strip()
            result += '\n'
            result += l
        return result.strip('\n')
    
    @staticmethod
    def __parseInstrumentDef(text):
        re_out = re.search('DEFINE\s+INSTRUMENT\s+([a-zA-Z0-9_]*)\s*\(([-+.a-zA-Z0-9_ \t\n\r=,/*{}\"]*)\)', text)
        if re_out:
            return re_out.group(1)


'''
Static utility functions related to handling mccode files.
'''
def get_instr_site(instr_file):
    ''' extracts and returns the rest of the line, from the text file instr_file, containing "%INSTRUMENT_SITE:" '''
    f = open(instr_file, 'rb')
    text = f.read().decode()
    f.close()
    
    site = '("%INSTRUMENT_SITE:" tag not found)'
    
    start = text.find('%INSTRUMENT_SITE:')
    if start > -1:
        end = text.find('\n', start)
        site = text[start+17:end].strip()
        
    return site

def get_instr_comp_files(mydir):
    ''' returns list of filename with path of all .instr and .comp recursively from dir "mydir" ''' 
    files_instr = [] 
    files_comp = []
    
    for (dirpath, dirname, files) in os.walk(mydir):
        for f in files:
            if os.path.splitext(f)[1] == '.instr':
                files_instr.append(dirpath + '/' + f)
            if os.path.splitext(f)[1] == '.comp':
                files_comp.append(dirpath + '/' + f)
    
    return files_instr, files_comp


def save_instrfile(instr, text):
    ''' 
    Creates and/or saves instrument file, makes sure the file extension is .instr.
    Returns file par_name, or empty string if no file was saved/created.
    '''
    if instr == '':
        return ''
    
    if str(instr).find('.') >= 0:
        if os.path.splitext(str(instr))[1] != '.instr':
            instr = instr + '.instr'
    else:
        instr = instr + '.instr'
    
    # TODO: add try-finally and error handling
    f = open(str(instr), 'w')
    f.write(text)
    f.close()
    
    return instr

def get_file_text_direct(file):
    ''' Opens a file for reading binary, reads it, decodes the results and returns the text. Fixes an occational issue on MacOSX. '''
    f = open(file, 'rb')
    text = f.read().decode()
    f.close()
    return text

def get_file_contents(filepath):
    ''' returns file contents if file exists '''
    if os.path.exists(str(filepath)):
        return get_file_text_direct(filepath)
    else:
        return ''


def run_subtool_to_completion(cmd, cwd=None, stdout_cb=None, stderr_cb=None):
    '''
    Synchronous run subprocess.popen with pipe buffer reads, 
    one-string command 'cmd' and running in directory 'cwd'.
    '''
    def call_if_not_none(fct, *args):
        ''' shorthand utility for calling a function if it is defined, and otherwise ignoring it '''
        if fct:
            fct(*args)
    if not cwd:
        cwd = os.getcwd()
    
    # open the process with all bells & whistles
    process = subprocess.Popen(cmd, 
                               stdout=subprocess.PIPE, 
                               stderr=subprocess.PIPE,
                               stdin=subprocess.PIPE,
                               shell=True,
                               universal_newlines=True,
                               cwd=cwd)
    # flush until EOF
    for stdoutdata in process.stdout:
        call_if_not_none(stdout_cb, stdoutdata.rstrip('\n'))
    for stderrdata in process.stderr:
        call_if_not_none(stderr_cb, stderrdata.rstrip('\n'))
    
    return process.returncode


def start_subtool_then_return(cmd, cwd=None):
    '''
    Asynchronous run subprocess.popen, using
    one-string command 'cmd' in directory 'cwd'.
    '''
    def call_if_not_none(fct, *args):
        ''' shorthand utility for calling a function if it is defined, and otherwise ignoring it '''
        if fct:
            fct(*args)
    if not cwd:
        cwd = os.getcwd()
    
    # open the process with all bells & whistles
    process = subprocess.Popen(cmd,
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE,
                               stdin=subprocess.PIPE,
                               shell=True,
                               universal_newlines=True,
                               cwd=cwd)
    
    return process.returncode


''' Unused code?

def _get_resultdirs_chron(mydir, prefix):
    def _chrono_sort(word1, word2):
        result1 = re.search('.*_([0-9]+)_([0-9]+)', word1)
        result2 = re.search('.*_([0-9]+)_([0-9]+)', word2)
        date1 = int(result1.group(1))
        date2 = int(result2.group(1))
        time1 = int(result1.group(2))
        time2 = int(result2.group(2))
        if date1 < date2:
            return 1
        elif date1 > date2:
            return -1
        if date1 == date2:
            if time1 < time2:
                return 1
            elif time1 > time2:
                return -1
            else:
                return 0
    
    subdirs = []
    for fileordir in os.listdir(mydir):
        if os.path.isdir(fileordir):
            if prefix in fileordir:
                subdirs.append(fileordir)
    subdirs.sort(cmp=lambda x,y: _chrono_sort(x,y))
    return subdirs

'''

def dumpfile_pqtg(scene, filenamebase='mcplot', format='png'):
    ''' save as png file. Pdf is not supported, althouhg svg kind-of is '''
    import pyqtgraph.exporters
    import pyqtgraph as pg

    outputfile = '%s.%s' % (filenamebase, format)
    # Check for existance of earlier exports
    if os.path.isfile(outputfile):
        index=1
        outputfile = '%s_%i.%s' % (filenamebase, index, format )
        while os.path.isfile(outputfile):
            index += 1
            outputfile = '%s_%i.%s' % (filenamebase, index, format)
            
    if format=='png':
        exporter = pg.exporters.ImageExporter(scene)
        exporter.export(outputfile)
    elif format=='svg':
        exporter = pg.exporters.SVGExporter(scene)
        exporter.export(outputfile)
        # This is where PDF export would be handled via conversion from SVG
        #    elif format=='pdf':
        #        exporter = pg.exporters.SVGExporter(scene)
        #        exporter.export(outputfile)
        #        import subprocess
        #        # TODO: error handling
        #        process = subprocess.Popen('svg2pdf %s %s.pdf' % (outputfile, '%s_%i.%s' % (filenamebase, index, 'pdf')), 
        #                                   stdout=subprocess.PIPE, 
        #                                   stderr=subprocess.PIPE,
        #                                   shell=True,
        #                                   universal_newlines=True)
    else:
        raise Exception('png, and svg are the only supported file formats (format=%s)' % format)
    if os.path.isfile(outputfile):
        print('Graphics output ' + outputfile + ' saved')

