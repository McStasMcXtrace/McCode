'''
Analysis tools for mcstas component files and instrument files.

@author: jaga
'''
import re


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
