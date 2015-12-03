''' 
Utility functions used by mcgui. Should be static.

@author: jaga
'''
import os
import re
import imp


''' Static functions related to handling mcstas files and more.
'''
class McGuiUtils(object):
    @staticmethod
    def getInstrumentSite(instr_file):
        ''' extracts and returns the rest of the line, from the text file instr_file, containing "%INSTRUMENT_SITE:" '''
        f = open(instr_file, 'r')
        text = f.read()
        f.close()
        
        site = '("%INSTRUMENT_SITE:" tag not found)'
        
        start = text.find('%INSTRUMENT_SITE:')
        if start > -1:
            end = text.find('\n', start)
            site = text[start+17:end].strip()
            
        return site
    
    @staticmethod
    def getInstrumentAndComponentFiles(mydir):
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
    
    @staticmethod
    def __getResultSubdirsChronologically(mydir, prefix):
        subdirs = []
        for fileordir in os.listdir(mydir):
            if os.path.isdir(fileordir):
                if prefix in fileordir:
                    subdirs.append(fileordir)
        subdirs.sort(cmp=lambda x,y: McGuiUtils.__chronoSort(x,y))
        return subdirs
    
    @staticmethod
    def __chronoSort(word1, word2):
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
    
    @staticmethod
    def saveInstrumentFile(instr, text):
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
    
    @staticmethod
    def getFileContents(filepath):
        ''' returns file contents if file exists '''
        if os.path.exists(str(filepath)):
            f = open(filepath, 'r')
            text = f.read()
            f.close()
            return text
        else:
            return ''
        
    @staticmethod
    def getMcCodeConfigOptions(MCCODE):
        if MCCODE == "mcstas":
            prefix = "mc"
        else:
            prefix = "mx"
        mcrun_lst =     [prefix+"run", prefix+"run-py"]
        mcplot_lst =    [prefix+"plot", prefix+"plot --format=Gnuplot", prefix+"plot --format=Matlab",
                         prefix+"plot-matlab", prefix+"plot-matplotlib-py", prefix+"plot-gnuplot-py", prefix+"plot-chaco-py"]
        mcdisplay_lst = [prefix+"display", prefix+"display --format=Matlab", prefix+"display --format=VRML", 
                         prefix+"display --format=Mantid", prefix+"display-matplotlib-py", prefix+"display-py", 
                         prefix+"display-r-py", prefix+"display-vtk-py"]
        return mcrun_lst, mcplot_lst, mcdisplay_lst

    @staticmethod
    def loadUserConfig(MCCODE,MCCODE_VERSION):
        userconfig=os.path.expandvars("$HOME/."+MCCODE+"/"+MCCODE_VERSION+"/mccode_config.py")
        if os.path.isfile(userconfig):
            print "Loading user configuration from "+userconfig
            imp.load_source('mccode_config', userconfig)
        
    @staticmethod
    def saveUserConfig(config_module,MCCODE,MCCODE_VERSION):
        # overrides previous config by creating a mccode_config.py file in the $HOME/.MCCODE folder
        conf_text_lines = [
            '# ',
            '\n' + '# mcstas/mcxtrace configuration.',
            '\n' + '# ',
            '\n' + 'configuration = {',
            McGuiUtils.__getIndentedDictLines(config_module.configuration),
            '\n' + '}',
            '\n',
            '\n' + '# ',
            '\n' + '# Compilation, parallelisation etc.',
            '\n' + '# ',
            '\n' + 'compilation = {',
            McGuiUtils.__getIndentedDictLines(config_module.compilation),
            '\n' + '}',
            '\n',
            '\n' + '# ',
            '\n' + '# Compilation, parallelisation etc.',
            '\n' + '# ',
            '\n' + 'platform = {',
            McGuiUtils.__getIndentedDictLines(config_module.platform),
            '\n' + '}',
            '\n']
        
        conf_text = ''.join(conf_text_lines)
        
        if (os.path.isdir(os.path.expandvars("$HOME/."+MCCODE+"/")) == False):
            # We use os.makedirs here because of missing os.path.mkdir on OS X... :-(
            os.makedirs(os.path.expandvars("$HOME/."+MCCODE+"/"))
            
        if (os.path.isdir(os.path.expandvars("$HOME/."+MCCODE+"/"+MCCODE_VERSION)) == False):
            # We use os.makedirs here because of missing os.path.mkdir on OS X... :-(
            os.makedirs(os.path.expandvars("$HOME/."+MCCODE+"/"+MCCODE_VERSION))
            
        f = open(os.path.expandvars("$HOME/."+MCCODE+"/"+MCCODE_VERSION+"/mccode_config.py"), 'w')
        f.write(conf_text)
        f.close()
        
    @staticmethod
    def __getIndentedDictLines(config_dict):
        lines = ''
        for name in config_dict:
            lines += '\n' + '    \"' + name + '\"' + ': \"' + config_dict[name] + '\",'
        return lines
