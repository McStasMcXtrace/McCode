''' 
Utility functions used by mcgui. Should be static.
'''
import os
import re

'''
Static utility functions related to handling mccode files.
'''
def get_instr_site(instr_file):
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

def _get_resultdirs_chron(mydir, prefix):
    subdirs = []
    for fileordir in os.listdir(mydir):
        if os.path.isdir(fileordir):
            if prefix in fileordir:
                subdirs.append(fileordir)
    subdirs.sort(cmp=lambda x,y: _chrono_sort(x,y))
    return subdirs

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

def get_file_contents(filepath):
    ''' returns file contents if file exists '''
    if os.path.exists(str(filepath)):
        f = open(filepath, 'r')
        text = f.read()
        f.close()
        return text
    else:
        return ''
