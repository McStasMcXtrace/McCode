''' 
Utility functions used by mcgui. Should be static.
'''
import os
import re
import subprocess
import time

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
    
    # read program output while the process is active
    while process.poll() == None:
        stdoutdata = process.stdout.readline().rstrip('\n')
        call_if_not_none(stdout_cb, stdoutdata)
        stderrdata = process.stderr.readline().rstrip('\n')
        call_if_not_none(stderr_cb, stderrdata)
        time.sleep(0.05)
    
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

