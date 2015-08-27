#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
This script outputs a modified mcstas component file, containing links and calls to specified sasview .c model files. 
The output filename is the same as the input filename, postfixed by "_new".
The input component file must contain appropriate AUTOGEN flags, which are used to clear and insert the generated code.
'''
import os
import logging
import re
import argparse

class SasViewModelFileInfo():
    '''
    Takes a sas_<modelname>.c file and parses include, function call and doc hint (non-q model parameter names). 
    Construct and then use the objects string properties to construct the desired C code or docs.
    '''
    def __init__(self, model_file_fulpath, pars_array_name):
        # check input
        # TODO: implement: throw exception on fail (user should use try-catch)
        
        # generate static info
        self.input_str = model_file_fulpath
        self.text = open(model_file_fulpath).read()
        self.pars_array_name = pars_array_name
        self.model_name = re.search(r'sas_(.*).c', os.path.basename(model_file_fulpath)).group(1)
        
        self.percent_include = '%%include %s;' % os.path.basename(model_file_fulpath)
        self.hash_include = '#include %s;' % os.path.basename(model_file_fulpath)
        
        self.sign_non_q = self.__getSignNonQ(self.text, False)
        self.sign_xy_non_q = self.__getSignNonQ(self.text, True)
        
        self.__num_model_pars = self.__getNumPars(self.sign_non_q)
        self.__num_model_pars_xy = self.__getNumPars(self.sign_xy_non_q)
        
        self.Iq_hint = self.__getHint(self.sign_non_q)
        self.Iq_xy_hint = self.__getHint(self.sign_xy_non_q)
        
        self.Iq_call = self.__getIqCall(pars_array_name, self.__num_model_pars, xy=False)
        self.Iqxy_call = self.__getIqCall(pars_array_name, self.__num_model_pars_xy, xy=True)
    
    def printMe(self):
        text = 'input: %s\ntext: %s\npars_name: %s\nmodel_name: %s\n%s\n%s\nsign_non_q: %s\nsign_xy_non_q: %s\nIq_hint: %s\nIq_xy_hint: %s\nIq_call: %s\nIqxy_call: %s\n' % (
                                            self.input_str, '(not shown)', self.pars_array_name, 
                                            self.model_name, self.percent_include, self.hash_include,
                                            self.sign_non_q, self.sign_xy_non_q,
                                            self.Iq_hint, self.Iq_xy_hint, self.Iq_call, self.Iqxy_call)
        return '#####\n' + text + '#####\n'
    
    @staticmethod
    def __getNumPars(sign):
        c = 1
        for comma in re.finditer(',', sign):
            c += 1
        return c
    
    @staticmethod
    def __getSignNonQ(text, xy=False):
        # get all cases covered
        define_str = r'#define\s+IQ_PARAMETER_DECLARATIONS\s+([\w\s,]*\n)'
        if xy:
            define_str = r'#define\s+IQXY_PARAMETER_DECLARATIONS\s+([\w\s,]*)\n'
        
        sign_str = r'float\s+Iq\(\s*float\s+q\s*,([\w\s,]*)\)'
        sign_str_2 = r'float\s+Iq\(\s*float\s+qval\s*,([\w\s,]*)\)'
        sign_str_3 = r'float\s+Iq\(\s*float\s+QQ\s*,([\w\s,]*)\)'
        if xy:
            sign_str = r'float\s+Iqxy\(\s*float\s+qx\s*,\s*float\s+qy\s*,([\w\s,]*)\)'
            # LOOK UP IN FILES:
            sign_str_2 = r'float\s+Iqxy\(\s*float\s+qx\s*,\s*float\s+qy\s*,([\w\s,]*)\)'
            sign_str_3 = r'float\s+Iqxy\(\s*float\s+qx\s*,\s*float\s+qy\s*,([\w\s,]*)\)'
        
        # logics to extract signature
        m = re.search(define_str, text)
        if m:
            # entire non-q sign is contained in the define
            sign = re.sub('\s+', ' ', m.group(1))
            sign = sign.strip(' ')
            return sign
        else:
            # entire sign is contained in the function declaration
            m = re.search(sign_str, text)
            if not m:
                m = re.search(sign_str_2, text)
            if not m:
                m = re.search(sign_str_3, text)
            if m: 
                sign = re.sub('\s+', ' ', m.group(1))
                sign = sign.strip(' ')
                return sign
            else:
                raise Exception("Iq(...) function signature not found")

    @staticmethod
    def __getHint(sign):
        sign = re.sub('float', '', sign)
        sign = re.sub('\s+', ' ', sign)
        return '(' + sign.strip(' ') + ')'

    @staticmethod
    def __getIqCall(pars_array_name, numpars, xy=False):
        # Iq(...) function begins with "float q" and Iqxy begins with "float qx, float qy"
        sign = 'q'
        if xy:
            sign = 'qx, qy'
        for i in range(numpars):
            sign += ', %s[%i]' % (pars_array_name, i)
        if xy:
            return 'Iqxy(%s);' % sign
        return 'Iq(%s);' % sign

def getFiles(look_dir, extension):
    file_list = []
    for (dirpath, dirnames, filenames) in os.walk(look_dir):
        for f in filenames:
            if os.path.splitext(f)[1] == '.' + extension:
                if re.match(r'sas_', os.path.basename(f)):
                    file_list.append(os.path.abspath(dirpath + '/' + f))
        break
    return sorted(file_list, key=lambda item: (int(item.lower().partition(' ')[0])
                                               if item.lower()[0].isdigit() else float('inf'), item.lower()))

def get_include_section(c_files, model_index_par_name):
    # null value (i == 0) is not actualized
    text = ''
    i = 1
    for f in c_files:
        text += '  #if %s == %d\n' % (model_index_par_name, i)
        text += '    %%include "%s"\n' % os.path.basename(f)
        text += '  #endif\n'
        i += 1
    return text

def get_Iq_sign(c_file, array_call_name = None): 
    # TODO: fix for  Iq(...) determined by #define IQ_PARAMETER_DECLARATIONS
    text = open(c_file).read() 
    m = re.search(r'float\s+Iq\(([\w\s,]*)\)', text)
    i = 0
    if m: 
        commas = re.finditer(',', m.group(1))  
        for c in commas: 
            i += 1 # i becomes the number of non-q parameters in Iq(...) (the first parameter is always q) 
    else: 
        logging.exception('something is wrong: m result not returned by file >>> %s' % c_file) 
    # Iq(...) function signature always starts with a "float q"
    sign = 'q'
    if array_call_name != None:
        for j in range(i):
            sign += ', %s[%i]' % (array_call_name, j)
    else:
        sign = m.group(1)
    
    return sign

def get_call_section(c_files, model_index_par_name, model_pars_name, return_par_name):
    # null value:
    text = ''
    text += '    float %s = 1;\n' % return_par_name
    
    i = 1
    for f in c_files:
        text += '    #if %s == %d\n' % (model_index_par_name, i) 
        text += '      %s = Iq(%s);\n' % (return_par_name, get_Iq_sign(f, model_pars_name)) 
        text += '    #endif\n' 
        i += 1 
        
    return text

def mod_comp_file(comp_file, docs_section, include_section, call_section):
    text = open(comp_file).read()
    
    pos_D = text.find("MDOC")
    pos_end_D = text.find('MDOC_END')
    pos_A = text.find("AUTOGEN_A")
    pos_end_A = text.find("AUTOGEN_END_A")
    pos_B = text.find("AUTOGEN_B")
    pos_end_B = text.find("AUTOGEN_END_B")
    
    if pos_D == -1 or pos_end_D == -1 or pos_A == -1 or pos_end_A == -1 or pos_B == -1 or pos_end_B == -1:
        logging.exception('mod_comp_file: AUTOGEN flag error.')
    
    ret_text = text[:pos_D+4] + '\n' + docs_section + text[pos_end_D-2:pos_A+9] + '\n' + include_section + '  // ' 
    ret_text += text[pos_end_A:pos_B+9] + '\n' + call_section + '    // ' + text[pos_end_B:]
    return ret_text

def get_define_Iq_sign(c_file):
    text = open(c_file).read() 
    m = re.search('#define IQ_PARAMETER_DECLARATIONS ([\w\s,]*)\#', text)
    if m:
        sign = re.sub('\s+', ' ', m.group(1))
        sign = sign.strip(' ')
        return m.group(1)
    return 'define resolved string'

def get_docs_section(c_files, left_padding = 2, log_num_models = 2):
    pad_format_str = '{:<' + str(left_padding) + '}' # e.g. '{:<16}'
    int_format_str = '{:>' + str(log_num_models) + '}' # e.g. '{:>2}'
    
    max_name_len = 0
    c_files_neat = []
    for f in c_files:
        f_neat = re.search(r'sas_(.*).c', os.path.basename(f)).group(1)
        c_files_neat.append(f_neat)
        if len(f_neat) > max_name_len:
            max_name_len = len(f_neat)
    name_format_str = '{:<' + str(max_name_len) + '}' # e.g. '{:<35}'
    
    text = pad_format_str.format('*')
    text += int_format_str.format(str(0)) + ' - None \n'
    
    i = 0
    for f in c_files:
        text += pad_format_str.format('*')
        name = c_files_neat[i]
        i += 1
        
        sign = get_Iq_sign(f)
        if re.search(r'IQ_PARAMETER_DECLARATIONS', sign):
            sign = sign.replace('IQ_PARAMETER_DECLARATIONS', get_define_Iq_sign(f))
        
        m = re.search(r'float\s+q\s*,([\w\s,]*)', sign)
        if not m: 
            m = re.search(r'float\s+qval\s*,([\w\s,]*)', sign)
        if not m: 
            m = re.search(r'float\s+QQ\s*,([\w\s,]*)', sign)
        if m: 
            sign = m.group(1).replace('float', '')
            sign = re.sub('\s+', ' ', sign)
            sign = sign.strip(' ')
        else:
            logging.exception(f + ': neither q, qval or QQ as first arg')
        
        text += int_format_str.format(str(i)) + ' - ' + name_format_str.format(name) + ' (' + sign + ') \n'
    
    return text + '* \n'

def test(args):
    logging.basicConfig(level=logging.DEBUG)
    
    logging.info('input comp file: %s', args.compfile[0])
    logging.info('model source dir: %s', args.cdir[0])

    # get model file info
    info_lst = []
    for f in getFiles(args.cdir[0].rstrip('/'), "c") : 
        logging.info('integrating: %s', f)
        info_lst.append(SasViewModelFileInfo(f, 'pars'))
        
    # print debug info if enabled    
    if logging.DEBUG:
        text = ''
        for info in info_lst:
            text = text + info.printMe()
            print(info.printMe())
        debug_file = os.path.splitext(os.path.basename(args.compfile[0]))[0] + '_modelinfo.txt'
        logging.info('output comp file: %s' % debug_file)
        f = open(debug_file, 'w')
        f.write(text)
        f.close()
        exit()
    
    # assemble proxy file
    # 3 functions needed: 1) generate docs, 2) generate includes, 3) generate call
    #model_pars_name = 'pars'
    #return_par_name = 'Iq_out' 
    

def main_org(args):
    logging.basicConfig(level=logging.INFO)
    
    logging.info('input comp file: %s', args.compfile[0])
    logging.info('model source dir: %s', args.cdir[0])
    
    # get info
    comp_file = args.compfile[0] 
    c_dir = args.cdir[0].rstrip('/') 
    c_files = getFiles(c_dir, "c") 
    for f in c_files: 
        logging.info('integrating: %s', f) 
    
    model_index_par_name = 'model_index' 
    model_pars_name = 'pars'
    return_par_name = 'Iq_out' 
    
    # construct AUTOGEN sections
    docs_section = get_docs_section(c_files, 4, 2)
    
    include_section = get_include_section(c_files, model_index_par_name)
    logging.debug('\n' + include_section)
    
    call_section = get_call_section(c_files, model_index_par_name, model_pars_name, return_par_name)
    logging.debug('\n' + call_section)
    
    # modify text
    text = mod_comp_file(comp_file, docs_section, include_section, call_section)
    logging.debug('\n' + text)
    
    # save new component file 
    comp_file_new = os.path.splitext(os.path.basename(comp_file))[0] + '_new.comp'
    logging.info('output comp file: %s' % comp_file_new)
    f = open(comp_file_new, 'w')
    f.write(text)
    f.close()

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('compfile', nargs='+', help='Component input filename. Mmust be a version of SANS_sasview_model.comp.')
    parser.add_argument('cdir', nargs='+', help='Directory containing sasview model .c files.')
    
    args = parser.parse_args()
    
    #test(args)
    main_org(args)
