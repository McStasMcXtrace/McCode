#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
This script outputs a modified mcstas component file, containing links and calls to specified sasview .c model files. 
The input component file must contain AUTOGEN flags, which are used to clear and insert the generated code.
'''
import os
import logging
import re
import argparse


def getFiles(look_dir, extension):
    file_list = []
    for (dirpath, dirnames, filenames) in os.walk(look_dir):
        for f in filenames:
            if os.path.splitext(f)[1] == '.' + extension:
                file_list.append(dirpath + '/' + f)
        break
    return file_list

def get_include_section(c_files, model_index_par_name):
    # null value (i == 0) is not actualized
    text = ''
    i = 1
    for f in c_files:
        text += '  #if %s == %d \n' % (model_index_par_name, i) 
        text += '    %%include "%s" \n' % os.path.basename(f) 
        text += '  #endif \n' 
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
    text += '    float %s = 1; \n' % return_par_name
    
    i = 1
    for f in c_files:
        text += '    #if %s == %d \n' % (model_index_par_name, i) 
        text += '      //%s = Iq(%s); \n' % (return_par_name, get_Iq_sign(f, model_pars_name)) 
        text += '    #endif \n' 
        i += 1 
        
    return text

def mod_comp_file(comp_file, docs_section, include_section, call_section):
    logging.info('component file: ' + comp_file)
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
    for f in c_files:
        f = os.path.basename(f)
        if len(f) > max_name_len:
            max_name_len = len(f)
    name_format_str = '{:<' + str(max_name_len) + '}' # e.g. '{:<35}'
    
    text = pad_format_str.format('*')
    text += int_format_str.format(str(0)) + ' - None \n'
    
    i = 1
    for f in c_files:
        text += pad_format_str.format('*')
        name = os.path.basename(f)
        
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
        i += 1
    
    return text + '* \n'

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    logging.info('component file: %s', args.compfile)
    logging.info('directory of C files: %s', args.cdir[0])
    
    # get info
    comp_file = args.compfile[0] 
    c_dir = args.cdir[0].rstrip('/') 
    c_files = getFiles(c_dir, "c") 
    for f in c_files: 
        logging.info('c file to integrate: %s', f) 
    
    model_index_par_name = 'model_index' 
    model_pars_name = 'model_pars_ptr'
    return_par_name = 'Iq_answer' 
    
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
    f = open(os.path.splitext(os.path.basename(comp_file))[0] + '_new.comp', 'w')
    f.write(text)
    f.close()

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('compfile', nargs='+', help='Component input filename. Mmust be a version of SANS_sasview_model.comp.')
    parser.add_argument('cdir', nargs='+', help='Directory containing sasview model .c files.')
    
    args = parser.parse_args()

    main(args)
