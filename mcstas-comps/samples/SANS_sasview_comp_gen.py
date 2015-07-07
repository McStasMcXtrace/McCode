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
        text += '    #include %s \n' % f 
        text += '  #endif \n' 
        i += 1 
    return text

def get_Iq_sign(c_file, array_call_name = None): 
    # TODO: fix situations in which Iq(...) fct. signature is determined by a #define (now this just becomes a single parameter)
    
    text = open(c_file).read() 
    m = re.search('float\s+Iq\((.*)\)', text)
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
    
    text += '    float %s = 1 \n' % return_par_name
    text += '    #if SASVIEW_MODEL == 0 \n'
    text += '      //%s = NaN; \n' % return_par_name
    text += '    #endif \n'
    
    i = 1
    for f in c_files:
        text += '    #ifdef %s == %d \n' % (model_index_par_name, i) 
        text += '      //Iq(%s) \n' % get_Iq_sign(f, model_pars_name) 
        text += '    #endif \n' 
        i += 1 
    return text

def mod_comp_file(comp_file, docs_section, include_section, call_section):
    print(comp_file)
    text = open(comp_file).read()
    
    pos_D = text.find("Models: ")
    pos_end_D = text.find('model_pars: ')
    pos_A = text.find("AUTOGEN_A")
    pos_end_A = text.find("AUTOGEN_END_A")
    pos_B = text.find("AUTOGEN_B")
    pos_end_B = text.find("AUTOGEN_END_B")
    
    if pos_A == -1 or pos_end_A == -1 or pos_B == -1 or pos_end_B == -1:
        logging.exception('mod_comp_file: AUTOGEN flag error.')
    
    ret_text = text[:pos_D+7] + '\n' + docs_section + text[pos_end_D-2:pos_A+9] + '\n' + include_section + '  // ' 
    ret_text += text[pos_end_A:pos_B+9] + '\n' + call_section + '    // ' + text[pos_end_B:]
    return ret_text

def get_docs_section(c_files, pad_spaces):
    
    num_files = len(c_files)
    text = '*'
    for j in range(pad_spaces):
        text += ' '
    text += '0 - None \n'
    i = 1
    for f in c_files:
        text += '*'
        for j in range(pad_spaces):
            text += ' '
        name = os.path.basename(f)
        
        sign = get_Iq_sign(f)
        m = re.search(r'float\s+q\s*,(.*)', sign)
        if m: 
            sign = m.group(1).replace('float', '')
            sign = sign.replace('  ', ' ')
            sign = sign.strip(' ')
        
        text += str(i) + ' - ' + name + '    (' + sign + ') \n'
        i += 1
    
    return text + '* \n'

def main(args):
    logging.basicConfig(level=logging.DEBUG)
        
    logging.info('component file: %s', args.compfile)
    logging.info('directory of C files: %s', args.cdir[0])
    
    # get info
    comp_file = args.compfile[0] 
    c_dir = args.cdir[0].rstrip('/') 
    c_files = getFiles(c_dir, "c") 
    for f in c_files: 
        logging.debug('c file to integrate: %s', f) 
    
    model_index_par_name = 'model_index' 
    model_pars_name = 'model_pars'
    return_par_name = 'Iq_answer' 
    
    # construct AUTOGEN sections
    docs_section = get_docs_section(c_files, 15)
    
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
    parser.add_argument('cdir', nargs='+', help='Directory containing sasview model .c files. Must be a subdirectory of the component.')
    #parser.add_argument('ofile', help='Output file name')
    
    args = parser.parse_args()

    main(args)
