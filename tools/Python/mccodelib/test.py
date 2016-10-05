#!/usr/bin/env python
# -*- coding: utf-8 -*-
import logging
import argparse
import guiutils
import fileutils
import traceback

# get access to mcgui folder

def parse_displaysection(comproot, mcdisplay):
    ''' parse components in comproot and (optinally) print mcdisplay section  '''
    instr, comp = guiutils.McGuiUtils.get_instr_comp_files(comproot)
    # '/home/jaga/source/McCode/mcstas-comps'
    errors = []
    for c in comp:
        parser = fileutils.McComponentParser(c)
        try:
            parser.parse()
            parser.parseDisplaySection()
        except Exception as e:
            errors.append('error (%s): %s' % (parser.file, e.__str__()))
        
        if mcdisplay:
            print('********************')
            print('* %s' % parser.file)
            print('********************')
            print('\n%s\n' % parser.mcdisplay)
        
    for e in errors:
        print e

def parse_comps():
    ''' parse componens and print info '''
    files_instr, files_comp = guiutils.McGuiUtils.get_instr_comp_files('/usr/share/mcstas/2.1')
    for f in files_comp:
        parser = fileutils.McComponentParser(f)
        try:
            parser.parse()
        except Exception, e:
            traceback.print_stack()
            print('exception: ', e)
        
        print('*****')
        print('component file:        \n' + parser.file)
        print('          name: :      \n' + parser.name)
        print('          info:        \n' + parser.info)
        print('          description: \n' + parser.description)
        for p in parser.pars:
            par = fileutils.McComponentParser.McComponentParInfo(p)
            print('--> parameter name:          ' + par.par_name)
            print('-->           default value: ' + par.default_value)
            print('-->           type:          ' + par.type)
            print('-->           docstring:     ' + par.doc_and_unit)
        print('*****')

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    # TODO: log test sequence
    parse_displaysection(args.comproot[0], args.mcdisplay)
    

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--mcdisplay', action='store_true', help='print mcdisplay sections flag' )
    parser.add_argument('comproot', nargs=1, help='components root dir dir' )

    args = parser.parse_args()

    main(args)
