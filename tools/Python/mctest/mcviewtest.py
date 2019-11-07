#!/usr/bin/env python
# -*- coding: utf-8 -*-
import logging
import argparse
import json
import os
from os.path import basename, join, isdir, splitext
from os import mkdir
import sys
import re
import time

#sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
#from mccodelib import mccode_config


def main(args):
    if args.verbose:
        logging.basicConfig(level=logging.DEBUG, format="%(message)s")
    else:
        logging.basicConfig(level=logging.INFO, format="%(message)s")

    testdir = args.testdir[0]
    reflabel = args.reflabel

    logging.info(testdir)
    logging.info(reflabel)



'''

Startup: 

- testdir is required
- if no reflabel is selected, prompt with options

Execution:

- load reference data
- load remaining test data 
- all data is loaded as <label>_testresults.json (as this should be the output of all tests)
- craete rows by matching "displayname" entries in all test files:
    - rows are defined by the reference
    - entries are sorted alphanum, tests from the same instr file must follow successively without interruption
    - entries in test labels without a reference are displayed below the rest with "no ref" in the reference slot
    - a header row is generated
    - a list of content rows is generated
    - entries in a row are a tuple of stateidx, ctime, rtime, detval and detvalrefpercentage
    - state==0: green, ctime, rtime, detval, ref%
    - state==1: red,   ctime, rtime, detval, ref%
    - state==2: gray,  ctime
    - state==3: black

Output:

- use jinja2 for html templating, it's available with py3, and the django templating language is seemingly based on it
- from jinja2 import Template <- create from a string which is the loaded template


'''








if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('testdir', nargs=1, help='test data is drawn from this root folder')
    parser.add_argument('--reflabel', nargs="?", help='reference label name')
    parser.add_argument('--verbose', action='store_true', help='output excessive information for debug purposes')
    args = parser.parse_args()

    main(args)


