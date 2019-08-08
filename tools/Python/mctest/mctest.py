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

sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from mccodelib import utils


class InstrTest:
    ''' instruent test house keeping object '''
    def __init__(self, sourcefile, localfile):
        self.sourcefile = sourcefile
        self.localfile = localfile
        self.parvals = ""
        self.detector = ""
    def get_json_str(self):
        return json.dumps({
            "sourcefile", self.sourcefile,
            "localfile", self.localfile,
            "parvals", self.parvals,
            "detector", self.detector,
        })


def branch_test(mcstasroot, branchname, testroot):
    ''' will test a single mcstas "branch" or version '''
    # create test dir
    branchdir = join(mcstasroot, branchname)
    testdir = join(testroot, branchname)
    try:
        mkdir(testdir)
    except:
        logging.info("multiple tests in the same folder not yet implemented, exiting...")
        quit()

    # copy instr files and record info
    logging.info("finding instrument files in %s ..." % branchdir)
    instrs, comps = utils.get_instr_comp_files(branchdir, recursive=True)
    logging.info("copying instruments to test dir %s, extracting test examples..." % testdir)
    tests = []
    for f in instrs:
        instrname = splitext(basename(f))[0]
        instrdir = join(testdir, instrname)
        # create the test foldr for this instrument
        mkdir(instrdir)

        text = open(f).read()
        f_new = join(instrdir, basename(f))
        # create a new file with the instr text in it - e.g. a local copy of the instrument file
        open(f_new, 'w').write(text)

        test = InstrTest(sourcefile=f, localfile=f_new)
        tests.append(test)

        # extract and record %Example info from text
        m = re.search("\%Example\:([^\n]*)Detector\:([^\n]*)", text)
        if m:
            test.parvals = m.group(1).strip()
            test.detector = m.group(2).strip()
            logging.info("TEST   : %s" % instrname)
        else:
            logging.info("NO TEST: %s" % instrname)

    # TODO: enable environment

    # TODO: compile, record time

    # TODO: run, record time

    # TODO: save test objects to disk


def main(args):
    logging.basicConfig(level=logging.INFO)

    # setup
    testroot = "/tmp/mctest-test"
    mcstasroot = "/usr/share/mcstas/"
    if args.mcstasroot:
        mcstasroot = args.mcstasroot
    logging.info("using mcstas root: %s" % mcstasroot)
    
    dirnames = []
    for (dirpath, dirnames, files) in os.walk(mcstasroot):
        break
    if args.versions:
        logging.info("subfolders of %s:" % testroot) 
        for v in dirnames:
            print(v)
        logging.info("")
        logging.info("use --version=someversion to only test that version")
        quit()

    # create root test folder
    if not os.path.exists(testroot):
        # TODO: does mkdir throw an exception when folder creation fails? (If so, use a try-catch.)
        mkdir(testroot)
    if not os.path.exists(testroot):
        logging.info("test root folder could not be craeted, exiting...")
        quit()
    testdir = join(testroot, utils.get_datetimestr())
    if not os.path.exists(testdir):
        mkdir(testdir)

    # iterate mcstas branches
    if args.version != None:
        selected_version = args.version[0]
        if not isdir(join(mcstasroot, selected_version)):
            logging.info("mcstas vesion %s could not be found, exiting..." % selected_version)
            quit()
        dirnames = [selected_version]
    for branchdirname in dirnames:
        branch_test(mcstasroot, branchdirname, testdir)

    #logging.info("integration test complete")


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--mcstasroot', nargs='?', help='select custom mcstas root')
    parser.add_argument('--version', nargs=1, help='select mcstas installation to test')
    parser.add_argument('--versions', action='store_true', help='display local mcstas installations')
    args = parser.parse_args()

    main(args)

