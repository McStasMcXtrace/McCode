#!/usr/bin/env python
# -*- coding: utf-8 -*-
import logging
import argparse
import json
import os
from os.path import basename, join, isdir, splitext
from os import mkdir
from collections import OrderedDict
import sys
import re
import time

sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from mccodelib import utils


class InstrTest:
    ''' instruent test house keeping object '''
    def __init__(self, sourcefile, localfile):
        self.sourcefile = sourcefile
        self.localfile = localfile
        self.instrname = splitext(basename(sourcefile))[0]
        self.hastest = False
        self.parvals = ""
        self.detector = ""
        self.compiled = False
        self.compiletime = None
        self.ran = False
        self.runtime = None
    def get_json_repr(self):
        return {
            "sourcefile"  : self.sourcefile,
            "localfile"   : self.localfile,
            "instrname"   : self.instrname,
            "parvals"     : self.parvals,
            "detector"    : self.detector,
            "compiled"    : self.compiled,
            "compiletime" : self.compiletime, 
            "ran"         : self.ran,
            "runtime"     : self.runtime,
        }

def print_to_console(str):
    ''' used with popen wrapper '''
    logging.info(str)

def print_to_console_debug(str):
    ''' used with popen wrapper '''
    logging.debug(str)

class LineLogger():
    ''' allows logging lines to memory, to be saved to disk later on '''
    def __init__(self):
        self.lst = []
    def logline(self, str):
        self.lst.append(str)
    def save(self, filename):
        text = "\n".join(self.lst) + "\n"
        open(filename, 'w').write(text)

def test_env_settings(mcstasroot, branchname):
    ''' test mcstas vesion switching mechanism '''
    branchdir = join(mcstasroot, branchname)

    os.environ["MCSTAS"] = branchdir
    oldpath = os.environ["PATH"]
    os.environ["PATH"] = "%s/miniconda3/bin:%s/bin:%s" % (branchdir, branchdir, oldpath)

    # run the mcstas --version command
    logging.info("%s environment:" % branchname)
    cmd = "mcstas --version"
    utils.run_subtool_to_completion(cmd, stdout_cb=print_to_console, stderr_cb=print_to_console)
    logging.info("")

    # TODO: should we test the existence of mcrun?

    # restore environment
    del os.environ["MCSTAS"]
    os.environ["PATH"] = oldpath

def branch_test(mcstasroot, branchname, testroot):
    ''' test a single mcstas "branch" or version that is present on the system '''
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
    instrs, _ = utils.get_instr_comp_files(branchdir, recursive=True)
    instrs.sort()


    # DEBUG!:
    #instrs = instrs[:5]


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
            test.hastest = True
            logging.info("TEST   : %s" % instrname)
        else:
            logging.info("NO TEST: %s" % instrname)

    # modify environment
    os.environ["MCSTAS"] = branchdir
    oldpath = os.environ["PATH"]
    os.environ["PATH"] = "%s/miniconda3/bin:%s/bin:%s" % (branchdir, branchdir, oldpath)
    try:
        # compile, record time
        for test in tests:
            log = LineLogger()
            t1 = time.time()
            cmd = "mcrun --info %s" % test.localfile
            utils.run_subtool_to_completion(cmd, cwd=join(testdir, test.instrname), stdout_cb=log.logline, stderr_cb=log.logline)
            t2 = time.time()
            test.compiled = True
            test.compiletime = t2 - t1
            log.save(join(testdir, test.instrname, "std.txt"))

        # run, record time
        for test in tests:
            log = LineLogger()
            if not test.hastest:
                continue
            t1 = time.time()
            cmd = "mcrun %s %s" % (test.localfile, test.parvals)
            utils.run_subtool_to_completion(cmd, cwd=join(testdir, test.instrname), stdout_cb=log.logline, stderr_cb=log.logline)
            t2 = time.time()
            test.ran = True
            test.runtime = t2 - t1
            log.save(join(testdir, test.instrname, "std.txt"))

    finally:
        # clean up path changes
        del os.environ["MCSTAS"]
        os.environ["PATH"] = oldpath

    #
    # TODO: check results against monitor values
    #

    # save test objects to disk
    for test in tests:
        text = json.dumps(test.get_json_repr())
        f = join(testdir, test.instrname, test.instrname + '.json')
        open(f, 'w').write(text)

    # save giant all-containing test object on disk
    results = OrderedDict()
    for test in tests:
        results[test.instrname] = test.get_json_repr()
    f = join(testdir, "results.json")
    open(f, 'w').write(json.dumps(results, indent=4))


def main(args):
    logging.basicConfig(level=logging.INFO, format="%(message)s")

    # setup
    testroot = "/tmp/mctest-test"
    mcstasroot = "/usr/share/mcstas/"
    if args.mcstasroot:
        mcstasroot = args.mcstasroot
    logging.info("Using mcstas root: %s" % mcstasroot)
    if args.testenv:
        logging.info("Test environment mode, using output of 'mcstas --vesion'")
    if args.versions:
        logging.info("Subfolders containing an 'environment' file are:")
    logging.info("")
    dirnames = []
    branchnames = []
    for (_, dirnames, _) in os.walk(mcstasroot):
        break
    for d in dirnames:
        for (_, _, files) in os.walk(join(mcstasroot, d)):
            break
        if "environment" in files:
            branchnames.append(d)
    if args.versions:
        for v in branchnames:
            logging.info(v)
        logging.info("")
        logging.info("Use the --version=[version] option to test a specific version.")
        quit()

    # create root test folder
    if not os.path.exists(testroot):
        if not args.testenv:
            mkdir(testroot)
    if not os.path.exists(testroot):
        logging.info("test root folder could not be craeted, exiting...")
        quit()
    testdir = join(testroot, utils.get_datetimestr())
    if not os.path.exists(testdir) and not args.testenv:
        mkdir(testdir)

    # iterate mcstas branches
    if args.version != None:
        selected_version = args.version[0]
        if not isdir(join(mcstasroot, selected_version)):
            logging.info("mcstas vesion %s could not be found, exiting..." % selected_version)
            quit()
        dirnames = [selected_version]
    for branchdirname in dirnames:
        if args.testenv:
            test_env_settings(mcstasroot, branchdirname)
        else:
            branch_test(mcstasroot, branchdirname, testdir)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--mcstasroot', nargs='?', help='select custom mcstas root')
    parser.add_argument('--version', nargs=1, help='select mcstas installation to test')
    parser.add_argument('--versions', action='store_true', help='display local mcstas installations')
    parser.add_argument('--testenv', action='store_true', help='display local mcstas installations')
    args = parser.parse_args()

    main(args)

