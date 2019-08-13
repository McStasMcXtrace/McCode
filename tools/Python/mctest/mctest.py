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
import math

sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from mccodelib import utils


class InstrTest:
    ''' instruent test house keeping object '''
    def __init__(self, sourcefile, localfile):
        self.sourcefile = sourcefile
        self.localfile = localfile
        self.instrname = splitext(basename(sourcefile))[0]
        self.hastest = None

        self.parvals = ""
        self.detector = ""
        self.targetval = None

        self.compiled = None
        self.compiletime = None
        self.ran = None
        self.runtime = None
        self.testcomplete = False
    def extract_detector_parvals(self, text=None):
        ''' extracts parameter values, detector and targetvalue, assuming [detector]_I=[targetvalue] '''
        if text is None:
            text = open(self.localfile).read()
        m = re.search("\%Example\:([^\n]*)Detector\:([^\n]*)_I=([0-9.]+)", text)
        if m:
            self.parvals = m.group(1).strip()
            self.detector = m.group(2).strip()
            self.targetval = m.group(3).strip()
            self.hastest = True
        else:
            self.hastest = False
    def get_json_repr(self):
        return {
            "sourcefile"   : self.sourcefile,
            "localfile"    : self.localfile,
            "instrname"    : self.instrname,
            "hastest"      : self.hastest,
            "parvals"      : self.parvals,
            "detector"     : self.detector,
            "targetval"    : self.targetval,
            "compiled"     : self.compiled,
            "compiletime"  : self.compiletime, 
            "ran"          : self.ran,
            "runtime"      : self.runtime,
            "testcomplete" : self.testcomplete,
        }
    def save(self, infolder):
        text = json.dumps(self.get_json_repr())
        f = open(join(infolder, self.instrname) + ".json", 'w').write(text)

def print_to_console(str):
    ''' used with popen wrapper '''
    logging.info(str)

def print_to_console_debug(str):
    ''' used with popen wrapper '''
    logging.debug(str)

class LineLogger():
    ''' log lines to memory, then save to disk '''
    def __init__(self):
        self.lst = []
    def logline(self, addstr):
        self.lst.append(addstr)
    def save(self, filename):
        text = "\n".join(self.lst) + "\n"
        f = open(filename, 'w').write(text)
    def find(self, searchstr):
        for l in self.lst:
            if re.search(searchstr, l):
                return True
        return False

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

def branch_test(mcstasroot, branchname, testroot, limitinstrs=None):
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
    logging.info("")
    logging.info("")
    logging.info("Testing branch:         %s" % branchname)
    logging.info("Finding instruments in: %s" % branchdir)
    instrs, _ = utils.get_instr_comp_files(branchdir, recursive=True)
    instrs.sort()

    # limt runs if required
    if limitinstrs:
        instrs = instrs[:limitinstrs]

    # copy instrument files and extract tests
    logging.info("Copying instruments to: %s" % testdir)
    tests = []
    # max instr name length for pretty-output
    maxnamelen = 0
    for f in instrs:
        l = len(basename(f)) - 5
        if l > maxnamelen:
            maxnamelen = l
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
        test.extract_detector_parvals(text)
        if test.hastest:
            formatstr = "%-" + "%ds: TEST" % maxnamelen
            logging.info(formatstr % instrname)
        else:
            formatstr = "%-" + "%ds: NO TEST" % maxnamelen
            logging.info(formatstr % instrname)

    # modify environment
    os.environ["MCSTAS"] = branchdir
    oldpath = os.environ["PATH"]
    os.environ["PATH"] = "%s/miniconda3/bin:%s/bin:%s" % (branchdir, branchdir, oldpath)
    try:
        # compile, record time
        logging.info("")
        logging.info("Compiling instruments [seconds]...")
        for test in tests:
            log = LineLogger()
            t1 = time.time()
            cmd = "mcrun --info %s" % test.localfile
            utils.run_subtool_to_completion(cmd, cwd=join(testdir, test.instrname), stdout_cb=log.logline, stderr_cb=log.logline)
            t2 = time.time()
            # TODO: detect success / failure from process return code
            test.compiled = not log.find("error:")
            test.compiletime = t2 - t1
            # log to terminal
            if test.compiled:
                formatstr = "%-" + "%ds: " % maxnamelen + \
                    "{:3d}.".format(math.floor(test.compiletime)) + str(test.compiletime-int(test.compiletime)).split('.')[1][:2]
                logging.info(formatstr % test.instrname)
            else:
                formatstr = "%-" + "%ds: COMPILE ERROR" % maxnamelen
                logging.info(formatstr % instrname + ", " + cmd)
            # record compile stdout/err
            log.save(join(testdir, test.instrname, "compile_std.txt"))

            # save (incomplete) test results to disk
            test.save(infolder=join(testdir, test.instrname))

        # run, record time
        logging.info("")
        logging.info("Running tests...")
        for test in tests:
            log = LineLogger()
            if not test.hastest:
                continue
            t1 = time.time()
            cmd = "mcrun %s %s" % (test.localfile, test.parvals)
            utils.run_subtool_to_completion(cmd, cwd=join(testdir, test.instrname), stdout_cb=log.logline, stderr_cb=log.logline)
            t2 = time.time()
            # TODO: detect success / failure from process return code
            test.ran = not log.find("error:")
            test.runtime = t2 - t1
            # log to terminal
            if test.compiled:
                formatstr = "%-" + "%ds: " % (maxnamelen+1) + \
                    "{:3d}.".format(math.floor(test.runtime)) + str(test.runtime-int(test.runtime)).split('.')[1][:2]
                logging.info(formatstr % test.instrname)
            else:
                formatstr = "%-" + "%ds: RUNTIME ERROR" % (maxnamelen+1)
                logging.info(formatstr % instrname + ", " + cmd)

            # record run stdout/err
            log.save(join(testdir, test.instrname, "run_std.txt"))

            # TOO: check monitor against target value

            # save test result to disk
            test.testcomplete = True
            test.save(infolder=join(testdir, test.instrname))

    finally:
        # clean up path changes
        del os.environ["MCSTAS"]
        os.environ["PATH"] = oldpath

        # save summary containing all test objects by instrname
        try:
            results = OrderedDict()
            for test in tests:
                results[test.instrname] = test.get_json_repr()
            open(join(testdir, "results.json"), 'w').write(json.dumps(results, indent=4))
        except Exception as e:
            logging.error("could not save master results file: %s" % str(e))


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
        logging.info("")
    if args.versions:
        logging.info("Subfolders containing an 'environment' file are:")
        logging.info("")
    testlimit = None
    if args.limit:
        try:
            testlimit = int(args.limit[0])
        except:
            logging.info("--limit must be a number")
            quit()

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
            branch_test(mcstasroot, branchdirname, testdir, testlimit)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--mcstasroot', nargs='?', help='select custom mcstas root')
    parser.add_argument('--version', nargs=1, help='select mcstas installation to test')
    parser.add_argument('--limit', nargs=1, help='do only the first [num] tests')
    parser.add_argument('--versions', action='store_true', help='display local mcstas installations')
    parser.add_argument('--testenv', action='store_true', help='display local mcstas installations')
    args = parser.parse_args()

    try:
        main(args)
    except KeyboardInterrupt:
        print()


