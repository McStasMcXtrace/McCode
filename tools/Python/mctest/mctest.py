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
        self.testval = None

        self.compiled = None
        self.compiletime = None
        self.ran = None
        self.runtime = None
        self.testcomplete = False
    def extract_detector_parvals(self, text=None):
        ''' extracts parameter values, detector and targetvalue, assuming [detector]_I=[targetvalue] '''
        if text is None:
            text = open(self.localfile).read()
        m = re.search("\%Example\:([^\n]*)Detector\:([^\n]*)_I=([0-9.+-e]+)", text)
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
            "testval"          : self.testval,

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

def test_env_settings(mccoderoot, branchname):
    ''' test mccode vesion switching mechanism '''
    branchdir = join(mccoderoot, branchname)

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

def branch_test(mccoderoot, branchname, testroot, limitinstrs=None):
    ''' test a single mccode "branch" or version that is present on the system '''
    # create test dir
    branchdir = join(mccoderoot, branchname)
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
            logging.debug(formatstr % instrname)
        else:
            formatstr = "%-" + "%ds: NO TEST" % maxnamelen
            logging.debug(formatstr % instrname)

    # modify environment
    oldpath = activate_mccode_version(branchdir)
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
                formatstr = "%-" + "%ds: COMPILE ERROR using:\n" % maxnamelen
                logging.info(formatstr % test.instrname + cmd)
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

            for (_, dirnames, _) in os.walk(join(testdir, test.instrname)):
                if len(dirnames) > 0:
                    lns = open(join(testdir, test.instrname, dirnames[0], "mccode.sim")).read().splitlines()
                    componentlines = [l for l in lns if re.match("  component:", l)]
                    filenamelines = [l for l in lns if re.match("  filename:", l)]
                    idx = 0
                    for l in componentlines:
                        if re.match("  component: %s" % test.detector, l):
                            break
                        idx = idx + 1
                    try:
                        filename = re.match("  filename: (.+)", filenamelines[idx]).group(1)
                    except:
                        print("ERROR: targetval for detector %s could not be extracted from %s" % (test.detector, test.instrname))
                        continue
                    with open(join(testdir, test.instrname, dirnames[0], filename)) as fp:
                        while True:
                            l = fp.readline()
                            if not l:
                                break
                            m = re.match("# values: ([0-9+-e.]+) ([0-9+-e.]+) ([0-9]+)", l)
                            if m :
                                I = m.group(1)
                                I_err = m.group(2)
                                N = m.group(3)
                                test.testval = I
                                break
                break

            # save test result to disk
            test.testcomplete = True
            test.save(infolder=join(testdir, test.instrname))

    finally:
        # clean up path changes
        deactivate_mccode_version(oldpath)

        # save summary containing all test objects by instrname
        try:
            results = OrderedDict()
            for test in tests:
                results[test.instrname] = test.get_json_repr()
            open(join(testdir, "results.json"), 'w').write(json.dumps(results, indent=4))
        except Exception as e:
            logging.error("could not save master results file: %s" % str(e))

def activate_mccode_version(branchdir):
    '''
    Modify environment, returns path as it was.
    
    branchdir: mccode version install directory
    '''
    os.environ["MCSTAS"] = branchdir
    oldpath = os.environ["PATH"]
    os.environ["PATH"] = "%s/miniconda3/bin:%s/bin:%s" % (branchdir, branchdir, oldpath)
    return oldpath

def deactivate_mccode_version(oldpath):
    '''
    Clean up path changes.
    
    oldpath: this path is restored
    '''
    del os.environ["MCSTAS"]
    os.environ["PATH"] = oldpath

def main(args):
    if args.verbose:
        logging.basicConfig(level=logging.DEBUG, format="%(message)s")
    else:
        logging.basicConfig(level=logging.INFO, format="%(message)s")

    # setup
    testroot = "/tmp/mctest"
    if args.testroot:
        testroot = args.testroot
    mccoderoot = "/usr/share/mcstas/"
    if args.mccoderoot:
        mccoderoot = args.mccoderoot
    logging.info("Using mccode root: %s" % mccoderoot)
    if args.testenvs:
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
    for (_, dirnames, _) in os.walk(mccoderoot):
        break
    for d in dirnames:
        for (_, _, files) in os.walk(join(mccoderoot, d)):
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
        if not args.testenvs:
            mkdir(testroot)
    if not os.path.exists(testroot):
        logging.info("test root folder could not be craeted, exiting...")
        quit()
    testdir = join(testroot, utils.get_datetimestr())
    if not os.path.exists(testdir) and not args.testenvs:
        mkdir(testdir)

    # iterate mccode branches
    if args.testversion != None:
        selected_version = args.testversion
        if not isdir(join(mccoderoot, selected_version)):
            logging.info("mccode vesion %s could not be found, exiting..." % selected_version)
            quit()
        dirnames = [selected_version]
    for branchdirname in dirnames:
        if args.testenvs:
            test_env_settings(mccoderoot, branchdirname)
        else:
            branch_test(mccoderoot, branchdirname, testdir, testlimit)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('testversion', nargs="?", help='mccode version to test')
    parser.add_argument('--mccoderoot', nargs='?', help='manually select root search folder for mccode installations')
    parser.add_argument('--testroot', nargs='?', help='output test results under this root folder')
    parser.add_argument('--limit', nargs=1, help='test only the first [LIMIT] instrs in every version')
    parser.add_argument('--versions', action='store_true', help='display local versions')
    parser.add_argument('--testenvs', action='store_true', help='more detailed local versions info')
    parser.add_argument('--verbose', action='store_true', help='output a test/notest instrument status header before each test')
    args = parser.parse_args()

    try:
        main(args)
    except KeyboardInterrupt:
        print()


