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


def create_instr_test_objs(sourcefile, localfile, header):
    tests = []
    ms = re.findall("\%Example\:([^\n]*)Detector\:([^\n]*)_I=([0-9.+-e]+)", header)
    if ms:
        testnb = 1
        for m in ms:
            parvals = m[0].strip()
            detector = m[1].strip()
            targetval = m[2].strip()
            tests.append(InstrTest(sourcefile, localfile, parvals, detector, targetval, testnb))
            testnb = testnb + 1
    else:
        tests.append(InstrTest(sourcefile, localfile))
    return tests

class InstrTest:
    ''' instruent test house keeping object '''
    def __init__(self, sourcefile, localfile, parvals=None, detector=None, targetval=None, testnb=0):
        self.sourcefile = sourcefile
        self.localfile = localfile
        self.instrname = splitext(basename(sourcefile))[0]
        self.testnb = testnb
        
        self.parvals = parvals
        self.detector = detector
        self.targetval = targetval
        self.testval = None

        self.compiled = None
        self.compiletime = None
        self.didrun = None
        self.runtime = None
    def get_json_repr(self):
        return {
            "sourcefile"   : self.sourcefile,
            "localfile"    : self.localfile,
            "instrname"    : self.instrname,
            "testnb"       : self.testnb,

            "parvals"      : self.parvals,
            "detector"     : self.detector,
            "targetval"    : self.targetval,
            "testval"      : self.testval,

            "compiled"     : self.compiled,
            "compiletime"  : self.compiletime,
            "didrun"       : self.didrun,
            "runtime"      : self.runtime,
        }
    def save(self, infolder):
        text = json.dumps(self.get_json_repr())
        key = self.instrname
        if self.testnb > 1:
            key = "%s_%d" % (key, self.testnb)
        f = open(join(infolder, key) + ".json", 'w').write(text)
    def get_display_name(self):
        if self.testnb > 1:
            return self.instrname + "_%d" % self.testnb
        else:
            return self.instrname

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
    cmd = "mcstas --version"
    utils.run_subtool_to_completion(cmd, stdout_cb=print_to_console, stderr_cb=print_to_console)
    logging.info("")

    # TODO: should we test the existence of mcrun?

    # restore environment
    del os.environ["MCSTAS"]
    os.environ["PATH"] = oldpath

def mccode_test(branchdir, testdir, limitinstrs=None):
    ''' test a single mccode "branch" or version that is present on the system '''

    # copy instr files and record info
    logging.info("")
    logging.info("")
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
        # create the test foldr for this instrument
        instrname = splitext(basename(f))[0]
        instrdir = join(testdir, instrname)
        mkdir(instrdir)

        # create a new file with the instr text in it - e.g. a local copy of the instrument file
        text = open(f).read()
        f_new = join(instrdir, basename(f))
        open(f_new, 'w').write(text)

        # create a test object for every test defined in the instrument header
        instrtests = create_instr_test_objs(sourcefile=f, localfile=f_new, header=text)
        tests = tests + instrtests

        # extract and record %Example info from text
        numtests = len(instrtests) 
        if numtests == 0:
            formatstr = "%-" + "%ds: NO TEST" % maxnamelen
            logging.debug(formatstr % instrname)
        elif numtests == 1:
            formatstr = "%-" + "%ds: TEST" % maxnamelen
            logging.debug(formatstr % instrname)
        else:
            formatstr = "%-" + "%ds: TESTS (%d)" % (maxnamelen, numtests)
            logging.debug(formatstr % instrname)


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
        if not test.testnb > 0:
            continue
        t1 = time.time()
        cmd = "mcrun %s %s" % (test.localfile, test.parvals)
        utils.run_subtool_to_completion(cmd, cwd=join(testdir, test.instrname), stdout_cb=log.logline, stderr_cb=log.logline)
        t2 = time.time()
        # TODO: detect success / failure from process return code
        test.didrun = not log.find("error:")
        test.runtime = t2 - t1
        # log to terminal
        if test.compiled:
            formatstr = "%-" + "%ds: " % (maxnamelen+1) + \
                "{:3d}.".format(math.floor(test.runtime)) + str(test.runtime-int(test.runtime)).split('.')[1][:2]
            logging.info(formatstr % test.get_display_name())
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

    # let the outside world create full report, just return test objects
    return tests

def activate_mccode_version(version, mccoderoot):
    '''
    Modify environment, returns path as it was.
    
    branchdir: mccode version install directory
    '''
    branchdir = os.path.join(mccoderoot, version)
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

def create_test_dir(testroot, datetime, label):
    ''' create and return test directory as: testroot/datetime/label'''
    if not os.path.exists(testroot):
        mkdir(testroot)
    if not os.path.exists(testroot):
        logging.info("test root folder could not create test folder, exiting...")
        quit()
    datetimedir = join(testroot, datetime)
    if not os.path.exists(datetimedir):
        mkdir(datetimedir)
    testdir = join(datetimedir, label)
    if not os.path.exists(testdir):
        mkdir(testdir)
    return testdir

def run_default_test(testroot, mccoderoot, limit):
    # get default/system version number
    logger = LineLogger()
    utils.run_subtool_to_completion("mcrun --version", stdout_cb=logger.logline)
    version = logger.lst[-1].strip()

    # create single-run test directory
    testdir = create_test_dir(testroot, utils.get_datetimestr(), version)

    logging.info("Testing: %s" % version)
    # TODO: run test
    # TODO: write master report


    print("default test impl. not complete")
    quit()

def run_version_test(testroot, mccoderoot, limit, version):
    # verify that version exists
    if not os.path.isfile(os.path.join(mccoderoot, version, "environment")):
        print("mccode version %scould not be found, exiting..." % version)
        quit(1)

    # create single-run test directory
    testdir = create_test_dir(testroot, utils.get_datetimestr(), version)

    oldpath = activate_mccode_version(version, mccoderoot)
    try:
        logging.info("Testing: %s" % version)
        # TODO: run test
        # TODO: write master report
    finally:
        deactivate_mccode_version(oldpath)


    print("version test impl. not complete")
    quit(1)

def run_configs_test(testroot, mccoderoot, limit):
    # get test directory datetime string
    datetime = utils.get_datetimestr()

    # test labels loop
    # TODO: get configs
    configs = [] 
    for c in configs:

        oldpath = activate_mccode_version(c.version, mccoderoot)
        try:
            # TODO: do mccode_config.py copy-in
            try:
                # crate local test root
                testdir = create_test_dir(testroot, datetime, c.label)
                logging.info("Testing: %s" % c.label)
                # TODO: run test
                # TODO: write master report
            finally:
                pass
                # TODO: mccode_config.py restore
        finally:
            deactivate_mccode_version(oldpath)


    print("configs test impl. not complete")
    quit(1)

def show_installed_versions(mccoderoot):
    logging.info("Test environment mode, using output of 'mcstas --vesion'")
    logging.info("")

    dirnames = []
    branchnames = []
    for (_, dirnames, _) in os.walk(mccoderoot):
        break
    for d in dirnames:
        for (_, _, files) in os.walk(join(mccoderoot, d)):
            break
        if "environment" in files:
            branchnames.append(d)

    for v in branchnames:
        test_env_settings(mccoderoot, v)
    logging.info("Selectable version names are: %s" % ", ".join(branchnames))
    logging.info("")


def main(args):

    # mutually excusive main branches
    default = None                  # test system mccode version as-is
    version = args.testversion      # test a specific mccode version (also) present on the system
    configs = args.configs          # test all config versions, which are versions of mccode_config.py, located in mccodelib/MCCODE
    vinfo = args.versions           # display mccode versions installed on the system

    # modifying options
    verbose = args.verbose          # display more info during runs
    testroot = args.testroot        # use non-default test output root location
    mccoderoot = args.mccoderoot    # use non-default mccode system install location
    limit = args.limit              # only test the first [limit] instruments (useful for debugging purposes)

    # set modifications first
    if verbose:
        logging.basicConfig(level=logging.DEBUG, format="%(message)s")
    else:
        logging.basicConfig(level=logging.INFO, format="%(message)s")
    if not testroot:
        testroot = "/tmp/mctest"
    logging.debug("Using test root:      %s" % testroot)
    # TODO: make sure testroot is valid
    if not mccoderoot:
        mccoderoot = "/usr/share/mcstas/"
    # TODO: make sure mccoderoot is valid
    logging.debug("Using mccode root:    %s" % mccoderoot)
    if limit:
        try:
            limit = int(args.limit[0])
        except:
            logging.info("--limit must be a number")
            quit(1)
    logging.debug("")

    # decide and run main branch
    if version and configs or version and vinfo or configs and vinfo:
        print("WARNING: version, --configs and --versions are mutually exclusive, exiting")
        quit()
    default = not version and not configs and not vinfo
    if default:
        run_default_test(testroot, mccoderoot, limit)
    elif version:
        run_version_test(testroot, mccoderoot, limit, version)
    elif configs:
        run_configs_test(testroot, mccoderoot, limit)
    elif vinfo:
        show_installed_versions(mccoderoot)




    ''' PREV

    branchnames = get_mccode_versions(mccoderoot)

    # iterate mccode branches
    if args.testversion != None:
        selected_version = args.testversion
        if not isdir(join(mccoderoot, selected_version)):
            logging.info("mccode vesion %s could not be found, exiting..." % selected_version)
            quit()
        branchnames = [selected_version]
    for branchdirname in branchnames:
        mccode_test(mccoderoot, branchdirname, testdir, testlimit)
    '''


'''
Rework sketch.

1) behavior - overview

(no args), default behavior: Test the current default mccode installation as-is, mc/mx switch built into the dist and tool.
[testversion]: tests a local mcstas/mxtrace version currently installed on the system (as now)
--mccoderoot: Override mccode installation root directory, if non-standard
--testroot: Where to put the test output files (overrides default value)
--limit: a db tool which works as it does now
--versions: As now, but fused with --testenvs to form the latter 
--testenvs: Removed as an option, fused with --versions
--verbose: Print more info, as now
--configs: Test not system versions, but configs defined as local mccode_config.py files

2) behavior, --configs

When --configs is chosen, each mccode_config.py_[LABEL] file should be used to define a test run.
These are located in mccodelib/mcstas-tests or mccodelib/mcxtrace-tests. Each test is executed using the following
sequence:
- for each label:
- load the mccode_config.py file into memory
- read the MCCODE_VERSION and MCCODE_LIB_DIR from the config member of this module
- validate that this version exists on the system
[- give a notice/warning if the mccoderoot variable does not correspond to the contents of the config file]
- backup the dist mccode_config.py
- push the label config file
- execute the test in a try-except-finally block
- print any error in except
- revert the mccode_config.py file in finally
[- consider outputting compact statuses to the terminal in labels mode, progress bars plus overviews]

3) design

- new hierarchy: test_all, TestLabel and TestRun
- new setup logics accomodating args and using the above 

4) impl. preparatory tasks

- rename label configs to be properly loadable .py python modules - OK
- elliminate --testenvs in favour of --versions - OK
- rename class InstrTest to TestRun - OK
- handle more than one test pr. instrument
- ensure multiple tests pr. instrument are possible

5) impl. tasks

- create Test and TestRun classes
- create TestLabels function, which also uses Test and TestRun, but rolls through each config file, copying, restoring, etc.
- rework setup logics to accomodate all args combinatinos


'''


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('testversion', nargs="?", help='mccode version to test')
    parser.add_argument('--configs', action='store_true', help='test config files under mccodelib/MCCODE')
    parser.add_argument('--mccoderoot', nargs='?', help='manually select root search folder for mccode installations')
    parser.add_argument('--testroot', nargs='?', help='output test results under this root folder')
    parser.add_argument('--limit', nargs=1, help='test only the first [LIMIT] instrs in every version')
    parser.add_argument('--versions', action='store_true', help='display local versions info')
    parser.add_argument('--verbose', action='store_true', help='output a test/notest instrument status header before each test')
    args = parser.parse_args()

    try:
        main(args)
    except KeyboardInterrupt:
        print()

