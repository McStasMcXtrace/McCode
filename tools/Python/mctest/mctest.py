#!/usr/bin/env python
# -*- coding: utf-8 -*-
import logging
import argparse
import json
import os
join = os.path.join
from os.path import basename, join, isdir, splitext
from os import mkdir
from collections import OrderedDict
import sys
import re
import time
import math

sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from mccodelib import utils, mccode_config


#
# Functionality
#

def create_instr_test_objs(sourcefile, localfile, header):
    ''' returns a list containing one initialized test object pr %Example within the instr file '''
    tests = []
    ms = re.findall("\%Example:([^\n]*)Detector\:([^\n]*)_I=([0-9.+-e]+)", header)
    if len(ms) > 0:
        testnb = 1
        for m in ms:
            parvals = m[0].strip()
            detector = m[1].strip()
            targetval = m[2].strip()
            tests.append(InstrExampleTest(sourcefile, localfile, parvals, detector, targetval, testnb))
            testnb = testnb + 1
    else:
        tests.append(InstrExampleTest(sourcefile, localfile))
    return tests

class InstrExampleTest:
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
        self.errmsg = None
    def get_json_repr(self):
        return {
            "displayname"  : self.get_display_name(),
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
            "errmsg"       : self.errmsg,
        }
    def save(self, infolder):
        text = json.dumps(self.get_json_repr())
        f = open(join(infolder, self.get_display_name()) + ".json", 'w').write(text)
    def get_display_name(self):
        if self.testnb > 1:
            return self.instrname + "_%d" % self.testnb
        else:
            return self.instrname

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

def mccode_test(branchdir, testdir, limitinstrs=None):
    ''' this main test function tests the given mccode branch/version '''

    # copy instr files and record info
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
        numtests = len([t for t in instrtests if t.testnb > 0]) 
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
        # if binary exists, set compile time = 0 and continue
        binfile = os.path.splitext(test.localfile)[0] + ".out"
        if os.path.exists(binfile):
            test.compiled = True
            test.compiletime = 0
        else:
            log = LineLogger()
            t1 = time.time()
            cmd = "mcrun --info %s" % test.localfile
            utils.run_subtool_to_completion(cmd, cwd=join(testdir, test.instrname), stdout_cb=log.logline, stderr_cb=log.logline)
            t2 = time.time()
            test.compiled = os.path.exists(binfile)
            test.compiletime = t2 - t1

            # log to terminal
            if test.compiled:
                formatstr = "%-" + "%ds: " % maxnamelen + \
                    "{:3d}.".format(math.floor(test.compiletime)) + str(test.compiletime-int(test.compiletime)).split('.')[1][:2]
                logging.info(formatstr % test.get_display_name())
            else:
                formatstr = "%-" + "%ds: COMPILE ERROR using:\n" % maxnamelen
                logging.info(formatstr % test.instrname + cmd)

            # save compile stdout/stderr
            log.save(join(testdir, test.instrname, "compile_stdout.txt"))

        # save (incomplete) test results to disk
        test.save(infolder=join(testdir, test.instrname))

    # run, record time
    logging.info("")
    logging.info("Running tests...")
    for test in tests:
        if not test.compiled:
            logging.info("%s did not compile" % test.instrname)
            continue
        
        log = LineLogger()

        # runable tests have testnb > 0
        if test.testnb <= 0:
            formatstr = "%-" + "%ds:   NO TEST" % (maxnamelen+1)
            logging.info(formatstr % test.get_display_name())
            continue

        # run the test, record time and runtime success/fail
        t1 = time.time()
        cmd = "mcrun %s %s -d%d" % (test.localfile, test.parvals, test.testnb)
        retcode = utils.run_subtool_to_completion(cmd, cwd=join(testdir, test.instrname), stdout_cb=log.logline, stderr_cb=log.logline)
        t2 = time.time()
        test.didrun = not log.find("error:") or retcode != 0
        test.runtime = t2 - t1

        # record run stdout/err
        log.save(join(testdir, test.instrname, "run_stdout.txt"))

        # log to terminal
        if test.didrun:
            formatstr = "%-" + "%ds: " % (maxnamelen+1) + \
                "{:3d}.".format(math.floor(test.runtime)) + str(test.runtime-int(test.runtime)).split('.')[1][:2]
            logging.info(formatstr % test.get_display_name())
        else:
            formatstr = "%-" + "%ds: RUNTIME ERROR" % (maxnamelen+1)
            logging.info(formatstr % instrname + ", " + cmd)
            continue

        # target value extraction: look for a matching entry in mccode.sim, then select the filename in the same entry/of the same index
        lns = open(join(testdir, test.instrname, str(test.testnb), "mccode.sim")).read().splitlines()
        componentlines = [l for l in lns if re.match("  component:", l)]
        filenamelines = [l for l in lns if re.match("  filename:", l)]
        idx = 0
        for l in componentlines:
            if re.match("  component: %s" % test.detector, l):
                break
            idx = idx + 1
        try:
            filename = re.match("\s*filename:\s+(.+)", filenamelines[idx]).group(1)
        except:
            # is test.detector istead a valid filename?
            detector_was_a_filename = False
            if re.search("\.", test.detector):
                filename = test.detector
                detector_was_a_filename = os.path.isfile(join(testdir, test.instrname, str(test.testnb), filename))
            # neither an entry in mccode.sim, nor a file of the same name was found, print an error message and continue
            if not detector_was_a_filename:
                msg = "ERROR: targetval for monitor name %s could not be extracted from instr. %s" % (test.detector, test.instrname)
                test.errmsg = msg
                logging.info(msg)
                continue

        # extract tested target value from the monitor file
        with open(join(testdir, test.instrname, str(test.testnb), filename)) as fp:
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

        # save test result to disk
        test.testcomplete = True
        test.save(infolder=join(testdir, test.instrname))

    # let the outside world create full report, just return test objects as a json obj
    return [t.get_json_repr() for t in tests]

#
# Utility
#

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
    ''' clean up path changes, restoring oldpath '''
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

#
# Program functions for every main test mode
#

def run_default_test(testroot, mccoderoot, limit):
    ''' tests the default mccode version '''

    # get default/system version number
    logger = LineLogger()
    utils.run_subtool_to_completion("mcrun --version", stdout_cb=logger.logline)
    version = logger.lst[-1].strip()

    # create single-run test directory
    testdir = create_test_dir(testroot, utils.get_datetimestr(), version)

    logging.info("Testing: %s" % version)
    logging.info("")
    results = mccode_test(os.path.join(mccoderoot, version), testdir, limit)
    
    reportfile = os.path.join(testdir, "testresults_%s.json" % version)
    open(os.path.join(reportfile), "w").write(json.dumps(results, indent=2))

    logging.debug("")
    logging.debug("Test results written to: %s" % reportfile)


def run_version_test(testroot, mccoderoot, limit, version):
    ''' as run_default_test, but activates/deactivates and ses a specific mccode version if it exists '''

    # verify that version exists
    if not os.path.isfile(os.path.join(mccoderoot, version, "environment")):
        print("mccode version %scould not be found, exiting..." % version)
        quit(1)

    # create single-run test directory
    testdir = create_test_dir(testroot, utils.get_datetimestr(), version)

    oldpath = activate_mccode_version(version, mccoderoot)
    try:
        logging.info("Testing: %s" % version)
        logging.info("")

        results = mccode_test(os.path.join(mccoderoot, version), testdir, limit)
    finally:
        deactivate_mccode_version(oldpath)

    reportfile = os.path.join(testdir, "testresults_%s.json" % version)
    open(os.path.join(reportfile), "w").write(json.dumps(results, indent=2))

    logging.debug("")
    logging.debug("Test results written to: %s" % reportfile)


def run_configs_test(testroot, mccoderoot, limit):
    '''
    Test a suite of configs, each a mccode_config_LABEL.py file, that is copied to the dist dir
    prior to starting the test. This action modifies the C-flags and the compiler used during
    the test. The original mccode_config.py file is restored after each test.
    '''

    def activate_config(version, mccoderoot, configfile):
        ''' activate a confige given by configfile, returns bckfile for use with deactivate_config '''
        libdir = join(mccoderoot, version, "tools", "Python", "mccodelib")
        os.rename(join(libdir, "mccode_config.py"), join(libdir, "mccode_config.py_BAK"))
        open(join(libdir, "mccode_config.py"), "w").write(open(configfile).read())
        return join(libdir, "mccode_config.py_BAK")
    
    def deactivate_config(bckfile):
        ''' use to restore changes made by activate_config '''
        restoreto = join(os.path.dirname(bckfile), "mccode_config.py")
        os.rename(bckfile, restoreto)
    
    def extract_config_mccode_version(configfile):
        for l in open(configfile).read().splitlines():
            m = re.match("\s*\"MCCODE_VERSION\": (.+),", l)
            if m:
                return m.group(1).strip("'")
    
    def get_config_files():
        ''' look in "__file__/../mccodelib/MCCODE-test" location or config files'''
        lookin = join(os.path.dirname(__file__), "..", "mccodelib", mccode_config.configuration["MCCODE"] + "-test")
        for (_, _, files) in os.walk(lookin):
            return [join(lookin, f) for f in files]

    # get test directory datetime string
    datetime = utils.get_datetimestr()

    # test labels loop
    for f in get_config_files():
        version = extract_config_mccode_version(f)
        label = os.path.splitext(os.path.basename(f))[0].lstrip("mccode_config")

        oldpath = activate_mccode_version(version, mccoderoot)
        try:
            bckfile = activate_config(version, mccoderoot, f)
            try:
                logging.info("")
                logging.info("Testing label: %s" % label)

                # craete the proper test dir
                testdir = create_test_dir(testroot, datetime, label)
                results = mccode_test(os.path.join(mccoderoot, version), testdir, limit)

                # write local test result
                reportfile = os.path.join(testdir, "testresults_%s.json" % label)
                open(os.path.join(reportfile), "w").write(json.dumps(results, indent=2))
            
                logging.debug("")
                logging.debug("Test results written to: %s" % reportfile)
            finally:
                deactivate_config(bckfile)
        finally:
            deactivate_mccode_version(oldpath)


def show_installed_versions(mccoderoot):
    ''' utility function, prints identified mccode versions to console '''

    def print_to_console(str):
        ''' used with popen wrapper '''
        logging.info(str)

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
        # test environment
        branchdir = join(mccoderoot, v)
    
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

