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
            targetval = float(m[2].strip())
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

def _monitorname_filename_match(dfolder, monname):
    '''
    mccode.sim is organized in sections, e.g. data sections are coded by from "begin data" to "end data",
    within which "  component:" and "  filename:" tags may be available.
    
    returns the filename or None
    '''
    look_for_filename = False
    lns = open(join(dfolder, "mccode.sim")).read().splitlines()
    for l in lns:
        if re.match("  component: %s$" % monname, l):
            # flag this data section 
            look_for_filename = True
        if look_for_filename:
            m = re.match("\s*filename:\s+(.+)", l)
            if m:
                filename = m.group(1) 
                if os.path.isfile(join(dfolder,filename)):
                    return filename 
            if re.match("end data", l):
                # the filename can for 0D monitors be monname.dat
                zeroDfilename = join(dfolder, monname + ".dat")
                if os.path.isfile(zeroDfilename):
                    return zeroDfilename
                return None

def extract_testvals(datafolder, monitorname):
    '''
    Extract monitor I (as well as Ierr and N) from results dir given monitor name.
    
    Returns an error string or a tuple containing (I, I_err, N), or None if datafile does not contain a "values" line.
    '''
    # get any filename in mccode.sim matching monitorname AKA test.detector 
    filename = _monitorname_filename_match(datafolder, monitorname)
    if filename is None:
        return "ERROR: targetval could not be extracted from monitor %s" % (monitorname)

    # extract tested target value from the monitor file
    with open(join(datafolder, filename)) as fp:
        while True:
            l = fp.readline()
            if not l:
                break
            m = re.match("# values: ([0-9+-e.]+) ([0-9+-e.]+) ([0-9]+)", l)
            if m :
                I = float(m.group(1))
                I_err = float(m.group(2))
                N = float(m.group(3))
                return (I, I_err, N)
                break

def mccode_test(branchdir, testdir, limitinstrs=None, instrfilter=None):
    ''' this main test function tests the given mccode branch/version '''

    # copy instr files and record info
    logging.info("Finding instruments in: %s" % branchdir)
    instrs, _ = utils.get_instr_comp_files(join(branchdir, "examples"), recursive=True, instrfilter=instrfilter)
    instrs.sort()

    # limt runs if required
    if limitinstrs:
        instrs = instrs[:limitinstrs]

    # max instr name length for pretty-output
    maxnamelen = 0
    for f in instrs:
        l = len(basename(f)) - 5
        if l > maxnamelen:
            maxnamelen = l

    # create test objects and copy instrument files
    logging.info("Copying instruments to: %s" % testdir)
    tests = []
    for f in instrs:
        # create the test foldr for this instrument
        instrname = splitext(basename(f))[0]
        instrdir = join(testdir, instrname)
        mkdir(instrdir)

        # create a new file with the instr text in it - e.g. a local copy of the instrument file
        text = open(f, encoding='utf-8').read()
        f_new = join(instrdir, basename(f))
        open(f_new, 'w', encoding='utf-8').write(text)

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
            cmd = "mcrun --info %s &> compile_stdout.txt" % test.localfile
            utils.run_subtool_noread(cmd, cwd=join(testdir, test.instrname))
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
                            
        # save (incomplete) test results to disk
        test.save(infolder=join(testdir, test.instrname))

    # run, record time
    logging.info("")
    logging.info("Running tests...")
    for test in tests:
        if not test.compiled:
            formatstr = "%-" + "%ds:   NO COMPILE" % (maxnamelen+1)
            logging.info(formatstr % test.instrname)
            continue
        
        # runable tests have testnb > 0
        if test.testnb <= 0:
            formatstr = "%-" + "%ds:   NO TEST" % (maxnamelen+1)
            logging.info(formatstr % test.get_display_name())
            continue

        # run the test, record time and runtime success/fail
        t1 = time.time()
        global ncount, mpi
        if mpi is not None:
            cmd = "mcrun -s 1000 %s %s -n%s --mpi=%s -d%d &> run_stdout.txt" % (test.localfile, test.parvals, ncount, mpi, test.testnb)
        else:
            cmd = "mcrun -s 1000 %s %s -n%s -d%d  &> run_stdout.txt" % (test.localfile, test.parvals, ncount, test.testnb)
        retcode = utils.run_subtool_noread(cmd, cwd=join(testdir, test.instrname))
        t2 = time.time()
        didwrite = os.path.exists(join(testdir, test.instrname, str(test.testnb), "mccode.sim"))
        test.didrun = retcode != 0 or didwrite
        test.runtime = t2 - t1

        # log to terminal
        if test.didrun:
            formatstr = "%-" + "%ds: " % (maxnamelen+1) + \
                "{:3d}.".format(math.floor(test.runtime)) + str(test.runtime-int(test.runtime)).split('.')[1][:2]
            logging.info(formatstr % test.get_display_name())
        else:
            formatstr = "%-" + "%ds: RUNTIME ERROR" % (maxnamelen+1)
            logging.info(formatstr % instrname + ", " + cmd)
            continue

        # test value extraction
        extraction = extract_testvals(join(testdir, test.instrname, str(test.testnb)), test.detector)
        if type(extraction) is tuple:
            test.testval = extraction[0]
        else:
            test.testval = -1

        # save test result to disk
        test.testcomplete = True
        test.save(infolder=join(testdir, test.instrname))

    #    cpu type: cat /proc/cpuinfo |grep name |uniq | cut -f2- -d: 
    #    gpu type: nvidia-smi -L | head -1 |cut -f2- -d: |cut -f1 -d\(

    metalog = LineLogger()
    utils.run_subtool_to_completion("cat /proc/cpuinfo |grep name |uniq | cut -f2- -d: | xargs echo", stdout_cb=metalog.logline)
    cpu_type = ",".join(metalog.lst)

    metalog = LineLogger()
    utils.run_subtool_to_completion("nvidia-smi -L | head -1 |cut -f2- -d: |cut -f1 -d\(", stdout_cb=metalog.logline) 
    gpu_type = ",".join(metalog.lst)
    if "failed because" in gpu_type:
        gpu_type = "none"

    metalog = LineLogger()
    utils.run_subtool_to_completion("hostname", stdout_cb=metalog.logline)
    hostnamestr = ",".join(metalog.lst)

    metalog = LineLogger()
    utils.run_subtool_to_completion('echo "$USER"', stdout_cb=metalog.logline)
    username = ",".join(metalog.lst)

    metainfo = OrderedDict()
    metainfo["ncount"] = ncount
    metainfo["mpi"] = mpi
    metainfo["date"] = utils.get_datetimestr()
    metainfo["hostname"] = hostnamestr
    metainfo["user"] = username
    metainfo["cpu_type"] = cpu_type
    metainfo["gpu_type"] = gpu_type

    # displayname must be unique, we can return a dict, which eases comparison between tests
    obj = {}
    for t in tests:
        obj[t.get_display_name()] = t.get_json_repr()
    obj["_meta"] = metainfo
    return obj

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

def create_test_dir(testdir):
    ''' just create testdir or exit '''
    if not os.path.exists(testdir):
        mkdir(testdir)
    if not os.path.exists(testdir):
        logging.info("could not create test folder, exiting...")
        quit()

def create_label_dir(testdir, label):
    if not os.path.exists(testdir):
        mkdir(testdir)
    if not os.path.exists(testdir):
        logging.info("could not create test folder, exiting...")
        quit()
    labeldir = join(testdir, label)
    if not os.path.exists(labeldir):
        mkdir(labeldir)
    return labeldir

def create_datetime_testdir(testroot):
    datetime = utils.get_datetimestr()
    create_label_dir(testroot, datetime)
    return join(testroot, datetime)

#
# Program functions for every main test mode
#

def run_default_test(testdir, mccoderoot, limit, instrfilter):
    ''' tests the default mccode version '''

    # get default/system version number
    logger = LineLogger()
    utils.run_subtool_to_completion("mcrun --version", stdout_cb=logger.logline)
    try:
        version = logger.lst[-1].strip()
    except:
        logging.info("no 'mcstas --version' output, try using --configs")
        quit(1)

    # create single-run test directory
    labeldir = create_label_dir(testdir, version)

    logging.info("Testing: %s" % version)
    logging.info("")
    results = mccode_test(os.path.join(mccoderoot, version), labeldir, limit, instrfilter)
    
    reportfile = os.path.join(labeldir, "testresults_%s.json" % version)
    open(os.path.join(reportfile), "w").write(json.dumps(results, indent=2))

    logging.debug("")
    logging.debug("Test results written to: %s" % reportfile)


def run_version_test(testdir, mccoderoot, limit, instrfilter, version):
    ''' as run_default_test, but activates/deactivates and ses a specific mccode version if it exists '''

    # verify that version exists
    if not os.path.isfile(os.path.join(mccoderoot, version, "environment")):
        print("mccode version %s could not be found, exiting..." % version)
        quit(1)

    # create single-run test directory
    labeldir = create_label_dir(testdir, version)

    oldpath = activate_mccode_version(version, mccoderoot)
    try:
        logging.info("Testing: %s" % version)
        logging.info("")

        results = mccode_test(os.path.join(mccoderoot, version), labeldir, limit, instrfilter)
    finally:
        deactivate_mccode_version(oldpath)

    reportfile = os.path.join(labeldir, "testresults_%s.json" % version)
    open(os.path.join(reportfile), "w").write(json.dumps(results, indent=2))

    logging.debug("")
    logging.debug("Test results written to: %s" % reportfile)


def run_configs_test(testdir, mccoderoot, limit, configfilter, instrfilter):
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
    
    def get_config_files(configfltr):
        ''' look in "__file__/../mccodelib/MCCODE-test" location or config files'''
        lookin = join(os.path.dirname(__file__), "..", "mccodelib", mccode_config.configuration["MCCODE"] + "-test")
        if configfltr is not None and os.path.isfile(configfltr):
            return [configfltr]
        for (_, _, files) in os.walk(lookin):
            if configfltr is not None:
                return [join(lookin, f) for f in files if re.search("^mccode_config_%s\.py$" % configfltr, f)]
            else:
                return [join(lookin, f) for f in files if re.search("^mccode_config_.*\.py$", f)]

    # get test directory datetime string
    datetime = utils.get_datetimestr()

    # test labels loop
    for f in get_config_files(configfilter):
        version = extract_config_mccode_version(f)
        label = os.path.splitext(os.path.basename(f))[0].lstrip("mccode_config")

        oldpath = activate_mccode_version(version, mccoderoot)
        try:
            bckfile = activate_config(version, mccoderoot, f)
            try:
                logging.info("")
                label=label+"_"+ncount
                logging.info("Testing label: %s" % label)

                # craete the proper test dir
                labeldir = create_label_dir(testdir, label)
                results = mccode_test(os.path.join(mccoderoot, version), labeldir, limit, instrfilter)

                # write local test result
                reportfile = os.path.join(labeldir, "testresults_%s.json" % label)
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

ncount = None
mpi = None

def main(args):
    # mutually excusive main branches
    default = None                  # test system mccode version as-is
    version = args.testversion      # test a specific mccode version (also) present on the system
    configs = args.configs          # test all config versions, which are versions of mccode_config.py, located in mccodelib/MCCODE
    configfilter = args.config      # test only config matching this (and enable --configs if --config=... is used)
    if configfilter:
        configs = True
    vinfo = args.versions           # display mccode versions installed on the system

    # modifying options
    verbose = args.verbose          # display more info during runs
    testroot = args.testroot        # use non-default test output root location
    testdir = args.testdir          # use non-default test output location (overrides testroot)
    mccoderoot = args.mccoderoot    # use non-default mccode system install location
    limit = args.limit              # only test the first [limit] instruments (useful for debugging purposes)
    instrfilter = args.instr        # test only matching instrs

    # set modifications first
    if verbose:
        logging.basicConfig(level=logging.DEBUG, format="%(message)s")
    else:
        logging.basicConfig(level=logging.INFO, format="%(message)s")
    if not testroot:
        testroot = "/tmp/mctest"
    if not testdir:
        testdir = create_datetime_testdir(testroot)
        logging.debug("Using test root:         %s" % testroot)
    else:
        create_test_dir(testdir)
        logging.debug("Using explicit test dir: %s" % testdir)

    if not mccoderoot:
        mccoderoot = "/usr/share/mcstas/"
    if not os.path.exists(mccoderoot):
        logging.info("mccoderoot does not exist")
        quit(1)
    logging.debug("Using mccode root:       %s" % mccoderoot)
    if limit:
        try:
            limit = int(args.limit[0])
        except:
            logging.info("--limit must be a number")
            quit(1)
    logging.debug("")

    global ncount, mpi
    if args.ncount:
        ncount = args.ncount[0]
    else:
        ncount = "1e6"
    logging.info("ncount is: %s" % ncount)
    if args.mpi:
        mpi = args.mpi[0]
        logging.info("mpi count is: %s" % mpi)

    # decide and run main branch
    if version and configs or version and vinfo or configs and vinfo:
        print("WARNING: version, --configs and --versions are mutually exclusive, exiting")
        quit()
    default = not version and not configs and not vinfo
    if default:
        run_default_test(testdir, mccoderoot, limit, instrfilter)
    elif version:
        run_version_test(testdir, mccoderoot, limit, instrfilter, version)
    elif configs:
        run_configs_test(testdir, mccoderoot, limit, configfilter, instrfilter)
    elif vinfo:
        show_installed_versions(mccoderoot)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('testversion', nargs="?", help='mccode version to test')
    parser.add_argument('--ncount', nargs=1, help='ncount sent to mcrun')
    parser.add_argument('--mpi', nargs=1, help='mpi nodecount sent to mcrun')
    parser.add_argument('--configs', action='store_true', help='test config files under mccodelib/MCCODE')
    parser.add_argument('--config', nargs="?", help='test this specific config only - label name or absolute path (enables --configs)')
    parser.add_argument('--instr', nargs="?", help='test only intruments matching this filter (py regex)')
    parser.add_argument('--mccoderoot', nargs='?', help='manually select root search folder for mccode installations')
    parser.add_argument('--testroot', nargs='?', help='output test results in a datetime folder in this root')
    parser.add_argument('--testdir', nargs='?', help='output test results directly in this dir (overrides testroot)')
    parser.add_argument('--limit', nargs=1, help='test only the first [LIMIT] instrs in every version')
    parser.add_argument('--versions', action='store_true', help='display local versions info')
    parser.add_argument('--verbose', action='store_true', help='output a test/notest instrument status header before each test')

    args = parser.parse_args()

    try:
        main(args)
    except KeyboardInterrupt:
        print()

