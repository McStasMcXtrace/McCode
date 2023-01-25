#!/usr/bin/env python3
# -*- coding: utf-8 -*-
from dataclasses import dataclass
from typing import Union
from pathlib import Path

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
def config_override(version: str):
    flavour = mccode_config.configuration['MCCODE']
    return Path(__file__).parent.joinpath("mccodelib", f"{flavour}-test", version).resolve()


def regular_expression(key: str):
    from re import compile
    parameters = "(?P<parameters>[^\n]*)"
    monitor = "(?P<monitor>[^\n]+)_I"
    not_end_comment_or_end_line = r"(?:\*(?!/)|[^\*\n])"
    inner = f"(?:{not_end_comment_or_end_line}+|\\[{not_end_comment_or_end_line}+\\])"
    intensity = f"(?P<intensity>{inner})"
    error = f"\\s*\\2_ERR\\s*=\\s*(?P<error>{inner})"
    case = f"{key}:{parameters}Detector:\\s*{monitor}\\s*=\\s*{intensity}(?:{error})?"
    case = f"(?P<case>{case})"
    return compile(case)


def filter_comments(contents: str):
    """Concatenate across continuation lines, then return only the comment line(s)"""
    from re import compile, DOTALL, finditer
    # Concatenate continuation lines
    contents = ' '.join(contents.split('\\\n'))
    # Match all C-style comments: blocks and lines
    comment = compile(r"(?:/\*(?:\*(?!/)|[^\*])*\*/|//[^\n]*\n)", DOTALL)
    # recombine string with only the comments
    comments = '\n'.join([x.group(0) for x in finditer(comment, contents)])
    return comments

TEST_CASE_REGEX = regular_expression("%Example")

@dataclass
class InstrExampleTestCase:
    case: str
    parameters: str
    monitor: str
    intensity:  Union[float, list[float]]
    error:  Union[None, float, list[float]] = None
    result: Union[None, float, list[float]] = None
    ran: Union[None, bool] = None
    runtime: Union[None, float] = None

    def __post_init__(self):
        self.parameters = self.parameters.strip()
        if isinstance(self.intensity, str):
            self.intensity = eval(self.intensity, None, None)
        if self.error is not None and isinstance(self.error, str):
            self.error = eval(self.error, None, None)
        if self.error is not None and not isinstance(self.error, type(self.intensity)):
            raise RuntimeError("Intensity and errors must evaluate to the same type!")
        # Stash-away whether this test is a scan
        self._is_scan = '-N' in self.parameters and (hasattr(self.intensity, '__len__') and len(self.intensity) > 0)

    @classmethod
    def scan(cls, header):
        from re import finditer
        tests = [cls(**match.groupdict()) for match in finditer(TEST_CASE_REGEX, header)]
        return tests

    def json_repr(self):
        return dict(parvals=self.parameters, detector=self.monitor, targetval=self.intensity, testval=self.result)

    def run(self, working_directory, cmd: str, no_particles: Union[int,float], test_no: int):
        logging.debug(f"Run test case {self.case}\nwith parameters {self.parameters} and monitor {self.monitor}")
        # cmd should have everything up through " -s 1000 [filename]"
        global openacc, mpi
        import time
        from pathlib import Path
        t0 = time.time_ns()
        cmd += f" {self.parameters} -n{no_particles}"
        if mpi is not None:
            if openacc is True:
                cmd += " --openacc"
            cmd += f" --mpi={mpi}"
        run_log = working_directory.joinpath(f"run_stdout_{test_no}.txt").resolve()
        cmd += f" -d {test_no} &> {run_log}"
        code = utils.run_subtool_noread(cmd, cwd=working_directory)
        t1 = time.time_ns()
        output_path = Path(working_directory).joinpath(f"{test_no}").resolve()
        self.result = self.extract_values(output_path)[0]  # TODO Use the error and number of particles information?
        self.ran = (code != 0 or output_path.joinpath("mccode.sim").is_file()) and self.result is not None
        self.runtime = (t1 - t0) / 1e9  # nanoseconds to seconds
        with open(run_log, 'r') as file:
            logging.info(file.read())

        return output_path

    def extract_values(self, directory):
        # Select begin XXX ... end XXX blocks:
        sim_ex = re.compile(r"begin (?P<environment>[a-zA-Z]+):?(?P<contents>(?:(?!begin)|.)+)end \1", re.DOTALL)

        def get_monitor_filename(root):
            with open(root.joinpath('mccode.sim')) as file:
                string = file.read()
            for match in re.finditer(sim_ex, string):
                if match.group('environment') == 'data':
                    lines = match.group('contents').split('\n')
                    data = {k.strip(): v.strip() for k, v in [n.split(': ') for n in lines if ': ' in n]}
                    if 'component' in data and data['component'] == self.monitor:
                        monitor_path = root.joinpath(data.get('filename', self.monitor + '.dat'))
                        return monitor_path if monitor_path.is_file() else None
            return None

        def get_monitor_values(root: Path):
            file_path = get_monitor_filename(root)
            if file_path is None:
                logging.debug(f"{directory} {self.monitor} file is 'None' -- did the simulation actually run?")
                return None, None, None
            if not file_path.is_file():
                logging.debug(f"Monitor file {file_path} does not exist")
                return None, None, None
            with open(file_path, 'r') as monitor_file:
                monitor_lines = monitor_file.readlines()
            monitor_lines = list(filter(lambda x: '# values:' in x, monitor_lines))
            if len(monitor_lines) != 1:
                logging.debug(f"{len(monitor_lines)} values in {file_path}")
            if len(monitor_lines) == 0:
                return None, None, None
            return [float(x) for x in monitor_lines[0].strip().split(': ')[1].split()]

        points = 0
        if self._is_scan:
            with open(directory.joinpath('mccode.sim')) as file:
                lines = file.readlines()
            lines = list(filter(lambda x: "Numpoints:" in x, lines))
            points = int(lines[0].strip().split(': ')[1]) if len(lines) == 1 else 0

        if points:
            values, errors, counts = zip(*[get_monitor_values(directory.joinpath(f'{p}')) for p in range(points)])
            return values, errors, counts

        return tuple(get_monitor_values(directory))


class InstrExampleTests:
    def __init__(self, sourcefile, localfile, max_filename=160):
        self.sourcefile = sourcefile
        self.localfile = localfile
        with open(localfile, 'r') as file:
            contents = file.read()
        self.tests = InstrExampleTestCase.scan(filter_comments(contents))
        self.compiled = None
        self.compiletime = 0
        self.didrun = None
        self.runtime = None
        self.errmsg = None

        self.log_name = "{fn:<{fm}}".format(fn=str(localfile), fm=f"{max_filename:d}s")[:max_filename]
        no_test = "TEST" if len(self.tests) < 2 else f"TEST {len(self.tests)}"
        if len(self.tests) < 1:
            no_test = "NO TEST"
        logging.debug(f"{self.log_name}: {no_test}")

    @property
    def instrname(self):
        return splitext(basename(self.sourcefile))[0]

    def json_repr(self):
        def base(extend: dict, n=None):
            source = str(self.sourcefile)
            local = str(self.localfile)
            d = dict(displayname=self.instrname if n is None else f"{self.instrname}_{n}",
                     sourcefile=source, localfile=local, instrname=self.instrname,
                     testnb=1 if n is None else n, compiled=self.compiled, compiletime=self.compiletime,
                     didrun=self.didrun, runtime=self.runtime, errmsg=self.errmsg)
            d.update(extend)
            return d
        return [base(x.json_repr(), n if len(self.tests) > 1 else None) for n, x in enumerate(self.tests)]

    def json_dict(self):
        return {x['displayname']: x for x in self.json_repr()}

    def save(self, directory):
        for content in self.json_repr():
            with open(directory.joinpath(content['displayname'] + '.json'), 'w') as file:
                file.write(json.dumps(content))

    def compile(self, version=None):
        binary = self.localfile.with_suffix('.out')  # TODO: change this to be platform specific
        if binary.is_file():
            self.compiled = True
        elif self.compiled is None:
            logging.info(f"{binary} does not exist (yet)")
            from pathlib import Path
            import time
            global mpi, openacc
            t0 = time.time_ns()
            cmd = mccode_config.configuration["MCRUN"]
            if version is not None:
                cmd += f" --override-config={config_override(version)}"
            if openacc:
                cmd += " --openacc "
            if mpi:
                cmd += " --mpi=1 "
            cmd += f" --verbose -c -n0 {self.localfile.resolve()} &> compile_stdout.txt"
            utils.run_subtool_noread(cmd, cwd=self.localfile.parent)
            t1 = time.time_ns()
            self.compiled = binary.is_file()
            self.compiletime = (t1 - t0) / 1e9  # convert from nanoseconds to seconds
            if self.compiled:
                logging.info(f"{self.log_name}: {self.compiletime:3.2f} s")
            else:
                logging.info(f"{self.log_name}: COMPILE ERROR using\n{cmd}")

    def test(self, version, no_particles):
        if not self.compiled:
            logging.info(f"{self.log_name} NO COMPILE")
            return
        if len(self.tests) < 1:
            logging.info(f"{self.log_name} NO TEST")
            return

        cmd = mccode_config.configuration["MCRUN"]
        if version:
            cmd += f" --override-config={config_override(version)}"
        cmd += f" -s 1000 {self.localfile.resolve()}"

        for number, to_test in enumerate(self.tests):
            to_test.run(self.localfile.parent, cmd, no_particles, number)
            msg = f"{to_test.runtime:3.2f} s" if to_test.ran else "RUNTIME ERROR"
            logging.info(f"{self.log_name}({number+1:2d}/{len(self.tests):2d}): {msg}")

        self.save(self.localfile.parent)


class LineLogger:
    """ log lines to memory, then save to disk """
    def __init__(self):
        self.lst = []

    def logline(self, line):
        self.lst.append(line)

    def save(self, filename):
        with open(filename, 'w') as file:
            file.write('\n'.join(self.lst) + '\n')

    def find(self, sub):
        for line in self.lst:
            if re.search(sub, line):
                return True
        return False


def mccode_test(branchdir: Path, testdir: Path, limitinstrs=None, instrfilter=None, version=None):
    """ this main test function tests the given mccode branch/version """
    from pathlib import Path
    from shutil import copy

    # copy instr files and record info
    logging.info(f"Finding instruments in: {branchdir}\n")
    instrs, _ = utils.get_instr_comp_files(branchdir.joinpath("examples"), recursive=True, instrfilter=instrfilter)
    instrs.sort()

    # limt runs if required
    if limitinstrs:
        instrs = instrs[:limitinstrs]

    if len(instrs) == 0:
        logging.info("No instruments found for testing! Check configuration")
        exit(0)

    # max instr name length for pretty-output
    #maxnamelen = max([len(basename(file)) for file in instrs]) - 5
    maxnamelen = max([len(str(file)) for file in instrs])

    # create test objects and copy instrument files
    logging.info("Copying instruments to: %s" % testdir)
    tests = []
    for f in instrs:
        file = Path(f)
        # create the test folder for this instrument
        instrument_directory = Path(testdir).joinpath(file.stem)
        # if instrument_directory.is_dir():
        #     raise RuntimeError(f"The specified testing directory {instrument_directory} already exists!")
        if not instrument_directory.is_dir():
            instrument_directory.mkdir(parents=True)

        # create a new file with the instr text in it - e.g. a local copy of the instrument file
        new_file = instrument_directory.joinpath(file.name)
        copy(file, new_file)

        # create a test object for every test defined in the instrument header
        # (automatically logs test information at debug level)
        instrument_tests = InstrExampleTests(file, new_file, maxnamelen)
        tests.append(instrument_tests)

    # compile, record time
    global ncount, mpi, openacc
    logging.info("")
    logging.info("Compiling instruments [seconds]...")
    for test in tests:
        test.compile(version)
        # save (incomplete) test results to disk
        test.save(test.localfile.parent)

    # run, record time
    logging.info("")
    logging.info("Running tests...")
    for test in tests:
        test.test(version, ncount)

    #    cpu type: cat /proc/cpuinfo |grep name |uniq | cut -f2- -d: 
    #    gpu type: nvidia-smi -L | head -1 |cut -f2- -d: |cut -f1 -d\(

    metalog = LineLogger()
    utils.run_subtool_to_completion("cat /proc/cpuinfo |grep name | uniq | cut -f2- -d: | xargs echo", stdout_cb=metalog.logline)
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
        obj.update(t.json_dict())
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


def create_test_dir(test_dir: Path):
    """ just create test_dir or exit """
    if not test_dir.is_dir():
        test_dir.mkdir(parents=True)


def create_label_dir(test_dir: Path, label: str):
    label_dir = test_dir.joinpath(label)
    if not label_dir.is_dir():
        label_dir.mkdir(parents=True)
    return label_dir


def create_datetime_testdir(testroot):
    datetime = utils.get_datetimestr()
    return create_label_dir(testroot, datetime)

#
# Program functions for every main test mode
#

def write_report(label_dir, version, results):
    report_file = label_dir.joinpath(f"testresults_{version}.json")
    with open(os.path.join(report_file), "w") as file:
        file.write(json.dumps(results, indent=2))
    logging.debug("")
    logging.debug(f"Test results written to: {report_file}")


def run_default_test(testdir: Path, mccoderoot: Path, limit, instrfilter):
    """ tests the default mccode version """
    # get default/system version number
    logger = LineLogger()
    cmd = f"{mccode_config.configuration['MCRUN']} --version"
    utils.run_subtool_to_completion(cmd, stdout_cb=logger.logline)
    if len(logger.lst) < 1:
        logging.info(f"no '{cmd}' output, try using --configs")
        quit(1)

    version = logger.lst[-1].strip()
    # create single-run test directory
    label_dir = create_label_dir(testdir, version)

    logging.info(f"Testing: {version}")
    logging.info("")
    results = mccode_test(mccoderoot.joinpath(version), label_dir, limit, instrfilter)

    write_report(label_dir, version, results)


def run_version_test(testdir: Path, mccoderoot: Path, limit, instrfilter, version):
    ''' as run_default_test, but activates/deactivates and ses a specific mccode version if it exists '''

    # verify that version exists
    if not mccoderoot.joinpath(version, "environment").is_file():
        logging.info(f"mccode version {version} could not be found, exiting...")
        quit(1)

    # create single-run test directory
    label_dir = create_label_dir(testdir, version)

    old_path = activate_mccode_version(version, mccoderoot)
    try:
        logging.info(f"Testing: {version}")
        logging.info("")
        results = mccode_test(mccoderoot.joinpath(version), label_dir, limit, instrfilter, version)
    finally:
        deactivate_mccode_version(old_path)

    write_report(label_dir, version, results)


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
                return m.group(1).strip("\""), os.path.basename(os.path.dirname(configfile))
    
    def get_config_files(configfltr):
        ''' look in "__file__/../mccodelib/MCCODE-test" location or config files'''
        lookin = join(os.path.dirname(__file__), "..", "mccodelib", mccode_config.configuration["MCCODE"] + "-test")
        print("getting config files...")
        print(configfltr + " vs " + os.path.join(lookin,configfltr,'mccode_config.json'))
        if configfltr is not None and os.path.isfile(os.path.join(lookin,configfltr,'mccode_config.json')):
            print("returning " + os.path.join(lookin,configfltr,'mccode_config.json'))
            return [ os.path.join(lookin,configfltr,'mccode_config.json') ]
        for (_, _, files) in os.walk(lookin):
            print("Looking")
            if configfltr is not None:
                return [join(lookin, f) for f in files if re.search("^%s/mccode_config.json$" % configfltr, f)]
            else:
                return [join(lookin, f) for f in files if re.search("^mccode_config.json$", f)]

    # get test directory datetime string
    datetime = utils.get_datetimestr()

    # test labels loop
    for f in get_config_files(configfilter):
        [version, label] = extract_config_mccode_version(f)

        oldpath = activate_mccode_version(version, mccoderoot)
        try:
            #bckfile = activate_config(version, mccoderoot, f)
            try:
                logging.info("")
                label0 = label
                label = label + "_" + ncount
                logging.info(f"Testing label: {label}")
                # create the proper test dir
                label_dir = create_label_dir(testdir, label)
                results = mccode_test(mccoderoot.joinpath(version), label_dir, limit, instrfilter, label0)
                # write local test result
                write_report(label_dir, version, results)
            finally:
                pass
        finally:
            deactivate_mccode_version(oldpath)


def show_installed_versions(mccoderoot: Path):
    """ utility function, prints identified mccode versions to console """

    def print_to_console(str):
        """ used with popen wrapper """
        logging.info(str)

    flavour = mccode_config.configuration["MCCODE"]
    logging.info(f"Test environment mode, using output of '{flavour} --vesion'")
    logging.info("")

    # collect all folders which contain an 'environment' file
    branchnames = [x.parent for x in mccoderoot.rglob('environment') if x.is_file()]

    for branchdir in branchnames:
        # test environment
        os.environ["MCSTAS"] = str(branchdir)
        old_path = os.environ["PATH"]
        os.environ['PATH'] = f"{branchdir.joinpath('miniconda3','bin')}:{branchdir.joinpath('bin')}:{old_path}"
    
        # run the mcstas --version command
        cmd = f"{flavour} --version"
        utils.run_subtool_to_completion(cmd, stdout_cb=print_to_console, stderr_cb=print_to_console)
        logging.info("")
    
        # TODO: should we test the existence of m[xc]run?
    
        # restore environment
        del os.environ["MCSTAS"]
        os.environ["PATH"] = old_path

    version_names = ','.join([x.name for x in branchnames])
    logging.info("Selectable version names are: {version_names}")
    logging.info("")

ncount = None
mpi = None
openacc = None


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
        testroot = Path("/tmp/mctest")
    if not testdir:
        testdir = create_datetime_testdir(testroot)
        logging.debug(f"Using test root:         {testroot}")
    else:
        if not isinstance(testdir, Path) and isinstance(testdir, str):
            testdir = Path(testdir)
        create_test_dir(testdir)
        logging.debug(f"Using explicit test dir: {testdir}")

    if not mccoderoot:
        mccoderoot = Path("/usr/share/mcstas/")
    if isinstance(mccoderoot, str):
        mccoderoot = Path(mccoderoot)
    if not mccoderoot.is_dir():
        logging.info("mccoderoot does not exist")
        quit(1)
    logging.debug(f"Using mccode root:       {mccoderoot}")
    if limit:
        try:
            limit = int(args.limit[0])
        except ValueError as ex:
            logging.debug(f"{ex}")
            logging.info("--limit must be a number")
            quit(1)
    logging.debug("")

    global ncount, mpi, skipnontest, openacc
    if args.ncount:
        ncount = args.ncount[0]
    else:
        ncount = "1e6"
    logging.info(f"ncount is: {ncount}")
    if args.mpi:
        mpi = args.mpi[0]
        logging.info(f"mpi count is: {mpi}")
    if args.openacc:
        openacc = True
        logging.info("openacc is enabled")

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
    flavour = mccode_config.configuration['MCCODE']
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('testversion', nargs="?", help='mccode version to test')
    parser.add_argument('--ncount', nargs=1, help=f'ncount sent to {flavour}')
    parser.add_argument('--mpi', nargs=1, help=f'mpi nodecount sent to {flavour}')
    parser.add_argument('--openacc', action='store_true', help=f'openacc flag sent to {flavour}')
    parser.add_argument('--configs', action='store_true', help=f'test config files under mccodelib/{flavour}-test')
    parser.add_argument('--config', nargs="?",
                        help='test this specific config only - label name or absolute path (enables --configs)')
    parser.add_argument('--instr', nargs="?", help='test only instruments matching this filter (py regex)')
    parser.add_argument('--mccoderoot', nargs='?', help='manually select root search folder for mccode installations')
    parser.add_argument('--testroot', nargs='?', help='output test results in a datetime folder in this root')
    parser.add_argument('--testdir', nargs='?', help='output test results directly in this dir (overrides testroot)')
    parser.add_argument('--limit', nargs=1, help='test only the first [LIMIT] instruments in every version')
    parser.add_argument('--versions', action='store_true', help='display local versions info')
    parser.add_argument('--verbose', action='store_true',
                        help='output a test/notest instrument status header before each test')
    parser.add_argument('--skipnontest', action='store_true', help='Skip compilation of instruments without a test')

    args = parser.parse_args()

    try:
        main(args)
    except KeyboardInterrupt:
        print()

