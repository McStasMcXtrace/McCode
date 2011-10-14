
import atexit
import logging
import os
import re
import tempfile
import yaml

from os.path import isfile, dirname, basename, splitext
from subprocess import Popen, PIPE
from decimal import Decimal


LOG = logging.getLogger('mcstas.mcstas')


def modified(path):
    ''' Get modification time of path in seconds '''
    return os.stat(path).st_mtime


class ProcessException(Exception):
    ''' Exception/error in external process '''

    def __init__(self, executable, args, retval):
        Exception.__init__(self)
        self.executable = executable
        self.args = args
        self.retval = retval

    def __str__(self):
        return 'Got exit status %s from "%s %s"' % (self.retval,
                                                    self.executable,
                                                    ' '.join(self.args))


class Process:
    ''' An external process '''

    def __init__(self, executable):
        self.executable = executable

    def run(self, args=None, pipe=False):
        ''' Run external process with args '''

        # Unsafe to use [] as default (reference)
        if args is None:
            args = []

        # Redirect stdout and stderr?
        pipe = pipe and PIPE or None

        # Run executable
        LOG.debug('CMD: %s %s', self.executable, ' '.join(args))
        fid = Popen([self.executable] + args,
                    stdout=pipe,
                    stderr=pipe)
        stdout, stderr = fid.communicate()

        # Check if process terminated correctly
        retval = fid.wait()
        if retval != 0:
            raise ProcessException(self.executable, args, retval)

        return stdout


class McStas:
    ''' McStas instrument '''

    def __init__(self, instrument_file):
        if not isfile(instrument_file):
            raise IOError('No such instrument file: "%s"' % instrument_file)
        self.path = instrument_file
        self.name = splitext(basename(self.path))[0]
        self.options = None
        self.params = {}

        # Setup paths
        self.dir = tempfile.mkdtemp(prefix='mcstas-')
        self.cpath = '%s/%s.c' % (self.dir, self.name)
        self.binpath = '%s/%s.out' % (dirname(self.path), self.name)

        # Remove temporary files at exit
        atexit.register(self.cleanup)

    def set_parameter(self, key, value):
        ''' Set the value of an experiment parameter '''
        self.params[key] = value

    def prepare(self, options):
        ''' Prepare for simultation run '''
        self.options = options

        # Use mpi?
        mpi = options.use_mpi
        self.binpath += (mpi and '-mpi' or '')

        # Check if instrument code has changed
        if not options.force_compile and isfile(self.binpath) \
               and modified(self.path) < modified(self.binpath):
            LOG.info('Using existing binary: %s', self.binpath)
            return  # skip
        LOG.info('Recompiling: %s', self.binpath)

        # Generate C-code
        Process('mcstas').run(['-o', self.cpath, self.path])

        # Setup cflags
        cflags = [
            '-lm',  # math library
            options.no_cflags and '-O0' or '-O2',  # optimizing flags
            mpi and '-DUSE_MPI' or '-UUSE_MPI',  # MPI
        ]

        # Compiler optimisation
        args = ['-o', self.binpath] + cflags + [self.cpath]
        Process(options.cc).run(args)

    def run(self, pipe=False):
        ''' Run simulation '''
        args = []
        options = self.options
        mpi = self.options.use_mpi

        # Handle proxy options with values
        proxy_opts_val = ['seed', 'ncount', 'dir', 'file', 'format']
        for opt in proxy_opts_val:
            val = getattr(options, opt.replace('-', '_'))
            if val is not None:
                args.extend(['--%s' % opt, str(val)])

        # Handle proxy options without values (flags)
        proxy_opts_flags = ['trace', 'gravitation', 'data-only',
                            'no-output-files', 'info']
        for opt in proxy_opts_flags:
            val = getattr(options, opt.replace('-', '_'))
            if val:
                args.append('--%s' % opt)

        # Add parameters last
        args += ['%s=%s' % (key, value)
                 for key, value in self.params.items()]

        # Run McStas
        if not mpi:
            LOG.info('Running: %s', self.binpath)
            return Process(self.binpath).run(args, pipe=pipe)
        else:
            LOG.info('Running via MPI: %s', self.binpath)
            mpi_args = ['-np', str(options.mpi), self.binpath]
            mpi_args += args
            return Process(options.mpirun).run(mpi_args, pipe=pipe)

    def get_info(self):
        return McStasInfo(Process(self.binpath).run(['--info'], pipe=True))

    def cleanup(self):
        ''' Remove temporary files '''
        for path in (self.cpath,):
            try:
                os.remove(path)
            except OSError:
                pass  # file not found

        os.rmdir(self.dir)


class Detector(object):
    ''' A detector '''
    def __init__(self, name, intensity, error, count, path):
        self.name = name
        self.intensity = Decimal(intensity)
        self.error = Decimal(error)
        self.count = Decimal(count)
        self.path = path


class McStasInfo:
    ''' Parsing McStas experiment information (--info) '''

    PARAMETERS_RE = re.compile(r'^\s*Parameters:(.*)', flags=re.MULTILINE)
    SEPERATOR_RE = re.compile(r'^([^:]+):\s*')
    QUOTE_RE = re.compile(r'^(\s*[^:]+):\s*([^\[\s].*)$', flags=re.MULTILINE)
    GROUP_RE = re.compile(r'begin ([^\s]+)(.+)end \1', flags=re.DOTALL)
    PARAM_RE = re.compile(r'^\s*Param:\s+"', flags=re.MULTILINE)

    def __init__(self, data):
        self.data = data
        self.info = self._parse_info()

    def _parse_info(self):
        """
        Parse the raw McStas info output
        The output resembles YAML but not quite.
        It's converted to YAML by:
          0. Ensuring a space after 'key:' -> 'key: '
          1. Adding qoutes 'key: value' -> 'key: "value"'
          2. Changing 'begin foobar\n ...\n end foobar' -> 'foobar:\n'
          3. Add unique suffix number to each param:
               Param: lambda=0.7
               Param: DM=1.8
               -->
               Param0: lambda=0.7
               Param1: DM=1.8
          4. Split up 'Parameters' to form a list
        """

        def escape(line):
            ''' Escape \ and " '''
            return line.replace('\\', '\\\\').replace('"', r'\"')

        def quote(match):
            ''' Quote a value '''
            return '%s: "%s"' % (match.group(1), escape(match.group(2)))

        def param_number(match):
            ''' Assign unique number to each param '''
            param_number.prev_param_number += 1
            return match.group(0).replace('Param',
                                          'Param%i' % param_number.prev_param_number)
        # start count at 0 (previous is -1)
        setattr(param_number, 'prev_param_number', -1)

        def parameters_to_list(match):
            old_str = match.group(1)
            if old_str.strip():
                new_str = ' [%s]' % ','.join(match.group(1).split())
                return match.group(0).replace(old_str, new_str)
            return match.group(0).strip() + ' []'

        yaml_str = self.data
        yaml_str = self.PARAMETERS_RE.sub(parameters_to_list, yaml_str)
        yaml_str = self.SEPERATOR_RE.sub(r'\1: ', yaml_str)
        yaml_str = self.QUOTE_RE.sub(quote, yaml_str)
        yaml_str = self.GROUP_RE.sub(r'\1:\2', yaml_str)
        yaml_str = self.PARAM_RE.sub(param_number, yaml_str)

        return yaml.load(yaml_str)

    def get(self, key):
        return self.info[key]

    def get_simulation(self):
        return self.get('simulation')

    def get_instrument(self):
        return self.get('instrument')


class McStasResult:
    ''' Parsing of McStas experiment output '''

    DETECTOR_RE = r'Detector: ([^\s]+)_I=([^ ]+) \1_ERR=([^\s]+) \1_N=([^ ]+) "(\1[^"]+)"'

    def __init__(self, data):
        self.data = data
        self.detectors = None

    def get_detectors(self):
        ''' Extract detector information '''
        if self.detectors is not None:
            return self.detectors
        res = re.findall(self.DETECTOR_RE, self.data)

        return [Detector(name, intensity, error, count, path)
                for name, intensity, error, count, path in res]
