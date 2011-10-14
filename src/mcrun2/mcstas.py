
import tempfile
import atexit
import os
import logging

from os.path import isfile, dirname, basename, splitext
from subprocess import Popen, PIPE


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
        LOG.debug('CMD: %s %s' % (self.executable, args))
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

        # Setup paths
        self.dir = tempfile.mkdtemp(prefix='mcstas-')
        self.cpath = '%s/%s.c' % (self.dir, self.name)
        self.binpath = '%s/%s.out' % (dirname(self.path), self.name)

        # Remove temporary files at exit
        atexit.register(self.cleanup)


    def prepare(self, options):
        ''' Prepare for simultation run '''
        self.options = options

        # Use mpi?
        mpi = options.use_mpi

        self.binpath += (mpi and '-mpi' or '')

        # Check if instrument code has changed
        if not options.force_compile and isfile(self.binpath) \
               and modified(self.path) < modified(self.binpath):
            LOG.info('Using existing binary: %s' % self.binpath)
            return  # skip
        LOG.info('Recompiling: %s' % self.binpath)

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
        args += options.params

        # Run McStas
        if not mpi:
            LOG.info('Running: %s' % self.binpath)
            Process(self.binpath).run(args, pipe=pipe)
        else:
            LOG.info('Running via MPI: %s' % self.binpath)
            mpi_args = ['-np', str(options.mpi), self.binpath]
            mpi_args += args
            Process(options.mpirun).run(mpi_args, pipe=pipe)


    def cleanup(self):
        ''' Remove temporary files '''
        for path in (self.cpath,):
            try:
                os.remove(path)
            except OSError:
                pass  # file not found

        os.rmdir(self.dir)

