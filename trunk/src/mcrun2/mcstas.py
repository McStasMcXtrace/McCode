
import tempfile
import atexit
import os
import logging

from os.path import isfile, dirname, basename, splitext
from subprocess import Popen, PIPE


LOG = logging.getLogger('mcstas.mcstas')


def modified(path):
    return os.stat(path).st_mtime


class ProcessException(Exception):
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
    def __init__(self, executable):
        self.executable = executable

    def run(self, args=None):
        if args is None:
            args = []

        # Run executable
        LOG.debug('CMD: %s %s' % (self.executable, args))
        fd = Popen([self.executable] + args,
                   stdout=None,
                   stderr=None)
        stdout, stderr = fd.communicate()

        # Check if process terminated correctly
        retval = fd.wait()
        if retval != 0:
            raise ProcessException(self.executable, args, retval)

        return stdout


class McStas:
    def __init__(self, instrument_file):
        if not isfile(instrument_file):
            raise IOError('No such instrument file: "%s"' % instrument_file)
        self.path = instrument_file
        self.name = splitext(basename(self.path))[0]

        # Setup paths
        self.dir = tempfile.mkdtemp(prefix='mcstas-')
        self.cpath = '%s/%s.c' % (self.dir, self.name)
        self.binpath = '%s/%s.out' % (dirname(self.path), self.name)

        # Remove temporary files at exit
        atexit.register(self.cleanup)


    def prepare(self, options):
        ''' Prepare for simultation run '''
        # Check if instrument code has changed
        if not options.force_compile and isfile(self.binpath) \
               and modified(self.path) < modified(self.binpath):
            LOG.info('Using existing binary: %s' % self.binpath)
            return  # skip

        # Proceed with generation and compilation
        LOG.info('Recompiling: %s' % self.binpath)
        Process('mcstas').run(['-o', self.cpath, self.path])

        cc = 'gcc'

        # Compiler optimisation
        opt = options.no_cflags and '-O0' or '-O2'
        args = ['-o', self.binpath, opt, '-lm', self.cpath]

        Process(cc).run(args)


    def run(self, options):
        ''' Run simulation '''
        args = []
        print repr(options)

        # Handle proxy options, pass through
        proxy_opts_val = ['seed', 'ncount', 'dir', 'file', 'format']
        proxy_opts_flags = ['trace', 'gravitation', 'data-only',
                            'no-output-files', 'info']

        for opt in proxy_opts_val:
            val = getattr(options, opt.replace('-', '_'))
            if val is not None:
                args.extend(['--%s' % opt, str(val)])

        for opt in proxy_opts_flags:
            val = getattr(options, opt.replace('-', '_'))
            if val:
                args.append('--%s' % opt)

        # Run McStas
        LOG.info('Running: %s' % self.binpath)
        Process(self.binpath).run(args)


    def cleanup(self):
        for path in (self.cpath,):
            try:
                os.remove(path)
            except OSError:
                pass # file not found

        os.rmdir(self.dir)


if __name__ == '__main__':
    formatter = logging.Formatter('%(asctime)s - %(message)s')

    ch = logging.StreamHandler()
    ch.setLevel(logging.INFO)
    ch.setFormatter(formatter)

    LOG.setLevel(logging.DEBUG)
    LOG.addHandler(ch)

    m = McStas('/home/jos/diku/McCode/trunk/src/McStasTest/examples/ILL_D4.instr')
    m.prepare()
    m.run()
