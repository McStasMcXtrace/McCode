'''
mcstas/mcxtrace configuration.
'''
configuration = {
    "MCCODE_VERSION": '2.2a',
    "MCCODE_LIB_DIR": '/usr/share/mcstas/2.2a/',
    "MCCODE": 'mcstas',
    "MCRUN": 'mcrun-py',
    "MCPLOT": 'mcplot-gnuplot-py',
    "MCDISPLAY": 'mcdisplay-matplotlib-py',
    "TOOL_NAME": 'mcgui-py',
    "PARTICLE": 'neutron',
}

'''
Compilation, parallelisation etc.
'''
compilation = {
    "CFLAGS": '-g -lm -O2',
    "NEXUSFLAGS": '-DUSE_NEXUS -lNeXus',
    "MPIFLAGS": '-DUSE_MPI -lmpi',
    "CC": 'gcc',
    "MPICC": 'mpicc',
    "MPIRUN": 'mpirun',
    "MPINODES": '4',
}

'''
Platform settings
'''
platform = {
    "EXESUFFIX": 'out',
}
