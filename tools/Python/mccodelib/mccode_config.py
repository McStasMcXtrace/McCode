import os
'''
mcstas/mcxtrace configuration.
'''
configuration = {
    "MCCODE_VERSION": '2.3',
    "MCCODE_LIB_DIR": '/usr/share/mcstas/2.3/',
    "MCCODE": 'mcstas',
    "MCRUN": 'mcrun-py',
    "MCPLOT": 'mcplot-gnuplot-py',
    "MCDISPLAY": 'mcdisplay-matplotlib-py',
    "TOOL_NAME": 'mcgui-py',
    "PARTICLE": 'neutron',
    "BROWSER": 'xdg-open',
}

# Set environment variables according to the above
os.environ["MCSTAS"] = configuration["MCCODE_LIB_DIR"]
os.environ["PATH"] = os.path.join(configuration["MCCODE_LIB_DIR"],"bin") + os.pathsep + os.environ["PATH"]

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
