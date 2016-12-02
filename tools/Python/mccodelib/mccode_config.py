import os
import json

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

def load_user_config():
    ''' loads a json user config to the dictionaries in this module '''
    userconfig = os.path.expandvars("$HOME/." + configuration['MCCODE'] + "/" + configuration['MCCODE_VERSION'] + "/mccode_config.json")
    if not os.path.isfile(userconfig):
        print("user config does not exist: %s" % userconfig)
        return
    
    print("loading user configuration from " + userconfig)
    text = open(userconfig).read()
    obj = json.loads(text)
    global configuration
    configuration = obj['configuration']
    global compilation
    compilation = obj['compilation']
    global platform
    platform = obj['platform']

def save_user_config():
    ''' attempts to save the current values to a local .json file '''
    userconfig = os.path.expandvars("$HOME/." + configuration['MCCODE'] + "/" + configuration['MCCODE_VERSION'] + "/mccode_config.json")
    text = json.dumps({'configuration' : configuration, 'compilation' : compilation, 'platform' : platform})
   
    userdir = os.path.expandvars("$HOME/." + configuration['MCCODE'] + "/" + configuration['MCCODE_VERSION'])
    if not os.path.isdir(userdir):
        try:
            os.mkdir(userdir)
        except Exception as e:
            print("Directory %s could not be created: %s " % (userdir, e.__str__())) 
    f = None
    try:
        f = open(str(userconfig), 'w')
        f.write(text)
        print("userconfig saved to %s" % userconfig)
    except Exception as e:
        print("userconfig could not be saved to %s: %s" % (userconfig, e.__str__()))
    finally:
        if f:
            f.close()

def get_options():
    ''' values below are not enforced in the dicts, but probably used to populate certain gui menus '''
    if configuration['MCCODE'] == "mcstas":
        prefix = "mc"
    else:
        prefix = "mx"
    mcrun_lst =     [prefix+"run", prefix+"run-py"]
    mcplot_lst =    [prefix+"plot-pyqtgraph-py",prefix+"plot", prefix+"plot --format=Gnuplot", prefix+"plot --format=Matlab",
                     prefix+"plot-matlab", prefix+"plot-matplotlib-py", prefix+"plot-gnuplot-py", prefix+"plot-chaco-py"]
    mcdisplay_lst = [prefix+"display-webgl-py",prefix+"display", prefix+"display --format=Matlab", prefix+"display --format=VRML", 
                     prefix+"display --format=Mantid", prefix+"display-matplotlib-py", prefix+"display-py", 
                     prefix+"display-r-py", prefix+"display-vtk-py"]
    return mcrun_lst, mcplot_lst, mcdisplay_lst

