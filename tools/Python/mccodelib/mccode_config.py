import os
import json
import sys

'''
Dynamic vs static LIBDIR location
'''
LIBDIR = os.path.join(os.path.dirname(__file__),"..","..","..")
LIBDIR_FALLBACK = '/home/pkwi/McStas/mcstas/3.0-dev/'
if sys.platform == 'darwin':
    LIBDIR = LIBDIR_FALLBACK

def check_env_vars():
    ''' checks the OS environment variables '''
    global configuration
    global compilation

    # MCCODE_LIB_DIR
    if not os.getenv('MCSTAS_OVERRIDE') is None:
        configuration['MCCODE_LIB_DIR'] = os.getenv('MCSTAS_OVERRIDE')
    # CFLAGS
    if not os.getenv('MCSTAS_CFLAGS_OVERRIDE') is None:
        compilation['CFLAGS'] = os.getenv('MCSTAS_CFLAGS_OVERRIDE')
    # CC
    if not os.getenv('MCSTAS_CC_OVERRIDE') is None:
        compilation['CC'] = os.getenv('MCSTAS_CC_OVERRIDE')
    # MPICC
    if not os.getenv('MCSTAS_MPICC_OVERRIDE') is None:
        compilation['MPICC'] = os.getenv('MCSTAS_MPICC_OVERRIDE')
    if not os.getenv('MCSTAS_MPIRUN_OVERRIDE') is None:
        compilation['MPIRUN'] = os.getenv('MCSTAS_MPIRUN_OVERRIDE')

def load_config(path=None):
    ''' loads a json user config to the dictionaries in this module '''
    global configuration
    global compilation
    global platform

    if path==None:
        level="system"
        userconfig = os.path.join(os.path.dirname(__file__),"mccode_config.json")
        info=""
    elif path=="user":
        level="user"
        if os.name == 'nt':
            userdir =  os.path.join(os.path.expandvars("$USERPROFILE"),"AppData",configuration['MCCODE'],configuration['MCCODE_VERSION'])
        else:
            userdir =  os.path.join(os.path.expandvars("$HOME"),"." + configuration['MCCODE'],configuration['MCCODE_VERSION'])
        userconfig = os.path.join(userdir,"mccode_config.json")
        info = " from " + userconfig
    
        if not os.path.isfile(userconfig):
            return
    else:
        level="override"
        userconfig= os.path.join(path,"mccode_config.json")
        info = " from " + userconfig
    
    print("loading " + level + " configuration" + info )
    text = open(userconfig).read()
    obj = json.loads(text)
    configuration = obj['configuration']
    compilation = obj['compilation']
    platform = obj['platform']
    # Finally, fill in the location of MCCODE_LIB_DIR based on location of Python script
    configuration["MCCODE_LIB_DIR"]=LIBDIR


def save_user_config():
    ''' attempts to save the current values to a local .json file '''
    text = json.dumps({'configuration' : configuration, 'compilation' : compilation, 'platform' : platform}, indent=2)

    if os.name == 'nt':
        homedirconf =  os.path.join(os.path.expandvars("$USERPROFILE"),"AppData",configuration['MCCODE'])
    else:
        homedirconf =  os.path.join(os.path.expandvars("$HOME"),"." + configuration['MCCODE'])

    if not os.path.isdir(homedirconf):
        try:
            os.mkdir(homedirconf)
        except Exception as e:
            print("Directory %s could not be created: %s " % (homedirconf, e.__str__()))
            
    userdir = os.path.join(homedirconf,configuration['MCCODE_VERSION'])
    if not os.path.isdir(userdir):
        try:
            os.mkdir(userdir)
        except Exception as e:
            print("Directory %s could not be created: %s " % (userdir, e.__str__())) 
    userconfig = os.path.join(userdir,"mccode_config.json")
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
    if os.name == 'nt':
        suffix='-pl'
        suffix2='.pl'
    else:
        suffix='.pl'
        suffix2=''
        
    if configuration['MCCODE'] == "mcstas":
        prefix = "mc"
        mcdisplay_lst = [prefix+"display-webgl",
                         prefix+"display-pyqtgraph",
                         prefix+"display-pyqtgraph --tof",
                         prefix+"display-mantid",
                         prefix+"display-matlab"+suffix+" --format=Matlab"]
        mcrun_lst =     [prefix+"run", prefix+"run --format=NeXus", prefix+"run --format=NeXus --IDF"]
        format_lst =    ["McCode", "NeXus", "NeXus --IDF", "NeXus -c", "NeXus --IDF -c"]
    else:
        prefix = "mx"
        mcdisplay_lst = [prefix+"display-webgl",
                         prefix+"display-pyqtgraph",
                         prefix+"display-matlab"+suffix+" --format=Matlab"]
        mcrun_lst =     [prefix+"run", prefix+"run --format=NeXus"]
        format_lst =    ["McCode", "NeXus", "NeXus -c"]

        
    mcplot_lst =    [prefix+"plot-pyqtgraph",prefix+"plot-matplotlib", prefix+"plot-matlab"]

    return mcrun_lst, mcplot_lst, mcdisplay_lst, format_lst

def get_mccode_prefix():
    ''' returns 'mc' or 'mx' depending on system configuration '''
    if configuration["MCCODE"] == "mcstas":
        return "mc"
    else:
        return "mx"

'''
mcstas/mcxtrace configuration.
'''
configuration = {
}

'''
Compilation, parallelisation etc.
'''
compilation = {
}

'''
Platform settings
'''
platform = {
}

load_config()
    
# Set environment variables according to the above
os.environ[configuration["MCCODE"].upper()] = configuration["MCCODE_LIB_DIR"]
os.environ["PATH"] = os.path.join(configuration["MCCODE_LIB_DIR"],"bin") + os.pathsep + os.environ["PATH"]

