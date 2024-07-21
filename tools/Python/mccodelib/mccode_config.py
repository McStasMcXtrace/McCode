import os
import pathlib
import json
import platform as OsPlatform
import subprocess

thisdir = pathlib.Path(__file__).parent.absolute().resolve()
tooldir = ( thisdir / '..' / '..' ).absolute().resolve()
resourcedir = ( tooldir / '../resources' ).absolute().resolve()
bindir = ( tooldir / '../../../bin' ).absolute().resolve()
libdir = ( tooldir / '../../../lib' ).absolute().resolve()

def check_env_vars():
    ''' checks the OS environment variables '''
    global configuration
    global compilation

    # Initially figure out if this is McStas or McXtrace
    MCCODE_OVERRIDE        = configuration["MCCODE"].upper() + '_OVERRIDE'
    MCCODE_CFLAGS_OVERRIDE = configuration["MCCODE"].upper() + '_CFLAGS_OVERRIDE'
    MCCODE_CC_OVERRIDE     = configuration["MCCODE"].upper() + '_CC_OVERRIDE'
    MCCODE_MPICC_OVERRIDE  = configuration["MCCODE"].upper() + '_MPICC_OVERRIDE'

    # MCCODE_LIB_DIR
    if os.getenv(MCCODE_OVERRIDE) is not None:
        configuration['MCCODE_LIB_DIR'] = os.getenv(MCCODE_OVERRIDE)
    # CFLAGS
    if os.getenv(MCCODE_CFLAGS_OVERRIDE) is not None:
        compilation['CFLAGS'] = os.getenv(MCCODE_CFLAGS_OVERRIDE)
    # CC
    if os.getenv(MCCODE_CC_OVERRIDE) is not None:
        compilation['CC'] = os.getenv(MCCODE_CC_OVERRIDE)
    # MPICC
    if os.getenv(MCCODE_MPICC_OVERRIDE) is not None:
        compilation['MPICC'] = os.getenv(MCCODE_MPICC_OVERRIDE)

def load_config(path=None):
    ''' loads a json user config to the dictionaries in this module '''
    global configuration
    global compilation
    global platform
    global directories

    if path is None:
        level="system"
        userconfig = os.path.join(os.path.dirname(__file__),"mccode_config.json")
        info=""
    elif path=="user":
        level="user"
        if OsPlatform.system() == 'Windows':
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

    #slightly nasty workaround to suppress print statement below:
    if os.environ.get('MCCODE_SUPPRESS_LOAD_CONFIG_PRINT_STATEMENT','') != '1':
        print("loading " + level + " configuration" + info )
    text = open(userconfig).read()
    obj = json.loads(text)
    configuration = obj['configuration']
    compilation = obj['compilation']

    # Special handling of build-env detected settings on conda
    if configuration['ISCONDAPKG']=='1':
        # Map existing ${CONDA_PREFIX} 'symbols' to actual current CONDA_PREFIX
        conda_prefix = os.environ.get('CONDA_PREFIX')
        if conda_prefix:
            entries_with_conda_prefix=['CFLAGS','NEXUSFLAGS','MPIFLAGS','OACCFLAGS','CC','MPICC','MPIRUN']
            conda_prefix = str(pathlib.Path(conda_prefix).absolute().resolve())
            for e in entries_with_conda_prefix:
                if '${CONDA_PREFIX}' in compilation[e]:
                    compilation[e] = str(compilation[e].replace('${CONDA_PREFIX}',conda_prefix))
                else:
                    compilation[e] = str(compilation[e])

        # On macOS, map ${XCRUN_DETECTED} 'symbols' to actual, current "SDK" path from xcrun
        if OsPlatform.system() == 'Darwin':
            cmd = "xcrun --show-sdk-path"
            errmsg = lambda : f'Errors encountered while executing cmd: {cmd}'
            try:
                returncode = 1
                with subprocess.Popen( cmd,
                                   shell=True,
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE ) as proc:
                    xcrun_path = proc.communicate()[0].decode("utf-8").rstrip()
                    returncode = proc.returncode
            except:
                print(errmsg())
                raise
            entries_with_isysroot=['CFLAGS','NEXUSFLAGS','MPIFLAGS','OACCFLAGS','CC','MPICC','MPIRUN']
            conda_prefix = str(pathlib.Path(xcrun_path).absolute().resolve())
            for e in entries_with_isysroot:
                if '${XCRUN_DETECTED}' in compilation[e]:
                    compilation[e] = str(compilation[e].replace('${XCRUN_DETECTED}',xcrun_path))
                else:
                    compilation[e] = str(compilation[e])

    platform = obj['platform']
    directories = obj['directories']
    # Fill in the location of MCCODE_LIB_DIR based on location of Python script
    configuration["MCCODE_LIB_DIR"]=str(resourcedir) #NB: Is actually in the 'directories' part now (as a non-absolute directory name).
    # Finally, set dynamically deduced directory paths:
    directories['bindir'] = str( bindir )
    directories['libdir'] = str( libdir )
    directories['tooldir'] = str( tooldir )
    directories['resourcedir'] = str( resourcedir )
    directories['pydir'] = str( tooldir / 'Python' )
    directories['datadir'] = str( resourcedir / 'data' )
    directories['sharedir'] = str( resourcedir / 'share' )
    directories['compdir'] = str( resourcedir )
    directories['exampledir'] = str( resourcedir / 'examples' )


def save_user_config():
    ''' attempts to save the current values to a local .json file '''
    text = json.dumps({'configuration' : configuration, 'compilation' : compilation, 'platform' : platform, 'directories' : directories}, indent=2)

    if OsPlatform.system() == 'Windows':
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
    if OsPlatform.system() == 'Windows':
        suffix='-pl'
        #suffix2='.pl'
    else:
        suffix='.pl'
        #suffix2=''

    if configuration['MCCODE'] == "mcstas":
        prefix = "mc"
        mcdisplay_lst = [prefix+"display-webgl",
                         prefix+"display-webgl-classic",
                         prefix+"display-pyqtgraph",
                         prefix+"display-pyqtgraph --tof",
                         prefix+"display-matplotlib",
                         prefix+"display-matlab",
                         prefix+"display-octave",
                         prefix+"display-cad",
                         prefix+"display-mantid"]
        mcrun_lst =     [prefix+"run", prefix+"run --format=NeXus", prefix+"run --format=NeXus --IDF"]
        format_lst =    ["McCode", "NeXus", "NeXus --IDF", "NeXus -c", "NeXus --IDF -c"]
    else:
        prefix = "mx"
        mcdisplay_lst = [prefix+"display-webgl",
                         prefix+"display-webgl-classic",
                         prefix+"display-pyqtgraph",
                         prefix+"display-matplotlib",
                         prefix+"display-matlab",
                         prefix+"display-octave",
                         prefix+"display-cad"]
        mcrun_lst =     [prefix+"run", prefix+"run --format=NeXus"]
        format_lst =    ["McCode", "NeXus", "NeXus -c"]


    mcplot_lst =    [prefix+"plot-pyqtgraph",
                     prefix+"plot-matplotlib", 
                     prefix+"plot-matlab",
                     prefix+"plot-svg"]

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
'''
Directories
'''
directories = {
}

load_config()

# Set environment variables according to the above
os.environ[configuration["MCCODE"].upper()] = str(resourcedir)
os.environ["PATH"] = '%s%s%s'%(str(bindir),os.pathsep,os.environ.get("PATH",''))
