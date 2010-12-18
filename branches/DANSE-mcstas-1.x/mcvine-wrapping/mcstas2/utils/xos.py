__doc__="""extension utilities for os"""
__author__="Jiao Lin"

def getEnv( env_variable, default):
    """return environment variable of env_variable if possible, otherwise
    return default"""
    from os import environ
    try:
        variable = environ[env_variable]
    except:
        variable = default
    return variable

def removedirs( thepath ):
    "recursively remove directory 'thepath'"
    from os.path import exists
    if not exists(thepath) : return
    from os import remove,rmdir,listdir
    from os.path import join
    for item in listdir(thepath):
        item = join(thepath,item)
        try:
            remove(item)
        except:
            try:
                rmdir(item)
            except:
                try:
                    removedirs(item)
                except:
                    print "cannot empty directory %s"% item
                    raise 'cannot empty the directory'
    rmdir(thepath)


def makedirs( thepath ):
    """recursively make directory.
    if the given directory exists, do nothing.
    if the given path is a file, raise an error.
    """
    from os.path import exists, isdir
    if exists(thepath):
        if isdir(thepath) :
            return
        else :
            raise 'failed to make the directory %s, which already exists and is not a directory' % thepath
    else :
        from os import makedirs
        makedirs(thepath)
