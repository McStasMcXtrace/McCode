'''
Utility function for handling flexible DEPENDENCY lines.
'''
import os
import subprocess
import pathlib
from . import mccode_config

def evaluate_dependency_str( depstr, verbose=False ):
    """Evaluates CMD(), ENV() and GETPATH() parts of DEPENDENCY (a.k.a. "CFLAGS")
    strings.

    If neither "CMD(", "ENV(" or "GETPATH(" is found in the input string, the input
    string will always be returned completely unchanged!

    Input can either be bytes or str objects, and an object of the same type
    will be returned.

    The current implementation does not support usage of parentheses inside the
    CMD(..), ENV(..) or GETPATH(..) blocks, nor nesting of these apart from the fact
    that ENV(..) and DATAFIFILE(..) blocks can be used inside CMD(..) blocks.

    """
    #Make sure we can handle both raw bytes and str objects. We assume utf8 is
    #sufficient for when we might have to convert output of environment
    #variables or commands.:
    is_bytes = lambda x : hasattr(x,'decode')
    use_bytes = is_bytes( depstr )
    if use_bytes:
        to_target_fmt = lambda x : ( x if is_bytes(x) else x.encode() )
    else:
        to_target_fmt = lambda x : ( x if not is_bytes(x) else x.decode() )

    s_ev = to_target_fmt('ENV(')
    s_cmd = to_target_fmt('CMD(')
    s_df = to_target_fmt('GETPATH(')

    if not s_ev in depstr and not s_cmd in depstr and not s_df in depstr:
        return depstr #<--- early exit for most callers

    as_str = lambda s : ( s.decode() if hasattr(s,'decode') else s )
    s_endparan = to_target_fmt(')')

    def evalmarker( s, startmarker, evalfct ):
        if not startmarker in s:
            return s
        before, ca = s.split(startmarker,1)
        if not s_endparan in ca:
            raise ValueError( 'Missing closing parenthesis in dependency '
                              +f'string after opening "{as_str(startmarker)}"' )
        content, after = ca.split(s_endparan,1)
        if startmarker in content:
            raise ValueError( f'Must close one "{as_str(startmarker)}..)"'
                              + ' before opening a new one.' )
        return before + evalfct(content) + evalmarker(after,startmarker,evalfct)

    def evalfct_df(s):
        dfile=as_str(s.strip())
        fullfile=pathlib.Path(mccode_config.configuration['MCCODE_LIB_DIR']).joinpath(dfile).absolute().resolve().as_posix()

        returnpath=str(fullfile)
        if verbose:
            print(f"   --> pointing path {dfile} to {returnpath}")
        return to_target_fmt(returnpath)

    def evalfct_env(s):
        ev=as_str(s.strip())
        val=os.environ.get(ev,'')
        if verbose:
            print(f"   --> resolving env var {ev} to {val}")
        return to_target_fmt(val)

    def evalfct_cmd(s):
        cmd = as_str(s)
        errmsg = lambda : f'Errors encountered while executing cmd: {cmd}'
        try:
            if verbose:
                print(f"   --> launching cmd: {cmd}")
            returncode = 1
            with subprocess.Popen( cmd,
                                   shell=True,
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE ) as proc:
                output = proc.communicate()[0]
                returncode = proc.returncode
        except:
            print(errmsg())
            raise
        if returncode != 0:
            raise RuntimeError(errmsg())
        #discard empty lines in output, requiring at most one line with content:
        lines = [ e.strip() for e in as_str(output).splitlines() if e.strip() ]
        if len(lines)>1:
            raise RuntimeError(f'Command produced more than a single line of output: {cmd}')
        return to_target_fmt(lines[0]) if lines else ''

    s = evalmarker( depstr, s_ev, evalfct_env )
    s = evalmarker( s, s_df, evalfct_df )
    s = evalmarker( s, s_cmd, evalfct_cmd )
    assert use_bytes == is_bytes(s)
    return s
