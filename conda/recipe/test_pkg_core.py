#!/usr/bin/env python3

import os
import pathlib
import shutil
import shlex
import subprocess

def launch( cmd ):
    print(f'Testing command: {cmd}')
    res = subprocess.run( shlex.split(cmd) )
    if res.returncode != 0:
        raise SystemExit(f'Command "{cmd}" failed!')

expected_files = [ 'bin/mcstas',
                   'bin/mcrun',
                   'bin/mcgui',
                   'share/mcstas/tools/Python/mccodelib/__init__.py',
                   'share/mcstas/resources/examples/BNL_H8.instr' ]

cmds = ['mcstas --help',
        'mcstas --version',
        'mcrun --showcfg bindir',
        'mcrun --showcfg resourcedir',
        'mcrun --showcfg libdir',
        ]

conda_prefix_dir = ( pathlib.Path(os.environ.get('PREFIX')).absolute().resolve() )
srcdir = ( pathlib.Path('.') / 'src' ).absolute().resolve()
testdir1 = ( pathlib.Path('.') / f'testdir_core_1' ).absolute().resolve()
testdir2 = ( pathlib.Path('.') / f'testdir_core_2' ).absolute().resolve()

if ( conda_prefix_dir / 'share' / 'mcstas' / 'resources' / 'data' ).exists():
    raise SystemExit('share/mcstas/resources/data should not be created by mcstas-core')

for f in expected_files:
    if not conda_prefix_dir.joinpath( *( f.split('/') ) ).exists():
        raise SystemExit(f'Missing file: {f}')

for c in cmds:
    launch(c)

testdir1.mkdir()
os.chdir( testdir1 )
shutil.copy( srcdir / 'mcstas-comps' / 'examples' / 'BNL_H8.instr', 'BNL_H8.instr' )
launch( 'mcstas BNL_H8.instr' )
launch( 'mcrun -c BNL_H8.instr lambda=2.36 -s1000 -n1e5' )

testdir2.mkdir()
os.chdir( testdir2 )
shutil.copy( srcdir / 'mcstas-comps' / 'examples' / 'Union_manual_example.instr', 'Union_manual_example.instr' )
launch( 'mcstas Union_manual_example.instr' )
launch( 'mcrun -c Union_manual_example.instr -s1000 -n1e5' )

