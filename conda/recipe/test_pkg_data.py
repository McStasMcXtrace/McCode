#!/usr/bin/env python3
import os
import pathlib

expected_files = [ 'share/mcstas/resources/data/Be.laz' ]

forbidden_files = [ 'bin/mcstas',
                    'bin/mcrun',
                    'bin/mcgui',
                    'share/mcstas/tools/Python/mccodelib/__init__.py',
                    'share/mcstas/tools/Python/resources/examples/BNL_H8.instr',
                    'share/mcstas/resources/examples/BNL_H8.instr' ]
conda_prefix_dir = ( pathlib.Path(os.environ.get('PREFIX')).absolute().resolve() )

for f in expected_files:
    if not conda_prefix_dir.joinpath( *( f.split('/') ) ).exists():
        raise SystemExit(f'Missing file: {f}')

for f in forbidden_files:
    if conda_prefix_dir.joinpath( *( f.split('/') ) ).exists():
        raise SystemExit(f'Forbidden file for mcstas-data: {f}')
