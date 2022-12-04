#!/bin/sh

# rappid-build script for mcstas-kernel dev:


./3rdparty/sync.sh

WORK=`pwd`

export MCINSTALL_PREFIX=/usr/share/

./mkdist mcstas 3.0-dev "" "" deb64 "" -- justinst
./mkdist mcstas-comps 3.0-dev "" "" deb64 "" -- justinst
# can be out-commented after the first build iteration:
./mkdist mcstas-tools-perl-cmdline 3.0-dev tools/Legacy-Perl-cmdline/ "" deb64 "" -- justinst
./mkdist mcstas-tools-python-mcrun 3.0-dev tools/Python/mcrun/ "" deb64 "" -- justinst
./mkdist mcstas-tools-python-mccodelib 3.0-dev tools/Python/mccodelib/ "" deb64 "" -- justinst
