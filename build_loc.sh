#!/bin/sh

# rappid-build script for mcstas-kernel dev:

git submodule init
git submodule update

WORK=`pwd`

export MCINSTALL_PREFIX=/usr/share/

./mkdist mcstas 3.0-test "" "" deb64 "" -- OSXapp
./mkdist mcstas-comps 3.0-test "" "" deb64 "" -- OSXapp
# can be out-commented after the first build iteration:
./mkdist mcstas-tools-perl-cmdline 3.0-test tools/Legacy-Perl-cmdline/ "" deb64 "" -- OSXapp
