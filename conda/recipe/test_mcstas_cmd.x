#!/usr/bin/env bash
set -e
set -u
set -x
mcstas --help
mcstas --version
mkdir test1
cp src/mcstas-comps/examples/BNL_H8.instr test1
#Comment out since this does not work yet: ( cd test1 && mcstas BNL_H8.instr )
#TODO: test environment script as well.
