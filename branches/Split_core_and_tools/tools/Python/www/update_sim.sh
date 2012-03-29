#!/bin/bash

## This script checks the sim/src for new or modified instrument source files
## and update sim/bin accordingly.


# Scan src/sim for new instrument files and generate binaries
./sim/compile.sh

echo ""

# Add new instruments to the database
./populate_db.py
