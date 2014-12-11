#!/bin/bash

## This script checks the sim/ dir for new or modified instrument source files
## and update sim/bin accordingly.


# Scan src/sim for new instrument files and generate binaries
./scripts/compile.sh

echo ""

# Generate documentation
python scripts/generate_docs.py

echo ""

# Add new instruments to the database
python populate_db.py
