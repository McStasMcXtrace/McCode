#!/bin/bash

## This script checks the sim/ dir for new or modified instrument source files
## and update sim/bin accordingly.


# Scan src/sim for new instrument files and generate binaries
./scripts/compile.sh

echo ""

# Generate documentation
python manage.py generate_docs

# Add new instruments to the database
python manage.py populate_db
