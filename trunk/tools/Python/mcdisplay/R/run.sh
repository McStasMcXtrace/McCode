#!/bin/sh

CSVCOMPS="comps.csv"
CSVLINES="lines.csv"

mcrun --trace -n1e2 $1 | ./rewrite.py ${CSVCOMPS} ${CSVLINES}

R_PROFILE_USER=display.r R ${CSVCOMPS} ${CSVLINES} --no-save
