#!/bin/sh

CSVCOMPS="comps.csv"
CSVLINES="lines.csv"

cat $1 | ./rewrite.py ${CSVCOMPS} ${CSVLINES}

R_PROFILE_USER=display.r R ${CSVCOMPS} ${CSVLINES} --no-save
