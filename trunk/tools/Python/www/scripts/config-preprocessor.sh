#!/bin/sh

# Replace macros in config file (CURRENT_{USER,GROUP}, PWD)

# grab user and group
USR="`whoami`"
GRP="`groups ${USR} | cut -f4 -d' '`"

# escape / to \/ in workdir
PWD="`pwd|sed 's/\//\\\\\//g'`"

sed s/CURRENT_USER/${USR}/g | \
    sed s/CURRENT_GROUP/${GRP}/g | \
    sed s/PWD/${PWD}/g
