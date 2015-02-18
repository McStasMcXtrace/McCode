#!/bin/sh
#
# Simple script for downloading stuff via curl or wget, depending on which is available...

HASCURL=`which curl | wc -l`
HASWGET=`which wget | wc -l`

if [ $HASCURL == 1 ]; then
    curl -O $*
elif [ $HASWGET == 1 ]; then
    wget $*
else
    echo "Sorry, neither wget nor curl found on your path - exiting!"
    exit 1;
fi
