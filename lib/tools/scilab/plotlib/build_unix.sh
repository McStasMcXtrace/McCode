#!/bin/sh
# 
# Unix build tool for scilab plotlib (Stephane Mottelet)
# 
# Runs distributed builder.sce if SCILAB was found on path by configure
#
# Peter Willendrup, Risoe, 20030922

if [ ! "$1" = "none" ] ; then
  echo exit | $1 -nw -f builder.sce 
fi;
