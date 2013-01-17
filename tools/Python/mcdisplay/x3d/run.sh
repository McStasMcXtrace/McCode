#!/bin/sh

mcrun --trace -n1 $* | ./rewrite.py test.x3d &&
InstantPlayer test.x3d
