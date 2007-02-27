#!/bin/sh

cd `dirname $0`
export BROWSER=/Applications/Safari.app/Contents/MacOS/Safari
./configure
make pgplot
make
echo
echo
echo Please enter your password to allow installation of McStas:
echo
sudo make install

