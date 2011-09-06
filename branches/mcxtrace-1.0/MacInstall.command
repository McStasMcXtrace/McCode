#!/bin/sh

cd `dirname $0`

# Determine if this is Snow Leopard (10.6)
SL=`sw_vers | grep 10.6 | wc -l`

if [ $SL = 1 ] ; then
    export PERL=/usr/bin/perl5.8.9
else
    export PERL=/usr/bin/perl
fi

export BROWSER=/usr/bin/open
./configure PERL=$PERL
make pgplot
make
echo
echo
echo Please enter your password to allow installation of McStas:
echo
sudo make install
echo Creating shortcut for McStas on your Desktop:
cp -rp McStas.app ~/Desktop/
