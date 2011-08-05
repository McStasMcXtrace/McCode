#!/bin/sh

cd `dirname $0`

export PERL=/usr/bin/perl5.10
$PERL Makefile.PL
make
echo
echo
echo Please enter your password to allow installation of Perl-Tk:
echo
sudo make install
