#!/usr/bin/perl
# -*- perl -*-
use strict;
use Test;
use Tk;

plan tests => 1;

my $mw = MainWindow->new;
my $foo = "\260";
$mw->Label(-textvariable => \$foo);
ok($foo, "\260");

