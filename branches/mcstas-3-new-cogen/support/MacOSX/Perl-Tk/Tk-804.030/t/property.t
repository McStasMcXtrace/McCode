#!/usr/bin/perl -w
# -*- perl -*-

#
# $Id: $
# Author: Slaven Rezic
#

use strict;

use Tk;
use Data::Dumper;

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "1..0 # skip: no Test::More module\n";
	exit;
    }
}

unless ($Tk::platform eq 'unix') {
    plan skip_all => 'property only work on X11';
    exit 0;
}

plan tests => 38;

my $mw = tkinit;
$mw->geometry("+10+10");

{
    my @prop = $mw->property('list');
    # Here a test used to check if @prop is empty. But KDE defines a
    # _KDE_NET_WM_USER_CREATION_TIME property, so the test cannot be
    # used anymore.
    pass("property list call on windows");
    ok(!$mw->property('exists','_PERL_TK_ThisPropertyDoesNotExist'), "Not-existent property");
}

{
    my @prop = $mw->property('list','root');
    cmp_ok(scalar(@prop), ">=", 1, "It's very likely that there are properties on the root window");
    ok(!$mw->property('exists','_PERL_TK_ThisPropertyDoesNotExist'.rand(10),'root'), "Not-existent property");
}

# format=8 differs from format=16/32: The value to set and get is a
# string. If getting the property value, then a "\0" is appended (I
# don't know if this is intentional, this may change). With
# format=16/32 the values are array references with integers.
{
    my $format = 8;

    $mw->property('set', '_PERL_TK_TestProperty_1', "CARDINAL", $format, "a");
    ok($mw->property('exists', '_PERL_TK_TestProperty_1'), "Format=$format, Property exists");
    is($mw->property('get', '_PERL_TK_TestProperty_1'), "a\0", "Expected get result")
	or diag(Dumper [$mw->property('get', '_PERL_TK_TestProperty_1')]);
    is_xprop('_PERL_TK_TestProperty_1', [map { ord } split //, "a\0"], "Expected xprop result");

    $mw->property('set', '_PERL_TK_TestProperty_1', "CARDINAL", $format, "abc");
    ok($mw->property('exists', '_PERL_TK_TestProperty_1'), "Format=$format, Property exists, longer value");
    is($mw->property('get', '_PERL_TK_TestProperty_1'), "abc\0")
	or diag(Dumper [$mw->property('get', '_PERL_TK_TestProperty_1')]);
    is_xprop('_PERL_TK_TestProperty_1', [map { ord } split //, "abc\0"]);

    my @list = $mw->property('list');
    ok((grep { $_ eq '_PERL_TK_TestProperty_1' } @list), "Found test property in list");

    my @list2 = $mw->property('list', hex $mw->id);
    is_deeply(\@list, \@list2, "Same list with explicite window id");

    $mw->property('delete', '_PERL_TK_TestProperty_1');
    ok(!$mw->property('exists', '_PERL_TK_TestProperty_1'), "Property deleted");

    my @list_after_delete = $mw->property('list');
    ok(!(grep { $_ eq '_PERL_TK_TestProperty_1' } @list_after_delete), "Not appearing in list anymore");
}

for my $format (16, 32) {
    $mw->property('set', '_PERL_TK_TestProperty_1', "CARDINAL", $format, [1]);
    ok($mw->property('exists', '_PERL_TK_TestProperty_1'), "Format=$format, Property exists");
    is_deeply([$mw->property('get', '_PERL_TK_TestProperty_1')], ["CARDINAL", 1], "Expected get result")
	or diag(Dumper [$mw->property('get', '_PERL_TK_TestProperty_1')]);
    is_xprop('_PERL_TK_TestProperty_1', [1], "Expected xprop result");

    $mw->property('set', '_PERL_TK_TestProperty_1', "CARDINAL", $format, [1,2,3]);
    ok($mw->property('exists', '_PERL_TK_TestProperty_1'), "Format=$format, Property exists, longer value");
    is_deeply([$mw->property('get', '_PERL_TK_TestProperty_1')], ["CARDINAL", 1,2,3])
	or diag(Dumper [$mw->property('get', '_PERL_TK_TestProperty_1')]);
    is_xprop('_PERL_TK_TestProperty_1', [1,2,3]);

    $mw->property('delete', '_PERL_TK_TestProperty_1');
    ok(!$mw->property('exists', '_PERL_TK_TestProperty_1'), "Property deleted");
}

# Test with ATOMs
{
    $mw->property('set', '_PERL_TK_TestProperty_2', "ATOM", 32, ['_PERL_TK_TestAtom_1']);
    ok($mw->property('exists', '_PERL_TK_TestProperty_2'), "Property with ATOM exists");
    is_deeply([$mw->property('get', '_PERL_TK_TestProperty_2')], ['ATOM', '_PERL_TK_TestAtom_1'], "Expected get result")
	or diag(Dumper [$mw->property('get', '_PERL_TK_TestProperty_2')]);
    is_xprop('_PERL_TK_TestProperty_2', ['_PERL_TK_TestAtom_1'], "Expected xprop result");

    my @list = $mw->property('list');
    ok((grep { $_ eq '_PERL_TK_TestProperty_2' } @list), "Found test property in list");

    $mw->property('delete', '_PERL_TK_TestProperty_2');
    ok(!$mw->property('exists', '_PERL_TK_TestProperty_2'), "Property deleted");
}

# Test with STRINGs
{
    $mw->property('set', '_PERL_TK_TestProperty_3', "STRING", 8, 'TestString');
    ok($mw->property('exists', '_PERL_TK_TestProperty_3'), "Property with STRING exists");
    is($mw->property('get', '_PERL_TK_TestProperty_3'), "TestString\0", "Expected get result")
	or diag(Dumper [$mw->property('get', '_PERL_TK_TestProperty_3')]);
    is_xprop('_PERL_TK_TestProperty_3', ['"TestString"'], "Expected xprop result");

    my @list = $mw->property('list');
    ok((grep { $_ eq '_PERL_TK_TestProperty_3' } @list), "Found test property in list");

    $mw->property('delete', '_PERL_TK_TestProperty_3');
    ok(!$mw->property('exists', '_PERL_TK_TestProperty_3'), "Property deleted");
}

sub is_xprop {
    my($prop, $expected, $testname) = @_;
 SKIP: {
	skip("xprop not in PATH", 1)
	    if !xprop_is_in_path();
	my $mw_id = $mw->id;
	chomp(my $res = `xprop -notype -id $mw_id $prop`);
	$res =~ s{^\Q$prop\E\s*=\s*}{};
	my(@bytes) = split /\s*,\s*/, $res;
	is_deeply(\@bytes, $expected, $testname)
	    or diag(Dumper \@bytes);
    }
}

{
    my $xprop_is_in_path;
    sub xprop_is_in_path {
	my($prog) = "xprop";
	my $path = eval {
	    require File::Spec;
	    require Config;
	    return $prog if (File::Spec->file_name_is_absolute($prog) and -f $prog and -x $prog);
	    my $sep = $Config::Config{'path_sep'} || ':';
	    foreach (split(/$sep/o, $ENV{PATH})) {
		return "$_/$prog" if (-x "$_/$prog" && !-d "$_/$prog");
	    }
	    undef;
	};
	warn $@ if $@; # unlikely to happen
	$xprop_is_in_path = $path; # cache
	$path;
    }
}

__END__
