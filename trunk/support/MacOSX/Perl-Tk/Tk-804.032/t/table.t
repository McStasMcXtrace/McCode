#!/usr/bin/perl -w
# -*- perl -*-

#
# Author: Slaven Rezic
#

use strict;

use Tk;

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "1..0 # skip: no Test::More module\n";
	exit;
    }
}

plan tests => 16;

if (!defined $ENV{BATCH}) { $ENV{BATCH} = 1 }

my $mw = tkinit;
$mw->geometry("+10+10");

use_ok("Tk::Table");

my $table = $mw->Table(-rows => 10,
		       -columns => 10,
		       -scrollbars => "se",
		       -fixedrows => 1,
		       -fixedcolumns => 1,
		       -takefocus => 1,
		      );
isa_ok($table, "Tk::Table");
$table->pack(qw(-fill both -expand 1));

$table->put(0,0,"Simple Label");
my $simple_label_w = $table->get(0,0);
is($simple_label_w->cget("-text"), "Simple Label");

my $b = $table->Button(-text => "Simple Button");
$table->put(1,1,$b);
is($table->get(1,1), $b);

is($table->totalColumns, 2, "Number of occupied columns");
is($table->totalRows, 2, "Number of occupied rows");

$table->see(0,0);
pass("See method works with coordinates");
$table->see($b);
pass("See method works with widget");

my($b_row, $b_col) = $table->Posn($b);
is($b_row, 1, "Row of Simple Button");
is($b_col, 1, "Column of Simple Button");

is($table->Subwidget("xscrollbar"), undef,
   "Before update no scrollbars were created");
$table->update;
isa_ok($table->Subwidget("xscrollbar"), "Tk::Scrollbar",
       "x scrollbar");
isa_ok($table->Subwidget("yscrollbar"), "Tk::Scrollbar",
       "y scrollbar");

{
    my $b2 = $table->Button(-text => "2nd button");
    $table->put(0,1,$b2);
    is($table->get(0,1), $b2);

    ok Tk::Exists($b2), 'Button exists before clear() method';
    $table->clear;
    ok !Tk::Exists($b2), 'Button was destroyed by clear() method';
}

if ($ENV{BATCH}) {
    $mw->after(150, sub { $mw->destroy });
}

MainLoop;

__END__
