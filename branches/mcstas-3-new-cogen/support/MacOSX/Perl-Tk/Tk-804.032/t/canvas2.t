#!/usr/bin/perl -w
# -*- perl -*-

#
# $Id: $
#

use strict;

use Data::Dumper;
use Getopt::Long;
use Tk;
use Tk::Balloon;
use Tk::Canvas;

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "1..0 # skip: no Test::More module\n";
	exit;
    }
}

my @dashes1 = ([4,4],
	       [2,2],
	       [1,1],
	       [8,8],
	       [16,16],
	       [1,2,3,4,5,6],
	       '.',
	       '. ',
	       ',',
	       ', ',
	       '-',
	       '- ',
	       '_',
	       '_ ',
	       '.,-_',
	       #'.,-_.,-_.,-_.,-_', # dumps core!
	      );

my @dashes2 = (
	       # now equivalent pairs
	       '.',   [2,4],
	       '-',   [6,4],
	       '-.',  [6,4,2,4],
	       '-..', [6,4,2,4,2,4],
	       '.  ', [2,8],
	       ',',   [4,4],
	      );

my @dashes;
push @dashes, map { +{dash => $_, width => 1} } @dashes1;
push @dashes, map { +{dash => $_, width => 2} } @dashes2;

plan tests => 6 + 3 * @dashes;

my $show;
GetOptions("show!" => \$show)
    or die "usage: $0 [-show]";

my $mw = MainWindow->new;
$mw->geometry("+10+10");
my $b = $mw->Balloon(-initwait => 50, -balloonposition => 'mouse');
$b->Subwidget("message")->configure(-font => "monospace");

{
    local $TODO;
    $TODO = "Dash errors not yet corrected" if $Tk::VERSION < 804.027502;

    my $c = $mw->Canvas;
    eval { $c->createLine(0,0,1,1,-dash => [4]) };
    like($@, qr{\Qbad dash list "4": must be a list of integers or a format like "-.."}, "error with one number in list");
    eval { $c->createLine(0,0,1,1,-dash => 4) };
    like($@, qr{\Qbad dash list "4": must be a list of integers or a format like "-.."}, "error with one number in list");
    eval { $c->createLine(0,0,1,1,-dash => ',;') };
    like($@, qr{\Qbad dash list ",;": must be a list of integers or a format like "-.."}, "error with bad dash list");
    eval { $c->createLine(0,0,1,1,-dash => ['x',4]) };
    like($@, qr{\Qexpected integer in the range 1..255 but got "x"}, "non-integer in dash list");
}

{
    use constant DIR_HORIZ => 0;
    use constant DIR_DIAG  => 1;
    use constant DIR_VERT  => 2;

    my @c;
    my @itemlabel;
    for (0 .. 2) {
	$c[$_] = $mw->Canvas(-height => 20 + 6 * @dashes)->pack;
	$itemlabel[$_] = {};
	$b->attach($c[$_], -msg => $itemlabel[$_]);
    }

    $mw->update; # to refresh Width/Height
    my $c_x2 = $c[0]->Width - 10;
    my $c_y2 = $c[0]->Height - 10;

    for my $dir (DIR_HORIZ, DIR_DIAG, DIR_VERT) {
	my $x  = 10;
	my $y  = 10;
	my $x2 = $dir == DIR_VERT  ? $x : $c_x2;
	my $y2 = $dir == DIR_HORIZ ? $y : $dir == DIR_DIAG ? $y + 20 : $c_y2;
	my $xd = $dir == DIR_VERT  ? 6 : 0;
	my $yd = $dir == DIR_VERT  ? 0 : 6;
	for my $def (@dashes) {
	    my($dash, $width) = @{$def}{qw(dash width)};
	    my $dash_printable = Data::Dumper->new([$dash],['dash'])->Indent(0)->Dump;
	    my $item = $c[$dir]->createLine($x, $y, $x2, $y2, -width => $width, -dash => $dash);
	    pass("Created dash $dash_printable (direction=$dir)");
	    $itemlabel[$dir]->{$item} = $dash_printable;

	    $x +=$xd;
	    $x2+=$xd;
	    $y +=$yd;
	    $y2+=$yd;
	}
    }
}

{
    eval { $mw->Canvas(-scrollregion => "1 2") };
    like $@, qr{bad scrollRegion "1 2"}, "Expected error message for bad scrollregion";
}

{
    my $c = $mw->Canvas;
    my $item = $c->createText(0,0,-text=>"foo");
    eval { $c->select('clear',$item,"bla","foo") };
    # This used to segfault for Tk <= 804.029_501
    like $@, qr{\Qwrong # args: should be ".canvas\E\d+\Q select clear tagOrId index"}, 'select clear error message';
    $c->destroy;
}

if (!$show) {
    $mw->after(1000, sub { $mw->destroy });
}
MainLoop;

__END__
