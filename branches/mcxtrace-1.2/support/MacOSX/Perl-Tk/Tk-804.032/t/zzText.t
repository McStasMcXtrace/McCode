# -*- perl -*-
BEGIN { $|=1; $^W=1; }
use strict;
use Test;
use Tk;

BEGIN { plan tests => 22 };

my $mw = Tk::MainWindow->new;
eval { $mw->geometry('+10+10'); };  # This works for mwm and interactivePlacement

my $text;
{
   eval { require Tk::Text; };
   ok($@, "", 'Problem loading Tk::Text');
   eval { $text = $mw->Text(); };
   ok($@, "", 'Problem creating Text widget');
   ok( Tk::Exists($text) );
   eval { $text->grid; };
   ok($@, "", '$text->grid problem');
}
##
## A scrolled (e.g., entry) does not work work as embedded window
##
{
    my $normal = $text->Entry();
    my $scroll = $text->Scrolled('Entry');
    ok( Tk::Exists($normal) );
    ok( Tk::Exists($scroll) );

    eval { $text->window('create','end', -window=>$normal); };
    ok($@ , "", "can't embedd \$normal=$normal. Error is: $@");
    eval { $text->window('create','end', -window=>$scroll); };
    ok($@ , "", "can't embedd \$scroll=$scroll. Error is: $@");

    $text->update;

    ok( ($normal->manager), 'text', "\$normal=$normal not managed by text widget");
    ok( ($scroll->manager), 'text', "\$scroll=$scroll not managed by text widget");

    ok( ($normal->geometry eq '1x1+0+0'), '', '$normal not visible. Geometry'.$normal->geometry);
    ok( ($scroll->geometry eq '1x1+0+0'), '', '$scroll not visible. Geometry'.$scroll->geometry);
}
##
## Bugs 1) windowCreate(-create=>callback) method expects a pathname
##		and not a widget ref.
##      2) eval {} does not catch error message below
##
##ptksh> $l=$t->Label(-text=>'fool');
##ptksh> $t->window('create','end',-create=>sub{$l})
##ptksh> Tk::Error: bad window path name "Tk::Label=HASH(0x1405b4198)"
##  at /home/ach/perl/5.004_64/site+standard/auto/Tk/Error.al line 13.
##
{
    my $l;
    eval { $l = $text->Label(-text=>'an embedded label'); };
    ok($@ , "", "error create widget for later windowCreate: $@");

    eval { $text->window('create', 'end', -create=>sub{ $l }); };
    ok($@ , "", "windowCreate definition had problems: $@");

    eval { $text->update; };  # make sure Text is visible so -create will be called
    ok($@ , "", "windowCreate('1.0',-create=>callback) does not work: $@");

## The next test is disabled ...
#
#   # test if error message of -create is catched by eval
#   eval { $text->window('create', '1.0', -create=>sub{"generate an error"}); };
#   ok($@ , "", "a foo windowCreate definition had problems: $@");
#   eval { $text->update; };
#   ok($@ ne "", 1, "windowCreate(-create=>callback) err msg not catched.");
#
## ... because:
#
# From: Nick Ing-Simmons <nick@ni-s.u-net.com>
# Date: Sat, 18 Apr 1998 20:32:59 +0100
# Message-id: <199804181932.UAA12664@ni-s.u-net.com>
# ...
# This from zzText.t - is not going to be fixed any time soon.
#
# ... test code above ...
#
# It is doing its own error handling, and there is no way to propagate the
# error without a complete re-write of Text's display code.
}
##
## windowCreate(-window=>doesnotexist) does not give an error. Fixed in Tk800.004
##
{
    eval { $text->window('create', '1.0', -window => 'doesnotexist'); };
    ok($@ ne "", 1, "windowCreate -window does not complain if argument is not a widget ref");
}
##
## Dump -window returned pathname instead of widget ref. E.g.
## window,.text.label,1.0.  Fixed in Tk800.005
##
{
    $text->delete('1.0', 'end');
    my $l = $text->Label(-text => 'Frog');
    $text->windowCreate('end', -window => $l);
    my @list = $text->dump('-window', '1.0', '1.0 lineend');
    ok(scalar(@list), 3, "Dumped list of a window has not 3 elements");
    ok($list[0], 'window', "dump() does not return correct type");
    ok($list[1], $l, "dump() does not return the widget reference");
    ok($list[2], '1.0', "dump() returns wrong index");
}
##
## in Tk800.004 yview("$line.0") displays line 14 on top. yview("$line.0 linestart")
## works as expected
##
{
    $text->delete('1.0', 'end');
    for (1..70) { $text->insert('end',"$_\n"); }
    my $idx1 = "13.0";
    my $idx2 = "13.0 linestart";

    ok($text->compare($idx1, '==', $idx2), 1,
	"Opps, '$idx1' and '$idx2' are not the same ???"
	);

    $text->yview($idx1); $text->update(); my $before1 = ($text->yview)[0];
    $text->yview($idx2); $text->update(); my $before2 = ($text->yview)[0];
    ok($before1, $before2, "Opps, yview shows different portions for equal indices");

    $text->delete('1.0', 'end');
}

1;
__END__
