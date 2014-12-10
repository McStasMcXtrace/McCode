# -*- perl -*-
BEGIN { $^W = 1; $| = 1; }

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

plan tests => 32;

my $mw = Tk::MainWindow->new;
eval { $mw->geometry('+10+10'); };  # This works for mwm and interactivePlacement

my $hlist;
{
   eval { require Tk::HList; };
   is($@, "", 'Loading Tk::HList');
   eval { $hlist = $mw->HList(); };
   is($@, "", 'Creating HList widget');
   ok( Tk::Exists($hlist) );
   eval { $hlist->grid; };
   is($@, "", '$hlist->grid');
   eval { $hlist->update; };
   is($@, "", '$hlist->update.');

   $hlist->delete("all");
   $hlist->add("entry with spaces");
   my @bbox = $hlist->info('bbox', 'entry with spaces');
   my @info = $hlist->info('item', @bbox[0, 1]);
   is($info[0], 'entry with spaces', 'Spaces in entry path');
}
##
## With Tk800.004:
##   1) headerSize returns "x y" instead of [x,y].
##   2) Error headerSize err msg for non existant col contains garbage. E.g.
##	Column "KC@" does not exist at ...
##   3) infoSelection not defined (test is just a bothering reminder to
##      check all other Submethods that should be defined are defined).
##   4) entryconfigure -style contains garbage
##
{
    my $hl = $mw->HList(-header=>1)->grid;
    $hl->headerCreate(0, -text=>'a heading');

    my @dim;
    eval { @dim = $hl->headerSize(0); };
    is($@, '', "headerSize method");
    is(scalar(@dim), 2, 'headerSize returned a 2 element array: |'.
       join('|',@dim,'')
      );
    eval { $hlist->update; };
    is($@, "", '$hlist->update.');

    eval { $hl->header('size', 1); }; # does not exist
    isnt($@, "", "Error for non existent header field");
    like($@, qr/^Column "1" does not exist/,
	 "Error message matches /^Column \"1\" does not exist/"
	);
    eval { $hlist->update; };
    is($@, "", '$hlist->update.');

    eval { $hl->info('selection'); };
    is($@, "", "info('selection') method.");
    eval { $hl->infoSelection; };
    is($@, "", "infoSelection method.");
    eval { $hlist->update; };
    is($@, "", '$hlist->update.');

    $hl->add(1,-text=>'one');
    my $val1 = ( $hl->entryconfigure(1, '-style') )[4];
    # comment out the next line and at least I get always a SEGV
    isnt(defined($val1), "entryconfigure -style is not defined");
    my $val2 = $hl->entrycget(1, '-style');
    isnt(defined($val2), "entrycget -style is not defined");
    # ok($val1, $val2, "entryconfigure and entrycget do not agree");

    my @bbox = $hl->infoBbox(1);
    is(scalar(@bbox), 4, "\@bbox 4 items");
    my $bbox = $hl->infoBbox(1);
    is(ref($bbox), 'ARRAY', "$bbox is an ARRAY");
    foreach my $a (@bbox)
     {
      is($a, shift(@$bbox), "\$bbox values OK");
     }
    $hl->destroy;
}

SKIP: {
    skip("Aborts with Tk804", 1)
	if $Tk::VERSION <= 804.027;

    my $hl = $mw->HList;

    $hl->add("top", -text => "top");
    $hl->add("top.item1", -text => "item1");
    $hl->add("top.item2", -text => "item2");

    $hl->add("top.item3", -at => 0, -text => "item2");
    $hl->add("top.item4", -before => "top.item1", -text => "item2");
    $hl->add("top.item5", -after => "top.item1", -text => "item2");

    pass("No abort with -at/-before/-after");

    $hl->destroy;
}

{
    my $hl = $mw->HList;

    $hl->add("top", -text => "top");
    $hl->add("top.item1", -text => "item1");
    $hl->add("top.item2", -text => "item2");

    ok(!$hl->info('hidden', 'top.item1'), "Item initially not hidden");
    $hl->hide('entry','top.item1');
    ok($hl->info('hidden', 'top.item1'), "Item now hidden");
    $hl->show('entry','top.item1');
    ok(!$hl->info('hidden', 'top.item1'), "Item not hidden again");
    $hl->hideEntry('top.item1');
    ok($hl->info('hidden', 'top.item1'), "Undocumented method hideEntry also works");

    $hl->destroy;
}

{
    my $hl = $mw->HList;
    $hl->addchild("");
    pass("addchild with empty string");
    $hl->destroy;
}

TODO: {
    todo_skip "Currently dumps core", 1;

    my $hl = $mw->HList;
    $hl->add(0);
    eval { $hl->itemCreate(0, 0, -text => "Something", -data => "invalid") };
    like($@, qr{Bad option `-data' at}, "-data not valid for itemCreate");
}

{
    my $hl = $mw->HList(-columns => 2);
    $hl->headerCreate(0, -text => 'h1');
    is $hl->headerCget(0, '-text'), 'h1', 'headerCget call';
    eval { $hl->headerCget(1, '-text') };
    like $@, qr{Column "1" does not have a header}, 'Error message on headerCget call on column without a header';
    $hl->destroy;
}

1;
__END__

