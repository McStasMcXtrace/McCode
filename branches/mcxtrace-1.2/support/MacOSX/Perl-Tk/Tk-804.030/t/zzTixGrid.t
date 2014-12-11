# -*- perl -*-
BEGIN { $^W=1; $|=1; }

use strict;
use Test;
use Tk;

BEGIN { plan tests => 33,
#       todo => [18,26,32]
      };

my $mw = Tk::MainWindow->new;
eval { $mw->geometry('+10+10'); };  # This works for mwm and interactivePlacement

my $tixgrid;
{
   eval { require Tk::TixGrid; };
   ok($@, "", 'Problem loading Tk::TixGrid');
   eval { $tixgrid = $mw->TixGrid(); };
   ok($@, "", 'Problem creating TixGrid widget');
   ok( Tk::Exists($tixgrid) );
   eval { $tixgrid->grid; };
   ok($@, "", '$tixgrid->grid problem');
}
##
## TixGrid->nearest gives always a 'TCL panic' if tixgrid is visible in Tk800.003
##
## ptksh> p $tg->nearest(10,10)
## No results
## Tcl_Panic at (eval 7) line 1.
##
{
    my @entry;
    eval { @entry = $tixgrid->nearest(10,10); };  # there should be no entry
    ok($@ eq "");
    ok(
	scalar(@entry),
	0,
        "nearest returned array of size " . @entry . " instead of 0. " .
    	join('|','@entry=', @entry,'')
    );

    ## Make widget visible, nearest -> SEGV
    $tixgrid->update;
    eval { @entry = $tixgrid->nearest(10,10); };  # there should be no entry
    ok($@ eq "");

}
##
## Tk800.004: selectionSet ignores -selectunit
## selectionClear also does not work.
##
{
    my $g = $mw->TixGrid->grid;

    # populate
    eval
      {
        for my $x (0..10)
          {
	    for my $y (0..10)
              {
	        $g->set($x,$y, -itemtype=>'text', -text=>"($x,$y)" );
	      }
          }
      };
    ok($@, "", "Problem populate TixGrid with items");

    my $b = '';

    # test column selection
    eval { $g->configure(-selectunit=>'column'); };   ok($@, "", "Problem col configure -selectunit=>column");
    eval { $g->selection('set', 1,1); };              ok($@, "", "problem col set selection");
    eval { $b=$g->selection('includes', 1,1); };      ok($@, "", "problem selection includes");
    eval { $g->update; };                             ok($@, "", "problem col update");

    eval { $b=$g->selection('includes', 1,1); };      ok($@, "", "problem selection includes");
    ok($b, 1,  "oops col selection does not contain the item");
    eval { $b=$g->selection('includes', 1,0=>1,10);}; ok($b, 1,  "oops col selection does not contain the col");
    eval { $g->update; };                             ok($@, "", "problem col update");

    eval { $g->selectionClear(1,5); };                ok($@, "", "problem col sel clear");
#   eval { $b=$g->selection('includes', 1,1); };      ok($b, "",  "oops col selection is not cleared");

    # test row selection
    eval { $g->configure(-selectunit=>'row'); };      ok($@, "", "Problem row configure -selectunit=>row");
    eval { $g->selection('set', 2,2); };              ok($@, "", "problem row set selection");
    eval { $g->update; };                             ok($@, "", "problem row update");
    eval { $b=$g->selection('includes',2,2); };       ok($b, 1,  "oops row selection does not contain the item");
    eval { $b=$g->selection('includes',0,2=>10,2); }; ok($b, 1,  "oops row selection does not contain the row");
    eval { $g->update; };                             ok($@, "", "problem row update");
    eval { $g->selectionClear(5,2); };                ok($@, "", "problem row sel clear");
#   eval { $b=$g->selection('includes', 2,2); };      ok($b, "",  "oops row selection is not cleared");

    # test cell selection
    eval { $g->configure(-selectunit=>'cell'); };     ok($@, "", "Problem cell configure -selectunit=>cell");
    eval { $g->selection('set', 3,3); };              ok($@, "", "problem cell set selection");
    eval { $b=$g->update; };                          ok($@, "", "problem cell update");
    eval { $b=$g->selection('includes', 3,3); };      ok($b, 1,  "oops cell selection does not contain the item");
    eval { $b=$g->selection('includes', 2,3); };      ok($b, 0, "oops cell selection contain a not selected item");
#   eval { $b=$g->selection('includes', 2,2); };      ok($b, 0, "oops cell selection contain a not selected item");
    eval { $b=$g->selection('includes', 4,3); };      ok($b, 0, "oops cell selection contain a not selected item");
    eval { $b=$g->selection('includes', 3,4); };      ok($b, 0, "oops cell selection contain a not selected item");
    eval { $g->update; };                             ok($@, "", "problem cell update");
    eval { $g->selectionClear(3,3); };                ok($@, "", "problem cell sel clear");
#   eval { $b=$g->selection('includes', 3,3); };      ok($b, 0, "oops cell selection not cleared");
}

1;
__END__

