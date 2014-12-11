# -*- perl -*-
BEGIN { $|=1; $^W=1; }
use strict;
use Tk;

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "# tests only work with installed Test::More module\n";
	print "1..1\n";
	print "ok 1\n";
	exit;
    }
}

BEGIN { plan tests => 94};

my $mw = Tk::MainWindow->new;
eval { $mw->geometry('+10+10'); };  # This works for mwm and interactivePlacement

my $scrl;
my $text;
{
   eval { require Tk::Text; };
   is($@, "", "loading Tk::Text");
   eval { $scrl = $mw->Scrolled('Text', -scrollbars=>'sw', -setgrid=>1); };
   is($@, "", "creating Scrolled('Text')");
   ok( Tk::Exists($scrl) );
   eval { $scrl->grid(-sticky => 'nw'); };
   is($@, "", 'managing Scrolled Text with grid');
   eval { $scrl->update; };
   is($@, "", 'update');
   eval { $text = $scrl->Subwidget('text'); };
   is($@, "", 'get subwidget text');
}
##
## -fg/-foreground was not propagated to Text widget until
## and including Tk800.003
##
{
    my ($oldcol, $txtcol);
    my $newcol = 'yellow';

    for my $opt ( qw(-fg -foreground -bg -background) )
      {
        eval { $oldcol = $scrl->cget($opt); };
        is($@, "", "cget $opt");

        isnt($oldcol, $newcol, "colors aren't already the same $oldcol=$newcol");

	## Set
        eval { $scrl->configure($opt=>$newcol); };
        is($@, "", "configure $opt => $newcol");
        eval { $txtcol = $text->cget($opt); };
        is($@, "", "text cget $opt");
        is($txtcol, $newcol, "$opt propagated to Text subwidget");
        $mw->update;

	## ReSet
        eval { $scrl->configure($opt=>$oldcol); };
        is($@, "", "Reset: configure $opt => $oldcol");
        eval { $txtcol = $scrl->cget($opt); };
        is($@, "", "Reset: text cget $opt");
        is($txtcol, $oldcol, "Reset scrolled $opt color");
        $mw->update;
      }
}
##
## Scrolled suppress size changes up to and including at least Tk800.003
## and including Tk800.004.  config/cget are okay but geometry uncovers it.
##
{
    my ($oldsize, $newsize, $oldgeo, $newgeo);

    for my $opt (qw(-height -width))
      {
        for my $chg (qw(-5 5))
          {
            eval { $oldsize = $scrl->cget($opt); };
            is($@, "", "Sizechk: cget $opt");
            eval { $oldgeo  = $scrl->geometry; };
            is($@, "", "Sizechk: geometry $opt");

	    ## Set
            eval { $scrl->configure($opt=>($oldsize+$chg)); };
            is($@, "", "configure $opt => $oldsize + $chg");
            eval { $mw->update; };
            is($@, "", "Sizechg: Error update configure $opt");
            eval { $newsize = $text->cget($opt); };
            is($@, "", "Sizechg: cget $opt");
            is($newsize, $oldsize+$chg, "No size change.");

	    # check if geometry has changed
            eval { $newgeo  = $scrl->geometry; };
            is($@, "", "Sizechk: new geometry $opt");
            is($newgeo, $oldgeo, "Sizechk: geometry is the same " .
		"($newgeo) for $opt => $oldsize+($chg)"
		);

	    ## ReSet
            eval { $scrl->configure($opt=>$oldsize); };
            is($@, "", "Reset size: configure $opt => $oldsize");
            eval { $mw->update; };
            eval { $newsize = $text->cget($opt); };
            is($@, "", "Reset size: text cget $opt");
            is($newsize, $oldsize, "Reset size: scrolled $opt ");
            is($@, "", "Sizechg: no error reset update configure $opt");
            eval { $newgeo  = $scrl->geometry; };
            is($@, "", "Sizechk: reset geometry $opt");

	    local $TODO = "Next one often fails - window stays same size but moves";
            # e.g. expect 589x341+0+32 get 589x341+17+32
            # tried changing -sticky above as a fix?
	    # Only seen on metacity, but not fvwm2 or twm
            is($newgeo, $oldgeo, "Sizechk: geometry has changed not reset" .
		" for $opt => $oldsize+($chg)"
		);
          }
      }
}

1;
__END__
