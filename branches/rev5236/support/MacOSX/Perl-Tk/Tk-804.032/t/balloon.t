# -*- perl -*-
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

plan tests => 14;

# Run
#    env BATCH=0 perl -Mblib t/balloon.t
# for interactive balloon test
if (!defined $ENV{BATCH}) { $ENV{BATCH} = 1 }

my $mw = Tk::MainWindow->new;
eval { $mw->geometry('+10+10'); };  # This works for mwm and interactivePlacement

my $statusbar = $mw->Label->pack;

my $balloon;
eval { require Tk::Balloon; };
is($@, "", 'Loading Tk::Balloon');
eval { $balloon = $mw->Balloon; };
is($@, "", 'Creating Balloon widget');
ok( Tk::Exists($balloon), "Existance of ballon" );

my $l = $mw->Label->pack;
eval { $balloon->attach($l, -msg => "test"); };
is($@, "", 'Attaching message to Label widget');
eval { $balloon->attach($l, -statusmsg => "test1", -balloonmsg => "test2"); };
is($@, "", 'Attaching statusmsg/baloonmsg to Label widget');

{
    my $c = $mw->Canvas(-width => 100, -height => 50)->pack;
    my $ci = $c->createLine(0,0,10,10);
    eval { $balloon->attach($c, -msg => {$ci => "test"}); };
    is($@, "", 'Attaching message to Canvas item');
}

{
    my $c = $mw->Scrolled("Canvas", -width => 100, -height => 50)->pack;
    my $ci = $c->createLine(0,0,10,10);
    eval { $balloon->attach($c, -msg => {$ci => "test"}); };
    is($@, "", 'Attaching message to scrolled Canvas item');
}

my $menubar = $mw->Menu;
$mw->configure(-menu => $menubar);
my $filemenu = $menubar->cascade(-label => "~File", -tearoff => 0);
$filemenu->command(-label => "Test1");
$filemenu->command(-label => "Test2");
$filemenu->command(-label => "Test3");
my $filemenu_menu = $filemenu->cget(-menu);

eval { $balloon->attach($filemenu_menu,
			-msg => ["Test1 msg", "Test2 msg", "Test3 msg"]); };
is($@, "", 'Attaching message to Menu');

eval { $balloon->configure(-motioncommand => \&motioncmd); };
is($@, "", "Set motioncommand option");
eval { $balloon->configure(-motioncommand => undef); };
is($@, "", "Reset motioncommand option");

{
    my $lb = $mw->Listbox(-height => 6)->pack;
    $lb->insert("end",1,2,3,4);
    eval { $balloon->attach($lb, -msg => ['one','two','three','four']); };
    is($@, "", 'Attaching message to Listbox items');
}

{
    my $slb = $mw->Scrolled('Listbox', -height => 2)->pack;
    $slb->insert("end",1,2,3,4);
    eval { $balloon->attach($slb,
			    -msg => ['one','two','three','four']); };
    is($@, "", 'Attaching message to scrolled Listbox items');
}

{
    require Tk::NoteBook;
    my $nb = $mw->NoteBook->pack;
    for (1..4) {
	my $p = $nb->add("page$_", -label => "Page$_");
	$p->Label(-text => "Page $_")->pack;
    }
    $balloon->attach($nb,
		     -balloonposition => 'mouse',
		     -msg => { page1 => "This is page1",
			       page2 => "This is page2",
			       page3 => "This is page3",
			       page4 => "This is page4",
			     });
    pass("Attached hash ballooninfo to NoteBook");
}

{
    my $nb = $mw->NoteBook->pack;
    for (1..4) {
	my $p = $nb->add("page$_", -label => "Page$_");
	$p->Label(-text => "Page $_")->pack;
    }
    $balloon->attach($nb,
		     -balloonposition => 'mouse',
		     -msg => "Balloon applies to whole notebook",
		    );
    pass("Attached scalar ballooninfo to NoteBook");
}

## not yet:
#  $l->eventGenerate("<Motion>");
#  sub motioncmd {
#      my(@args) = @_;
#      warn "<<<@args";
#  }

MainLoop if !$ENV{BATCH};

sub motioncmd { warn "dummy motioncommand\n" }

__END__
