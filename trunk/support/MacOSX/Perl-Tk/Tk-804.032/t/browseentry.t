# -*- perl -*-
BEGIN { $|=1; $^W=1; }
use strict;
use FindBin;
use lib $FindBin::RealBin;

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "1..0 # skip: no Test::More module\n";
	exit;
    }
}

use TkTest qw(catch_grabs);

plan tests => 22;

use_ok("Tk");
use_ok("Tk::BrowseEntry");

my $mw;
eval {$mw = Tk::MainWindow->new();};
eval { $mw->geometry('+10+10'); };
is($@, "", "can create MainWindow");
ok(Tk::Exists($mw), "MainWindow creation");

my(@listcmd, @browsecmd);
my $listcmd   = sub { @listcmd = @_ };
my $browsecmd = sub { @browsecmd = @_ };

my( $bla, $be );
eval { $be = $mw->BrowseEntry(-listcmd => $listcmd,
			  -browsecmd => $browsecmd,
			  -textvariable => \$bla,
				 )->pack; };
is("$@", "", "can create BrowseEntry");
ok(Tk::Exists($be), "BrowseEntry creation");

$be->insert('end', 1, 2, 3);
is($be->get(0), 1, "correct element in listbox");

$be->idletasks;
# this can "fail" if KDE screen save is up, or user is doing something
# else - such snags are what we should expect when calling binding
# methods directly ...
catch_grabs { $be->BtnDown } 0;
ok(@listcmd, "-listcmd");
ok($listcmd[0]->isa('Tk::BrowseEntry'), "1st argument in -listcmd");

my $listbox = $be->Subwidget('slistbox')->Subwidget('listbox');
ok($listbox->isa('Tk::Listbox'), "listbox subwidget");

$listbox->selectionSet(0);
$listbox->idletasks;
my($x, $y) = $listbox->bbox($listbox->curselection);
$be->LbChoose($x, $y);
is(@browsecmd, 2, "-browsecmd");
ok($browsecmd[0]->isa('Tk::BrowseEntry'),
   "1st argument in -browsecmd");
is($browsecmd[1], 1, "2nd argument in -browsecmd");

my $be2;
eval { $be2 = $mw->BrowseEntry(-choices => [qw/a b c d e/],
			   -textvariable => \$bla,
			   -state => "normal",
				  )->pack; };
is("$@", "", "create BrowseEntry");
ok(Tk::Exists($be2), "BrowseEntry creation");

catch_grabs {
    # Testcase:
    # From: "Puetz Kevin A" <PuetzKevinA AT JohnDeere.com>
    # Message-ID: <0B4BDC724143544EB509F90F7791EB64026EF8E1@edxmb16.jdnet.deere.com>
    my $var = 'val2';
    my $browse = $mw->BrowseEntry
	(-label => 'test',
	 -listcmd => sub { $_[0]->choices([undef, 'val1','val2']) },
	 -variable => \$var,
	)->pack;
    is($var, 'val2');
    $browse->update;
    $browse->BtnDown;
    $browse->update;
    is($var, 'val2');
    $browse->destroy;
} 2;

{
    # http://perlmonks.org/?node_id=590170
    my $active_text_color = "#000000";
    my $bgcolor = "#FFFFFF";
    my $text_font = 'helvetica 12';
    my $browse = $mw->BrowseEntry(-label=>'Try Me:',
				  -labelPack=>[qw(-side left -anchor w)],
				  -labelFont=>$text_font,
				  -labelForeground=>$active_text_color,
				  -labelBackground=>$bgcolor,
				  -width=>5,
				  -choices=>[qw(A B C)],
				 )->pack(-side=>'left', -expand=>1, -fill=>'x');
    my @children = $browse->children;
    is(scalar(@children), 3, "Auto-creation of Frame label");
    is((scalar grep { $_->isa("Tk::LabEntry") } @children), 1, "Has one LabEntry");
    is((scalar grep { $_->isa("Tk::Button")   } @children), 1, "Has one Button");
    is((scalar grep { $_->isa("Tk::Toplevel") } @children), 1, "Has one Toplevel");
    is((scalar grep { $_->isa("Tk::Label")    } @children), 0, "Has no Label");
}

#&Tk::MainLoop;

1;
__END__
