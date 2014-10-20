#!/usr/bin/perl -w
# -*- perl -*-

#
# Author: Slaven Rezic
#

use strict;
use FindBin;
use lib $FindBin::RealBin;

use Encode qw(encode);
use File::Copy qw(cp);
use File::Spec::Functions qw(catfile);
use File::Temp qw(tempdir);

use Tk;
use Tk::FBox;

BEGIN {
    if (!eval q{
	use Test::More;
	use Devel::Peek;
	1;
    }) {
	print "1..0 # skip: no Test::More module\n";
	exit;
    }
}

use TkTest qw(catch_grabs);

if (!defined $ENV{BATCH}) { $ENV{BATCH} = 1 }

my $dir = tempdir(CLEANUP => 1);
die "Cannot create temporary directory" if !$dir;

my $encoding = "iso-8859-1";
if ($^O eq 'darwin') {
    $encoding = 'utf-8';
}

# Preparations. May fail on some systems, see
# https://rt.cpan.org/Ticket/Display.html?id=75347
my($umlautdir, $umlautgif, $umlautxpm, $umlautxbm, $eurogif);
eval {
    $umlautdir = catfile $dir, encode($encoding, "äöüß");
    mkdir $umlautdir
	or die "Cannot create $umlautdir: $!";

    $umlautgif = catfile $umlautdir, encode($encoding, "äöüß.gif");
    cp(Tk->findINC("Xcamel.gif"), $umlautgif)
	or die "Can't copy Xcamel.gif to $umlautgif: $!";

    $umlautxpm = catfile $umlautdir, encode($encoding, "èé.xpm");
    cp(Tk->findINC("Camel.xpm"), $umlautxpm)
	or die "Can't copy Camel.xpm to $umlautxpm: $!";
    utf8::upgrade($umlautxpm); # upgrade to utf8 it before Perl/Tk...

    $umlautxbm = catfile $umlautdir, encode($encoding, "ÈÉ.xbm");
    cp(Tk->findINC("Tk.xbm"), $umlautxbm)
	or die "Can't copy Tk.xbm to $umlautxbm: $!";

    $eurogif = catfile $umlautdir, "\xe2\x82\xac.gif"; # the utf8-representation of \x{20ac} (Euro sign)
    cp(Tk->findINC("Xcamel.gif"), $eurogif)
	or die "Can't copy Xcamel.gif to $eurogif: $!";
};
if ($@) {
    plan skip_all => $@;
    exit 0;
}

plan tests => 13;

my $mw = tkinit;
$mw->geometry("+10+10");

######################################################################
# Various image formats
{
    my $p = eval { $mw->Photo(-file => $umlautgif) };
    is($@, "", "Create a photo with non-ascii chars in filename");
    $p->delete if $p;
}

{
    my $p = eval { $mw->Pixmap(-file => $umlautxpm) };
    is($@, "", "Create a pixmap with non-ascii chars in filename");
    $p->delete if $p;
}

{
    my $p = eval { $mw->Bitmap(-file => $umlautxbm) };
    is($@, "", "Create a bitmap with non-ascii chars in filename");
    $p->delete if $p;
}

{
    my $p = eval { $mw->Photo(-file => $eurogif) };
    is($@, "", "Create a photo with chars > 0xff in filename");
    $p->delete if $p;
}

######################################################################
# Bitmaps from file
{
    my $l = eval { $mw->Label(-bitmap => '@' . $umlautxbm) };
    is($@, "", "Create a widget with bitmap from filename with non-ascii chars");
    if (Tk::Exists($l)) {
	$l->pack;
	$l->update;
	$l->after(100);
	$l->destroy;
    }
}

######################################################################
# File box
catch_grabs {
    my $fb = $mw->FBox;
    $fb->configure(-initialdir => $umlautdir);
    $fb->after(500, sub { $fb->destroy }) if $ENV{BATCH};
    my $value = $fb->Show;
    Dump($value) if defined $value;
    pass("Setting FBox -initialdir with non-ascii directory name");
} 1;

catch_grabs {
    my $fb = $mw->FBox;
    $fb->configure(-initialfile => $umlautgif);
    $fb->after(500, sub { $fb->destroy }) if $ENV{BATCH};
    my $value = $fb->Show;
    Dump($value) if defined $value;
    pass("Setting FBox -initialfile with non-ascii file name");
} 1;

######################################################################
# Text
{
    my @warnings;
    my $t = $mw->Text->pack;
    $t->insert("end", "\xfc" x 20);
    $t->markSet('insert','end');
    $t->focusForce;
    $t->update;

    {
	@warnings = ();
	local $SIG{__WARN__} = sub { push @warnings, @_ };
	$t->eventGenerate('<Control-KeyPress>', -keysym=>'Left');
    }
    is("@warnings", "", "No utf-8 warnings");
    is($t->index("insert"), "1.0", "Text: left word movement");

    {
	@warnings = ();
	local $SIG{__WARN__} = sub { push @warnings, @_ };
	$t->eventGenerate('<Control-KeyPress>', -keysym=>'Right');
    }
    is("@warnings", "", "No utf-8 warnings");
    is($t->index("insert"), "1.20", "Text: right word movement"); # XXX 1.20 correct?

    $t->destroy;
}

{
    local $TODO = "Fix utf-8 warnings+errors in Text widget";

    my @warnings;
    my $t = $mw->Text->pack;
    $t->insert("end", "\xfc" x 20);
    $t->markSet('anchor',$t->index("current -1c"));
    {
	@warnings = ();
	local $SIG{__WARN__} = sub { push @warnings, @_ };
	is($t->index("1.19 wordstart"), "1.0");
    }
    is("@warnings", "", "No utf-8 warnings");
    $t->destroy;
}

#MainLoop;

__END__
