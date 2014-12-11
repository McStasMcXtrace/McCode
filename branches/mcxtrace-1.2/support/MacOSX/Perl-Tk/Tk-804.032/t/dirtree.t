#!/usr/bin/perl -w
# -*- perl -*-

use strict;

use Tk;

BEGIN {
    if (!eval q{
	use Cwd;
	use File::Spec;
	use Test::More;
	1;
    }) {
	print "1..0 # skip: no Test::More, Cwd and/or File::Spec modules\n";
	exit;
    }
}

plan tests => 8;

if (!defined $ENV{BATCH}) { $ENV{BATCH} = 1 }

use_ok('Tk::DirTree');

my $mw = new MainWindow;
$mw->geometry("+10+10");
$mw->Button(
            -text => 'exit',
            -command => sub { $mw->destroy; },
           )->pack(qw( -side bottom -pady 6 ));
my $f = $mw->Scrolled('DirTree',
                      -width => 55,
                      -height => 33,
                      -directory => File::Spec->rootdir(),
		      -browsecmd => sub {
			  my $currdir = shift;
			  diag "You are browsing through <$currdir>";
		      },
                     )->pack(qw( -fill both -expand 1 ));
pass('after create, with -directory option');
my $tree = $f->Subwidget('scrolled');
isa_ok($tree, 'Tk::DirTree');

my $testdir;
if (eval { require File::Temp; require File::Spec; 1 }) {
    $testdir = File::Temp::tempdir("dirtree-XXXXXX", TMPDIR => 1, CLEANUP => 1);
    if ($testdir) {
	for my $chr (65..90, 97..122, 192..255) {
	    my $testsubdir = File::Spec->catfile($testdir, chr($chr));
	    mkdir $testsubdir, 0777;
	}
    }
}
$testdir = cwd if !$testdir;

$f->configure(-directory => $testdir);
$mw->update;
pass("After setting directory to " . $testdir);

{
    my $d = $mw->DirTreeDialog(-initialdir => cwd);
    isa_ok($d, "Tk::DirTreeDialog");
    $mw->after(100, sub { $d->{ok} = 1 });
    my $got_dir = $d->Show;
    is($got_dir, cwd, "DirTreeDialog returned expected directory");
}

if ($ENV{BATCH}) {
    $mw->after(300, sub { $mw->destroy });
}
pass('before MainLoop');
MainLoop;
pass('after MainLoop');

__END__
