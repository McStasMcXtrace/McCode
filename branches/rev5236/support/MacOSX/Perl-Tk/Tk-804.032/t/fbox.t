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

use Getopt::Long;

use TkTest qw(catch_grabs);

plan tests => 15;

if (!defined $ENV{BATCH}) { $ENV{BATCH} = 1 }

use_ok("Tk");
use_ok("Tk::FBox");

my $top = new MainWindow;
eval { $top->geometry('+10+10'); }; # This works for mwm and interactivePlacement

my $f;

my $delay = 250;

GetOptions("delay=i" => \$delay)
    or die "usage: $0 [-delay ...ms]";

######################################################################
# open
eval {
    $f = $top->FBox(-defaultextension => ".PL",
		    -filetypes => [
				   ['Text Files',       ['.txt', '.text']],
				   ['TCL Scripts',      '.tcl'           ],
				   ['C Source Files',   '.c',      'TEXT'],
				   ['GIF Files',        '.gif',          ],
				   ['GIF Files',        '',        'GIFF'],
				   ['All Files',        '*',             ],
				  ],
		    -initialdir => ".",
		    -initialfile => "Makefile.PL",
		    -title => "Load file (with filetypes)",
		    -type => "open",
		    -filter => "*.PL", # ignored
		    -font => "Helvetica 14",
		   );
};
is($@, "", "creating Tk::FBox widget");

catch_grabs {
    destroy_if_visible($f) if $ENV{BATCH};
    my $result = $f->Show;
    if (!$ENV{BATCH}) {
	diag "Result is <$result>";
    }
    pass("After showing FBox")
} 1;

######################################################################
# open without filetypes
eval {
    $f = $top->FBox(-defaultextension => ".PL",
		    -initialdir => ".",
		    -initialfile => "Makefile.PL",
		    -title => "Load file (without filetypes, with filter)",
		    -type => "open",
		    -filter => "*.PL",
		    -font => "Helvetica 14",
		   );
};
is($@, "", "creating Tk::FBox widget");

catch_grabs {
    destroy_if_visible($f) if $ENV{BATCH};
    my $result = $f->Show;
    if (!$ENV{BATCH}) {
	diag "Result is <$result>";
    }
    pass("After showing FBox")
} 1;

######################################################################
# save
eval {
    $f = $top->FBox(-defaultextension => ".PL",
		    -filetypes => [
				   ['Text Files',       ['.txt', '.text']],
				   ['TCL Scripts',      '.tcl'           ],
				   ['C Source Files',   '.c',      'TEXT'],
				   ['GIF Files',        '.gif',          ],
				   ['GIF Files',        '',        'GIFF'],
				   ['All Files',        '*',             ],
				  ],
		    -initialdir => ".",
		    -initialfile => "Makefile.PL",
		    -title => "Save file",
		    -type => "save",
		    -filter => "*.PL", # ignored
		    -font => "Helvetica 14",
		   );
};
is($@, "", "creating Tk::FBox widget for save");

catch_grabs {
    destroy_if_visible($f) if $ENV{BATCH};
    my $result = $f->Show;
    if (!$ENV{BATCH}) {
	diag "Result is <$result>";
    }
    pass("After showing FBox");
} 1;

######################################################################
# dir
eval {
    $f = $top->FBox(-initialdir => ".",
		    -title => "Choose directory",
		    -type => "dir",
		    -font => "Helvetica 14",
		   );
};
is($@, "", "creating Tk::FBox widget for choosing directories");

catch_grabs {
    destroy_if_visible($f) if $ENV{BATCH};
    my $result = $f->Show;
    if (!$ENV{BATCH}) {
	diag "Result is <$result>";
    }
    pass("After showing FBox");
} 1;

######################################################################
# getOpenFile etc.
TODO: {
## XXX works everywhere?
#     skip("getOpenFile etc. only on X11", 3)
# 	if $Tk::platform ne 'unix';
    todo_skip("known coredumps with multiple MainWindows on some systems", 5)
	if 1;

    catch_grabs {
	my $mw = MainWindow->new;
	$mw->geometry('+0+0');
	$mw->after($delay, sub { $mw->destroy }) if $ENV{BATCH};
	my $result = $mw->getOpenFile;
	if (!$ENV{BATCH}) {
	    diag "Result is <$result>";
	}
	pass("called getOpenFile");
    };

    catch_grabs {
	my $mw = MainWindow->new;
	$mw->geometry('+0+0');
	$mw->after($delay, sub { $mw->destroy }) if $ENV{BATCH};
	my $result = $mw->getSaveFile;
	if (!$ENV{BATCH}) {
	    diag "Result is <$result>";
	}
	pass("called getSaveFile");
    };

    catch_grabs {
	my $mw = MainWindow->new;
	$mw->geometry('+0+0');
	$mw->after($delay, sub { $mw->destroy }) if $ENV{BATCH};
	my $result = $mw->chooseDirectory;
	if (!$ENV{BATCH}) {
	    diag "Result is <$result>";
	}
	pass("called chooseDirectory");
    };

    catch_grabs {
	my $mw = MainWindow->new;
	$mw->geometry('+0+0');
	$mw->after($delay, sub { $mw->destroy }) if $ENV{BATCH};
	my $result = $mw->getOpenFile(-multiple => 1, -title => "getOpenFile with -multiple");
	ok(!defined $result || ref($result) eq "ARRAY", "Result of -multiple is an array reference or undef");
	if (!$ENV{BATCH}) {
	    diag "Result is <@$result>" if $result;
	}
	pass("called getOpenFile with -multiple");
    };
}

# Tk::FBox is internally using waitVisibility. The test may however
# try to destroy the window before the visibility ever changed,
# leading to errors like
#
#     window ".fbox" was deleted before its visibility changed
#
# To prevent this, the <Visibility> events are trapped. If there
# was no such event when the window is about to be destroyed,
# then the script waits another second, up to a maximum of
# 10 seconds.
sub destroy_if_visible {
    my $w = shift;
    my $visibility_changed = 0;
    $w->bind('<Visibility>' => sub { $visibility_changed = 1 });
    my $trials = 0;
    my $destroy_if_visibility_changed;
    $destroy_if_visibility_changed =
	sub {
	    if ($visibility_changed) {
		$w->destroy;
	    } else {
		$trials++;
		if ($trials > 10) {
		    diag "Window never got visible, destroying nevertheless...";
		    $w->destroy;
		} else {
		    if ($trials == 1) {
			diag "Slow delivery of <Visibility> event, waiting...";
		    }
		    $w->after(1000, $destroy_if_visibility_changed);
		}
	    }
	};
    $w->after($delay, $destroy_if_visibility_changed);
}

1;
__END__
