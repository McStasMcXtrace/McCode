# -*- perl -*-
BEGIN { $|=1; $^W=1; }
use strict;
use Test;

BEGIN { plan test => 6 };

eval { require Tk };
ok($@, "", "loading Tk module");

eval { require Tk::FileSelect };
ok($@, "", "loading Tk::FileSelect module");

my $top = new MainWindow;
eval { $top->geometry('+10+10'); };  # This works for mwm and interactivePlacement
my $f = $top->FileSelect;
ok($f->cget('-filter'), "*", "filter not equal *");

$f = $top->FileSelect(-defaultextension => 'c');
ok($f->cget('-filter'), "*.c", "filter/defaultextension mismatch");

$f = $top->FileSelect(-filter => '*.h');
ok($f->cget('-filter'), "*.h", "filter not equal *.h");

if (eval { require File::Temp; 1 }) {
    my $tempdir = File::Temp::tempdir(TMPDIR => 1, CLEANUP => 1);
    my $tempfile = "$tempdir/bla'foo";
    open FH, "> $tempfile"
	or die "Cannot create $tempfile: $!";
    close FH; # just touch

    my $fs = $top->FileSelect(-directory => $tempdir);
    my $tries = 0;
    my $slow_machine = 0;
    my $fs_Accept;
    $fs_Accept = sub {
	if ($tries > 10) {
	    warn "Too many retries, maybe machine is too slow...";
	    $slow_machine = 1;
	    $fs->Accept;
	} else {
	    my $file_list = $fs->Subwidget("file_list");
	    if (!Tk::Exists($file_list) || !$file_list->viewable) {
		$tries++;
		$fs->after(100, $fs_Accept);
	    } else {
		$file_list->selectionSet(0);
		$fs->Accept;
	    }
	}
    };
    $fs->after(100, $fs_Accept);
    my $res = $fs->Show;
    if ($slow_machine) {
	ok(1);
    } else {
	if ($res eq $tempfile) {
	    ok(1);
	} else {
	    # On MacOSX /tmp is hardlinked to /private/tmp. Check
	    # stat() output to determine if it's the same file.
	    my($dev_ino_tempfile) = join(" ", ((stat($tempfile))[0,1]));
	    my($dev_ino_res)      = join(" ", ((stat($res))     [0,1]));
	    ok($dev_ino_res, $dev_ino_tempfile, "device/inode of tempfile and result not the same");
	}
    }
} else {
    ok(1); # skipping, not File::Temp
}

1;

__END__
