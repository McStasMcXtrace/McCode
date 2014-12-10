# Copyright (C) 2003,2006,2007,2010 Slaven Rezic. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.

package TkTest;

use strict;
use vars qw(@EXPORT @EXPORT_OK $eps $VERSION);
$VERSION = '4.009';

use base qw(Exporter);
@EXPORT    = qw(is_float is_float_pair checked_test_harness);
@EXPORT_OK = qw(catch_grabs wm_info);

sub _is_in_path ($);

use POSIX qw(DBL_EPSILON);
$eps = DBL_EPSILON;

sub checked_test_harness ($@) {
    my($skip_test, @test_harness_args) = @_;

    require ExtUtils::Command::MM;
    # In case of cygwin, use'ing Tk before forking (which is done by
    # Test::Harness) may lead to "remap" errors, which are normally
    # solved by the rebase or rebaseall utilities.
    #
    # Here, I just skip the DISPLAY check on cygwin to not force users
    # to run rebase.
    #
    if (!($^O eq 'cygwin' || $^O eq 'MSWin32')) {
	eval q{
           use blib;
           use Tk;
        };
	die "Strange: could not load Tk library: $@" if $@;
    }

    if (defined $Tk::platform && $Tk::platform eq 'unix') { # undef for cygwin+MSWin32, because Tk not yet loaded
	my $mw = eval { MainWindow->new() };
	if (!Tk::Exists($mw)) {
	    local @ARGV = $skip_test;
	    return ExtUtils::Command::MM::test_harness(@test_harness_args);
	}
	$mw->destroy;
    }

    return ExtUtils::Command::MM::test_harness(@test_harness_args);
}

sub is_float ($$;$) {
    my($value, $expected, $testname) = @_;
    local $Test::Builder::Level = $Test::Builder::Level+1;
    my @value    = split /[\s,]+/, $value;
    my @expected = split /[\s,]+/, $expected;
    my $ok = 1;
    for my $i (0 .. $#value) {
	if ($expected[$i] =~ /^[\d+-]/) {
	    if (abs($value[$i]-$expected[$i]) > $eps) {
		$ok = 0;
		last;
	    }
	} else {
	    if ($value[$i] ne $expected[$i]) {
		$ok = 0;
		last;
	    }
	}
    }
    if ($ok) {
	Test::More::pass($testname);
    } else {
	Test::More::is($value, $expected, $testname); # will fail
    }
}

sub is_float_pair ($$;$) {
    my($values, $expected, $testname) = @_;
    local $Test::Builder::Level = $Test::Builder::Level+1;
    for my $def ([0, "first value"],
		 [1, "second value"],
		) {
	my($inx, $testname_add) = @$def;
	is_float($values->[$inx], $expected->[$inx], (defined $testname ? "$testname " : "") . "($testname_add)");
    }
}

sub catch_grabs (&;$) {
    my($code, $tests) = @_;
    $tests = 1 if !defined $tests;
    my $tests_before = Test::More->builder->current_test;
    eval {
	$code->();
    };
    if ($@) {
	if ($@ !~ m{^grab failed: (window not viewable|another application has grab)}) {
	    die $@;
	} else {
	    Test::More::diag("Ignore grab problem: $@");
	}
    }
    my $tests_after = Test::More->builder->current_test;
    if ($tests_after - $tests_before != $tests) {
	for (1 .. $tests - ($tests_after - $tests_before)) {
	    Test::More::pass("Ignore test because other application had grab");
	}
    }
}

# Note that version guesses are done by issuing a
# <windowmanager --version> command. But there's no
# guarantee that the window manager executable in path
# is the same one as currently running. Especially it's
# possible that the window manager is not running at all
# on the same machine!
sub wm_info ($) {
    my $mw = shift;

    return () if $Tk::platform ne "unix";

    my $wm_name     = "<unknown>";
    my $wm_version  = "<unknown>";

    my($type,$windowid) = eval { $mw->property('get', '_NET_SUPPORTING_WM_CHECK', 'root') };
    if (defined $windowid) {
	($wm_name) = eval { $mw->property('get', '_NET_WM_NAME', $windowid) };
	if (!$wm_name) {
	    if (eval { $mw->property('get', '_WINDOWMAKER_NOTICEBOARD', $windowid); 1 }
		|| eval { $mw->property('get', '_WINDOWMAKER_ICON_TILE', $windowid); 1 }) {
		$wm_name = "WindowMaker";
		if (_is_in_path 'wmaker') {
		    my($maybe_wm_version) = `wmaker --version` =~ m{Window Maker\s+([\d\.]+)}i;
		    if ($maybe_wm_version) {
			$wm_version = "$maybe_wm_version (maybe)";
		    }
		}
	    } else {
		$wm_name = "<unknown> (property _NET_SUPPORTING_WM_CHECK exists, but getting _NET_WM_NAME fails)";
	    }
	} else {
	    $wm_name =~ s{\0}{}g; # trailing zero bytes seen with Xfwm4
	    if ($wm_name eq 'Metacity') {
		($wm_version) = eval { $mw->property('get', '_METACITY_VERSION', $windowid) };
	    } else {
		# just guess the VERSION property
		my($maybe_wm_version) = eval { $mw->property('get', '_'.$wm_name.'_VERSION', $windowid) };
		if ($maybe_wm_version) {
		    $wm_version = $maybe_wm_version;
		} else {
		    if ($wm_name eq 'FVWM') {
			if (_is_in_path 'fvwm') {
			    # -version is understood by both fvwm 2.4.x and 2.5.x
			    my($maybe_wm_version) = `fvwm -version` =~ m{fvwm\s+([\d\.]+)}i;
			    if ($maybe_wm_version) {
				$wm_version = "$maybe_wm_version (maybe)";
			    }
			}
		    } elsif ($wm_name eq 'KWin') {
			if (_is_in_path 'kwin') {
			    my($maybe_wm_version) = `kwin --version` =~ m{KWin:\s+([\d\.]+)}i;
			    if ($maybe_wm_version) {
				$wm_version = "$maybe_wm_version (maybe)";
			    }
			}
		    } elsif ($wm_name eq 'Xfwm4') {
			if (_is_in_path 'xfwm4') {
			    my($maybe_wm_version) = `xfwm4 --version` =~ m{xfwm4\s+version\s+([\d\.]+)}i;
			    if ($maybe_wm_version) {
				$wm_version = "$maybe_wm_version (maybe)";
			    }
			}
		    } elsif ($wm_name eq 'Fluxbox') {
			if (_is_in_path 'fluxbox') {
			    my($maybe_wm_version) = `fluxbox -v` =~ m{fluxbox\s+([\d\.]+)}i;
			    if ($maybe_wm_version) {
				$wm_version = "$maybe_wm_version (maybe)";
			    }
			}
			# fluxbox is also defining this property in the root window:
			# _BLACKBOX_PID(CARDINAL) = 69367
		    }
		}
	    }
	}
    } else {
	my($dtwm_integer) = eval { $mw->property('get', 'DTWM_IS_RUNNING', 'root') };
	if (defined $dtwm_integer) { # XXX really have to check this
                                     # integer, probably a Window id?
	    $wm_name = "dtwm";
	}
    }

    $wm_name =~ s{\0}{}g; # null byte at end seen in xfwm4 4.2.3.2

    (name    => $wm_name,
     version => $wm_version,
    );
}

# REPO BEGIN
# REPO NAME is_in_path /home/e/eserte/work/srezic-repository 
# REPO MD5 e18e6687a056e4a3cbcea4496aaaa1db

=head2 is_in_path($prog)

=for category File

Return the pathname of $prog, if the program is in the PATH, or undef
otherwise.

=cut

sub _is_in_path ($) {
    my($prog) = @_;
    require File::Spec;
    if (File::Spec->file_name_is_absolute($prog)) {
	if ($^O eq 'MSWin32') {
	    return $prog       if (-f $prog && -x $prog);
	    return "$prog.bat" if (-f "$prog.bat" && -x "$prog.bat");
	    return "$prog.com" if (-f "$prog.com" && -x "$prog.com");
	    return "$prog.exe" if (-f "$prog.exe" && -x "$prog.exe");
	    return "$prog.cmd" if (-f "$prog.cmd" && -x "$prog.cmd");
	} else {
	    return $prog if -f $prog and -x $prog;
	}
    }
    require Config;
    %Config::Config = %Config::Config if 0; # cease -w
    my $sep = $Config::Config{'path_sep'} || ':';
    foreach (split(/$sep/o, $ENV{PATH})) {
	if ($^O eq 'MSWin32') {
	    # maybe use $ENV{PATHEXT} like maybe_command in ExtUtils/MM_Win32.pm?
	    return "$_\\$prog"     if (-f "$_\\$prog" && -x "$_\\$prog");
	    return "$_\\$prog.bat" if (-f "$_\\$prog.bat" && -x "$_\\$prog.bat");
	    return "$_\\$prog.com" if (-f "$_\\$prog.com" && -x "$_\\$prog.com");
	    return "$_\\$prog.exe" if (-f "$_\\$prog.exe" && -x "$_\\$prog.exe");
	    return "$_\\$prog.cmd" if (-f "$_\\$prog.cmd" && -x "$_\\$prog.cmd");
	} else {
	    return "$_/$prog" if (-x "$_/$prog" && !-d "$_/$prog");
	}
    }
    undef;
}
# REPO END

1;

__END__
